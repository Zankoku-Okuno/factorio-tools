-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- {-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main
  ( main
  -- * Factories
  , Factory(..)
  , Process(..)
  , Recipe(..)
  , Building(..)
  , Fuel(..)
  , Module(..)
  -- * Throughput
  , Thruputs
  , Flux(..)
  , normalizeFlux
  -- ** Constructors
  , zeroThruput
  , perSecond
  , perMinute
  , yellowBelts
  , redBelts
  , blueBelts
  -- Stack-dependent Throughput
  , StackThruput(..)
  , cargoWagonsPerMinute
  , fluidWagonsPerMinute
  ) where

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Map (Map)
import GHC.Records.Compat (HasField(..))

import qualified Data.Map as Map


main :: IO ()
main = do
  let spm = 220 & perMinute
  putStrLn $ "items/s: " ++ show spm.perSecond
  putStrLn $ "items/m: " ++ show spm.perMinute
  putStrLn $ "yellow belts: " ++ show spm.ybelts
  let plates = 2 & cargoWagonsPerMinute 100
  putStrLn $ "plates: " ++ show plates.perMinute
  putStrLn $ "------------"
  let p = Process
          { recipe = advancedOil
          , building = OilRefinery
          , modules = []
          , beaconModules = []
          }
      fact = Whitelist ["ethylene"] $ Scale 5 $ Atom p
  putStrLn $ "Flux for AdvOil: " ++ show (flux fact)
  let cable3 = Process
         { recipe = copperCable
         , building = Assembler 1
         , modules = [], beaconModules = []
         }
      gc2 = Process
         { recipe = greenCircuits
         , building = Assembler 1
         , modules = [], beaconModules = []
         }
      gcChain = Sequence [Scale 3 $ Atom cable3, Scale 2 $ Atom gc2]
  putStrLn $ "Flux for std GreenCirc factory: " ++ show (normalizeFlux . flux $ gcChain)
  let kovarexP = Process
        { recipe = kovarex
        , building = Centrifuge
        , modules = [], beaconModules = []
        }
  putStrLn $ "Flux for Kovarex: " ++ show (normalizeFlux . flux $ SelfLoop $ Scale 60 $ Atom kovarexP)



------------------------------------

-- the `Factory` type is the "langauge" for specifying production facilities in groups
-- the lowest-level factories are `Process` types
--   these hold the recipe they are working on, the properties of the building making it, and the modules installed/beaconed

-- from a Process, basic input/output can be calculated
-- other `Factory`s just combine these
-- TODO I want a "Requirements" type to specify max input/min output
--   these limits can be compared with a factory to check for deficit/surplus input and excess output
-- TODO one Factory ctor should be a graph data structure that connects different factoriy's i/o

data Factory
  = Atom Process
  | Scale Double Factory
  | Sum [Factory]
  | Whitelist [String] Factory
  | Sequence [Factory] -- outputs from one factory can take load off the inputs of the next
  | SelfLoop Factory -- the outputs of this factory can take load off its own inputs

data Process = Process
  { recipe :: Recipe
  , building :: Building
  , modules :: [Module]
  , beaconModules :: [Module]
  }

data Recipe = Recipe
  { inputs :: Thruputs
  , outputs :: Thruputs
  , time :: Double
  -- , producedBy :: [String] -- TODO to help me out validating recipe-building pairs
  }

data Building
  -- Extractors
  = BurnerMiner
    { fuel :: Fuel
    }
  | ElectricMiner
  | OffshorePump
  | Pumpjack
  -- Smelters
  | FueledFurnace
    { tier :: Int -- TODO limit this to 1-2
    , fuel :: Fuel
    }
  | ElectricFurnace
  -- Construction
  | Assembler
    { tier :: Int -- TODO limit this to 1-3
    }
  | OilRefinery
  | ChemicalPlant
  | Centrifuge

data Fuel
  = Wood
  | Coal
  | SoldFuel
  | RocketFuel
  | NuclearFuel

data Module
  = Energy Int -- TODO fix a number 1-3
  | Production Int
  | Speed Int


------------------------------------

-- essentially, a mathematical vector in the space of all items/fluids that can be produced
data Thruputs = Thruputs (Map String Thruput)
  deriving (Show)

addThrus :: Thruputs -> Thruputs -> Thruputs
addThrus (Thruputs xs) (Thruputs ys) = Thruputs $ Map.unionWith (+) xs ys

subThrus :: Thruputs -> Thruputs -> Thruputs
subThrus (Thruputs xs) (Thruputs ys) = Thruputs $ Map.unionWith (-) xs ys

scaleThrus :: Double -> Thruputs -> Thruputs
scaleThrus a (Thruputs xs) = Thruputs $ Map.map ((a & perSecond) *) xs

filterThrus :: [String] -> Thruputs -> Thruputs
filterThrus whitelist (Thruputs xs) = Thruputs $ xs `Map.intersection` whitemap
  where
  whitemap = Map.fromList [(item, 0) | item <- whitelist]

thruputsFromList :: [(String, Double)] -> Thruputs
thruputsFromList = Thruputs . Map.fromList . map (second perSecond)

intersectThrus :: Thruputs -> Thruputs -> Thruputs
intersectThrus (Thruputs a) (Thruputs b) = Thruputs $ Map.intersectionWith min a b

data Flux = Flux
  { inputs :: Thruputs
  , outputs :: Thruputs
  -- TODO electricity, pollution
  }
  deriving (Show)

normalizeFlux :: Flux -> Flux
normalizeFlux fx = Flux
  { inputs = normThrus fx.inputs
  , outputs = normThrus fx.outputs
  }
  where
  normThrus (Thruputs x) = Thruputs $ Map.filter (/= 0) x

zeroFlux :: Flux
zeroFlux = Flux
  { inputs = Thruputs Map.empty
  , outputs = Thruputs Map.empty
  }

addFlux :: Flux -> Flux -> Flux
addFlux x y = Flux
  { inputs = addThrus x.inputs y.inputs
  , outputs = addThrus x.outputs y.outputs
  }

scaleFlux :: Double -> Flux -> Flux
scaleFlux a x = Flux
  { inputs = scaleThrus a x.inputs
  , outputs = scaleThrus a x.outputs
  }

filterFlux :: [String] -> Flux -> Flux
filterFlux whitelist x = x{outputs = filterThrus whitelist x.outputs}

-- TODO processFlux, flux also need info about global bonuses
processFlux :: Process -> Flux
processFlux proc = Flux
  { inputs = timeEffects $ proc.recipe.inputs
  -- TODO module effects
  , outputs = timeEffects $ proc.recipe.outputs
  }
  where
  timeEffects = scaleThrus (craftTimeEffect * buildingSpeedEffects) -- TODO module effects
  craftTimeEffect = 1/proc.recipe.time
  buildingSpeedEffects = case proc.building of
    BurnerMiner{} -> 0.25
    ElectricMiner -> 0.5
    OffshorePump -> 1
    Pumpjack -> 1
    FueledFurnace{tier} -> case tier of { 1 -> 1; 2 -> 2 }
    ElectricFurnace -> 2
    Assembler{tier} -> case tier of { 1 -> 0.5; 2 -> 0.75; 3 -> 1.25 }
    OilRefinery -> 1
    ChemicalPlant -> 1
    Centrifuge -> 1

flux :: Factory -> Flux
flux (Atom proc) = processFlux proc
flux (Scale a fact) = scaleFlux a (flux fact)
flux (Sum facts) = foldr addFlux zeroFlux (flux <$> facts)
flux (Whitelist items fact) = filterFlux items (flux fact)
flux (Sequence facts) = foldl go zeroFlux facts
  where
  go fx (flux -> fx') =
    let shared = intersectThrus fx.outputs fx'.inputs
     in Flux
        { inputs = fx.inputs `addThrus` (fx'.inputs `subThrus` shared)
        , outputs = (fx.outputs `subThrus` shared) `addThrus` fx'.outputs
        }
flux (SelfLoop fact) =
  let fx = flux fact
      shared = intersectThrus fx.inputs fx.outputs
   in Flux
      { inputs = fx.inputs `subThrus` shared
      , outputs = fx.outputs `subThrus` shared
      }



-- TODO performance requirements for factories (max input/min output requirements)

data Performance = Performance
  { surplusInput :: Thruputs
  , excessOutput :: Thruputs
  }

-- TODO Requirements -> Flux -> Performance

------------------------------------

newtype Thruput = Thruput { _perSecond :: Double }
  deriving
    ( Eq, Ord, Enum
    , Num, Floating, Fractional, Real, RealFloat, RealFrac
    , Read, Show
    )

zeroThruput :: Thruput
zeroThruput = Thruput 0

perSecond :: Double -> Thruput
perSecond = Thruput

perMinute :: Double -> Thruput
perMinute = Thruput . (/60.0)

yellowBelts :: Double -> Thruput
yellowBelts = Thruput . (*ybeltThru)
redBelts :: Double -> Thruput
redBelts = Thruput . (*rbeltThru)
blueBelts :: Double -> Thruput
blueBelts = Thruput . (*bbeltThru)

cargoWagonsPerMinute :: Int -> Double -> Thruput
cargoWagonsPerMinute stackSize = perMinute . (* cargoWagonSize stackSize)
fluidWagonsPerMinute :: Double -> Thruput
fluidWagonsPerMinute = perMinute . (*fluidWagonSize)

instance HasField "perSecond" Thruput Double where
  hasField x = (Thruput, _perSecond x)
instance HasField "perMinute" Thruput Double where
  hasField x = (\perMinute -> Thruput (perMinute / 60.0), x.perSecond * 60.0)


instance HasField "ylanes" Thruput Double where
  hasField x = (\ylanes -> Thruput (ylanes * ylaneThru), x.perSecond / ylaneThru)
instance HasField "ybelts" Thruput Double where
  hasField x = (\ybelts -> Thruput (ybelts * ybeltThru), x.perSecond / ybeltThru)
instance HasField "rlanes" Thruput Double where
  hasField x = (\rlanes -> Thruput (rlanes * rlaneThru), x.perSecond / rlaneThru)
instance HasField "rbelts" Thruput Double where
  hasField x = (\rbelts -> Thruput (rbelts * rbeltThru), x.perSecond / rbeltThru)
instance HasField "blanes" Thruput Double where
  hasField x = (\blanes -> Thruput (blanes * blaneThru), x.perSecond / blaneThru)
instance HasField "bbelts" Thruput Double where
  hasField x = (\bbelts -> Thruput (bbelts * bbeltThru), x.perSecond / bbeltThru)

-- TODO inserter throughputs

-- FIXME I'm not too happy with a second thruput type
-- perhaps I should always specify the item with the thruput
data StackThruput = ItemThru
  { stackSize :: Int
  , thruput :: Thruput
  }

-- instance HasField "cargoWagonsPerMinute" StackThruput (Int, Double) where
--   hasField x = let itemsPerWagon = cargoWagonSize x.stackSize in
--     (\wagons -> x{thruput.perMinute = wagons * itemsPerWagon}, x.perMinute / (wagons * itemsPerWagon))
-- instance HasField "fluidWagonsPerMinute" Thruput (Int -> Double) where
--   hasField x (\wagons -> x{perMinute = wagons * fluidWagonSize}, x.perMinute / fluidWagonSize)

ybeltThru, rbeltThru, bbeltThru :: Double
ybeltThru = 15.0
rbeltThru = 30.0
bbeltThru = 45.0

ylaneThru, rlaneThru, blaneThru :: Double
ylaneThru = ybeltThru / 2.0
rlaneThru = rbeltThru / 2.0
blaneThru = bbeltThru / 2.0

cargoWagonSize :: Int -> Double
cargoWagonSize maxStackSize = 40 * fromIntegral maxStackSize
fluidWagonSize :: Double
fluidWagonSize = 25e3

------------------------------------

copperCable :: Recipe
copperCable = Recipe
  { inputs = thruputsFromList ["copper-plate" .: 1]
  , outputs = thruputsFromList ["copper-cable" .: 2]
  , time = 0.5
  }

greenCircuits :: Recipe
greenCircuits = Recipe
  { inputs = thruputsFromList ["copper-cable" .: 3, "iron-plate" .: 1]
  , outputs = thruputsFromList ["green-circuits" .: 1]
  , time = 0.5
  }

advancedOil :: Recipe
advancedOil = Recipe
  { inputs = thruputsFromList ["crude-oil" .: 100, "water" .: 50]
  , outputs = thruputsFromList ["heavy-oil" .: 25, "light-oil" .: 45, "ethylene" .: 55]
  , time = 5
  }

kovarex :: Recipe
kovarex = Recipe
  { inputs = thruputsFromList ["u235" .: 40, "u238" .: 5]
  , outputs = thruputsFromList ["u235" .: 41, "u238" .: 2]
  , time = 60
  }

------------------------------------


infixl 9 .:
(.:) :: a -> b -> (a, b)
a .: b = (a, b)
