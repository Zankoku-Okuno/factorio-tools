{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Factorio.Technology
  ( TechName
  , Technologies(..)
  , Technology(..)
  , ResearchCost(..)
  ) where

import Data.Aeson
import Factorio.Util

import Data.Aeson.Types (explicitParseField)
import Data.Graph.DGraph (DGraph)
import Data.Graph.Traversal (bfsVertices)
import Data.Graph.Types ((-->))
import Data.List (nub)
import Data.Text (Text)

import qualified Data.Graph.DGraph as DGraph


type TechName = Text

data Technologies = Techs
  { techs :: [Technology]
  , graph :: DGraph Text ()
  }
  deriving(Show)

data Technology = Tech
  { name :: TechName
  , cost :: ResearchCost
  , directPrereqs :: [TechName]
  , allPrereqs :: [TechName]
  }
  deriving(Show)

data ResearchCost = ResearchCost
  { types :: [Text]
  , count :: Int
  , seconds :: Int
  }
  deriving(Show)


instance FromJSON Technologies where
  parseJSON v = do
    techList <- parseJSON v
    let graph = DGraph.fromArcsList $ concatMap toArcs techs
        techs = addClosure graph <$> techList
    pure Techs{techs, graph}
    where
    addClosure fullGraph t@Tech{directPrereqs} =
      t{allPrereqs = nub $ concatMap (bfsVertices fullGraph) directPrereqs}
    toArcs Tech{name,directPrereqs} = (name -->) <$> directPrereqs

instance FromJSON Technology where
  parseJSON = withObject "Technology" $ \v -> do
    name <- v .: "name"
    cost <- v .: "unit"
    directPrereqs <- v .:? "prerequisites" .!= []
    let allPrereqs = undefined
    pure Tech{name,cost,directPrereqs,allPrereqs}

instance FromJSON ResearchCost where
  parseJSON = withObject "ResearchCost" $ \v -> do
    (ingredients :: [(Text, Int)]) <- v .: "ingredients"
    let types = fst <$> ingredients
    count <- explicitParseField softNumber v "count"
    seconds <- v .: "time"
    pure ResearchCost{types,count,seconds}
