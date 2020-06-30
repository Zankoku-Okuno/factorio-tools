{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Main (main) where

import Factorio.Item
import Factorio.Recipe
import Factorio.Technology

import Data.Aeson
import System.Exit
import System.IO

-- import Data.Graph.Visualize (plotDGraphPng)

import qualified Data.ByteString.Lazy as LBS


main :: IO ()
main = do
  (factorioData :: FactorioData) <- eitherDecode' <$> LBS.getContents >>= \case
    Right factorioData -> pure factorioData
    Left err -> hPutStrLn stderr err >> exitFailure
  -- plotDGraphPng (techGraph factorioData) "tech"
  print $ factorioData.items



data FactorioData = FactorioData
  { items :: [Item]
  , recipes :: [Recipe]
  , technology :: Technologies
  }
  deriving(Show)


instance FromJSON FactorioData where
  parseJSON = withObject "FactorioData" $ \v -> do
    items <- v .: "items"
    recipes <- v .: "recipes"
    technology <- v .: "technology"
    pure FactorioData{items,recipes,technology}
