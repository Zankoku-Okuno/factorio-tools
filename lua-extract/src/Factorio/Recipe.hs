{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Factorio.Recipe
  ( ItemName
  , Recipe(..)
  ) where

import Factorio.Item

import Data.Aeson

import Control.Monad (when)
import Data.Aeson.Types (Parser)
import Data.Map (Map)

import qualified Data.Map as Map
-- import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V


data Recipe = Recipe
  { name :: String
  , ingredients :: Map ItemName Double
  , results :: Map ItemName Double
  , craftTime :: Double
  -- TODO `producedBy` would be great!
  }
  deriving(Show)


instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \v -> do
    name <- v .: "name"
    (normal :: Object) <- v .: "normal"
    ingredients <- Map.fromList <$> (traverse toIngredient =<< normal .: "ingredients")
    craftTime <- normal .:? "energy_required" .!= 0.5
    results <- normal .:? "result" >>= \case
      Just oneResult -> do
        resultCount <- normal .:? "result_count" .!= 1
        itemName <- parseJSON oneResult
        pure $ Map.singleton itemName resultCount
      Nothing -> Map.fromList <$> (traverse toResult =<< normal .: "results")
    pure Recipe{Factorio.Recipe.name,ingredients,results,craftTime}

toIngredient :: Value -> Parser (ItemName, Double)
toIngredient (Object obj) = do
  itemName <- obj .: "name"
  amount <- obj .:? "amount" .!= 1
  pure (itemName, amount)
toIngredient (Array arr) = do
  when (V.length arr /= 2) $ fail ("toIngredient(Array): " ++ show arr)
  itemName <- parseJSON $ arr V.! 0
  amount <- parseJSON $ arr V.! 1
  pure (itemName, amount)
toIngredient v = fail $ "toIngredient: " ++ show v

toResult :: Value -> Parser (ItemName, Double)
toResult (Object obj) = do
  itemName <- obj .: "name"
  amount <- obj .:? "amount" .!= 1
  probability <- obj .:? "probability" .!= 1
  pure (itemName, probability * amount)
toResult (Array arr) = do
  when (V.length arr /= 2) $ fail ("toResult(Array): " ++ show arr)
  itemName <- parseJSON $ arr V.! 0
  amount <- parseJSON $ arr V.! 1
  pure (itemName, amount)
toResult v = fail $ "toResult: " ++ show v
