{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Factorio.Item
    ( ItemName
    , Item(..)
    ) where

import Data.Aeson

import Data.Text (Text)


type ItemName = Text

data Item = Item
  { name :: ItemName
  , stackSize :: Int
  }
  deriving(Show)

instance FromJSON Item where
  parseJSON = withObject "Item" $ \obj -> do
    name <- obj .: "name"
    stackSize <- obj .: "stack_size"
    pure Item{name,stackSize}
