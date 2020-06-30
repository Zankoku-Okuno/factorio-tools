{-# LANGUAGE LambdaCase #-}

module Factorio.Util where

import Data.Aeson
import Data.Aeson.Types (Parser)

import qualified Data.Text as T


softNumber :: (FromJSON a) => Value -> Parser a
softNumber = parseJSON . \case
  String str -> Number (read $ T.unpack str)
  v -> v
