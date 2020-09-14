{-# LANGUAGE OverloadedStrings #-}

module ExpressionDTO where

import Data.Aeson hiding (json)
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

newtype ExpressionDTO = ExpressionDTO Text deriving (Show)

instance FromJSON ExpressionDTO where
    parseJSON (Object v) = ExpressionDTO <$> v .:  "expression"

instance ToJSON ExpressionDTO where
     toJSON (ExpressionDTO expression) =
         object ["result" .= expression]