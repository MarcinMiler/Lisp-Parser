{-# LANGUAGE OverloadedStrings #-}

module Api where

import Data.LispError
import Primitives
import ExpressionDTO
import Cors

import Control.Monad

import Web.Scotty
import Data.Text.Lazy as T

main = scotty 4000 $ do
    middleware allowCors

    post "/parse" $ do
        (ExpressionDTO expression) <- jsonData :: ActionM ExpressionDTO
        evaluateExpression $ T.unpack expression

evaluateExpression args =
   json $ ExpressionDTO $ T.pack $ extractValue $ trapError $ fmap show $ readExpr args >>= eval
