{-# LANGUAGE ExistentialQuantification #-}

module Data.Unpacker where

import Data.LispVal
import Data.LispError

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)