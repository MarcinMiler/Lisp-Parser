{-# LANGUAGE ExistentialQuantification #-}

module Data.Unpacker where

import Data.Lisp

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)