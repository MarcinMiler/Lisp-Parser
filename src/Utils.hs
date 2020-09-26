module Utils where

import Control.Monad.Except

import Data.Lisp

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError action = catchError action (return . show)