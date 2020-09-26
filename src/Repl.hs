module Repl where

import System.IO
import System.Environment
import Control.Monad

import Data.Lisp
import Primitives
import Variables

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

eval2 :: Env -> String -> IO String
eval2 env expr = evalString env expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

run expr = primitiveBindings >>= flip eval2 expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp > ") . evalAndPrint

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ head args
        _ -> putStrLn "Program takes only 0 or 1 argument"