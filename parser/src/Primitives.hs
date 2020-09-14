{-# LANGUAGE OverloadedStrings #-}

module Primitives where

import Data.LispVal
import Data.LispError
import Data.Unpacker
import Parsers.Parsers

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except

trapError action = catchError action (return . show)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        _  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func) 
                  ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp (+)),
              ("quotient", numericBinOp quot),
              ("remainder", numericBinOp rem),
              ("=", numBoolBinOp (==)),
              ("<", numBoolBinOp (<)),
              (">", numBoolBinOp (>)),
              ("/=", numBoolBinOp (/=)),
              (">=", numBoolBinOp (>=)),
              ("<=", numBoolBinOp (<=)),
              ("&&", boolBoolBinOp (&&)),
              ("||", boolBoolBinOp (||)),
              ("string=?", strBoolBinOp (==)),
              ("string<?", strBoolBinOp (<)),
              ("string>?", strBoolBinOp (>)),
              ("string<=?", strBoolBinOp (<=)),
              ("string>=?", strBoolBinOp (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinOp op []            = throwError $ NumArgs 2 []
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params        = Number . foldl1 op <$> mapM unpackNum params

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                                left <- unpacker $ head args
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

numBoolBinOp  = boolBinOp unpackNum
strBoolBinOp  = boolBinOp unpackStr
boolBoolBinOp = boolBinOp unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]          = return $ List xs
cdr [DottedList [_] x]       = return x
cdr [DottedList (_ : xs) x]  = return $ DottedList xs x
cdr [badArg]                 = throwError $ TypeMismatch "pair" badArg
cdr badArgList               = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
                                             where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                    Left err -> False
                                                    Right (Bool val) -> val
eqv [_, _]                             = return $ Bool False
eqv badArgList                         = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2 
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- or <$> mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool x) = return x
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool