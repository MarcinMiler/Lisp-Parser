module Data.Lisp where

import Data.IORef
import Control.Monad.Except

import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef LispVal)]

data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | Parser         ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String

instance Show LispError where
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected ++ 
                                         " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++
                                         ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

data LispVal = Atom           String
             | List           [LispVal]
             | DottedList     [LispVal] LispVal
             | Number         Integer
             | Float          Double
             | Character      Char
             | String         String
             | Bool           Bool
             | PrimitiveFunc  ([LispVal] -> ThrowsError LispVal)
             | Func           { params  :: [String]
                              , vararg  :: Maybe String
                              , body    :: [LispVal]
                              , closure :: Env
                              }

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Character char) = [char]
    show (Atom name) = name
    show (Number contents) = show contents
    show (Float contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show Func { params = args, vararg = varargs, body = body, closure = env } =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
