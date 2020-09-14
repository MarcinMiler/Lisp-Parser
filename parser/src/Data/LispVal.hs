module Data.LispVal where

data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Number     Integer
             | Float      Double
             | Character  Char
             | String     String
             | Bool       Bool

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
