module Parsers.Parsers where

import Control.Monad

import Data.Lisp

import Text.ParserCombinators.Parsec hiding (spaces)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    s1 <- many (letter <|> symbol)
    return $ case s1 of
        "space"   -> Character ' '
        "newline" -> Character '\n'
        [x]       -> Character x

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ (Float . read) $ x ++ "." ++ y

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseCharacter
        <|> parseAtom
        <|> parseString
        <|> try parseFloat
        <|> try parseNumber
        <|> parseQuoted
        <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x