module Playground.Parser (
    parseLisp,
    LispValue(..),
    Env,
    nullEnv
)
where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

type Env = [(String, LispValue)]

nullEnv :: Env
nullEnv = []

data LispValue = Atom String
    | List [LispValue]
    | Number Integer
    | String String
    | Bool Bool
    | Function { closure :: Env, params :: [String], body :: [LispValue] }
    deriving (Show, Eq)

-- TODO IITO lexeme
spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispValue
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "true" -> Bool True
        "false" -> Bool False
        _ -> Atom atom

parseString :: Parser LispValue
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

parseNumber :: Parser LispValue
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispValue
parseList = liftM List $ sepBy parseExpresssion spaces

parseQuote :: Parser LispValue
parseQuote = do
    _ <- char '\''
    x <- parseExpresssion
    return $ List [Atom "quote", x]

parseExpresssion :: Parser LispValue
parseExpresssion = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuote
    <|> do 
            _ <- char '('
            x <- try parseList
            _ <- char ')'
            return x

parseLisp :: String -> Either ParseError LispValue
parseLisp input = parse parseExpresssion "lisp" input
