module Playground (
    parseLisp,
    parseEval,
    LispValue(..)
)
where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data LispValue = Atom String
    | List [LispValue]
    | Number Integer
    | String String
    | Bool Bool
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
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseNumber :: Parser LispValue
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispValue
parseList = liftM List $ sepBy parseExpresssion spaces

parseQuote :: Parser LispValue
parseQuote = do
    char '\''
    x <- parseExpresssion
    return $ List [Atom "quote", x]

parseExpresssion :: Parser LispValue
parseExpresssion = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuote
    <|> do 
            char '('
            x <- try parseList
            char ')'
            return x

parseLisp :: String -> Either ParseError LispValue
parseLisp input = parse parseExpresssion "lisp" input

numericBinaryOperator :: (Integer -> Integer -> Integer) -> [LispValue] -> LispValue
numericBinaryOperator operator arguments = Number $ foldl1 operator $ map lispValueAsInteger arguments

lispValueAsInteger :: LispValue -> Integer
lispValueAsInteger (Number n) = n

builtins :: [(String, [LispValue] -> LispValue)]
builtins = [
    ("+", numericBinaryOperator (+)),
    ("-", numericBinaryOperator (-)),
    ("*", numericBinaryOperator (*)),
    ("/", numericBinaryOperator div),
    ("mod", numericBinaryOperator mod)]

apply :: String -> [LispValue] -> LispValue
apply functionName args = maybe (Bool False) ($ args) $ lookup functionName builtins

eval :: LispValue -> LispValue
eval value@(String _) = value
eval value@(Number _) = value
eval value@(Bool _) = value
eval (List [Atom "quote", value]) = value
eval (List (Atom func : args)) = apply func $ map eval args

parseEval :: String -> Either ParseError LispValue
parseEval input = either (\e -> Left e) (\v -> Right $ eval v) (parseLisp input)
