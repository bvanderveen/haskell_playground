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

type Env = [(String, LispValue)]
type Builtin = [LispValue] -> LispValue

nullEnv :: Env
nullEnv = []

numericBinaryOperator :: (Integer -> Integer -> Integer) -> Builtin
numericBinaryOperator operator arguments = Number $ foldl1 operator $ map lispValueAsInteger arguments

lispValueAsInteger :: LispValue -> Integer
lispValueAsInteger (Number n) = n

builtins :: [(String, Builtin)]
builtins = [
    ("+", numericBinaryOperator (+)),
    ("-", numericBinaryOperator (-)),
    ("*", numericBinaryOperator (*)),
    ("/", numericBinaryOperator div),
    ("mod", numericBinaryOperator mod)]

bindEnv :: Env -> [(String, LispValue)] -> Env
bindEnv env bindings = env ++ bindings

getEnv :: Env -> String -> LispValue
getEnv env name = maybe (error $ "Attempted to get unbound name " ++ name) id $ lookup name env

defEnv :: Env -> String -> LispValue -> (Env, LispValue)
defEnv env name value = let result = snd (eval env value) in (bindEnv env [(name, result)], result)

apply :: LispValue -> [LispValue] -> LispValue
apply func args = case func of
    (Function closure params body) -> 
        if num args /= num params 
            then error "Mismatch args/params"
            else
                let e = bindEnv closure $ zip params args in 
                    -- this is really clumsy. if function has no body result should be error or nil rather than False
                    snd $ foldl (\(e', v) f  -> eval e' f) (e, Bool False) body
        where
            num = toInteger . length


eval :: Env -> LispValue -> (Env, LispValue)
eval env value@(String _) = (env, value)
eval env value@(Number _) = (env, value)
eval env value@(Bool _) = (env, value)
eval env (Atom a) = (env, getEnv env a)
eval env (List [Atom "quote", value]) = (env, value)

eval env (List [Atom "def", Atom name, value]) = 
    let v' = snd $ eval env value in 
        (bindEnv env [(name, v')], v')

eval env (List (Atom "lambda" : List params : body)) = 
    (env, Function env (map (\v -> case v of (Atom a) -> a) params) body)

eval env (List (func : args)) = 
    case func of 
    (Atom a) -> case lookup a builtins of
        Just builtin -> (env, builtin $ map (\a -> snd $ eval env a) args)
        Nothing -> applyNormal
    otherwise -> applyNormal
    where
        applyNormal = 
            (env, apply (snd $ eval env func) $ map (\a -> snd $ eval env a) args)

parseEval :: String -> Either ParseError LispValue
parseEval input = either (\e -> Left e) (\v -> Right $ snd $ eval nullEnv v) (parseLisp input)
