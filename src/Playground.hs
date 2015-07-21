module Playground (
    parseEval
)
where

import Playground.Parser
import Text.ParserCombinators.Parsec

type Builtin = [LispValue] -> LispValue

numericBinaryOperator :: (Integer -> Integer -> Integer) -> Builtin
numericBinaryOperator operator arguments = Number $ foldl1 operator $ map lispValueAsInteger arguments

equalValues :: [LispValue] -> LispValue
equalValues [l, r] = Bool $ l == r

lispValueAsInteger :: LispValue -> Integer
lispValueAsInteger (Number n) = n
lispValueAsInteger v = error $ "Not an integer: " ++ show v

builtins :: [(String, Builtin)]
builtins = [
    ("+", numericBinaryOperator (+)),
    ("-", numericBinaryOperator (-)),
    ("*", numericBinaryOperator (*)),
    ("/", numericBinaryOperator div),
    ("mod", numericBinaryOperator mod),
    ("=", equalValues)]

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
            then error $ "Expected " ++ (show $ num params) ++ " arguments, got " ++ (show $ num args)
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
eval env (List [Atom "if", pred, t, f]) = (env, case snd (eval env pred) of
    (Bool p) -> snd $ eval env $ if p then t else f)
eval env (List [Atom "quote", value]) = (env, value)

eval env (List [Atom "def", Atom name, value]) = 
    let e' = bindEnv env [(name, v')]
        v' = snd $ eval e' value in 
        (e', v')

eval env (List (Atom "lambda" : List params : body)) = 
    (env, Function env (map (\v -> case v of (Atom a) -> a) params) body)

eval env (List (func : args)) = 
    case func of 
    (Atom a) -> case lookup a builtins of
        Just builtin -> (env, builtin $ evaluateSome args)
        Nothing -> applyNormal
    otherwise -> applyNormal
    where
        evaluateOne arg = snd $ eval env arg
        evaluateSome args = map evaluateOne args
        evaluateFunc func = snd $ eval env func
        applyNormal = 
            (env, apply (evaluateFunc func) $ evaluateSome args)

parseEval :: String -> Either ParseError LispValue
parseEval input = either (\e -> Left e) (\v -> Right $ snd $ eval nullEnv v) (parseLisp input)
