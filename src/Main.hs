import System.IO
import Playground
import Playground.Parser

parseEvalInEnvString :: Env -> String -> (Env, String)
parseEvalInEnvString env input = case parseEvalInEnv env input of
    (Left e) -> (env, show e)
    (Right v) -> (fst v, showValue $ snd v)

repl :: Env -> IO ()
repl env = putStr "> " >> hFlush stdout >>
    do 
        input <- getLine
        if input == "quit" 
            then return ()
            else 
                let result = parseEvalInEnvString env input in
                putStrLn (snd result) >> repl (fst result)

main :: IO ()
main = repl nullEnv
