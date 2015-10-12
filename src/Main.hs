--import Playground
import System.IO
import Playground.Parser
import Playground

repl :: Env -> IO ()
repl env = putStr "> " >> hFlush stdout >>
    do 
        input <- getLine
        if input == "quit" 
            then return ()
            else 
                case parseEvalInEnv env input of
                    (Left e) -> putStrLn (show e) >> repl env
                    (Right v) -> putStrLn (showValue $ snd v) >> repl (fst v)

main :: IO ()
main = repl nullEnv
