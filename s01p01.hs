module Main where
import System.Environment
import Set1Module

main :: IO ()
main = do
    args <- getArgs
    mapM_ (putStrLn . hexTo64) args
