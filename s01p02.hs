module Main where
import System.Environment
import Set1Module

main :: IO ()
main = do
    args <- getArgs
    putStrLn . xorHex $ args
