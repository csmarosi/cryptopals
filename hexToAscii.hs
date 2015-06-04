module Main where
import System.Environment
import Set1Module
import Data.Char

main :: IO ()
main = do
    args <- getArgs
    mapM_ (putStrLn . \x -> map chr . hexToByteList $ x) args
