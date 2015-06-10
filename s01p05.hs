module Main where
import System.Environment
import Set1Module

main :: IO ()
main = do
    args <- getArgs
    stdIo <- getContents
    putStrLn $ intToAscii (repeatKeyXor (init $ asciiToInt stdIo) (asciiToInt $ args !! 0))
