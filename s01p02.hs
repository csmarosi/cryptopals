module Main where
import System.Environment
import Set1Module
import Data.Bits

xorHex:: [String] -> String
xorHex (a:b:xs) = intToHex $ zipWith xor (hexToInt a) (hexToInt b)

main :: IO ()
main = do
    args <- getArgs
    putStrLn . xorHex $ args
