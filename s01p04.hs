module Main where
import System.Environment
import Set1Module
import Control.Parallel.Strategies

slow :: String -> (String, [Int])
slow x = (x, getXorKey x)

-- to test possible parallelizations
fast :: String -> (String, [Int])
fast x = (x, [0..255])

main :: IO ()
main = do
    content <- readFile "4.txt" --a 327 lines long file
    print $ parMap rpar slow $ lines content
