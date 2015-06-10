module Main where
import System.Environment
import Set1Module

slow :: String -> (String, [Int])
slow x = (x, getXorKey $ hexToInt x)

main :: IO ()
main = do
    content <- readFile "4.txt"
    let withKeys = map slow $ lines content
        candidates = filter (\x -> 1 == (length . snd $ x)) withKeys
    mapM_ (putStrLn . intToAscii . decryptXor) $ map (hexToInt . fst) candidates
