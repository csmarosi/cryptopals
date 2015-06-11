module Main where
import System.Environment
import Set1Module
import Data.Char

main :: IO ()
main = do
    stdIo <- getContents
    let contentInt = init $ map ord stdIo
        keys = getKeysForRepKeyXor contentInt [1..50]
        keySize = getKeySizeForRepKeyXor contentInt [1..50]
    print $ keySize
    print $ map (map (map chr)) $ keys
    putStrLn $ intToAscii $ repeatKeyXor contentInt (head . head $ keys)
