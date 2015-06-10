module Set1Module (hexToInt, asciiToInt, base64ToInt,intToHex, intToAscii, intToBase64,
                   xorAgainst, decryptXor, getXorKey) where

--Design notes:
-- 1. Everything that is not conversion should be Int->Int (mainly because of xor)
-- 2. Names are: hex ascii base64 and int

import Data.Char
import Data.Bits
import qualified ByteConverters

hexToInt = ByteConverters.hexToInt
asciiToInt = ByteConverters.asciiToInt
base64ToInt = ByteConverters.base64ToInt
intToHex = ByteConverters.intToHex
intToAscii = ByteConverters.intToAscii
intToBase64 = ByteConverters.intToBase64


xorAgainst :: [Int] -> Int -> [Int]
xorAgainst plain key = zipWith xor plain (repeat key)

isText :: Int -> Bool
isText a = elem a $ map ord (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ " '\n,-!.")

textScrore :: [Int] -> Int -> Bool
textScrore cypher key = and $ map isText $ xorAgainst cypher key

getXorKey :: [Int] -> [Int]
getXorKey cypher = filter (textScrore cypher) [0..255]

decryptXor :: [Int] -> [Int]
decryptXor a = xorAgainst a $ getXorKey a !! 0
