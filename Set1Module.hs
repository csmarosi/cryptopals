module Set1Module (hexToInt, asciiToInt, base64ToInt,intToHex, intToAscii, intToBase64
                  ,xorAgainst, decryptXor, getXorKey, repeatKeyXor
                  ,hammingDistance, getKeySizeForRepKeyXor, getKeysForRepKeyXor
                  ) where

--Design notes:
-- 1. Everything that is not conversion should be Int->Int (mainly because of xor)
-- 2. Names are: hex ascii base64 and int

import Data.Char
import Data.Bits
import Data.List
import qualified ByteConverters

hexToInt = ByteConverters.hexToInt
asciiToInt = ByteConverters.asciiToInt
base64ToInt = ByteConverters.base64ToInt
intToHex = ByteConverters.intToHex
intToAscii = ByteConverters.intToAscii
intToBase64 = ByteConverters.intToBase64


--TODO: argMin must exist as a standard function
argMin :: (Int -> Float) -> [Int] -> [Int]
argMin fun arg = map (\x -> arg !! x) ind
    where min = minimum list + 0.001
          ind = filter (\x -> ((list !! x) <= min)) [0..(length list - 1)]
          list = map fun arg

divideInt :: Int -> Int -> Float
divideInt a b = (fromIntegral a) / (fromIntegral b)


xorAgainst :: [Int] -> Int -> [Int]
xorAgainst plain key = zipWith xor plain (repeat key)

isPlainText :: [Int] -> Bool
isPlainText text = and $ map (\x -> elem x plainChar) text
    where plainChar = map ord (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ " '\n,-!.")

--TODO: black magic here; fix this naive implementation
textScore :: [Int] -> Float
textScore text = 0.0 - length (filter (\x -> elem x commonChars) text) `divideInt` length text
    where commonChars = map ord "etaoin shrdlu"

--TODO: Questionable: signature promises a list, but tries hard to return at most 1 element
getXorKey :: [Int] -> [Int]
getXorKey cypher = argMin (textScore . xorAgainst cypher) filteredKeys
    where filteredKeys = filter (isPlainText . xorAgainst cypher) [0..255]

decryptXor :: [Int] -> [Int]
decryptXor a = xorAgainst a $ getXorKey a !! 0

repeatKeyXor :: [Int] -> [Int] -> [Int]
repeatKeyXor str key = zipWith xor str $ cycle key


numberOfSetBits :: Int -> Int
numberOfSetBits x
    | x == 0    = 0
    | otherwise = 1 + (numberOfSetBits (x .&. (x - 1)))

hammingDistance :: [Int] -> [Int] -> Int
hammingDistance a b
    | (length a) == (length b) = sum (map (\ (x, y) -> numberOfSetBits (xor x y)) (zip a b))
    | otherwise                = error "hammingDistance: different sizes!"


every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

--TODO: black magic here
hammingNormal :: [Int] -> Int -> Float
hammingNormal cypher keyLenCand = hammingDistance cypher1 cypher2  `divideInt` blockLen
    where keyBlocks = div (length cypher) (keyLenCand)
          blockLen = (keyBlocks - 1) * keyLenCand
          cypher1 = take blockLen cypher
          cypher2 = take blockLen (drop keyLenCand cypher)

getKeySizeForRepKeyXor :: [Int] -> [Int] -> [Int]
getKeySizeForRepKeyXor ct kl = argMin (hammingNormal ct) kl

getKeysForRepKeyXor :: [Int] -> [Int] -> [[[Int]]]
getKeysForRepKeyXor ct kl = map possibleKey (getKeySizeForRepKeyXor ct kl)
    where chunk kS n = getXorKey $ every kS (drop n ct)
          possibleKey keyLen = transpose $ map (chunk keyLen) [1..keyLen]
