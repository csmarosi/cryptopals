module Set1Module (xorHex, hexTo64, hexToByteList, xorAgainst, decryptXor, getXorKey, intToHex ) where

import Data.Char
import Data.Bits
import Numeric

hexToInt:: Char -> Int
hexToInt a = fst . head . readHex $ [a]

hexToByteList:: String -> [Int]
hexToByteList [] = []
hexToByteList (x1:x2:xs) = [16*(hexToInt x1) + (hexToInt x2)] ++ hexToByteList xs

intToHex :: [Int] -> String
intToHex list = foldr (\y x -> (showHex y "") ++ x) "" list

xorHex:: [String] -> String
xorHex (a:b:xs) = intToHex $ zipWith xor (hexToByteList a) (hexToByteList b)


listToInt :: [Int] -> Integer
listToInt list = foldl (\x y -> x * 256 + toInteger y) 0 list

intToMyStr :: Integer -> Char
intToMyStr a
    | a `elem` [ 0..25] = chr $ fromInteger a + ord 'A'
    | a `elem` [26..51] = chr $ fromInteger a + ord 'a' - 26
    | a `elem` [52..61] = chr $ fromInteger a + ord '0' - 52
    | a == 62 = '+'
    | a == 63 = '/'
    | otherwise = '!'

divide :: Integer -> String
divide a =
    if (a `div` 64) > 0
        then divide (a `div` 64) ++ [intToMyStr (a `mod` 64)]
        else [intToMyStr a]

-- TODO bug: cannot append padding!
hexTo64 :: String -> String
hexTo64 hex =
    if (length hex) >= 6
        then (divide . listToInt . hexToByteList $ take 6 hex) ++ (hexTo64 $ drop 6 hex)
        else ""


xorAgainst :: String -> Int -> [Int]
xorAgainst hex c = zipWith xor (hexToByteList hex) (repeat c)

isText :: Int -> Bool
isText a = elem a $ map ord (['A'..'Z'] ++ ['a'..'z'] ++ [' '])

-- TODO: what black magic to use here?
textScrore :: String -> Int -> Int
textScrore hex c = length $ filter isText $ xorAgainst hex c

getXorKey :: String -> [Int]
getXorKey hex = filter (\x -> (res !! x) >= m) [0..255]
    where res = map (\x -> textScrore hex x) $ [0..255]
          m = (maximum res) - 3

decryptXor :: String -> String
decryptXor a = map chr (xorAgainst a $ getXorKey a !! 0)
