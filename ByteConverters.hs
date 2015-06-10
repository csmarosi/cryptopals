module ByteConverters (hexToInt, asciiToInt, base64ToInt,
                       intToHex, intToAscii, intToBase64 ) where

import Data.Char
import Numeric

hexToInt :: String -> [Int]
hexToInt [] = []
hexToInt (x1:x2:xs) = [16*(hexToChar x1) + (hexToChar x2)] ++ hexToInt xs
    where hexToChar a = fst . head . readHex $ [a]

asciiToInt :: String -> [Int]
asciiToInt s = map ord s

base64ToInt :: String -> [Int]
base64ToInt s = error "Not implemented!"

intToHex :: [Int] -> String
intToHex list = foldr (\y x -> (showHex y "") ++ x) "" list

-- TODO bug: cannot append padding!
intToBase64 :: [Int] -> String
intToBase64 list =
    if (length list) >= 3
        then (divide . listToInteger $ take 3 list) ++ (intToBase64 $ drop 3 list)
        else if (null list) then "" else error "Not implemented!"
    where listToInteger lst = foldl (\x y -> x * 256 + toInteger y) 0 lst

intToAscii :: [Int] -> String
intToAscii a = map chr a

--Util stuff
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
