module Main where

import Set1Module
import Test.HUnit
import Data.Char

test1_web = TestCase $ assertEqual
  "TC for the example in the page"
  "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  (intToBase64 . hexToInt $ "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")

test2_hexToInt = TestCase $ assertEqual
  "TC for an util function"
  [28,1,17,0,31,1,1,0,6,26,2,75,83,83,80,9,24,28]
  (hexToInt "1c0111001f010100061a024b53535009181c")

test3_xorAgainst = TestCase $ assertEqual
  "TC for an util function"
  (hexToInt "436f6f6b696e67204d432773206c696b65206120706f756e64206f66206261636f6e")
  (xorAgainst (hexToInt "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") 88)

test3_web = TestCase $ assertEqual
  "TC for the example in the page"
  [88]
  (getXorKey $ hexToInt "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")

main = runTestTT $ TestList [
  test1_web, test2_hexToInt, test3_xorAgainst, test3_web
  ]
