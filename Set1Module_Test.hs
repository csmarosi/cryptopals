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

repeatedKeyCyptherText = (hexToInt "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
repeatedKeyPlainText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"

test5_web = TestCase $ assertEqual
  "TC for the example in the page"
  repeatedKeyCyptherText
  (repeatKeyXor (asciiToInt repeatedKeyPlainText) (asciiToInt "ICE"))

test6_dist = TestCase $ assertEqual
  "TC for the example in the page"
  37
  (hammingDistance (asciiToInt "this is a test") (asciiToInt "wokka wokka!!!"))

test6_keylength = TestCase $ assertEqual
  "TC for an util function"
  [3]
  (getKeySizeForRepKeyXor repeatedKeyCyptherText [1..5])

test6_break = TestCase $ assertEqual
  "Decrypting test 5"
  [[map ord "ICE"]]
  (getKeysForRepKeyXor repeatedKeyCyptherText [1..5])

main = runTestTT $ TestList [
  test1_web, test2_hexToInt, test3_xorAgainst, test3_web
 ,test5_web
 ,test6_dist, test6_keylength, test6_break
  ]
