module Main where
import System.Environment
import Set1Module
import Data.Char

import Crypto.Cipher
import Crypto.Cipher.Types
import qualified Data.ByteString.Char8 as B

keyString = B.pack "YELLOW SUBMARINE"
Right key = makeKey keyString

aes128 :: AES128
aes128 = cipherInit key

main :: IO ()
main = do
    stdIo <- B.getContents
    putStrLn $ B.unpack $ ecbDecrypt aes128 stdIo
