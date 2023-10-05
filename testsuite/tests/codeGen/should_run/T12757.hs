{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B

answers :: [ByteString]
answers = map (B.filter (/= 0x20))
    [ "e3b0c442 98fc1c14 9afbf4c8 996fb924 27ae41e4 649b934c a495991b 7852b855"
    , "d7a8fbb3 07d78094 69ca9abc b0082e4f 8d5651e4 6d3cdb76 2d02d0bf 37c9e592"
    , "e4c4d8f3 bf76b692 de791a17 3e053211 50f7a345 b46484fe 427f6acc 7ecc81be"
    , "ba7816bf 8f01cfea 414140de 5dae2223 b00361a3 96177a9c b410ff61 f20015ad"
    , "248d6a61 d20638b8 e5c02693 0c3e6039 a33ce459 64ff2167 f6ecedd4 19db06c1"
    , "cf5b16a7 78af8380 036ce59e 7b049237 0b249b11 e8f07a51 afac4503 7afee9d1"
    , "cdc76e5c 9914fb92 81a1c7e2 84d73e67 f1809a48 a497200e 046d39cc c7112cd0"
    ]

x, y :: ByteString
x = "e3b0c442 98fc1c14 9afbf4c8 996fb924 27ae41e4 649b934c a495991b 7852b855"
y = B.filter (/= 0x20) x

main :: IO ()
main = do
    print (seq x ())
    print (seq y ())
    print (length answers)
    print (seq (head answers) ()) -- segfault!
