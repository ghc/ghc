{-# OPTIONS_GHC -fprof-auto #-}
module B where

s1 :: String
s1 = "a;sdlfkjas;dlkj"

s2 :: String
s2 = "asdflaksdfa;sdlfkjas;dlkj"

s3 :: [String]
s3 = [s1, s2]

s4 :: [String]
s4 = [s2, s1]

i1 :: Int
i1 = 234

i2 :: [Int]
i2 = [2,3,4,5]

data Value = Value String Int
