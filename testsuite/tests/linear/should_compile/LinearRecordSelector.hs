{-# LANGUAGE LinearTypes, DataKinds #-}
module LinearRecordSelector where

import GHC.Exts (Multiplicity(..))

data Test = A { test :: Int, test2 %Many :: String } | B { test :: Int, test3 %Many :: Char }

test1 :: Test %1 -> Int
test1 a = f a

testM :: Test -> Int
testM = f
