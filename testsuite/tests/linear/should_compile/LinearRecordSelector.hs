{-# LANGUAGE LinearTypes, DataKinds, OverloadedRecordDot, RebindableSyntax #-}
module LinearRecordSelector where

import GHC.Exts (Multiplicity(..))
import Prelude

data Test = A { test :: Int, test2 %Many :: String } | B { test %Many :: Int, test3 %Many :: Char }

test1 :: Test %1 -> Int
test1 a = test a

testM :: Test -> Int
testM a = test a

testX :: Test %m -> Int
testX = test

newtype NT = NT { unNT :: Int }

nt :: NT %m -> Int
nt a = unNT a
