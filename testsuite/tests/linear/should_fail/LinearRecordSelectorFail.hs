{-# LANGUAGE LinearTypes, DataKinds, OverloadedRecordDot, RebindableSyntax #-}
module LinearRecordSelector where

import GHC.Exts (Multiplicity(..))
import Prelude

data Test1 = A1 { testA11 :: Int, testA12 :: String }

-- Fails because testA12 is linear
test1 :: Test1 %1 -> Int
test1 a = testA11 a

data Test2 = A2 { testA2 :: Int } | B2 { testB2 %Many :: Char }

-- Fails because testA2 is partial
test2 :: Test2 %1 -> Int
test2 a = testA2 a
