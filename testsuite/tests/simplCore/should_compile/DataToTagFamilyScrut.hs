{-# LANGUAGE MagicHash, TypeFamilies #-}
module DataToTagFamilyScrut where

import GHC.Exts (dataToTag#, Int#, Int(I#))

data family Fam a b
data instance Fam () x = C0 x | C1 | C2 | C3 | C4 Bool | C5 Int | C6 | C7 | C8

testFun :: Fam () x -> Int#
testFun v = case  v  of
  C2 -> 13#
  C4 _ -> 15#
  C5 (I# w) -> w
  _ -> case  dataToTag# v  of
    0# -> -3#
    6# -> 12#
    _  -> 47#
