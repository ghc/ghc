{-# LANGUAGE MagicHash  #-}
module T13077a where

import GHC.Exts

data X = A | B | C

data T = MkT !X Int# Int#

g :: Int -> T
g 0 = MkT A 1# 2#
g n = g (n-1)

boo :: Int -> T
boo k = case g k of
          MkT x n _ -> let v = case x of
                                  A -> 1#
                                  B -> 2#
                                  C -> n
                       in MkT x v v
  -- Tests evaluated-ness for CPR
