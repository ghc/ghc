{-# OPTIONS -fglasgow-exts #-}

-- Made GHC 5.02.2 go into a loop when doing the 
-- context inference for deriving.  The -fglasgow-exts
-- flag is important; it didn't diverge without.

module ShouldFail where

data Empty a = E
newtype Id a = I a
newtype Pair v w a = P ((v a), (w a))
type Square a = Square_ Empty Id a
data Square_ v w a =
        End (v (v a))
      | Zero (Square_ v (Pair w w) a)
      | One (Square_ (Pair v w) (Pair w w) a) deriving Show
