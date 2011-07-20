{-# LANGUAGE RankNTypes #-}

module ShouldFail where

-- With the new typechecker (GHC 7.1), these now all pass

f1 :: (forall a. Eq a => [a]) -> Bool
f1 xs@(x:_)  = x

f2 :: (forall a. Eq a => [a]) -> Bool
f2 [x] = x

f3 :: (forall a. Eq a => [a]) -> Bool
f3 (x:[]) = x
