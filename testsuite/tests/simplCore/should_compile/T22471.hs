{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
-- Otherwise we get stuff about (+) may inline, which is
-- true enough, but not the point of this test

module T22471 where

foo :: (forall a. [a] -> Int) -> Int
foo len = len [1,2,3] + len "abc"

{-# RULES "foo" forall (f :: forall a. [a] -> Int).
                foo (\xs -> 1 + f xs) = 2 + foo f #-}


