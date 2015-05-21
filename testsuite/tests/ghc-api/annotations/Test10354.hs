{-# LANGUAGE PartialTypeSignatures #-}
module Test10354 where

f :: ((Eq a, _)) => a -> a -> Bool
f x y = x == y

bar :: (   ) => a-> Bool
bar = undefined

baz :: _ => a -> String
baz = undefined

foo :: ForceError
foo = undefined
