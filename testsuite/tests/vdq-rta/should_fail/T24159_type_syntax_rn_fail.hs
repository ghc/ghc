{-# LANGUAGE LinearTypes, DataKinds, ImpredicativeTypes #-}

module T24159_type_syntax_rn_fail where

import GHC.Exts

idVis :: forall a -> a -> a
idVis _ x = x

f1 :: Int -> Int
f1 = idVis (Int -> Int) (+1)

f2 :: Int %One -> Int
f2 = idVis (Int %One -> Int) (\x -> x)

f3 :: Int %1 -> Int
f3 = idVis (Int %1 -> Int) (\x -> x)

f4 :: Int %1 -> Int
f4 = idVis (Int %One -> Int) (\x -> x)

f5 :: Int %One -> Int
f5 = idVis (Int %1 -> Int) (\x -> x)

f6 :: b -> b
f6 = idVis (forall a. a -> a) id

f7 :: forall b -> b -> b
f7 = idVis (forall a -> a -> a) idVis

f8 :: x -> b -> b
f8 a = idVis (forall a. a -> a) id -- check shadowing

f9 :: () => Int -> Int
f9 = idVis (() => Int -> Int) id

f10 :: Show b => b -> String
f10 = idVis (forall a. Show a => a -> String) show

f11 :: forall a. forall b -> (a ~ Int, b ~ Bool) => a -> b
f11 = idVis (forall a1. forall b1 -> (a1 ~ Int, b1 ~ Bool) => a1 -> b1) (\ _ _ -> True)

