{-# LANGUAGE MagicHash #-}
-- !! test GEN reps w/ unboxed values in them
-- !! NB: it was the static ones that were hosed...
--
module Main ( main ) where

--import PrelBase
import GHC.Base

main = do
    putStr (shows (sum ([1..1{-30-}]++[1..1{-40-}]++[11,22])) "\n")
    putStr (shows (prog 1{-30-} 1{-40-}) "\n")

data Foo a
  = MkFoo [a] Int# [Int] Int# [(a,Int)] Int#
  -- The above will cause a *horrible* GEN rep'n.

prog :: Int -> Int -> Int

prog size_1 size_2
  = let
	list1 = static1 : (map mk_foo [1 .. size_1])
	list2 = static2 : (map mk_foo [1 .. size_2])
    in
    I# (add_up 0# list1 (reverse list2))

static1 = MkFoo (error "static11") 11# [] 11# (error "static12") 11#
static2 = MkFoo (error "static21") 22# [] 22# (error "static22") 22#

one, two :: Int
one = 1; two = 2

mk_foo i@(I# i#)
  = MkFoo (error "list1") i# [i,i] i# (error "list2") i#

add_up :: Int# -> [Foo a] -> [Foo a] -> Int#

add_up acc [] [] = acc
add_up acc [] ys  = add_up acc ys []
add_up acc (x:xs) (y:ys) = add_up (acc +# add x y) xs ys
add_up acc (x:xs) [] = add_up acc xs []

add :: Foo a -> Foo a -> Int#
add (MkFoo _ _ _ _ _ x) (MkFoo _ _ _ _ _ y)
  = x +# y
