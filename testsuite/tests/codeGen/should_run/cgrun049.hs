-- !! Data constructors with strict fields
-- This test should use -funbox-strict-fields

module Main ( main ) where

main = print (g (f t))

t = MkT 1 2 (3,4) (MkS 5 6)

g (MkT x _ _ _) = x

data T = MkT Int !Int !(Int,Int) !(S Int)

data S a = MkS a a 


{-# NOINLINE f #-}
f :: T -> T	-- Takes apart the thing and puts it
		-- back together differently
f (MkT x y (a,b) (MkS p q)) = MkT a b (p,q) (MkS x y)


