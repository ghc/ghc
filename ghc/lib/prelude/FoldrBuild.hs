module PreludeFoldrBuild where

import Builtin (error)

-----------------------------------------------------------------
-- This needs to be in a sperate module, other than in List.hs
-- NOTE: no foldr/build's are done on the module that foldr is defined in.

{-# MAGIC_UNFOLDING foldr foldr #-}
{-# GENERATE_SPECS foldr a b #-}
foldr			:: (a -> b -> b) -> b -> [a] -> b
foldr f z []		=  z
foldr f z (x:xs)	=  f x (foldr f z xs)

{-# MAGIC_UNFOLDING foldl foldl #-}
{-# GENERATE_SPECS foldl a b #-}
foldl			:: (a -> b -> a) -> a -> [b] -> a
foldl f z []		=  z
foldl f z (x:xs)	=  foldl f (f z x) xs

-- HACK: Magic unfoldings not implemented for unboxed lists
--	 Need to define a _build to avoid undefined symbol

{-# GENERATE_SPECS _build a #-}
_build 		:: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
_build g 	= g (:) []


