module Spec (

	Tree(..),

	tree1, tree2, tree3,

	lookup

    ) where

data Tree k a = Leaf k a
	      | Branch k (Tree k a) (Tree k a)

lookup eq lt k def (Leaf k1 v1) 
  = if eq k k1 then v1 else def
lookup eq lt k def (Branch k1 t1 t2)
  = if lt k k1 then lookup eq lt k def t1
	       else lookup eq lt k def t2

-- Versions of Tree:
-- SPEC	Tree Int# Float#
-- SPEC Tree Char# a
-- use	Tree Int# Int#,
-- use	Tree a Int#, 
-- use	Tree Char# a 	  (already requested)
-- use  Tree Char# Char#  (via lookup SPEC)

-- Versions of lookup:
-- SPEC lookup Char# Char# Char#
-- SPEC lookup Char# Char# a
-- use  lookup Int# Int# Int#

{-# SPECIALISE data Tree Int# Float# #-}
{-# SPECIALISE data Tree Char# a #-}

{-# SPECIALISE lookup :: (Char#->Char#->Bool) -> (Char#->Char#->Bool)
		      -> Char# -> Char# -> Tree Char# Char# -> Char# #-}
{-# SPECIALISE lookup :: (Char#->Char#->Bool) -> (Char#->Char#->Bool)
		      -> Char# -> a -> Tree Char# a -> a #-}

tree1   = case (lookup eqInt# ltInt# 1# 1# (Leaf 1# 1#)) of i# -> I# i#
tree2 k = Leaf k  1#
tree3 a = case 'k' of C# k# -> Leaf k# a

{- These should cause errors -}

{- *** # SPECIALISE data Tree Char# a #-}   	-- duplicate
{- *** # SPECIALISE data Tree Char# Int #-} 	-- boxed type
{- *** # SPECIALISE data Tree a b #-}		-- no spec

{- Essential Specialisations -}
{-# SPECIALISE data Tree a Float# #-}
{-# SPECIALISE data Tree Char# Int# #-}
{-# SPECIALISE lookup :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> Float# -> Tree b Float# -> Float# #-}
{-# SPECIALISE lookup :: (Int# -> Int# -> Bool) -> (Int# -> Int# -> Bool) -> Int# -> Float# -> Tree Int# Float# -> Float# #-}
