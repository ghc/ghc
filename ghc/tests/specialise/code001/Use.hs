module Use (

	UseTree,

	lookup1, lookup2, lookup3, tree1

    ) where

import Spec 	( Tree(..), lookup)

data UseTree a = UseTree (Tree Char# a)

	-- specialised version of UseTree Int# will be created
	-- however, since the a is not a direct component this is
	-- identical to the original version!
	-- ToDo: avoid creating such versions?

	-- this data declaration does not in itself require specialisations of Tree
	-- these will only be required by code which constructs the values placed
	-- inside a use of this data declaration

{- These should be ok -}

lookup1 = case (lookup eqInt# ltInt# 1# 1# (Leaf 1# 1#)) of i# -> I# i#

{- These should cause specialisation errors, unless added to Spec.hs -}

tree1   = UseTree (Leaf (case 'k' of C# k# -> k#) 1#)

lookup2 = case (lookup eqInt# ltInt# 1# 1.0# (Leaf 1# 1.0#)) of f# -> F# f#

lookup3 = case (lookup (==) (<) 1 1.0# (Leaf 1 1.0#)) of f# -> F# f#

