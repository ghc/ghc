-- -*- haskell -*-
-- Time-stamp: <2010-07-16 12:10:03 simonmar>
--
-- ADT of a binary tree (values only in leaves).
-- Parallel functions use par and seq directly.
-- ---------------------------------------------------------------------------

module Tree(Tree, 
            list2tree, tree2list, (^:), 
            tree_map, tree_fold, 
	    depth, create_forest, 
            force_tree, par_tree_map) where

import Control.Parallel
import Control.Parallel.Strategies

infixl 2 ^:

data Tree a = Leaf a
	    | Node (Tree a) (Tree a)
	    deriving (Eq, Read, Show)

tree_map :: (Integral a, Integral b) => (a -> b) -> Tree a -> Tree b
tree_map f (Leaf x) 		= Leaf (f x)
tree_map f (Node left right) 	= Node (tree_map f left) (tree_map f right)

par_tree_map :: (Integral a, Integral b) => (a -> b) -> Tree a -> Tree b
par_tree_map f (Leaf x) 		= Leaf (f x)
par_tree_map f (Node left right) 	= 
  Node (par_tree_map f left) (par_tree_map f right) `using` partree
  where
         partree (Node l r) = do
            l' <- rpar (l `using` rtree)
            r' <- rtree r
            return (Node l' r')

rtree t = force_tree t `pseq` return t

-- force evaluation of tree (could use Strategies module instead!)
force_tree :: (Integral a) => Tree a -> ()
force_tree t@(Leaf x) = x `seq` ()
force_tree t@(Node left right) = (force_tree left) `seq` 
	                         (force_tree right) `seq` 
				 ()
-- just would you'd expect
tree_fold :: (Integral a) => (a -> a -> a) -> a -> Tree a -> a
tree_fold o z (Leaf x) 		= z `o` x
tree_fold o z (Node left right) = tree_fold o z' right
				  where z' = tree_fold o z left

list2tree :: (Integral a) => [a] -> Tree a 
list2tree [] 	= error "list2tree: empty list"
list2tree [x] 	= Leaf x
list2tree l     = Node (list2tree left) (list2tree right)
		  where (left,right) = splitAt ((length l) `div` 2 ) l

tree2list :: (Integral a) => Tree a -> [a]
tree2list (Leaf x) 	= [x]
tree2list (Node left right) = tree2list left ++ tree2list right

-- combine 2 trees
(^:) :: (Integral a) => Tree a -> Tree a -> Tree a
t1 ^: t2 = Node t1 t2

depth :: Tree a -> Int
depth (Leaf _)		= 0
depth (Node left right) = max (depth left) (depth right) + 1

-- The following functions are useful for heavily heap allocating test fcts
create_forest :: (Integral a) => Tree a -> [Tree a] 
create_forest (Leaf x) 		= [ (Leaf y) | y <- [2..x], gcd x y == 1 ]
create_forest (Node left right) = [ (Node left' right') 
				  | left' <- create_forest left,
				    right' <- create_forest right]
