{-# OPTIONS -fglasgow-exts #-}

{-

This example demonstrates the inadequacy of an apparantly simpler
variation on gmapQ. To this end, let us first recall a few facts.
Firstly, function application (including constructor application) is
left-associative. This is the reason why we had chosen our generic
fold to be left-associative too. (In "The Sketch Of a Polymorphic
Symphony" you can a right-associative generic fold, too.) Secondly,
lists are right-associative. So we want to query a term by retrieving
a list of results from the immediate subterms, then the natural
left-to-right order of traversal needs some extra effort. That is, we
traverse a left-associative data-structure while we want to construct
a right-associative one. In the module Data.Generics, we solve the
problem by a common higher-order trick, that is, we do not cons lists
during folding but we pass functions on lists starting from the
identity function and passing [] to the resulting function. This is
captured by a fancy datatype constructor Q. Below, we shou that we
get indeed a unnatural right-to-left order if we just apply the simple
constant datatype constructor instead of Q.

Contributed by Ralf Laemmel, ralf@cwi.nl

-}

module Main where
import Data.Generics


-- The plain constant type constructor
newtype CONST x y = CONST x
unCONST (CONST x) = x


-- A variation on the gmapQ combinator using CONST and not Q
gmapQ' :: Data a => (forall a. Data a => a -> u) -> a -> [u]
gmapQ' f = unCONST . gfoldl f' z
  where
    f' r a = CONST (f a : unCONST r)
    z  = const (CONST [])


-- A trivial datatype used for this test case
data IntTree = Leaf Int | Fork IntTree IntTree
               deriving (Typeable, Data)


-- Select int if faced with a leaf 
leaf (Leaf i) = [i]
leaf _        = []


-- A test term
term = Fork (Leaf 1) (Leaf 2)


-- Process test term
--  gmapQ  gives left-to-right order
--  gmapQ' gives right-to-left order
--
main = print $ ( gmapQ   ([] `mkQ` leaf) term
               , gmapQ'  ([] `mkQ` leaf) term
               )
