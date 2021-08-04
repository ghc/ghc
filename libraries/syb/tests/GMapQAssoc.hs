{-# OPTIONS -fglasgow-exts #-}

module GMapQAssoc (tests) where

{-

This example demonstrates the inadequacy of an apparently simpler
variation on gmapQ. To this end, let us first recall a few facts.
Firstly, function application (including constructor application) is
left-associative. This is the reason why we had preferred our generic
fold to be left-associative too. (In "The Sketch Of a Polymorphic
Symphony" you can find a right-associative generic fold.)  Secondly,
lists are right-associative. Because of these inverse associativities
queries for the synthesis of lists require some extra effort to
reflect the left-to-right of immediate subterms in the queried list.
In the module Data.Generics, we solve the problem by a common
higher-order trick, that is, we do not cons lists during folding but
we pass functions on lists starting from the identity function and
passing [] to the resulting function. The following example
illustrates that we get indeed an undesirable right-to-left order if
we just apply the simple constant datatype constructor CONST instead
of the higher-order trick.

Contributed by Ralf Laemmel, ralf@cwi.nl

-}

import Test.Tasty.HUnit

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
tests = show ( gmapQ   ([] `mkQ` leaf) term
             , gmapQ'  ([] `mkQ` leaf) term
             ) @=? output

output = show ([[1],[2]],[[2],[1]])
