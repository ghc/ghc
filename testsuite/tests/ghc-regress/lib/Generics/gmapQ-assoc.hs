{-# OPTIONS -fglasgow-exts #-}

-- Demonstrate inadequacy of a naive variation on gmapQ

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
