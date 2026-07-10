{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module General.ListBuilder(
    ListBuilder, runListBuilder, newListBuilder,
    Tree(..), flattenTree, unflattenTree
    ) where

import Data.Semigroup
import Prelude


-- ListBuilder is opaque outside this module
newtype ListBuilder a = ListBuilder (Tree a)
    deriving (Semigroup, Monoid, Functor)

data Tree a
    = Empty
    | Leaf a
    | Branch (Tree a) (Tree a)
      deriving (Functor,Eq,Ord,Show)


instance Semigroup (Tree a) where
    Empty <> x = x
    x <> Empty = x
    x <> y = Branch x y

instance Monoid (Tree a) where
    mempty = Empty
    mappend = (<>)

flattenTree :: Tree a -> [a]
flattenTree x = f x []
    where
        f Empty acc = acc
        f (Leaf x) acc = x : acc
        f (Branch x y) acc = f x (f y acc)

unflattenTree :: Tree a -> [b] -> Tree b
unflattenTree t xs = fst $ f t xs
    where
        f Empty xs = (Empty, xs)
        f Leaf{} (x:xs) = (Leaf x, xs)
        f (Branch a b) xs = (Branch a2 b2, xs3)
            where (a2, xs2) = f a xs
                  (b2, xs3) = f b xs2

newListBuilder :: a -> ListBuilder a
newListBuilder = ListBuilder . Leaf

runListBuilder :: ListBuilder a -> [a]
runListBuilder (ListBuilder x) = flattenTree x
