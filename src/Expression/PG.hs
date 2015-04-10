{-# LANGUAGE FlexibleInstances #-}

module Expression.PG (
    PG (..) -- , fromList, mapP
    ) where

import Data.Monoid

-- A generic Parameterised Graph datatype
-- * p is the type of predicates
-- * v is the type of vertices
data PG p v = Epsilon
            | Vertex v
            | Overlay (PG p v) (PG p v)
            | Sequence (PG p v) (PG p v)
            | Condition p (PG p v)

instance Monoid (PG p v) where
    mempty  = Epsilon
    mappend = Overlay

-- For constructing a PG from an unordered list use mconcat.
--fromList :: [v] -> PG p v
--fromList = foldr Sequence Epsilon . map Vertex

-- Map over all PG predicates, e.g., partially evaluate a given PG.
--mapP :: (p -> p) -> PG p v -> PG p v
--mapP _ Epsilon         = Epsilon
--mapP _ v @ (Vertex _)  = v
--mapP f (Overlay   l r) = Overlay   (mapP f l) (mapP f r)
--mapP f (Sequence  l r) = Sequence  (mapP f l) (mapP f r)
--mapP f (Condition x r) = Condition (f x     ) (mapP f r)

