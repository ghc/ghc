{-# LANGUAGE FlexibleInstances #-}

module Expression.PG (
    module Expression.Predicate,
    PG (..),
    fromList,
    mapP,
    project,
    linearise
    ) where

import Data.Monoid
import Control.Applicative
import Expression.Predicate

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
fromList :: [v] -> PG p v
fromList = foldr Sequence Epsilon . map Vertex

-- Map over all PG predicates, e.g., partially evaluate a given PG.
mapP :: (p -> p) -> PG p v -> PG p v
mapP _ Epsilon         = Epsilon
mapP _ v @ (Vertex _)  = v
mapP f (Overlay   l r) = Overlay   (mapP f l) (mapP f r)
mapP f (Sequence  l r) = Sequence  (mapP f l) (mapP f r)
mapP f (Condition x r) = Condition (f x     ) (mapP f r)

-- Partially evaluate a PG using a truth-teller (compute a 'projection')
project :: TruthTeller a -> PG (Predicate a) v -> PG (Predicate a) v
project t = mapP (evaluate t)

-- Linearise a PG into a list. Returns Nothing if the given expression
-- cannot be uniquely evaluated due to remaining parameters.
-- Overlay subexpressions are evaluated in arbitrary order.
linearise :: PG (Predicate a) v -> Maybe [v]
linearise Epsilon         = Just []
linearise (Vertex v)      = Just [v]
linearise (Overlay   l r) = (++) <$> linearise l <*> linearise r -- TODO: union
linearise (Sequence  l r) = (++) <$> linearise l <*> linearise r
linearise (Condition x r) = case tellTruth x of
    Just True  -> linearise r
    Just False -> Just []
    Nothing    -> Nothing
