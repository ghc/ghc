{-# LANGUAGE FlexibleInstances #-}

module Expression.PGPredicate (
    module Expression.PG,
    module Expression.Predicate,
    PGPredicate (..),
    fence, (?), ite,
    whenExists,
    remove,
    project,
    linearise
    ) where

import Control.Applicative
import Expression.PG
import Expression.Predicate

type PGPredicate p v = PG (Predicate p) v

fence :: PGPredicate p v -> PGPredicate p v -> PGPredicate p v
fence = Sequence

(?) :: Predicate p -> PGPredicate p v -> PGPredicate p v
(?) = Condition

ite :: Predicate p -> PGPredicate p v -> PGPredicate p v -> PGPredicate p v
ite p t f = Overlay (p ? t) (Not p ? f)

infixl 7 ?

whenExists :: Eq v => v -> PGPredicate p v -> Predicate p
whenExists _ Epsilon         = Evaluated False
whenExists a (Vertex b)      = Evaluated $ a == b
whenExists a (Overlay   l r) = Or (whenExists a l) (whenExists a r)
whenExists a (Sequence  l r) = Or (whenExists a l) (whenExists a r)
whenExists a (Condition x r) = And x               (whenExists a r)

remove :: Eq v => v -> PGPredicate p v -> PGPredicate p v
remove _ Epsilon = Epsilon
remove a v @ (Vertex b)
    | a == b    = Epsilon
    | otherwise = v
remove a (Overlay   l r) = Overlay   (remove a l) (remove a r)
remove a (Sequence  l r) = Sequence  (remove a l) (remove a r)
remove a (Condition x r) = Condition x            (remove a r)

-- Partially evaluate a PG using a truth-teller (compute a 'projection')
project :: TruthTeller p -> PGPredicate p v -> PGPredicate p v
project t = mapP (evaluate t)

-- Linearise a PG into a list. Returns Nothing if the given expression
-- cannot be uniquely evaluated due to remaining parameters.
-- Overlay subexpressions are evaluated in arbitrary order.
linearise :: PGPredicate p v -> Maybe [v]
linearise Epsilon         = Just []
linearise (Vertex v)      = Just [v]
linearise (Overlay   l r) = (++) <$> linearise l <*> linearise r -- TODO: union
linearise (Sequence  l r) = (++) <$> linearise l <*> linearise r
linearise (Condition x r) = case tellTruth x of
    Just True  -> linearise r
    Just False -> Just []
    Nothing    -> Nothing
