{-# LANGUAGE FlexibleInstances #-}

module Expression.Predicate (
    module Expression.TruthTeller,
    Predicate (..),
    multiOr, multiAnd,
    evaluate, tellTruth
    ) where

import Control.Applicative
import Expression.TruthTeller

-- An abstract datatype for predicates that can depend on unevaluated variables
data Predicate a = Evaluated Bool                  -- Evaluated predicate
                 | Parameter a                     -- To be evaluated later
                 | Not (Predicate a)               -- Negate predicate
                 | And (Predicate a) (Predicate a) -- Conjunction
                 | Or  (Predicate a) (Predicate a) -- Disjunction

multiOr :: [Predicate a] -> Predicate a
multiOr = foldr Or (Evaluated False)

multiAnd :: [Predicate a] -> Predicate a
multiAnd = foldr And (Evaluated True)

-- Partially evaluate a Predicate using a TruthTeller
evaluate :: TruthTeller a -> Predicate a -> Predicate a
evaluate _ p @ (Evaluated _) = p
evaluate t p @ (Parameter q) = case t q of
    Just bool -> Evaluated bool
    Nothing   -> p
evaluate t (Not p  ) = Not (evaluate t p)
evaluate t (And p q) = And (evaluate t p) (evaluate t q)
evaluate t (Or  p q) = Or  (evaluate t p) (evaluate t q)

-- Attempt to fully evaluate a predicate (a truth teller!). Returns Nothing if
-- the predicate cannot be evaluated due to remaining parameters.
tellTruth :: TruthTeller (Predicate a)
tellTruth (Evaluated bool) = Just bool
tellTruth (Not p)          = not <$> tellTruth p
tellTruth (And p q)
    | p' == Just False || q' == Just False = Just False
    | p' == Just True  && q' == Just True  = Just True
    | otherwise                            = Nothing
  where
    p' = tellTruth p
    q' = tellTruth q
tellTruth (Or p q)
    | p' == Just True  || q' == Just True  = Just True
    | p' == Just False && q' == Just False = Just False
    | otherwise                            = Nothing
  where
    p' = tellTruth p
    q' = tellTruth q
tellTruth (Parameter _) = Nothing -- cannot evaluate Parameter
