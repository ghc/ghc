{-# LANGUAGE FlexibleInstances #-}

module Expression.TruthTeller (
    TruthTeller (..)
    ) where

import Data.Monoid

-- TruthTeller takes an argument and attempts to determine its truth.
-- Returns Nothing if the truth cannot be determined.
type TruthTeller a = a -> Maybe Bool

-- Monoid instance for truth-tellers (asks them one by one)
instance Monoid (TruthTeller a) where
    mempty        = const Nothing
    p `mappend` q = \a -> getFirst $ First (p a) <> First (q a)
