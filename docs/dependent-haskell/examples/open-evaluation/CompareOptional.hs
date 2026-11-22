{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module CompareOptional (readJust) where

import CompareAt (Result, Witness(..), decideOptional)

-- | Pass a constructed second argument through an imported unfolding.
readJust :: (Result 'False -> Maybe Bool -> Ordering) -> Bool -> Bool -> Ordering
readJust cmp x y = case decideOptional cmp x (Just y) of
    Less -> LT
    Equal -> EQ
    Greater -> GT
