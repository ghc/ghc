{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
module CharacterDecision (Decision(..), Space, spaceDecision) where

import qualified Data.Char as Char
import GHC.Matchability (U)

-- | Record the result of a particular character predicate.
data Decision (p :: Char %U -> Bool) (result :: Bool) where
    Yes :: Decision p True
    No  :: Decision p False

-- | Match the runtime result to refine the index of the decision.
decide :: foreach (p :: Char -> Bool) -> foreach (c :: Char) -> Decision p (p c)
decide p c = case p c of
    True -> Yes
    False -> No

-- | Apply the predicate by its name. This index stays opaque on literals.
type Space (c :: Char) = (Char.isSpace :: Char %U -> Bool) c

-- | Classify a character with the imported whitespace predicate.
spaceDecision :: foreach (c :: Char) -> Decision Char.isSpace (Space c)
spaceDecision c = decide Char.isSpace c
