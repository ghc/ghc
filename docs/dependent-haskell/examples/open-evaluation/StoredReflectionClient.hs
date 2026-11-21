{-# LANGUAGE GADTs, NoExperimentalDependentTypes, NoImpredicativeTypes, RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module StoredReflectionClient (readStoredReflection) where

import Data.Type.Equality ((:~:)(Refl))
import StoredReflection (boxedReflection)

-- | Use the same stored proof at Bool and Char.
readStoredReflection :: Maybe Bool -> Maybe Char -> Maybe (Maybe Bool, Maybe Char)
readStoredReflection flag letter = case boxedReflection of
    Nothing -> Nothing
    Just proof -> Just (case proof @Bool flag of Refl -> flag,
                        case proof @Char letter of Refl -> letter)
