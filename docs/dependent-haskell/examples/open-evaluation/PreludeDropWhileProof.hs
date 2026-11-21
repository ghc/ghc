{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeDropWhileProof (prefixAndSuffix) where

import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended)
import PreludeDropWhileSource (Suffix)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterRuntime (BooleanResult(..))
import PreludeTakeWhile (Prefix)

-- | Reconstruct the input list from its checked prefix and suffix.
prefixAndSuffix :: FilterDecisions p xs -> Appended (Prefix p xs) (Suffix p xs) :~: xs
prefixAndSuffix Done = Refl
prefixAndSuffix (Decide IsFalse _) = Refl
prefixAndSuffix (Decide IsTrue ds) = case prefixAndSuffix ds of
    Refl -> Refl
