{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeDropWhileSource (Suffix, Rejoined) where

import GHC.Exts (U)
import PreludeAppendLengthSource (Appended)

-- | Use the installed suffix function in a type.
type Suffix :: (a %U -> Bool) %U -> [a] %U -> [a]
type Suffix p xs = dropWhile p xs

-- | Append the two lists selected by the installed pair projections.
type Rejoined :: ([a], [a]) -> [a]
type Rejoined parts = Appended (fst parts) (snd parts)
