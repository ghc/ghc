{-# LANGUAGE ExperimentalDependentTypes, Modifiers #-}
module PreludeDropWhileSource (Suffix) where

import GHC.Exts (U)

-- | Use the installed suffix function in a type.
type Suffix :: (a %U -> Bool) %U -> [a] %U -> [a]
type Suffix p xs = dropWhile p xs
