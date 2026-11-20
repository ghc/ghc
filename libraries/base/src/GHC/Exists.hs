{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Explicit construction and matching of erased existential packages.

'Pack' is special syntax when ExperimentalDependentTypes is enabled.
-}
module GHC.Exists (pattern Pack) where

-- This declaration gives Pack a constructor name for ordinary import and
-- export processing. Its type is private and its constructor is not used
-- in the Core for existential packages.
data PackTag = Pack
