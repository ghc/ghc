{-# OPTIONS -Wcompat -Wno-error=pattern-namespace-specifier #-}
{-# LANGUAGE PatternSynonyms, ExplicitNamespaces #-}

module T25900 ( pattern Just
              , pattern Left
              , pattern (:|)
              ) where

import Data.List.NonEmpty (pattern (:|))
