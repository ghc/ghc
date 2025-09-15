-- The following program should trigger all warnings in the -Wcompat
-- warning group.

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module WCompatWarningsOnOff
  ( pattern Just, pattern Left, pattern (:|)    -- triggers -Wpattern-namespace-specifier
  ) where

import Data.List.NonEmpty (pattern (:|))        -- triggers -Wpattern-namespace-specifier

