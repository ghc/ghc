{-# LANGUAGE Safe #-}

-- | Matchability of arrows in types and kinds. Use 'M' for a matchable
-- arrow and 'U' for an unmatchable arrow.
module GHC.Matchability
  ( Matchability(..)
  , M
  , U
  ) where

import GHC.Internal.Types (Matchability(..), M, U)
