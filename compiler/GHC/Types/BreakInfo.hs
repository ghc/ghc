-- | A module for the BreakInfo type. Used by both the GHC.Runtime.Eval and
-- GHC.Runtime.Interpreter hierarchy, so put here to have a less deep module
-- dependency tree
module GHC.Types.BreakInfo (BreakInfo(..)) where

import GHC.Prelude
import GHC.Unit.Module

data BreakInfo = BreakInfo
  { breakInfo_module :: Module
  , breakInfo_number :: Int
  }
