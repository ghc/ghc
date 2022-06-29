-----------------------------------------------------------------------------
--
-- Sequel type for Stg to C-- code generation
--
-- (c) The University of Glasgow 2004-2006
--
-- This module is just a bucket of types used in StgToCmm.Monad and
-- StgToCmm.Closure. Its sole purpose is to break a cyclic dependency between
-- StgToCmm.Monad and StgToCmm.Closure which derives from coupling around
-- the BlockId and LocalReg types
-----------------------------------------------------------------------------

module GHC.StgToCmm.Sequel
  ( Sequel(..)
  , SelfLoopInfo
  ) where

import GHC.Cmm.BlockId
import GHC.Cmm

import GHC.Types.Id
import GHC.Utils.Outputable

import GHC.Prelude

--------------------------------------------------------------------------------
-- | A Sequel tells what to do with the result of this expression
data Sequel
  = Return              -- ^ Return result(s) to continuation found on the stack.

  | AssignTo
        [LocalReg]      -- ^ Put result(s) in these regs and fall through
                        -- NB: no void arguments here
                        --
        Bool            -- ^ Should we adjust the heap pointer back to recover
                        -- space that's unused on this path? We need to do this
                        -- only if the expression may allocate (e.g. it's a
                        -- foreign call or allocating primOp)

instance Outputable Sequel where
    ppr Return = text "Return"
    ppr (AssignTo regs b) = text "AssignTo" <+> ppr regs <+> ppr b

type SelfLoopInfo = (Id, BlockId, [LocalReg])
--------------------------------------------------------------------------------
