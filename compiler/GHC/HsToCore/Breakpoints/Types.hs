{-# LANGUAGE RecordWildCards #-}

-- | Information attached to Breakpoints generated from Ticks
--
-- The breakpoint information stored in 'ModBreaks' is generated during
-- desugaring from the ticks annotating the source expressions.
--
-- This information can be queried per-breakpoint using the 'BreakpointId'
-- datatype, which indexes tick-level breakpoint information.
--
-- 'ModBreaks' and 'BreakpointId's are not to be confused with
-- 'InternalModBreaks' and 'InternalBreakId's. The latter are constructed
-- during bytecode generation and can be found in 'GHC.ByteCode.Breakpoints'.
--
-- See Note [ModBreaks vs InternalModBreaks] and Note [Breakpoint identifiers]
module GHC.HsToCore.Breakpoints.Types
  ( -- * ModBreaks
    ModBreaks(..), modBreaks_locs

    -- ** Re-exports BreakpointId
  , BreakpointId(..), BreakTickIndex
  ) where

import GHC.Prelude
import Data.Array

import GHC.Types.SrcLoc (SrcSpan)
import GHC.Types.Name (OccName)
import GHC.Types.Tickish (BreakTickIndex, BreakpointId(..))
import GHC.Unit.Module (Module)
import Data.Coerce
import GHC.Utils.Binary (BinSrcSpan(..), Binary(..))
import Control.DeepSeq

--------------------------------------------------------------------------------
-- ModBreaks
--------------------------------------------------------------------------------

-- | All the information about the source-relevant breakpoints for a module
--
-- This information is constructed once during desugaring (with `mkModBreaks`)
-- from breakpoint ticks and fixed/unchanged from there on forward. It could be
-- exported as an abstract datatype because it should never be updated after
-- construction, only queried.
--
-- The arrays can be indexed using the int in the corresponding 'BreakpointId'
-- (i.e. the 'BreakpointId' whose 'Module' matches the 'Module' corresponding
-- to these 'ModBreaks') with the accessors 'modBreaks_locs', 'modBreaks_vars',
-- and 'modBreaks_decls'.
data ModBreaks
   = ModBreaks
   { modBreaks_locs_   :: !(Array BreakTickIndex BinSrcSpan)
        -- ^ An array giving the source span of each breakpoint.
   , modBreaks_vars   :: !(Array BreakTickIndex [OccName])
        -- ^ An array giving the names of the free variables at each breakpoint.
   , modBreaks_decls  :: !(Array BreakTickIndex [String])
        -- ^ An array giving the names of the declarations enclosing each breakpoint.
        -- See Note [Field modBreaks_decls]
   , modBreaks_ccs    :: !(Array BreakTickIndex (String, String))
        -- ^ Array pointing to cost centre info for each breakpoint;
        -- actual 'CostCentre' allocation is done at link-time.
   , modBreaks_module :: !Module
        -- ^ The module to which this ModBreaks is associated.
        -- We also cache this here for internal sanity checks.
   }

modBreaks_locs :: ModBreaks -> Array BreakTickIndex SrcSpan
modBreaks_locs = coerce . modBreaks_locs_

instance Binary ModBreaks where
  get bh = ModBreaks <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh

  put_ bh ModBreaks {..} =
    put_ bh modBreaks_locs_
      *> put_ bh modBreaks_vars
      *> put_ bh modBreaks_decls
      *> put_ bh modBreaks_ccs
      *> put_ bh modBreaks_module

instance NFData ModBreaks where
  rnf (ModBreaks a b c d e) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e