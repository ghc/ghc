-- | Breakpoint related types
module GHC.Types.Breakpoint
  ( BreakpointId (..)
  , InternalBreakpointId (..)
  , toBreakpointId
  )
where

import GHC.Prelude
import GHC.Unit.Module

-- | Breakpoint identifier.
--
-- See Note [Breakpoint identifiers]
data BreakpointId = BreakpointId
  { bi_tick_mod   :: !Module  -- ^ Breakpoint tick module
  , bi_tick_index :: !Int     -- ^ Breakpoint tick index
  }

-- | Internal breakpoint identifier
--
-- See Note [Breakpoint identifiers]
data InternalBreakpointId = InternalBreakpointId
  { ibi_tick_mod   :: !Module  -- ^ Breakpoint tick module
  , ibi_tick_index :: !Int     -- ^ Breakpoint tick index
  , ibi_info_mod   :: !Module  -- ^ Breakpoint info module
  , ibi_info_index :: !Int     -- ^ Breakpoint info index
  }

toBreakpointId :: InternalBreakpointId -> BreakpointId
toBreakpointId ibi = BreakpointId
  { bi_tick_mod   = ibi_tick_mod ibi
  , bi_tick_index = ibi_tick_index ibi
  }


-- Note [Breakpoint identifiers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Before optimization a breakpoint is identified uniquely with a tick module
-- and a tick index. See BreakpointId. A tick module contains an array, indexed
-- with the tick indexes, which indicates breakpoint status.
--
-- When we generate ByteCode, we collect information for every breakpoint at
-- their *occurrence sites* (see CgBreakInfo in GHC.ByteCode.Types) and these info
-- are stored in the ModIface of the occurrence module. Because of inlining, we
-- can't reuse the tick index to uniquely identify an occurrence; because of
-- cross-module inlining, we can't assume that the occurrence module is the same
-- as the tick module (#24712).
--
-- So every breakpoint occurrence gets assigned a module-unique *info index* and
-- we store it alongside the occurrence module (*info module*) in the
-- InternalBreakpointId datatype.
