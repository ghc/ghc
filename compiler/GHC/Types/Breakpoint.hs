{-# LANGUAGE RecordWildCards #-}

-- | Breakpoint related types
module GHC.Types.Breakpoint
  ( BreakpointId (..)
  , InternalBreakpointId (..)
  , BreakTickIndex, BreakInfoIndex
  )
where

import Control.DeepSeq
import GHC.Prelude
import GHC.Unit.Module
import GHC.Utils.Outputable
import Data.Data (Data)

-- | Breakpoint tick index
type BreakTickIndex = Int

-- | Internal breakpoint info index
type BreakInfoIndex = Int

-- | Breakpoint identifier.
--
-- See Note [Breakpoint identifiers]
data BreakpointId = BreakpointId
  { bi_tick_mod   :: !Module         -- ^ Breakpoint tick module
  , bi_tick_index :: !BreakTickIndex -- ^ Breakpoint tick index
  }
  deriving (Eq, Ord, Data)

-- | Internal breakpoint identifier
--
-- See Note [Breakpoint identifiers]
data InternalBreakpointId = InternalBreakpointId
  { ibi_info_mod   :: !Module         -- ^ Breakpoint tick module
  , ibi_info_index :: !BreakInfoIndex -- ^ Breakpoint tick index
  }
  deriving (Eq, Ord)

-- Note [Breakpoint identifiers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ROMES:TODO: UPDATE NOTE
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

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Outputable BreakpointId where
  ppr BreakpointId{..} =
    text "BreakpointId" <+> ppr bi_tick_mod <+> ppr bi_tick_index

instance Outputable InternalBreakpointId where
  ppr InternalBreakpointId{..} =
    text "InternalBreakpointId" <+> ppr ibi_info_mod <+> ppr ibi_info_index

instance NFData BreakpointId where
  rnf BreakpointId{..} =
    rnf bi_tick_mod `seq` rnf bi_tick_index

instance NFData InternalBreakpointId where
  rnf InternalBreakpointId{..} =
    rnf ibi_info_mod `seq` rnf ibi_info_index
