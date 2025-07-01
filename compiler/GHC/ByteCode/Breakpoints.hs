{-# LANGUAGE RecordWildCards #-}

-- | Breakpoint information constructed during ByteCode generation.
--
-- Specifically, code-generation breakpoints are referred to as "internal
-- breakpoints", the internal breakpoint data for a module is stored in
-- 'InternalModBreaks', and is uniquely identified at runtime by an
-- 'InternalBreakpointId'.
--
-- See Note [Breakpoint identifiers]
module GHC.ByteCode.Breakpoints
  ( -- * Internal Mod Breaks
    InternalModBreaks(..), CgBreakInfo(..)
  , mkInternalModBreaks

    -- ** Internal breakpoint identifier
  , InternalBreakpointId(..), BreakInfoIndex

    -- * Operations
  , toBreakpointId

    -- ** Internal-level operations
  , getInternalBreak, addInternalBreak

    -- ** Source-level information operations
  , getBreakLoc, getBreakVars, getBreakDecls, getBreakCCS

    -- * Utils
  , seqInternalModBreaks

  )
  where

import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Types.Name.Occurrence
import Control.DeepSeq
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import GHC.HsToCore.Breakpoints
import GHC.Iface.Syntax

import GHC.Unit.Module (Module)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import Data.Array

{-
Note [Breakpoint identifiers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Before optimization a breakpoint is identified uniquely with a tick module
and a tick index. See 'BreakpointId'. A tick module contains an array, indexed
with the tick indexes, which indicates breakpoint status.

When we generate ByteCode, we collect information for every breakpoint at
their *occurrence sites* (see CgBreakInfo) and these info
are stored in the ModIface of the occurrence module. Because of inlining, we
can't reuse the tick index to uniquely identify an occurrence; because of
cross-module inlining, we can't assume that the occurrence module is the same
as the tick module (#24712).

So every breakpoint occurrence gets assigned a module-unique *info index* and
we store it alongside the occurrence module (*info module*) in the
'InternalBreakpointId' datatype. This is the index that we use at runtime to
identify a breakpoint.
-}

--------------------------------------------------------------------------------
-- * Internal breakpoint identifiers
--------------------------------------------------------------------------------

-- | Internal breakpoint info index
type BreakInfoIndex = Int

-- | Internal breakpoint identifier
--
-- Indexes into the structures in the @'InternalModBreaks'@ produced during ByteCode generation.
-- See Note [Breakpoint identifiers]
data InternalBreakpointId = InternalBreakpointId
  { ibi_tick_mod   :: !Module         -- ^ Breakpoint tick module
  , ibi_tick_index :: !Int            -- ^ Breakpoint tick index
  , ibi_info_mod   :: !Module         -- ^ Breakpoint tick module
  , ibi_info_index :: !BreakInfoIndex -- ^ Breakpoint tick index
  }
  deriving (Eq, Ord)

toBreakpointId :: InternalBreakpointId -> BreakpointId
toBreakpointId ibi = BreakpointId
  { bi_tick_mod   = ibi_tick_mod ibi
  , bi_tick_index = ibi_tick_index ibi
  }

--------------------------------------------------------------------------------
-- * Internal Mod Breaks
--------------------------------------------------------------------------------

-- | Internal mod breaks store the runtime-relevant information of breakpoints.
--
-- Importantly, it maps 'InternalBreakpointId's to 'CgBreakInfo'.
--
-- 'InternalModBreaks' are constructed during bytecode generation and stored in
-- 'CompiledByteCode' afterwards.
data InternalModBreaks = InternalModBreaks
      { imodBreaks_breakInfo :: !(IntMap CgBreakInfo)
        -- ^ Access code-gen time information about a breakpoint, indexed by
        -- 'InternalBreakpointId'.

      , imodBreaks_modBreaks :: !ModBreaks
        -- ^ Store the original ModBreaks for this module, unchanged.
        -- Allows us to query about source-level breakpoint information using
        -- an internal breakpoint id.
      }

-- | Construct an 'InternalModBreaks'
mkInternalModBreaks :: Module -> IntMap CgBreakInfo -> ModBreaks -> InternalModBreaks
mkInternalModBreaks mod im mbs =
  assertPpr (mod == modBreaks_module mbs)
    (text "Constructing InternalModBreaks with the ModBreaks of a different module!") $
      InternalModBreaks im mbs

-- | Information about a breakpoint that we know at code-generation time
-- In order to be used, this needs to be hydrated relative to the current HscEnv by
-- 'hydrateCgBreakInfo'. Everything here can be fully forced and that's critical for
-- preventing space leaks (see #22530)
data CgBreakInfo
   = CgBreakInfo
   { cgb_tyvars  :: ![IfaceTvBndr] -- ^ Type variables in scope at the breakpoint
   , cgb_vars    :: ![Maybe (IfaceIdBndr, Word)]
   , cgb_resty   :: !IfaceType
   }
-- See Note [Syncing breakpoint info] in GHC.Runtime.Eval

-- | Get an internal breakpoint info by 'InternalBreakpointId'
getInternalBreak :: InternalBreakpointId -> InternalModBreaks -> CgBreakInfo
getInternalBreak (InternalBreakpointId _ _ info_mod info_ix) imbs =
  assert_modules_match info_mod (modBreaks_module $ imodBreaks_modBreaks imbs) $
    imodBreaks_breakInfo imbs IM.! info_ix

-- | Add a CgBreakInfo to an 'InternalModBreaks' at 'InternalBreakpointId'
addInternalBreak :: InternalBreakpointId -> CgBreakInfo -> InternalModBreaks -> InternalModBreaks
addInternalBreak (InternalBreakpointId _ _ info_mod info_ix) info imbs =
  assert_modules_match info_mod (modBreaks_module $ imodBreaks_modBreaks imbs) $
    imbs{imodBreaks_breakInfo = IM.insert info_ix info (imodBreaks_breakInfo imbs)}

-- | Assert that the module in the 'InternalBreakpointId' and in
-- 'InternalModBreaks' match.
assert_modules_match :: Module -> Module -> a -> a
assert_modules_match ibi_mod imbs_mod =
  assertPpr (ibi_mod == imbs_mod)
    (text "Tried to query the InternalModBreaks of module" <+> ppr imbs_mod
        <+> text "with an InternalBreakpointId for module" <+> ppr ibi_mod)

--------------------------------------------------------------------------------
-- Tick-level Breakpoint information
--------------------------------------------------------------------------------

-- | Get the source span for this breakpoint
getBreakLoc  :: InternalBreakpointId -> InternalModBreaks -> SrcSpan
getBreakLoc = getBreakXXX modBreaks_locs

-- | Get the vars for this breakpoint
getBreakVars  :: InternalBreakpointId -> InternalModBreaks -> [OccName]
getBreakVars = getBreakXXX modBreaks_vars

-- | Get the decls for this breakpoint
getBreakDecls :: InternalBreakpointId -> InternalModBreaks -> [String]
getBreakDecls = getBreakXXX modBreaks_decls

-- | Get the decls for this breakpoint
getBreakCCS :: InternalBreakpointId -> InternalModBreaks -> (String, String)
getBreakCCS = getBreakXXX modBreaks_ccs

-- | Internal utility to access a ModBreaks field at a particular breakpoint index
getBreakXXX :: (ModBreaks -> Array BreakTickIndex a) -> InternalBreakpointId -> InternalModBreaks -> a
getBreakXXX view (InternalBreakpointId tick_mod tick_id _ _) imbs =
  assert_modules_match tick_mod (modBreaks_module $ imodBreaks_modBreaks imbs) $ do
    view (imodBreaks_modBreaks imbs) ! tick_id

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- | Fully force an 'InternalModBreaks' value
seqInternalModBreaks :: InternalModBreaks -> ()
seqInternalModBreaks InternalModBreaks{..} =
    rnf (fmap seqCgBreakInfo imodBreaks_breakInfo)
  where
    seqCgBreakInfo :: CgBreakInfo -> ()
    seqCgBreakInfo CgBreakInfo{..} =
        rnf cgb_tyvars `seq`
        rnf cgb_vars `seq`
        rnf cgb_resty

instance Outputable InternalBreakpointId where
  ppr InternalBreakpointId{..} =
    text "InternalBreakpointId" <+> ppr ibi_info_mod <+> ppr ibi_info_index

instance NFData InternalBreakpointId where
  rnf InternalBreakpointId{..} =
    rnf ibi_info_mod `seq` rnf ibi_info_index

instance Outputable CgBreakInfo where
   ppr info = text "CgBreakInfo" <+>
              parens (ppr (cgb_vars info) <+>
                      ppr (cgb_resty info))
