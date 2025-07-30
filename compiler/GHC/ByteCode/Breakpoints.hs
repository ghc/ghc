{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- | Breakpoint information constructed during ByteCode generation.
--
-- Specifically, code-generation breakpoints are referred to as "internal
-- breakpoints", the internal breakpoint data for a module is stored in
-- 'InternalModBreaks', and is uniquely identified at runtime by an
-- 'InternalBreakpointId'.
--
-- See Note [ModBreaks vs InternalModBreaks] and Note [Breakpoint identifiers]
module GHC.ByteCode.Breakpoints
  ( -- * Internal Mod Breaks
    InternalModBreaks(..), CgBreakInfo(..)
  , mkInternalModBreaks, imodBreaks_module

    -- ** Internal breakpoint identifier
  , InternalBreakpointId(..), BreakInfoIndex
  , InternalBreakLoc(..)

    -- * Operations

    -- ** Internal-level operations
  , getInternalBreak

    -- ** Source-level information operations
  , getBreakLoc, getBreakVars, getBreakDecls, getBreakCCS
  , getBreakSourceId, getBreakSourceMod

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
import GHC.Utils.Binary
import GHC.Utils.Outputable
import GHC.Utils.Panic
import Data.Array

{-
Note [ModBreaks vs InternalModBreaks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'ModBreaks' and 'BreakpointId's must not to be confused with
'InternalModBreaks' and 'InternalBreakId's.

'ModBreaks' is constructed once during HsToCore from the information attached
to source-level breakpoint ticks and is never changed afterwards. A 'ModBreaks'
can be queried using 'BreakpointId's, which uniquely identifies a breakpoint
within the list of breakpoint information for a given module's 'ModBreaks'.

'InternalModBreaks' are constructed during bytecode generation and are indexed
by a 'InternalBreakpointId'. They contain all the information relevant to a
breakpoint for code generation that can be accessed during runtime execution
(such as a 'BreakArray' for triggering breakpoints). 'InternalBreakpointId's
are used at runtime to trigger and inspect breakpoints -- a 'BRK_FUN'
instruction receives 'InternalBreakpointId' as an argument.

We keep a mapping from 'InternalModBreaks' to a 'BreakpointId', which can then be used
to get source-level information about a breakpoint via the corresponding 'ModBreaks'.

Notably, 'InternalModBreaks' can contain entries for so-called internal
breakpoints, which do not necessarily have a source-level location attached to
it (i.e. do not have a matching entry in 'ModBreaks'). We may leverage this to
introduce breakpoints during code generation for features such as stepping-out.

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

When the internal breakpoint has a matching tick-level breakpoint we can fetch
the related tick-level information by first looking up a mapping
@'InternalBreakpointId' -> 'BreakpointId'@ in @'CgBreakInfo'@.
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
  { ibi_info_mod   :: !Module         -- ^ Breakpoint info module
  , ibi_info_index :: !BreakInfoIndex -- ^ Breakpoint info index
  }
  deriving (Eq, Ord)

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
        -- ^ Store the ModBreaks for this module
        --
        -- Recall Note [Breakpoint identifiers]: for some module A, an
        -- *occurrence* of a breakpoint in A may have been inlined from some
        -- breakpoint *defined* in module B.
        --
        -- This 'ModBreaks' contains information regarding all the breakpoints
        -- defined in the module this 'InternalModBreaks' corresponds to. It
        -- /does not/ necessarily have information regarding all the breakpoint
        -- occurrences registered in 'imodBreaks_breakInfo'. Some of those
        -- occurrences may refer breakpoints inlined from other modules.
      }

-- | Construct an 'InternalModBreaks'.
--
-- INVARIANT: The given 'ModBreaks' correspond to the same module as this
-- 'InternalModBreaks' module (the first argument) and its breakpoint infos
-- (the @IntMap CgBreakInfo@ argument)
mkInternalModBreaks :: Module -> IntMap CgBreakInfo -> ModBreaks -> InternalModBreaks
mkInternalModBreaks mod im mbs =
  assertPpr (mod == modBreaks_module mbs)
    (text "Constructing InternalModBreaks with the ModBreaks of a different module!") $
      InternalModBreaks im mbs

-- | Get the module to which these 'InternalModBreaks' correspond
imodBreaks_module :: InternalModBreaks -> Module
imodBreaks_module = modBreaks_module . imodBreaks_modBreaks

-- | Information about a breakpoint that we know at code-generation time
-- In order to be used, this needs to be hydrated relative to the current HscEnv by
-- 'hydrateCgBreakInfo'. Everything here can be fully forced and that's critical for
-- preventing space leaks (see #22530)
data CgBreakInfo
   = CgBreakInfo
   { cgb_tyvars  :: ![IfaceTvBndr] -- ^ Type variables in scope at the breakpoint
   , cgb_vars    :: ![Maybe (IfaceIdBndr, Word)]
   , cgb_resty   :: !IfaceType
   , cgb_tick_id :: !(Either InternalBreakLoc BreakpointId)
     -- ^ This field records the original breakpoint tick identifier for this
     -- internal breakpoint info. It is used to convert a breakpoint
     -- *occurrence* index ('InternalBreakpointId') into a *definition* index
     -- ('BreakpointId').
     --
     -- The modules of breakpoint occurrence and breakpoint definition are not
     -- necessarily the same: See Note [Breakpoint identifiers].
     --
     -- If there is no original tick identifier (that is, the breakpoint was
     -- created during code generation), we re-use the BreakpointId of something else.
     -- It would also be reasonable to have an @Either something BreakpointId@
     -- for @cgb_tick_id@, but currently we can always re-use a source-level BreakpointId.
     -- In the case of step-out, see Note [Debugger: Stepout internal break locs]
   }
-- See Note [Syncing breakpoint info] in GHC.Runtime.Eval

-- | Breakpoints created during code generation don't have a source-level tick
-- location. Instead, we re-use an existing one.
newtype InternalBreakLoc = InternalBreakLoc { internalBreakLoc :: BreakpointId }
  deriving newtype (Eq, NFData, Outputable)

-- | Get an internal breakpoint info by 'InternalBreakpointId'
getInternalBreak :: InternalBreakpointId -> InternalModBreaks -> CgBreakInfo
getInternalBreak (InternalBreakpointId mod ix) imbs =
  assert_modules_match mod (imodBreaks_module imbs) $
    imodBreaks_breakInfo imbs IM.! ix

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

-- | Get the source module and tick index for this breakpoint
-- (as opposed to the module where this breakpoint occurs, which is in 'InternalBreakpointId')
getBreakSourceId :: InternalBreakpointId -> InternalModBreaks -> BreakpointId
getBreakSourceId (InternalBreakpointId ibi_mod ibi_ix) imbs =
  assert_modules_match ibi_mod (imodBreaks_module imbs) $
    let cgb = imodBreaks_breakInfo imbs IM.! ibi_ix
     in either internalBreakLoc id (cgb_tick_id cgb)

-- | Get the source module for this breakpoint (where the breakpoint is defined)
getBreakSourceMod :: InternalBreakpointId -> InternalModBreaks -> Module
getBreakSourceMod (InternalBreakpointId ibi_mod ibi_ix) imbs =
  assert_modules_match ibi_mod (imodBreaks_module imbs) $
    let cgb = imodBreaks_breakInfo imbs IM.! ibi_ix
     in either (bi_tick_mod . internalBreakLoc) bi_tick_mod (cgb_tick_id cgb)

-- | Get the source span for this breakpoint
getBreakLoc :: (Module -> IO ModBreaks) -> InternalBreakpointId -> InternalModBreaks -> IO SrcSpan
getBreakLoc = getBreakXXX modBreaks_locs

-- | Get the vars for this breakpoint
getBreakVars :: (Module -> IO ModBreaks) -> InternalBreakpointId -> InternalModBreaks -> IO [OccName]
getBreakVars = getBreakXXX modBreaks_vars

-- | Get the decls for this breakpoint
getBreakDecls :: (Module -> IO ModBreaks) -> InternalBreakpointId -> InternalModBreaks -> IO [String]
getBreakDecls = getBreakXXX modBreaks_decls

-- | Get the decls for this breakpoint
getBreakCCS :: (Module -> IO ModBreaks) -> InternalBreakpointId -> InternalModBreaks -> IO ((String, String))
getBreakCCS = getBreakXXX modBreaks_ccs

-- | Internal utility to access a ModBreaks field at a particular breakpoint index
--
-- Recall Note [Breakpoint identifiers]: the internal breakpoint module (the
-- *occurrence* module) doesn't necessarily match the module where the
-- tick breakpoint was defined with the relevant 'ModBreaks'.
--
-- When the tick module is the same as the internal module, we use the stored
-- 'ModBreaks'. When the tick module is different, we need to look up the
-- 'ModBreaks' in the HUG for that other module.
--
-- When there is no tick module (the breakpoint was generated at codegen), use
-- the function on internal mod breaks.
--
-- To avoid cyclic dependencies, we instead receive a function that looks up
-- the 'ModBreaks' given a 'Module'
getBreakXXX :: (ModBreaks -> Array BreakTickIndex a) -> (Module -> IO ModBreaks) -> InternalBreakpointId -> InternalModBreaks -> IO a
getBreakXXX view lookupModule (InternalBreakpointId ibi_mod ibi_ix) imbs =
  assert_modules_match ibi_mod (imodBreaks_module imbs) $ do
    let cgb = imodBreaks_breakInfo imbs IM.! ibi_ix
    case either internalBreakLoc id (cgb_tick_id cgb) of
      BreakpointId{bi_tick_mod, bi_tick_index}
        | bi_tick_mod == ibi_mod
        -> do
          let these_mbs = imodBreaks_modBreaks imbs
          return $ view these_mbs ! bi_tick_index
        | otherwise
        -> do
          other_mbs <- lookupModule bi_tick_mod
          return $ view other_mbs ! bi_tick_index

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
        rnf cgb_resty `seq`
        rnf cgb_tick_id

instance Outputable InternalBreakpointId where
  ppr InternalBreakpointId{..} =
    text "InternalBreakpointId" <+> ppr ibi_info_mod <+> ppr ibi_info_index

instance NFData InternalBreakpointId where
  rnf InternalBreakpointId{..} =
    rnf ibi_info_mod `seq` rnf ibi_info_index

instance Outputable CgBreakInfo where
   ppr info = text "CgBreakInfo" <+>
              parens (ppr (cgb_vars info) <+>
                      ppr (cgb_resty info) <+>
                      ppr (cgb_tick_id info))

instance Binary CgBreakInfo where
  put_ bh CgBreakInfo {..} =
    put_ bh cgb_tyvars
      *> put_ bh cgb_vars
      *> put_ bh cgb_resty
      *> put_ bh cgb_tick_id

  get bh = CgBreakInfo <$> get bh <*> get bh <*> get bh <*> get bh

instance Binary InternalModBreaks where
  get bh = InternalModBreaks <$> get bh <*> get bh

  put_ bh InternalModBreaks {..} =
    put_ bh imodBreaks_breakInfo *> put_ bh imodBreaks_modBreaks

deriving via BreakpointId instance Binary InternalBreakLoc

instance Binary InternalBreakpointId where
  get bh = InternalBreakpointId <$> get bh <*> get bh

  put_ bh InternalBreakpointId {..} =
    put_ bh ibi_info_mod *> put_ bh ibi_info_index
