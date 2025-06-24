{-# LANGUAGE RecordWildCards #-}

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
    InternalModBreaks(imodBreaks_breakInfo), CgBreakInfo(..)
  , mkInternalModBreaks

    -- ** Operations
  , getInternalBreak, addInternalBreak

    -- ** Internal breakpoint identifier
  , InternalBreakpointId(..), BreakInfoIndex

    -- * Utils
  , seqInternalModBreaks

  )
  where

import GHC.Prelude
import Control.DeepSeq
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import GHC.Iface.Syntax
import GHC.Types.Tickish

import GHC.Unit.Module (Module)
import GHC.Utils.Outputable
import GHC.Utils.Panic

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
@'InternalBreakpointId' -> 'BreakpointId'@. See `internalBreakIdToBreakId`
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
  { ibi_info_mod   :: !Module         -- ^ Breakpoint tick module
  , ibi_info_index :: !BreakInfoIndex -- ^ Breakpoint tick index
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
      , imodBreaks_module    :: !Module
        -- ^ Cache the module corresponding to these 'InternalModBreaks' for
        -- sanity checks. Don't export it!
      }

-- | Construct an 'InternalModBreaks'
mkInternalModBreaks :: Module -> IntMap CgBreakInfo -> InternalModBreaks
mkInternalModBreaks mod im = InternalModBreaks im mod

-- | Information about a breakpoint that we know at code-generation time
-- In order to be used, this needs to be hydrated relative to the current HscEnv by
-- 'hydrateCgBreakInfo'. Everything here can be fully forced and that's critical for
-- preventing space leaks (see #22530)
data CgBreakInfo
   = CgBreakInfo
   { cgb_tyvars  :: ![IfaceTvBndr] -- ^ Type variables in scope at the breakpoint
   , cgb_vars    :: ![Maybe (IfaceIdBndr, Word)]
   , cgb_resty   :: !IfaceType
   , cgb_tick_id :: !BreakpointId
     -- ^ This field records the original breakpoint tick identifier for this
     -- internal breakpoint info. See Note [Breakpoint identifiers].
   }
-- See Note [Syncing breakpoint info] in GHC.Runtime.Eval

-- | Get an internal breakpoint info by 'InternalBreakpointId'
getInternalBreak :: InternalBreakpointId -> InternalModBreaks -> CgBreakInfo
getInternalBreak (InternalBreakpointId mod ix) imbs =
  assert_modules_match mod (imodBreaks_module imbs) $
    imodBreaks_breakInfo imbs IM.! ix

-- | Add a CgBreakInfo to an 'InternalModBreaks' at 'InternalBreakpointId'
addInternalBreak :: InternalBreakpointId -> CgBreakInfo -> InternalModBreaks -> InternalModBreaks
addInternalBreak (InternalBreakpointId mod ix) info imbs =
  assert_modules_match mod (imodBreaks_module imbs) $
    imbs{imodBreaks_breakInfo = IM.insert ix info (imodBreaks_breakInfo imbs)}

-- | Assert that the module in the 'InternalBreakpointId' and in
-- 'InternalModBreaks' match.
assert_modules_match :: Module -> Module -> a -> a
assert_modules_match ibi_mod imbs_mod =
  assertPpr (ibi_mod == imbs_mod)
    (text "Tried to query the InternalModBreaks of module" <+> ppr imbs_mod
        <+> text "with an InternalBreakpointId for module" <+> ppr ibi_mod)

-- TODO: See what Cheng has in .
-- mkCCSArray
--   :: Interp -> Module -> Int -> [Tick]
--   -> IO (Array BreakTickIndex (RemotePtr GHC.Stack.CCS.CostCentre))
-- mkCCSArray interp modul count entries
--   | interpreterProfiled interp = do
--       let module_str = moduleNameString (moduleName modul)
--       costcentres <- GHCi.mkCostCentres interp module_str (map mk_one entries)
--       return (listArray (0,count-1) costcentres)
--   | otherwise = return (listArray (0,-1) [])
--  where
--     mk_one t = (name, src)
--       where name = concat $ intersperse "." $ tick_path t
--             src = renderWithContext defaultSDocContext $ ppr $ tick_loc t
--   , modBreaks_ccs :: !(Array BreakTickIndex (RemotePtr CostCentre))
--        -- ^ Array pointing to cost centre for each breakpoint
--    ccs <- mkCCSArray interpProfiled mod count entries

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
