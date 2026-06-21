module GHC.Driver.GenerateCgIPEStub (generateCgIPEStub, lookupEstimatedTicks) where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Semigroup ((<>))
import GHC.Cmm
import GHC.Cmm.CLabel (CLabel, mkAsmTempLabel)
import GHC.Cmm.Dataflow (O)
import GHC.Cmm.Dataflow.Block (blockSplit, blockToList)
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Info.Build (emptySRT)
import GHC.Cmm.Pipeline (cmmPipeline)
import GHC.Data.FastString (FastString, mkFastString)
import GHC.Data.Stream (liftIO, liftEff)
import qualified GHC.Data.Stream as Stream
import GHC.Driver.Env (hsc_dflags, hsc_logger)
import GHC.Driver.Env.Types (HscEnv)
import GHC.Driver.Flags (GeneralFlag (..), DumpFlag(Opt_D_ipe_stats))
import GHC.Driver.DynFlags (gopt, targetPlatform)
import GHC.Driver.Config.StgToCmm
import GHC.Driver.Config.Cmm ( initCmmConfig )
import GHC.Prelude
import GHC.Runtime.Heap.Layout (isStackRep)
import GHC.Settings (platformTablesNextToCode)
import GHC.StgToCmm.Monad (getCmm, initC, runC, initFCodeState)
import GHC.StgToCmm.Prof (initInfoTableProv)
import GHC.StgToCmm.Types (CmmCgInfos (..), ModuleLFInfos)
import GHC.StgToCmm.Utils
import GHC.StgToCmm.CgUtils (CgStream)
import GHC.Types.IPE (InfoTableProvMap (provInfoTables), IpeSourceLocation)
import GHC.Types.Name.Set (NonCaffySet)
import GHC.Types.SrcLoc (srcSpanFile)
import GHC.Types.Tickish (GenTickish (SourceNote))
import GHC.Unit.Types (Module, moduleName)
import GHC.Unit.Module (moduleNameString, ModLocation, ml_hs_file)
import qualified GHC.Utils.Logger as Logger
import GHC.Utils.Outputable (ppr)
import GHC.Types.Unique.DSM

{-
Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stacktraces can be created from return frames as they are pushed to stack for every case scrutinee.
But to make them readable / meaningful, one needs to know the source location of each return frame.

Every return frame has a distinct info table and thus a distinct code pointer (for tables next to
code) or at least a distinct address itself. Info Table Provenance Entries (IPEs) are searchable by
this pointer and contain a source location.

The info table / info table code pointer to source location map is described in:
Note [Mapping Info Tables to Source Positions]

To be able to lookup IPEs for return frames one needs to emit them during compile time. This is done
by `generateCgIPEStub`.

This leads to the question: How to figure out the source location of a return frame?

The algorithm for determining source locations for stack info tables is implemented in
`lookupEstimatedTicks` as two passes over every 'CmmGroupSRTs'. The first pass generates estimated
source locations for any labels potentially corresponding to stack info tables in the Cmm code. The
second pass walks over the Cmm decls and creates an entry in the IPE map for every info table,
looking up source locations for stack info tables in the map generated during the first pass.

The rest of this note will document exactly how the first pass generates the map from labels to
estimated source positions. The algorithms are different depending on whether tables-next-to-code
is on or off. Both algorithms have in common that we are looking for a `CmmNode.CmmTick`
(containing a `SourceNote`) that is near what we estimate to be the label of a return stack frame.

With tables-next-to-code
~~~~~~~~~~~~~~~~~~~~~~~~

Let's consider this example:
```
 Main.returnFrame_entry() { //  [R2]
         { info_tbls: [(c18g,
                        label: block_c18g_info
                        rep: StackRep []
                        srt: Just GHC.CString.unpackCString#_closure),
                       (c18r,
                        label: Main.returnFrame_info
                        rep: HeapRep static { Fun {arity: 1 fun_type: ArgSpec 5} }
                        srt: Nothing)]
           stack_info: arg_space: 8
         }
     {offset

      [...]

       c18u: // global
           //tick src<Main.hs:(7,1)-(16,15)>
           I64[Hp - 16] = sat_s16B_info;
           P64[Hp] = _s16r::P64;
           _c17j::P64 = Hp - 16;
           //tick src<Main.hs:8:25-39>
           I64[Sp - 8] = c18g;
           R3 = _c17j::P64;
           R2 = GHC.IO.Unsafe.unsafePerformIO_closure;
           R1 = GHC.Base.$_closure;
           Sp = Sp - 8;
           call stg_ap_pp_fast(R3,
                               R2,
                               R1) returns to c18g, args: 8, res: 8, upd: 8;
```

The return frame `block_c18g_info` has the label `c18g` which is used in the call to `stg_ap_pp_fast`
(`returns to c18g`) as continuation (`cml_cont`). The source location we're after, is the nearest
`//tick` before the call (`//tick src<Main.hs:8:25-39>`).

In code the Cmm program is represented as a Hoopl graph. Hoopl distinguishes nodes by defining if they
are open or closed on entry (one can fallthrough to them from the previous instruction) and if they are
open or closed on exit (one can fallthrough from them to the next node).

Please refer to the paper "Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation"
for a detailed explanation.

Here we use the fact, that calls (represented by `CmmNode.CmmCall`) are always closed on exit
(`CmmNode O C`, `O` means open, `C` closed). In other words, they are always at the end of a block.

So, given a `CmmGraph`:
  - Look at the end of every block: If it is a `CmmNode.CmmCall` returning to some label, lookup
    the nearest `CmmNode.CmmTick` by traversing the middle part of the block backwards (from end to
    beginning).
  - Take the first `CmmNode.CmmTick` that contains a `Tickish.SourceNote` and map the label we
    found to it's payload as an `IpeSourceLocation`. (There are other `Tickish` constructors like
    `ProfNote` or `HpcTick`, these are ignored.)

See `labelsToSourcesWithTNTC` for the implementation of this algorithm.

Without tables-next-to-code
~~~~~~~~~~~~~~~~~~~~~~~~~~~

When tables-next-to-code is off, there is no return frame / continuation label in calls. The continuation (i.e. return
frame) is set in an explicit Cmm assignment. Thus the tick lookup algorithm has to be slightly different.

```
 sat_s16G_entry() { //  [R1]
         { info_tbls: [(c18O,
                        label: sat_s16G_info
                        rep: HeapRep { Thunk }
                        srt: Just _u18Z_srt)]
           stack_info: arg_space: 0
         }
     {offset
       c18O: // global
           _s16G::P64 = R1;
           if ((Sp + 8) - 40 < SpLim) (likely: False) goto c18P; else goto c18Q;
       c18P: // global
           R1 = _s16G::P64;
           call (stg_gc_enter_1)(R1) args: 8, res: 0, upd: 8;
       c18Q: // global
           I64[Sp - 16] = stg_upd_frame_info;
           P64[Sp - 8] = _s16G::P64;
           //tick src<Main.hs:20:9-13>
           I64[Sp - 24] = block_c18M_info;
           R1 = GHC.Show.$fShow[]_closure;
           P64[Sp - 32] = GHC.Show.$fShowChar_closure;
           Sp = Sp - 32;
           call stg_ap_p_fast(R1) args: 16, res: 8, upd: 24;
     }
 },
 _blk_c18M() { //  [R1]
         { info_tbls: [(c18M,
                        label: block_c18M_info
                        rep: StackRep []
                        srt: Just System.IO.print_closure)]
           stack_info: arg_space: 0
         }
     {offset
       c18M: // global
           _s16F::P64 = R1;
           R1 = System.IO.print_closure;
           P64[Sp] = _s16F::P64;
           call stg_ap_p_fast(R1) args: 32, res: 0, upd: 24;
     }
 },
```

In this example we have to lookup `//tick src<Main.hs:20:9-13>` for the return frame `c18M`.
Notice, that this cannot be done with the `Label` `c18M`, but with the `CLabel` `block_c18M_info`
(`label: block_c18M_info` is actually a `CLabel`).

Given a `CmmGraph`:
  - Check every `CmmBlock` from top (first) to bottom (last).
  - If a `CmmTick` holding a `SourceNote` is found, remember the source location in the tick.
  - If an assignment of the form `... = block_c18M_info;` (a `CmmStore` whose RHS is a
    `CmmLit (CmmLabel l)`) is found, map that label to the most recently visited source note's
    location.

See `labelsToSourcesSansTNTC` for the implementation of this algorithm.
-}

generateCgIPEStub
  :: HscEnv
  -> Module
  -> InfoTableProvMap
  -- ^ If the CmmInfoTables map refer Cmm symbols which were deterministically renamed, the info table provenance map must also be accordingly renamed.
  -> ( NonCaffySet
     , ModuleLFInfos
     , Map CmmInfoTable (Maybe IpeSourceLocation)
     , IPEStats
     )
  -> CgStream CmmGroupSRTs CmmCgInfos
generateCgIPEStub hsc_env this_mod denv (nonCaffySet, moduleLFInfos, infoTablesWithTickishes, initStats) = do
  let dflags   = hsc_dflags hsc_env
      platform = targetPlatform dflags
      logger   = hsc_logger hsc_env
      fstate   = initFCodeState platform
      cmm_cfg  = initCmmConfig dflags
  cgState <- liftIO initC

  -- NB: For determinism, don't use DetUniqFM to rename the IPE Cmm because
  -- detRenameCmm isn't idempotent and this Cmm references symbols in the rest
  -- of the code!  Instead, make sure all labels generated for IPE related code
  -- sources uniques from the DUniqSupply gotten from CgStream (see its use in
  -- initInfoTableProv/emitIpeBufferListNode).
  (mIpeStub, ipeCmmGroup) <- liftEff $ UDSMT $ \dus -> do

    -- Yield Cmm for Info Table Provenance Entries (IPEs)
    let denv' = denv {provInfoTables = Map.mapKeys cit_lbl infoTablesWithTickishes}
        (((mIpeStub, dus'), ipeCmmGroup), _) =
            runC (initStgToCmmConfig dflags this_mod) fstate cgState $
              getCmm (initInfoTableProv initStats (Map.keys infoTablesWithTickishes) denv' dus)

    return ((mIpeStub, ipeCmmGroup), dus')

  (_, ipeCmmGroupSRTs) <- liftEff $ withDUS $ cmmPipeline logger cmm_cfg (emptySRT this_mod) (removeDeterm ipeCmmGroup)
  Stream.yield ipeCmmGroupSRTs

  ipeStub <-
    case mIpeStub of
      Just (stats, stub) -> do
        -- Print ipe stats if requested
        liftIO $
          Logger.putDumpFileMaybe logger
            Opt_D_ipe_stats
            ("IPE Stats for module " ++ (moduleNameString $ moduleName this_mod))
            Logger.FormatText
            (ppr stats)
        return stub
      Nothing -> return mempty

  return CmmCgInfos {cgNonCafs = nonCaffySet, cgLFInfos = moduleLFInfos, cgIPEStub = ipeStub}

-- | Given:
--   * an initial mapping from info tables to possible source locations,
--   * initial 'IPEStats',
--   * a 'CmmGroupSRTs',
--
-- map every info table listed in the 'CmmProc's of the group to their possible
-- source locations and update 'IPEStats' for skipped stack info tables (in case
-- both -finfo-table-map and -fno-info-table-map-with-stack were given). See:
-- Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
--
-- Note: While it would be cleaner if we could keep the recursion and
-- accumulation internal to this function, this cannot be done without
-- separately traversing stream of 'CmmGroupSRTs' in 'GHC.Driver.Main'. The
-- initial implementation of this logic did such a thing, and code generation
-- performance suffered considerably as a result (see #23103).
lookupEstimatedTicks
  :: HscEnv
  -> ModLocation -- ^ location of the module being compiled, for IPE provenance
  -> Map CmmInfoTable (Maybe IpeSourceLocation)
  -> IPEStats
  -> CmmGroupSRTs
  -> IO (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
lookupEstimatedTicks hsc_env mod_location ipes stats cmm_group_srts =
    -- Pass 2: Create an entry in the IPE map for every info table listed in
    -- this CmmGroupSRTs. If the info table is a stack info table and
    -- -finfo-table-map-with-stack is enabled, look up its estimated source
    -- location in the map generate during Pass 1. If the info table is a stack
    -- info table and -finfo-table-map-with-stack is not enabled, skip the table
    -- and note it as skipped in the IPE stats. If the info table is not a stack
    -- info table, insert into the IPE map with no source location information
    -- (for now; see `convertInfoProvMap` in GHC.StgToCmm.Utils to see how source
    -- locations for these tables get filled in)
    pure $ foldl' collectInfoTables (ipes, stats) cmm_group_srts
  where
    dflags = hsc_dflags hsc_env
    platform = targetPlatform dflags

    -- Source file of the module being compiled, used to prefer current-module
    -- source ticks for return frames. See Note [Prefer current-module source
    -- ticks for return frames].
    mb_src_file = mkFastString <$> ml_hs_file mod_location

    -- Pass 1: Map every label meeting the conditions described in
    -- Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
    -- to the estimated source location (also as described in the aformentioned
    -- note).
    --
    -- Note: It's important that this remains a thunk so we do not compute this
    -- map if -fno-info-table-with-stack is given
    labelsToSources :: Map CLabel IpeSourceLocation
    labelsToSources =
      if platformTablesNextToCode platform then
        foldl' (labelsToSourcesWithTNTC mb_src_file) Map.empty cmm_group_srts
      else
        foldl' (labelsToSourcesSansTNTC mb_src_file) Map.empty cmm_group_srts

    collectInfoTables
      :: (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
      -> GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph
      -> (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
    collectInfoTables (!acc, !stats) (CmmProc h _ _ _) =
        mapFoldlWithKey go (acc, stats) (info_tbls h)
      where
        go :: (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
           -> Label
           -> CmmInfoTable
           -> (Map CmmInfoTable (Maybe IpeSourceLocation), IPEStats)
        go (!acc, !stats) lbl' tbl =
          let
            lbl =
              if platformTablesNextToCode platform then
                -- TNTC case, the mapped CLabel will be the result of
                -- mkAsmTempLabel on the info table label
                mkAsmTempLabel lbl'
              else
                -- Non-TNTC case, the mapped CLabel will be the CLabel of the
                -- info table itself
                cit_lbl tbl
          in
            if (isStackRep . cit_rep) tbl then
              if gopt Opt_InfoTableMapWithStack dflags then
                -- This is a stack info table and we DO want to put it in the
                -- info table map
                (Map.insert tbl (Map.lookup lbl labelsToSources) acc, stats)
              else
                -- This is a stack info table but we DO NOT want to put it in
                -- the info table map (-fno-info-table-map-with-stack was
                -- given), track it as skipped
                (acc, stats <> skippedIpeStats)
            else
              -- This is not a stack info table, so put it in the map with no
              -- source location (for now)
              (Map.insert tbl Nothing acc, stats)
    collectInfoTables (!acc, !stats) _ = (acc, stats)

-- | See Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
labelsToSourcesWithTNTC
  :: Maybe FastString -- ^ source file of the module being compiled
  -> Map CLabel IpeSourceLocation
  -> GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph
  -> Map CLabel IpeSourceLocation
labelsToSourcesWithTNTC mb_src_file acc (CmmProc _ _ _ cmm_graph) =
    foldl' go acc (toBlockList cmm_graph)
  where
    go :: Map CLabel IpeSourceLocation -> CmmBlock -> Map CLabel IpeSourceLocation
    go acc block =
        case (,) <$> returnFrameLabel <*> bestTickInBlock of
          Just (clabel, src_loc) -> Map.insert clabel src_loc acc
          Nothing -> acc
      where
        (_, middleBlock, endBlock) = blockSplit block

        returnFrameLabel :: Maybe CLabel
        returnFrameLabel =
          case endBlock of
            (CmmCall _ (Just l) _ _ _ _) -> Just $ mkAsmTempLabel l
            _ -> Nothing

        -- All SourceNotes in the block, in block order.
        -- See Note [Prefer current-module source ticks for return frames].
        bestTickInBlock = preferThisFile mb_src_file procFallback (blockToList middleBlock)

    -- Enclosing current-module note for the whole proc (its function's own
    -- span), used when a return frame's own block has no current-module tick.
    procFallback = enclosingThisFileTick mb_src_file (toBlockList cmm_graph)
labelsToSourcesWithTNTC _ acc _ = acc

{-
Note [Prefer current-module source ticks for return frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A return frame's source location is taken from the `SourceNote`s of the block
that *ends* in the frame's call (see
Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding]
above and `labelsToSourcesWithTNTC`). At `-O`, inlining means such a
block frequently carries `SourceNote`s for inlined library glue (`>>`, `>>=`,
`threadDelay`, ...) and the *nearest* note — the one historically chosen — is
often a library note rather than the user's code. The resulting IPE entry then
points at the library (its file and label; see `toCgIPE` in
GHC.StgToCmm.InfoTableProv, which takes the file from the note's own span), so a
backtrace of a thread blocked in such a primop shows no user frames.

For a concrete example, compile (at -O1)

    -- Scan.hs
    f3 :: IO ()
    f3 = threadDelay 1000000 >> putStrLn "done"

The body of `f3` reaches the inlined `threadDelay`'s internal `delay#` in a
block whose notes are *all* from `Conc.IO`/`Base` — the user's `Scan.hs` tick
for `f3` sits only in the proc's entry block:

    entry:                                    -- the proc's entry block
        //tick src<.../Base.hs:2306:5-18>
        //tick src<Scan.hs:13:1-43>           -- the enclosing `f3` span
        //tick src<.../Conc/IO.hs:(223,1)-(235,10)>
        ...
    delayBlk:                                 -- no Scan.hs note here:
        //tick src<.../Conc/IO.hs:232:5-13>
        //tick src<.../Base.hs:2268:1-9>
        //tick src<.../Conc/IO.hs:(232,25)-(235,10)>   -- nearest note
        call stg_delay#(R1) returns to delayCont, args: 8, res: 8, upd: 8;

Naively taking the nearest note attributes `delayCont` to `Conc/IO.hs:232`,
i.e. an internal of `threadDelay`, rather than `f3`.

To fix this we attribute a return frame's source location in the following
preference order:

  1. the nearest tick in the frame's block whose file is that of the module
     being compiled - the precise user call site. (When the user makes a
     blocking call directly, e.g. `f v = takeMVar v`, such a note is present in
     the call's block and this rule suffices; the `delayBlk` above has none.)
  2. failing that, the proc's *enclosing* current-module note (the outermost
     current-module `SourceNote` in the proc, i.e. its function's own span).
     For `f3` this is `src<Scan.hs:13:1-43>`, so `delayCont` is attributed to
     `f3` rather than to `threadDelay`'s internals.
  3. failing that, the nearest note of any module (the historical behaviour).

This mirrors the same-file preference the DWARF path uses in
`GHC.Cmm.DebugBlock.bestSrcTick` and that `GHC.Stg.Debug.quickSourcePos` uses
for closures.
-}

-- | Pick the 'IpeSourceLocation' to attribute to a return frame from the
-- source-note-bearing nodes of its block (in block order).
--
-- See Note [Prefer current-module source ticks for return frames].
preferThisFile :: Maybe FastString -> Maybe IpeSourceLocation -> [CmmNode O O] -> Maybe IpeSourceLocation
preferThisFile mb_src_file procFallback nodes =
    nearest fromThisFile <|> procFallback <|> nearest sourceNotes
  where
    sourceNotes = [ (span, name) | CmmTick (SourceNote span name) <- nodes ]
    fromThisFile = case mb_src_file of
      Just f  -> filter ((== f) . srcSpanFile . fst) sourceNotes
      Nothing -> []
    nearest = listToMaybe . reverse

-- | The outermost 'SourceNote' from the module being compiled across a proc's
-- blocks (in 'toBlockList' order, so the entry block's note — the function's own
-- span — comes first). Used as a fallback so inlined cross-module code is still
-- labelled with the enclosing user function. 'Nothing' when the proc has no
-- current-module note (e.g. when compiling the library itself).
enclosingThisFileTick :: Maybe FastString -> [CmmBlock] -> Maybe IpeSourceLocation
enclosingThisFileTick mb_src_file blocks =
    listToMaybe
      [ (span, name)
      | b <- blocks
      , let (_, mid, _) = blockSplit b
      , CmmTick (SourceNote span name) <- blockToList mid
      , Just (srcSpanFile span) == mb_src_file ]

-- | See Note [Stacktraces from Info Table Provenance Entries (IPE based stack unwinding)]
labelsToSourcesSansTNTC
  :: Maybe FastString -- ^ source file of the module being compiled
  -> Map CLabel IpeSourceLocation
  -> GenCmmDecl RawCmmStatics CmmTopInfo CmmGraph
  -> Map CLabel IpeSourceLocation
labelsToSourcesSansTNTC mb_src_file acc (CmmProc _ _ _ cmm_graph) =
    foldl' go acc (toBlockList cmm_graph)
  where
    -- See 'enclosingThisFileTick'.
    procFallback = enclosingThisFileTick mb_src_file (toBlockList cmm_graph)

    go :: Map CLabel IpeSourceLocation -> CmmBlock -> Map CLabel IpeSourceLocation
    go acc block = fst $ foldl' collectLabels (acc, (Nothing, Nothing)) (blockToList middleBlock)
      where
        (_, middleBlock, _) = blockSplit block

        -- We track the nearest preceding SourceNote from the module being
        -- compiled and the nearest of any module, and prefer the former (then
        -- the proc's enclosing current-module note) when attributing a return
        -- frame. See Note [Prefer current-module source ticks for return frames].
        collectLabels
          :: (Map CLabel IpeSourceLocation, (Maybe IpeSourceLocation, Maybe IpeSourceLocation))
          -> CmmNode O O
          -> (Map CLabel IpeSourceLocation, (Maybe IpeSourceLocation, Maybe IpeSourceLocation))
        collectLabels (!acc, st@(lastThis, lastAny)) b =
          case b of
            CmmStore _ (CmmLit (CmmLabel l)) _ ->
              case lastThis <|> procFallback <|> lastAny of
                Just src_loc -> (Map.insert l src_loc acc, (Nothing, Nothing))
                Nothing      -> (acc, st)
            CmmTick (SourceNote span name) ->
              let tick = (span, name)
                  lastThis'
                    | Just (srcSpanFile span) == mb_src_file = Just tick
                    | otherwise                              = lastThis
              in (acc, (lastThis', Just tick))
            _ -> (acc, st)
labelsToSourcesSansTNTC _ acc _ = acc
