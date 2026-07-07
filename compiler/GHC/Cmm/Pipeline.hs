
module GHC.Cmm.Pipeline (
  cmmPipeline
) where

import GHC.Prelude

import GHC.Driver.Flags

import GHC.Cmm
import GHC.Cmm.Config
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.CommonBlockElim
import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Info.Build
import GHC.Cmm.Lint
import GHC.Cmm.LayoutStack
import GHC.Cmm.ProcPoint
import GHC.Cmm.Sink
import GHC.Cmm.Switch.Implement
import GHC.Cmm.ThreadSanitizer

import GHC.Types.Unique.Supply
import GHC.Types.Unique.DSM

import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Misc ( partitionWith )

import GHC.Platform

import Control.Monad
import GHC.Utils.Monad (mapAccumLM)

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- TEMPORARY measurement hack, not for committing: GHC_CBE2=0 disables the
-- second CBE pass, giving a same-binary A/B for size/perf comparisons.
{-# NOINLINE cbe2Enabled #-}
cbe2Enabled :: Bool
cbe2Enabled = unsafePerformIO (lookupEnv "GHC_CBE2") /= Just "0"

-----------------------------------------------------------------------------
-- | Top level driver for C-- pipeline
-----------------------------------------------------------------------------

-- | Converts C-- with an implicit stack and native C-- calls into
-- optimized, CPS converted and native-call-less C--.  The latter
-- C-- can be used to generate assembly.
cmmPipeline
 :: Logger
 -> CmmConfig
 -> ModuleSRTInfo        -- Info about SRTs generated so far
 -> CmmGroup             -- Input C-- with Procedures
 -> DUniqSupply
 -> IO ((ModuleSRTInfo, CmmGroupSRTs), DUniqSupply) -- Output CPS transformed C--

cmmPipeline logger cmm_config srtInfo prog dus0 = do
  let forceRes ((info, group), us) = info `seq` us `seq` foldr seq () group
  let platform = cmmPlatform cmm_config
  withTimingSilent logger (text "Cmm pipeline") forceRes $ do
     (dus1, prog')  <- {-# SCC "tops" #-} mapAccumLM (cpsTop logger platform cmm_config) dus0 prog
     let (procs, data_) = partitionWith id prog'
     (srtInfo, dus, cmms) <- {-# SCC "doSRTs" #-} doSRTs cmm_config srtInfo dus1 procs data_
     dumpWith logger Opt_D_dump_cmm_cps "Post CPS Cmm" FormatCMM (pdoc platform cmms)

     return ((srtInfo, cmms), dus)

-- | The Cmm pipeline for a single 'CmmDecl'. Returns:
--
--   - in the case of a 'CmmProc': 'Left' of the resulting (possibly
--     proc-point-split) 'CmmDecl's and their 'CafEnv'. CAF analysis
--     necessarily happens *before* proc-point splitting, as described in Note
--     [SRTs].
--
--   - in the case of a `CmmData`, the unmodified 'CmmDecl' and a 'CAFSet' containing
cpsTop :: Logger -> Platform -> CmmConfig -> DUniqSupply -> CmmDecl -> IO (DUniqSupply, Either (CAFEnv, [CmmDecl]) (CAFSet, CmmDataDecl))
cpsTop _logger platform _ dus (CmmData section statics) =
  return (dus, Right (cafAnalData platform statics, CmmData section statics))
cpsTop logger platform cfg dus proc =
    do
      ----------- Control-flow optimisations ----------------------------------

      -- The first round of control-flow optimisation speeds up the
      -- later passes by removing lots of empty blocks, so we do it
      -- even when optimisation isn't turned on.
      --
      CmmProc h l v g <- {-# SCC "cmmCfgOpts(1)" #-}
           return $ cmmCfgOptsProc splitting_proc_points proc
      dump Opt_D_dump_cmm_cfg "Post control-flow optimisations (1)" g

      let !TopInfo {stack_info=StackInfo { arg_space = entry_off
                                         , do_layout = do_layout }} = h

      ----------- Eliminate common blocks -------------------------------------
      g <- {-# SCC "elimCommonBlocks" #-}
           condPass (cmmOptElimCommonBlks cfg) elimCommonBlocks g
                         Opt_D_dump_cmm_cbe "Post common block elimination"

      -- Any work storing block Labels must be performed _after_
      -- elimCommonBlocks

      ----------- Implement switches ------------------------------------------
      (g, dus) <- if cmmDoCmmSwitchPlans cfg
             then {-# SCC "createSwitchPlans" #-}
                  pure $ runUniqueDSM dus $ cmmImplementSwitchPlans platform g
             else pure (g, dus)
      dump Opt_D_dump_cmm_switch "Post switch plan" g

      ----------- ThreadSanitizer instrumentation -----------------------------
      g <- {-# SCC "annotateTSAN" #-}
          if cmmOptThreadSanitizer cfg
          then do
             -- TODO(#25273): Use the deterministic UniqDSM (ie `runUniqueDSM`) instead
             -- of UniqSM (see `initUs_`) to guarantee deterministic objects
             -- when doing thread sanitization.
            us <- mkSplitUniqSupply TsanTag
            return $ initUs_ us $
              annotateTSAN platform g
          else return g
      dump Opt_D_dump_cmm_thread_sanitizer "ThreadSanitizer instrumentation" g

      ----------- Proc points -------------------------------------------------
      let
        call_pps :: ProcPointSet -- LabelMap
        call_pps = {-# SCC "callProcPoints" #-} callProcPoints g
      (proc_points, dus) <-
         if splitting_proc_points
            then do
              let (pp, dus') = {-# SCC "minimalProcPointSet" #-} runUniqueDSM dus $
                    minimalProcPointSet platform call_pps g
              dumpWith logger Opt_D_dump_cmm_proc "Proc points"
                    FormatCMM (pdoc platform l $$ ppr pp $$ pdoc platform g)
              return (pp, dus')
            else
              return (call_pps, dus)

      ----------- Layout the stack and manifest Sp ----------------------------
      ((g, stackmaps), dus) <- pure $
         {-# SCC "layoutStack" #-}
         if do_layout
            then runUniqueDSM dus $ cmmLayoutStack cfg proc_points entry_off g
            else ((g, mapEmpty), dus)
      dump Opt_D_dump_cmm_sp "Layout Stack" g

      ----------- Sink and inline assignments  --------------------------------
      g <- {-# SCC "sink" #-} -- See Note [Sinking after stack layout]
           condPass (cmmOptSink cfg) (cmmSink platform) g
                    Opt_D_dump_cmm_sink "Sink assignments"

      ----------- Second common-block elimination ------------------------------
      -- See Note [Second pass of CBE]
      (g, call_pps) <-
        if cbe2Enabled && cmmOptElimCommonBlks cfg && cmmOptSink cfg && not splitting_proc_points
          then do
            let stackmap_key lbl = stackMapLivenessKey platform
                                     <$> mapLookup lbl stackmaps
                ok_to_merge l1 l2 = stackmap_key l1 == stackmap_key l2
            g <- return $ {-# SCC "elimCommonBlocks2" #-}
                 elimCommonBlocksWith ok_to_merge g
            dump Opt_D_dump_cmm_cbe "Post common block elimination (2)" g
            -- The pass may merge return points, so the call proc-point set
            -- must be recomputed for the passes below.
            return (g, callProcPoints g)
          else return (g, call_pps)

      ------------- CAF analysis ----------------------------------------------
      let cafEnv = {-# SCC "cafAnal" #-} cafAnal platform call_pps l g
      dumpWith logger Opt_D_dump_cmm_caf "CAFEnv" FormatText (pdoc platform cafEnv)

      (g, dus) <- if splitting_proc_points
           then do
             ------------- Split into separate procedures -----------------------
             let pp_map = {-# SCC "procPointAnalysis" #-}
                          procPointAnalysis proc_points g
             dumpWith logger Opt_D_dump_cmm_procmap "procpoint map"
                FormatCMM (ppr pp_map)
             (g, dus) <- {-# SCC "splitAtProcPoints" #-} pure $ runUniqueDSM dus $
                  splitAtProcPoints platform l call_pps proc_points pp_map
                                    (CmmProc h l v g)
             dumps Opt_D_dump_cmm_split "Post splitting" g
             return (g, dus)
           else
             -- attach info tables to return points
             return ([attachContInfoTables call_pps (CmmProc h l v g)], dus)

      ------------- Populate info tables with stack info -----------------
      g <- {-# SCC "setInfoTableStackMap" #-}
           return $ map (setInfoTableStackMap platform stackmaps) g
      dumps Opt_D_dump_cmm_info "after setInfoTableStackMap" g

      ----------- Control-flow optimisations -----------------------------
      g <- {-# SCC "cmmCfgOpts(2)" #-}
           return $ if cmmOptControlFlow cfg
                    then map (cmmCfgOptsProc splitting_proc_points) g
                    else g
      g <- return $ map (removeUnreachableBlocksProc platform) g
           -- See Note [unreachable blocks]
      dumps Opt_D_dump_cmm_cfg "Post control-flow optimisations (2)" g

      return (dus, Left (cafEnv, g))

  where dump = dumpGraph logger platform (cmmDoLinting cfg)

        dumps flag name
           = mapM_ (dumpWith logger flag name FormatCMM . pdoc platform)

        condPass do_opt pass g dumpflag dumpname =
            if do_opt
               then do
                    g <- return $ pass g
                    dump dumpflag dumpname g
                    return g
               else return g

        -- we don't need to split proc points for the NCG, unless
        -- tablesNextToCode is off.  The latter is because we have no
        -- label to put on info tables for basic blocks that are not
        -- the entry point.
        splitting_proc_points = cmmSplitProcPoints cfg

-- Note [Sinking after stack layout]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In the past we considered running sinking pass also before stack
-- layout, but after making some measurements we realized that:
--
--   a) running sinking only before stack layout produces slower
--      code than running sinking only before stack layout
--
--   b) running sinking both before and after stack layout produces
--      code that has the same performance as when running sinking
--      only after stack layout.
--
-- In other words sinking before stack layout doesn't buy as anything.
--
-- An interesting question is "why is it better to run sinking after
-- stack layout"? It seems that the major reason are stores and loads
-- generated by stack layout. Consider this code before stack layout:
--
--  c1E:
--      _c1C::P64 = R3;
--      _c1B::P64 = R2;
--      _c1A::P64 = R1;
--      I64[(young<c1D> + 8)] = c1D;
--      call stg_gc_noregs() returns to c1D, args: 8, res: 8, upd: 8;
--  c1D:
--      R3 = _c1C::P64;
--      R2 = _c1B::P64;
--      R1 = _c1A::P64;
--      call (P64[(old + 8)])(R3, R2, R1) args: 8, res: 0, upd: 8;
--
-- Stack layout pass will save all local variables live across a call
-- (_c1C, _c1B and _c1A in this example) on the stack just before
-- making a call and reload them from the stack after returning from a
-- call:
--
--  c1E:
--      _c1C::P64 = R3;
--      _c1B::P64 = R2;
--      _c1A::P64 = R1;
--      I64[Sp - 32] = c1D;
--      P64[Sp - 24] = _c1A::P64;
--      P64[Sp - 16] = _c1B::P64;
--      P64[Sp - 8] = _c1C::P64;
--      Sp = Sp - 32;
--      call stg_gc_noregs() returns to c1D, args: 8, res: 8, upd: 8;
--  c1D:
--      _c1A::P64 = P64[Sp + 8];
--      _c1B::P64 = P64[Sp + 16];
--      _c1C::P64 = P64[Sp + 24];
--      R3 = _c1C::P64;
--      R2 = _c1B::P64;
--      R1 = _c1A::P64;
--      Sp = Sp + 32;
--      call (P64[Sp])(R3, R2, R1) args: 8, res: 0, upd: 8;
--
-- If we don't run sinking pass after stack layout we are basically
-- left with such code. However, running sinking on this code can lead
-- to significant improvements:
--
--  c1E:
--      I64[Sp - 32] = c1D;
--      P64[Sp - 24] = R1;
--      P64[Sp - 16] = R2;
--      P64[Sp - 8] = R3;
--      Sp = Sp - 32;
--      call stg_gc_noregs() returns to c1D, args: 8, res: 8, upd: 8;
--  c1D:
--      R3 = P64[Sp + 24];
--      R2 = P64[Sp + 16];
--      R1 = P64[Sp + 8];
--      Sp = Sp + 32;
--      call (P64[Sp])(R3, R2, R1) args: 8, res: 0, upd: 8;
--
-- Now we only have 9 assignments instead of 15.
--
-- There is one case when running sinking before stack layout could
-- be beneficial. Consider this:
--
--   L1:
--      x = y
--      call f() returns L2
--   L2: ...x...y...
--
-- Since both x and y are live across a call to f, they will be stored
-- on the stack during stack layout and restored after the call:
--
--   L1:
--      x = y
--      P64[Sp - 24] = L2
--      P64[Sp - 16] = x
--      P64[Sp - 8]  = y
--      Sp = Sp - 24
--      call f() returns L2
--   L2:
--      y = P64[Sp + 16]
--      x = P64[Sp + 8]
--      Sp = Sp + 24
--      ...x...y...
--
-- However, if we run sinking before stack layout we would propagate x
-- to its usage place (both x and y must be local register for this to
-- be possible - global registers cannot be floated past a call):
--
--   L1:
--      x = y
--      call f() returns L2
--   L2: ...y...y...
--
-- Thus making x dead at the call to f(). If we ran stack layout now
-- we would generate less stores and loads:
--
--   L1:
--      x = y
--      P64[Sp - 16] = L2
--      P64[Sp - 8]  = y
--      Sp = Sp - 16
--      call f() returns L2
--   L2:
--      y = P64[Sp + 8]
--      Sp = Sp + 16
--      ...y...y...
--
-- But since we don't see any benefits from running sinking before stack
-- layout, this situation probably doesn't arise too often in practice.
--

{- Note [Second pass of CBE]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
The first CBE run happens before stack layout, where semantically identical
blocks often differ syntactically: locals get fresh uniques per copy (e.g. the
duplicated case alternatives of #20681), and CBE's block equality requires
local registers to match exactly.  That requirement is essential at that
stage, because locals are live across blocks, so alpha-renaming one block at
a time is unsound (#14754).

After stack layout and sinking, most of those locals are gone: values live
across calls travel via stack slots, and the sinking pass inlines the
remaining single-use temporaries.  Blocks that differed only in uniques
frequently become literally identical, so we run CBE once more.  This is
mostly a code-size optimisation, but duplicated return points also each get
their own info table, so it saves static data too.

Wrinkles:

* Return points now carry stack maps (used to populate info tables in
  setInfoTableStackMap).  Two identical blocks may in principle sit on frames
  with different pointer liveness, and after merging only the surviving
  label's info table remains.  So we only merge blocks whose stack maps agree
  on everything the info table records: stackMapLivenessKey in
  GHC.Cmm.LayoutStack.

* Merging can drop return-point labels, so the call proc-point set is
  recomputed before CAF analysis and attachContInfoTables.

* The graph's entry block must never be the eliminated side of a merge: its
  label is the procedure's externally visible symbol.  Post-layout blocks are
  bare enough that this genuinely happens (observed on nofib's real/veritas,
  producing undefined symbol references at link time).
  elimCommonBlocksWith enforces this for every CBE run.

* We don't run the pass when splitting proc points (unregisterised or
  non-TNTC backends): merging blocks reachable from different proc points
  breaks splitAtProcPoints' invariant that each block is reachable from
  exactly one proc point (#14989).

Historical note: a second CBE pass first landed as d5c4d46a62 and was
reverted (#14989) because it patched up the proc-point sets by label
substitution, which cannot express one block becoming reachable from two
proc points; restricting the pass to the non-splitting path avoids the
problem entirely.
-}

{- Note [inconsistent-pic-reg]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~
On x86/Darwin, PIC is implemented by inserting a sequence like

    call 1f
 1: popl %reg

at the proc entry point, and then referring to labels as offsets from
%reg.  If we don't split proc points, then we could have many entry
points in a proc that would need this sequence, and each entry point
would then get a different value for %reg.  If there are any join
points, then at the join point we don't have a consistent value for
%reg, so we don't know how to refer to labels.

Hence, on x86/Darwin, we have to split proc points, and then each proc
point will get its own PIC initialisation sequence.

This isn't an issue on x86/ELF, where the sequence is

    call 1f
 1: popl %reg
    addl $_GLOBAL_OFFSET_TABLE_+(.-1b), %reg

so %reg always has a consistent value: the address of
_GLOBAL_OFFSET_TABLE_, regardless of which entry point we arrived via.

-}

{- Note [unreachable blocks]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
The control-flow optimiser sometimes leaves unreachable blocks behind
containing junk code.  These aren't necessarily a problem, but
removing them is good because it might save time in the native code
generator later.

-}

dumpGraph :: Logger -> Platform -> Bool -> DumpFlag -> String -> CmmGraph -> IO ()
dumpGraph logger platform do_linting flag name g = do
  when do_linting $ do_lint g
  dumpWith logger flag name FormatCMM (pdoc platform g)
 where
  do_lint g = case cmmLintGraph platform g of
                 Just err -> do { fatalErrorMsg logger err
                                ; ghcExit logger 1
                                }
                 Nothing  -> return ()

dumpWith :: Logger -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
dumpWith logger flag txt fmt sdoc = do
  putDumpFileMaybe logger flag txt fmt sdoc
  when (not (logHasDumpFlag logger flag)) $
    -- If `-ddump-cmm-verbose -ddump-to-file` is specified,
    -- dump each Cmm pipeline stage output to a separate file.  #16930
    when (logHasDumpFlag logger Opt_D_dump_cmm_verbose)
      $ logDumpFile logger (mkDumpStyle alwaysQualify) flag txt fmt sdoc
  putDumpFileMaybe logger Opt_D_dump_cmm_verbose_by_proc txt fmt sdoc
