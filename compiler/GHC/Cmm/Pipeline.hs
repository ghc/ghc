{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module GHC.Cmm.Pipeline (
  -- | Converts C-- with an implicit stack and native C-- calls into
  -- optimized, CPS converted and native-call-less C--.  The latter
  -- C-- can be used to generate assembly.
  cmmPipeline
) where

import GHC.Prelude

import GHC.Cmm
import GHC.Cmm.Lint
import GHC.Cmm.Info.Build
import GHC.Cmm.CommonBlockElim
import GHC.Cmm.Switch.Implement
import GHC.Cmm.ProcPoint
import GHC.Cmm.ContFlowOpt
import GHC.Cmm.LayoutStack
import GHC.Cmm.Sink
import GHC.Cmm.Dataflow.Collections

import GHC.Types.Unique.Supply
import GHC.Driver.Session
import GHC.Utils.Error
import GHC.Driver.Types
import Control.Monad
import GHC.Utils.Outputable
import GHC.Platform
import Data.Either (partitionEithers)

-----------------------------------------------------------------------------
-- | Top level driver for C-- pipeline
-----------------------------------------------------------------------------

cmmPipeline
 :: HscEnv -- Compilation env including
           -- dynamic flags: -dcmm-lint -ddump-cmm-cps
 -> ModuleSRTInfo        -- Info about SRTs generated so far
 -> CmmGroup             -- Input C-- with Procedures
 -> IO (ModuleSRTInfo, CmmGroupSRTs) -- Output CPS transformed C--

cmmPipeline hsc_env srtInfo prog = withTimingSilent dflags (text "Cmm pipeline") forceRes $
  do let dflags = hsc_dflags hsc_env

     tops <- {-# SCC "tops" #-} mapM (cpsTop hsc_env) prog

     let (procs, data_) = partitionEithers tops
     (srtInfo, cmms) <- {-# SCC "doSRTs" #-} doSRTs dflags srtInfo procs data_
     dumpWith dflags Opt_D_dump_cmm_cps "Post CPS Cmm" FormatCMM (ppr cmms)

     return (srtInfo, cmms)

  where forceRes (info, group) =
          info `seq` foldr (\decl r -> decl `seq` r) () group

        dflags = hsc_dflags hsc_env

cpsTop :: HscEnv -> CmmDecl -> IO (Either (CAFEnv, [CmmDecl]) (CAFSet, CmmDecl))
cpsTop _ p@(CmmData _ statics) = return (Right (cafAnalData statics, p))
cpsTop hsc_env proc =
    do
       ----------- Control-flow optimisations ----------------------------------

       -- The first round of control-flow optimisation speeds up the
       -- later passes by removing lots of empty blocks, so we do it
       -- even when optimisation isn't turned on.
       --
       CmmProc h l v g <- {-# SCC "cmmCfgOpts(1)" #-}
            return $ cmmCfgOptsProc splitting_proc_points proc
       dump Opt_D_dump_cmm_cfg "Post control-flow optimisations" g

       let !TopInfo {stack_info=StackInfo { arg_space = entry_off
                                          , do_layout = do_layout }} = h

       ----------- Eliminate common blocks -------------------------------------
       g <- {-# SCC "elimCommonBlocks" #-}
            condPass Opt_CmmElimCommonBlocks elimCommonBlocks g
                          Opt_D_dump_cmm_cbe "Post common block elimination"

       -- Any work storing block Labels must be performed _after_
       -- elimCommonBlocks

       ----------- Implement switches ------------------------------------------
       g <- {-# SCC "createSwitchPlans" #-}
            runUniqSM $ cmmImplementSwitchPlans dflags g
       dump Opt_D_dump_cmm_switch "Post switch plan" g

       ----------- Proc points -------------------------------------------------
       let
         call_pps :: ProcPointSet -- LabelMap
         call_pps = {-# SCC "callProcPoints" #-} callProcPoints g
       proc_points <-
          if splitting_proc_points
             then do
               pp <- {-# SCC "minimalProcPointSet" #-} runUniqSM $
                  minimalProcPointSet (targetPlatform dflags) call_pps g
               dumpWith dflags Opt_D_dump_cmm_proc "Proc points"
                     FormatCMM (ppr l $$ ppr pp $$ ppr g)
               return pp
             else
               return call_pps

       ----------- Layout the stack and manifest Sp ----------------------------
       (g, stackmaps) <-
            {-# SCC "layoutStack" #-}
            if do_layout
               then runUniqSM $ cmmLayoutStack dflags proc_points entry_off g
               else return (g, mapEmpty)
       dump Opt_D_dump_cmm_sp "Layout Stack" g

       ----------- Sink and inline assignments  --------------------------------
       g <- {-# SCC "sink" #-} -- See Note [Sinking after stack layout]
            condPass Opt_CmmSink (cmmSink dflags) g
                     Opt_D_dump_cmm_sink "Sink assignments"

       ------------- CAF analysis ----------------------------------------------
       let cafEnv = {-# SCC "cafAnal" #-} cafAnal call_pps l g
       dumpWith dflags Opt_D_dump_cmm_caf "CAFEnv" FormatText (ppr cafEnv)

       g <- if splitting_proc_points
            then do
               ------------- Split into separate procedures -----------------------
               let pp_map = {-# SCC "procPointAnalysis" #-}
                            procPointAnalysis proc_points g
               dumpWith dflags Opt_D_dump_cmm_procmap "procpoint map"
                  FormatCMM (ppr pp_map)
               g <- {-# SCC "splitAtProcPoints" #-} runUniqSM $
                    splitAtProcPoints dflags l call_pps proc_points pp_map
                                      (CmmProc h l v g)
               dumps Opt_D_dump_cmm_split "Post splitting" g
               return g
             else do
               -- attach info tables to return points
               return $ [attachContInfoTables call_pps (CmmProc h l v g)]

       ------------- Populate info tables with stack info -----------------
       g <- {-# SCC "setInfoTableStackMap" #-}
            return $ map (setInfoTableStackMap platform stackmaps) g
       dumps Opt_D_dump_cmm_info "after setInfoTableStackMap" g

       ----------- Control-flow optimisations -----------------------------
       g <- {-# SCC "cmmCfgOpts(2)" #-}
            return $ if optLevel dflags >= 1
                     then map (cmmCfgOptsProc splitting_proc_points) g
                     else g
       g <- return (map removeUnreachableBlocksProc g)
            -- See Note [unreachable blocks]
       dumps Opt_D_dump_cmm_cfg "Post control-flow optimisations" g

       return (Left (cafEnv, g))

  where dflags = hsc_dflags hsc_env
        platform = targetPlatform dflags
        dump = dumpGraph dflags

        dumps flag name
           = mapM_ (dumpWith dflags flag name FormatCMM . ppr)

        condPass flag pass g dumpflag dumpname =
            if gopt flag dflags
               then do
                    g <- return $ pass g
                    dump dumpflag dumpname g
                    return g
               else return g

        -- we don't need to split proc points for the NCG, unless
        -- tablesNextToCode is off.  The latter is because we have no
        -- label to put on info tables for basic blocks that are not
        -- the entry point.
        splitting_proc_points = hscTarget dflags /= HscAsm
                             || not (tablesNextToCode dflags)
                             || -- Note [inconsistent-pic-reg]
                                usingInconsistentPicReg
        usingInconsistentPicReg
           = case (platformArch platform, platformOS platform, positionIndependent dflags)
             of   (ArchX86, OSDarwin, pic) -> pic
                  _                        -> False

-- Note [Sinking after stack layout]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
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

{- Note [inconsistent-pic-reg]

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

The control-flow optimiser sometimes leaves unreachable blocks behind
containing junk code.  These aren't necessarily a problem, but
removing them is good because it might save time in the native code
generator later.

-}

runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
  us <- mkSplitUniqSupply 'u'
  return (initUs_ us m)


dumpGraph :: DynFlags -> DumpFlag -> String -> CmmGraph -> IO ()
dumpGraph dflags flag name g = do
  when (gopt Opt_DoCmmLinting dflags) $ do_lint g
  dumpWith dflags flag name FormatCMM (ppr g)
 where
  do_lint g = case cmmLintGraph dflags g of
                 Just err -> do { fatalErrorMsg dflags err
                                ; ghcExit dflags 1
                                }
                 Nothing  -> return ()

dumpWith :: DynFlags -> DumpFlag -> String -> DumpFormat -> SDoc -> IO ()
dumpWith dflags flag txt fmt sdoc = do
  dumpIfSet_dyn dflags flag txt fmt sdoc
  when (not (dopt flag dflags)) $
    -- If `-ddump-cmm-verbose -ddump-to-file` is specified,
    -- dump each Cmm pipeline stage output to a separate file.  #16930
    when (dopt Opt_D_dump_cmm_verbose dflags)
      $ dumpAction dflags (mkDumpStyle alwaysQualify)
                   (dumpOptionsFromFlag flag) txt fmt sdoc
  dumpIfSet_dyn dflags Opt_D_dump_cmm_verbose_by_proc txt fmt sdoc
