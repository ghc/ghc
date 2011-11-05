{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course

module CmmPipeline (
  -- | Converts C-- with an implicit stack and native C-- calls into
  -- optimized, CPS converted and native-call-less C--.  The latter
  -- C-- can be used to generate assembly.
  cmmPipeline
) where

import CLabel
import Cmm
import CmmLive
import CmmBuildInfoTables
import CmmCommonBlockElim
import CmmProcPoint
import CmmSpillReload
import CmmRewriteAssignments
import CmmStackLayout
import CmmContFlowOpt
import OptimizationFuel

import DynFlags
import ErrUtils
import HscTypes
import Data.Maybe
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Outputable
import StaticFlags

-----------------------------------------------------------------------------
-- | Top level driver for C-- pipeline
-----------------------------------------------------------------------------
-- There are two complications here:
-- 1. We need to compile the procedures in two stages because we need
--    an analysis of the procedures to tell us what CAFs they use.
--    The first stage returns a map from procedure labels to CAFs,
--    along with a closure that will compute SRTs and attach them to
--    the compiled procedures.
--    The second stage is to combine the CAF information into a top-level
--    CAF environment mapping non-static closures to the CAFs they keep live,
--    then pass that environment to the closures returned in the first
--    stage of compilation.
-- 2. We need to thread the module's SRT around when the SRT tables
--    are computed for each procedure.
--    The SRT needs to be threaded because it is grown lazily.
-- 3. We run control flow optimizations twice, once before any pipeline
--    work is done, and once again at the very end on all of the
--    resulting C-- blocks.  EZY: It's unclear whether or not whether
--    we actually need to do the initial pass.
cmmPipeline  :: HscEnv -- Compilation env including
                       -- dynamic flags: -dcmm-lint -ddump-cps-cmm
             -> (TopSRT, [CmmGroup])    -- SRT table and accumulating list of compiled procs
             -> CmmGroup             -- Input C-- with Procedures
             -> IO (TopSRT, [CmmGroup]) -- Output CPS transformed C--
cmmPipeline hsc_env (topSRT, rst) prog =
  do let dflags = hsc_dflags hsc_env
     --
     showPass dflags "CPSZ"

     let tops = runCmmContFlowOpts prog
     (cafEnvs, tops) <- liftM unzip $ mapM (cpsTop hsc_env) tops
     -- tops :: [[(CmmDecl,CAFSet]]  (one list per group)

     let topCAFEnv = mkTopCAFInfo (concat cafEnvs)

     -- folding over the groups
     (topSRT, tops) <- foldM (toTops hsc_env topCAFEnv) (topSRT, []) tops

     let cmms :: CmmGroup
         cmms = reverse (concat tops)

     dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (pprPlatform (targetPlatform dflags) cmms)

     -- SRT is not affected by control flow optimization pass
     let prog' = runCmmContFlowOpts cmms

     return (topSRT, prog' : rst)

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
The identity and the last pass are stored in
mutable reference cells in an 'HscEnv' and are
global to one compiler session.
-}

-- EZY: It might be helpful to have an easy way of dumping the "pre"
-- input for any given phase, besides just turning it all on with
-- -ddump-cmmz

cpsTop :: HscEnv -> CmmDecl -> IO ([(CLabel, CAFSet)], [(CAFSet, CmmDecl)])
cpsTop _ p@(CmmData {}) = return ([], [(Map.empty, p)])
cpsTop hsc_env (CmmProc h@(TopInfo {stack_info=StackInfo {arg_space=entry_off}}) l g) =
    do
       -- Why bother doing these early: dualLivenessWithInsertion,
       -- insertLateReloads, rewriteAssignments?

       ----------- Eliminate common blocks -------------------
       g <- return $ elimCommonBlocks g
       dumpPlatform platform Opt_D_dump_cmmz_cbe "Post common block elimination" g
       -- Any work storing block Labels must be performed _after_ elimCommonBlocks

       ----------- Proc points -------------------
       let callPPs = callProcPoints g
       procPoints <- run $ minimalProcPointSet (targetPlatform dflags) callPPs g
       g <- run $ addProcPointProtocols callPPs procPoints g
       dumpPlatform platform Opt_D_dump_cmmz_proc "Post Proc Points Added" g

       ----------- Spills and reloads -------------------
       g <- run $ dualLivenessWithInsertion procPoints g
       dumpPlatform platform Opt_D_dump_cmmz_spills "Post spills and reloads" g

       ----------- Sink and inline assignments -------------------
       g <- runOptimization $ rewriteAssignments platform g
       dumpPlatform platform Opt_D_dump_cmmz_rewrite "Post rewrite assignments" g

       ----------- Eliminate dead assignments -------------------
       g <- runOptimization $ removeDeadAssignments g
       dumpPlatform platform Opt_D_dump_cmmz_dead "Post remove dead assignments" g

       ----------- Zero dead stack slots (Debug only) ---------------
       -- Debugging: stubbing slots on death can cause crashes early
       g <- if opt_StubDeadValues
                then run $ stubSlotsOnDeath g
                else return g
       dumpPlatform platform Opt_D_dump_cmmz_stub "Post stub dead stack slots" g

       --------------- Stack layout ----------------
       slotEnv <- run $ liveSlotAnal g
       let spEntryMap = getSpEntryMap entry_off g
       mbpprTrace "live slot analysis results: " (ppr slotEnv) $ return ()
       let areaMap = layout procPoints spEntryMap slotEnv entry_off g
       mbpprTrace "areaMap" (ppr areaMap) $ return ()

       ------------  Manifest the stack pointer --------
       g  <- run $ manifestSP spEntryMap areaMap entry_off g
       dumpPlatform platform Opt_D_dump_cmmz_sp "Post manifestSP" g
       -- UGH... manifestSP can require updates to the procPointMap.
       -- We can probably do something quicker here for the update...

       ------------- Split into separate procedures ------------
       procPointMap  <- run $ procPointAnalysis procPoints g
       dump Opt_D_dump_cmmz_procmap "procpoint map" procPointMap
       gs <- run $ splitAtProcPoints l callPPs procPoints procPointMap
                                       (CmmProc h l g)
       mapM_ (dumpPlatform platform Opt_D_dump_cmmz_split "Post splitting") gs

       ------------- More CAFs and foreign calls ------------
       cafEnv <- run $ cafAnal platform g
       let localCAFs = catMaybes $ map (localCAFInfo platform cafEnv) gs
       mbpprTrace "localCAFs" (pprPlatform platform localCAFs) $ return ()

       gs <- run $ mapM (lowerSafeForeignCalls areaMap) gs
       mapM_ (dumpPlatform platform Opt_D_dump_cmmz_lower "Post lowerSafeForeignCalls") gs

       -- NO MORE GRAPH TRANSFORMATION AFTER HERE -- JUST MAKING INFOTABLES
       gs <- return $ map (setInfoTableStackMap slotEnv areaMap) gs
       mapM_ (dumpPlatform platform Opt_D_dump_cmmz_info "after setInfoTableStackMap") gs
       gs <- return $ map (bundleCAFs cafEnv) gs
       mapM_ (dumpPlatform platform Opt_D_dump_cmmz_cafs "after bundleCAFs") gs
       return (localCAFs, gs)

              -- gs        :: [ (CAFSet, CmmDecl) ]
              -- localCAFs :: [ (CLabel, CAFSet) ] -- statics filtered out(?)

  where dflags = hsc_dflags hsc_env
        platform = targetPlatform dflags
        mbpprTrace x y z = if dopt Opt_D_dump_cmmz dflags then pprTrace x y z else z
        dump f = dumpWith ppr f
        dumpPlatform platform = dumpWith (pprPlatform platform)
        dumpWith pprFun f txt g = do
            -- ToDo: No easy way of say "dump all the cmmz, *and* split
            -- them into files."  Also, -ddump-cmmz doesn't play nicely
            -- with -ddump-to-file, since the headers get omitted.
            dumpIfSet_dyn dflags f txt (pprFun g)
            when (not (dopt f dflags)) $
                dumpIfSet_dyn dflags Opt_D_dump_cmmz txt (pprFun g)
        -- Runs a required transformation/analysis
        run = runInfiniteFuelIO (hsc_OptFuel hsc_env)
        -- Runs an optional transformation/analysis (and should
        -- thus be subject to optimization fuel)
        runOptimization = runFuelIO (hsc_OptFuel hsc_env)

-- This probably belongs in CmmBuildInfoTables?
-- We're just finishing the job here: once we know what CAFs are defined
-- in non-static closures, we can build the SRTs.
toTops :: HscEnv -> Map CLabel CAFSet -> (TopSRT, [[CmmDecl]])
                 -> [(CAFSet, CmmDecl)] -> IO (TopSRT, [[CmmDecl]])
toTops hsc_env topCAFEnv (topSRT, tops) gs =
  do let setSRT (topSRT, rst) g =
           do (topSRT, gs) <- setInfoTableSRT topCAFEnv topSRT g
              return (topSRT, gs : rst)
     (topSRT, gs') <- runFuelIO (hsc_OptFuel hsc_env) $ foldM setSRT (topSRT, []) gs
     return (topSRT, concat gs' : tops)
