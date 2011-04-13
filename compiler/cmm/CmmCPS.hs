{-# OPTIONS_GHC -XNoMonoLocalBinds #-}
-- Norman likes local bindings
-- If this module lives on I'd like to get rid of this flag in due course
module CmmCPS (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  -- Well, sort of.
  protoCmmCPS
) where

import CLabel
import Cmm
import CmmDecl
import CmmBuildInfoTables
import CmmCommonBlockElim
import CmmProcPoint
import CmmSpillReload
import CmmStackLayout
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
-- |Top level driver for the CPS pass
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
protoCmmCPS  :: HscEnv -- Compilation env including
                       -- dynamic flags: -dcmm-lint -ddump-cps-cmm
             -> (TopSRT, [Cmm])    -- SRT table and accumulating list of compiled procs
             -> Cmm                -- Input C-- with Procedures
             -> IO (TopSRT, [Cmm]) -- Output CPS transformed C--
protoCmmCPS hsc_env (topSRT, rst) (Cmm tops) =
  do let dflags = hsc_dflags hsc_env
     showPass dflags "CPSZ"
     (cafEnvs, tops) <- liftM unzip $ mapM (cpsTop hsc_env) tops
     let topCAFEnv = mkTopCAFInfo (concat cafEnvs)
     (topSRT, tops) <- foldM (toTops hsc_env topCAFEnv) (topSRT, []) tops
     let cmms = Cmm (reverse (concat tops))
     dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (ppr cmms)
     return (topSRT, cmms : rst)

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
The identity and the last pass are stored in
mutable reference cells in an 'HscEnv' and are
global to one compiler session.
-}

cpsTop :: HscEnv -> CmmTop -> IO ([(CLabel, CAFSet)], [(CAFSet, CmmTop)])
cpsTop _ p@(CmmData {}) = return ([], [(Map.empty, p)])
cpsTop hsc_env (CmmProc h@(TopInfo {stack_info=StackInfo {arg_space=entry_off}}) l g) =
    do
       -- Why bother doing it this early?
       -- g <- dual_rewrite run Opt_D_dump_cmmz "spills and reloads"
       --                       (dualLivenessWithInsertion callPPs) g
       -- g <- run $ insertLateReloads g -- Duplicate reloads just before uses
       -- g <- dual_rewrite runOptimization Opt_D_dump_cmmz "Dead Assignment Elimination"
       --                   (removeDeadAssignmentsAndReloads callPPs) g
       dump Opt_D_dump_cmmz "Pre common block elimination" g
       g <- return $ elimCommonBlocks g
       dump Opt_D_dump_cmmz "Post common block elimination" g

       -- Any work storing block Labels must be performed _after_ elimCommonBlocks

       ----------- Proc points -------------------
       let callPPs = callProcPoints g
       procPoints <- run $ minimalProcPointSet callPPs g
       g <- run $ addProcPointProtocols callPPs procPoints g
       dump Opt_D_dump_cmmz "Post Proc Points Added" g

       ----------- Spills and reloads -------------------
       g     <- 
              -- pprTrace "pre Spills" (ppr g) $
                dual_rewrite run Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion procPoints) g
                    -- Insert spills at defns; reloads at return points
       g     <-
                runOptimization $ rewriteAssignments g
       dump Opt_D_dump_cmmz "Post rewrite assignments" g
       g     <-
               -- pprTrace "post insertLateReloads" (ppr g) $
                dual_rewrite runOptimization Opt_D_dump_cmmz "Dead Assignment Elimination"
                                        (removeDeadAssignmentsAndReloads procPoints) g
                    -- Remove redundant reloads (and any other redundant asst)

       ----------- Debug only: add code to put zero in dead stack slots----
       -- Debugging: stubbing slots on death can cause crashes early
       g <- -- trace "post dead-assign elim" $
            if opt_StubDeadValues then run $ stubSlotsOnDeath g else return g


       --------------- Stack layout ----------------
       slotEnv <- run $ liveSlotAnal g
       mbpprTrace "live slot analysis results: " (ppr slotEnv) $ return ()
       let areaMap = layout procPoints slotEnv entry_off g
       mbpprTrace "areaMap" (ppr areaMap) $ return ()

       ------------  Manifest the stack pointer --------
       g  <- run $ manifestSP areaMap entry_off g
       dump Opt_D_dump_cmmz "after manifestSP" g
       -- UGH... manifestSP can require updates to the procPointMap.
       -- We can probably do something quicker here for the update...

       ------------- Split into separate procedures ------------
       procPointMap  <- run $ procPointAnalysis procPoints g
       dump Opt_D_dump_cmmz "procpoint map" procPointMap
       gs <- run $ splitAtProcPoints l callPPs procPoints procPointMap
                                       (CmmProc h l g)
       mapM_ (dump Opt_D_dump_cmmz "after splitting") gs

       ------------- More CAFs and foreign calls ------------
       cafEnv <- run $ cafAnal g
       let localCAFs = catMaybes $ map (localCAFInfo cafEnv) gs
       mbpprTrace "localCAFs" (ppr localCAFs) $ return ()

       gs <- run $ mapM (lowerSafeForeignCalls areaMap) gs
       mapM_ (dump Opt_D_dump_cmmz "after lowerSafeForeignCalls") gs

       -- NO MORE GRAPH TRANSFORMATION AFTER HERE -- JUST MAKING INFOTABLES
       let gs' = map (setInfoTableStackMap slotEnv areaMap) gs
       mapM_ (dump Opt_D_dump_cmmz "after setInfoTableStackMap") gs'
       let gs'' = map (bundleCAFs cafEnv) gs'
       mapM_ (dump Opt_D_dump_cmmz "after bundleCAFs") gs''
       return (localCAFs, gs'')
  where dflags = hsc_dflags hsc_env
        mbpprTrace x y z = if dopt Opt_D_dump_cmmz dflags then pprTrace x y z else z
        dump f txt g = dumpIfSet_dyn dflags f txt (ppr g)
        -- Runs a required transformation/analysis
        run = runInfiniteFuelIO (hsc_OptFuel hsc_env)
        -- Runs an optional transformation/analysis (and should
        -- thus be subject to optimization fuel)
        runOptimization = runFuelIO (hsc_OptFuel hsc_env)

        -- pass 'run' or 'runOptimization' for 'r'
        dual_rewrite r flag txt pass g =
          do dump flag ("Pre " ++ txt)  g
             g <- r $ pass g
             dump flag ("Post " ++ txt) $ g
             return g

-- This probably belongs in CmmBuildInfoTables?
-- We're just finishing the job here: once we know what CAFs are defined
-- in non-static closures, we can build the SRTs.
toTops :: HscEnv -> Map CLabel CAFSet -> (TopSRT, [[CmmTop]])
                 -> [(CAFSet, CmmTop)] -> IO (TopSRT, [[CmmTop]])
toTops hsc_env topCAFEnv (topSRT, tops) gs =
  do let setSRT (topSRT, rst) g =
           do (topSRT, gs) <- setInfoTableSRT topCAFEnv topSRT g
              return (topSRT, gs : rst)
     (topSRT, gs') <- runFuelIO (hsc_OptFuel hsc_env) $ foldM setSRT (topSRT, []) gs
     return (topSRT, concat gs' : tops)
