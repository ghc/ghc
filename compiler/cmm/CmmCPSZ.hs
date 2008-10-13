module CmmCPSZ (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  -- Well, sort of.
  protoCmmCPSZ
) where

import CLabel
import Cmm
import CmmBuildInfoTables
import CmmCommonBlockElimZ
import CmmProcPointZ
import CmmSpillReload
import CmmStackLayout
import DFMonad
import PprCmmZ()
import ZipCfgCmmRep

import DynFlags
import ErrUtils
import FiniteMap
import HscTypes
import Maybe
import Monad
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
protoCmmCPSZ :: HscEnv -- Compilation env including
                       -- dynamic flags: -dcmm-lint -ddump-cps-cmm
             -> (TopSRT, [CmmZ])  -- SRT table and 
             -> CmmZ              -- Input C-- with Procedures
             -> IO (TopSRT, [CmmZ]) -- Output CPS transformed C--
protoCmmCPSZ hsc_env (topSRT, rst) (Cmm tops)
  | not (dopt Opt_RunCPSZ (hsc_dflags hsc_env))
  = return (topSRT, Cmm tops : rst)                -- Only if -frun-cps
  | otherwise
  = do	let dflags = hsc_dflags hsc_env
        showPass dflags "CPSZ"
        (cafEnvs, toTops) <- liftM unzip $ mapM (cpsTop hsc_env) tops
        let topCAFEnv = mkTopCAFInfo (concat cafEnvs)
        (topSRT, tops) <- foldM (\ z f -> f topCAFEnv z) (topSRT, []) toTops 
        let cmms = Cmm (reverse (concat tops))
        dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (ppr cmms)
        return (topSRT, cmms : rst)

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
The identity and the last pass are stored in
mutable reference cells in an 'HscEnv' and are
global to one compiler session.
-}

cpsTop :: HscEnv -> CmmTopZ ->
          IO ([(CLabel, CAFSet)],
              (FiniteMap CLabel CAFSet -> (TopSRT, [[CmmTopZ]]) -> IO (TopSRT, [[CmmTopZ]])))
cpsTop _ p@(CmmData {}) =
  return ([], (\ _ (topSRT, tops) -> return (topSRT, [p] : tops)))
cpsTop hsc_env (CmmProc h l args g) =
    do 
       dump Opt_D_dump_cmmz "Pre Proc Points Added"  g
       let callPPs = callProcPoints g
       g <- dual_rewrite Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion callPPs) g
       g <- dual_rewrite Opt_D_dump_cmmz "Dead Assignment Elimination"
                         (removeDeadAssignmentsAndReloads callPPs) g
       dump Opt_D_dump_cmmz "Pre common block elimination" g
       g <- return $ elimCommonBlocks g
       dump Opt_D_dump_cmmz "Post common block elimination" g
       procPoints <- run $ minimalProcPointSet callPPs g
       -- print $ "call procPoints: " ++ (showSDoc $ ppr procPoints)
       g <- run $ addProcPointProtocols callPPs procPoints g
       dump Opt_D_dump_cmmz "Post Proc Points Added" g
       g     <- dual_rewrite Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion procPoints) g
                    -- Insert spills at defns; reloads at return points
       g     <- run $ insertLateReloads g -- Duplicate reloads just before uses
       dump Opt_D_dump_cmmz "Post late reloads" g
       g     <- dual_rewrite Opt_D_dump_cmmz "Dead Assignment Elimination"
                                        (removeDeadAssignmentsAndReloads procPoints) g
                    -- Remove redundant reloads (and any other redundant asst)
       -- Debugging: stubbing slots on death can cause crashes early
       g <-  if opt_StubDeadValues then run $ stubSlotsOnDeath g else return g
       mbpprTrace "graph before procPointMap: " (ppr g) $ return ()
       procPointMap <- run $ procPointAnalysis procPoints g
       slotEnv <- run $ liveSlotAnal g
       mbpprTrace "live slot analysis results: " (ppr slotEnv) $ return ()
       cafEnv <- run $ cafAnal g
       (cafEnv, slotEnv) <- return $ extendEnvsForSafeForeignCalls cafEnv slotEnv g
       mbpprTrace "slotEnv extended for safe foreign calls: " (ppr slotEnv) $ return ()
       let areaMap = layout procPoints slotEnv g
       mbpprTrace "areaMap" (ppr areaMap) $ return ()
       g  <- run $ manifestSP procPoints procPointMap areaMap g
       dump Opt_D_dump_cmmz "after manifestSP" g
       -- UGH... manifestSP can require updates to the procPointMap.
       -- We can probably do something quicker here for the update...
       procPointMap  <- run $ procPointAnalysis procPoints g
       gs <- pprTrace "procPointMap" (ppr procPointMap) $
               run $ splitAtProcPoints l callPPs procPoints procPointMap areaMap
                                       (CmmProc h l args g)
       mapM (dump Opt_D_dump_cmmz "after splitting") gs
       let localCAFs = catMaybes $ map (localCAFInfo cafEnv) gs
       mbpprTrace "localCAFs" (ppr localCAFs) $ return ()
       gs <- liftM concat $ run $ foldM (lowerSafeForeignCalls procPoints) [] gs
       mapM (dump Opt_D_dump_cmmz "after lowerSafeForeignCalls") gs

       -- NO MORE GRAPH TRANSFORMATION AFTER HERE -- JUST MAKING INFOTABLES
       let gs' = map (setInfoTableStackMap slotEnv areaMap) gs
       mapM (dump Opt_D_dump_cmmz "after setInfoTableStackMap") gs'
       -- Return: (a) CAFs used by this proc (b) a closure that will compute
       --  a new SRT for the procedure.
       let toTops topCAFEnv (topSRT, tops) =
             do let setSRT (topSRT, rst) g =
                      do (topSRT, gs) <- setInfoTableSRT cafEnv topCAFEnv topSRT g
                         return (topSRT, gs : rst)
                (topSRT, gs') <- run $ foldM setSRT (topSRT, []) gs'
                gs' <- mapM finishInfoTables (concat gs')
                pprTrace "localCAFs" (ppr localCAFs <+> ppr topSRT) $
                  return (topSRT, concat gs' : tops)
       return (localCAFs, toTops)
  where dflags = hsc_dflags hsc_env
        mbpprTrace x y z = if dopt Opt_D_dump_cmmz dflags then pprTrace x y z else z
        dump f txt g = dumpIfSet_dyn dflags f txt (ppr g)
        run = runFuelIO (hsc_OptFuel hsc_env)
        dual_rewrite flag txt pass g =
          do dump flag ("Pre " ++ txt)  g
             g <- run $ pass g
             dump flag ("Post " ++ txt) $ g
             return g
