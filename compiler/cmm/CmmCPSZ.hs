module CmmCPSZ (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  -- Well, sort of.
  protoCmmCPSZ
) where

import Cmm
import CmmCommonBlockElimZ
import CmmProcPointZ
import CmmSpillReload
import DFMonad
import PprCmmZ()
import ZipCfgCmmRep

import DynFlags
import ErrUtils
import HscTypes
import Monad
import Outputable

-----------------------------------------------------------------------------
-- |Top level driver for the CPS pass
-----------------------------------------------------------------------------
protoCmmCPSZ :: HscEnv -- Compilation env including
                       -- dynamic flags: -dcmm-lint -ddump-cps-cmm
             -> CmmZ     -- Input C-- with Proceedures
             -> IO CmmZ  -- Output CPS transformed C--
protoCmmCPSZ hsc_env (Cmm tops)
  | not (dopt Opt_RunCPSZ (hsc_dflags hsc_env))
  = return (Cmm tops)                -- Only if -frun-cps
  | otherwise
  = do	let dflags = hsc_dflags hsc_env
        showPass dflags "CPSZ"
        tops <- liftM concat $ mapM (cpsTop hsc_env) tops
        dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (ppr (Cmm tops))
        return $ Cmm tops

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
The identity and the last pass are stored in
mutable reference cells in an 'HscEnv' and are
global to one compiler session.
-}

cpsTop :: HscEnv -> CmmTopZ -> IO [CmmTopZ]
cpsTop _ p@(CmmData {}) = return [p]
cpsTop hsc_env (CmmProc h l args g) =
    do dump Opt_D_dump_cmmz "Pre Proc Points Added"  g
       let callPPs = callProcPoints g
       g <- dual_rewrite Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion callPPs) g
       dump Opt_D_dump_cmmz "Pre common block elimination" g
       g <- return $ elimCommonBlocks g
       dump Opt_D_dump_cmmz "Post common block elimination" g
       procPoints <- run $ minimalProcPointSet callPPs g
       print $ "call procPoints: " ++ (showSDoc $ ppr procPoints)
       g <- run $ addProcPointProtocols callPPs procPoints g
       dump Opt_D_dump_cmmz "Post Proc Points Added" g
       g     <- dual_rewrite Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion procPoints) g
                    -- Insert spills at defns; reloads at return points
       g     <- run $ insertLateReloads' g -- Duplicate reloads just before uses
       dump Opt_D_dump_cmmz "Post late reloads" g
       g     <- dual_rewrite Opt_D_dump_cmmz "Dead Assignment Elimination"
                                        (removeDeadAssignmentsAndReloads procPoints) g
                    -- Remove redundant reloads (and any other redundant asst)
       slotEnv <- run $ liveSlotAnal g
       print $ "live slot analysis results: " ++ (showSDoc $ ppr slotEnv)
       cafEnv <- run $ cafAnal g
       print $ "live CAF analysis results: " ++ (showSDoc $ ppr cafEnv)
       slotIGraph <- return $ igraph areaBuilder slotEnv g
       print $ "slot IGraph: " ++ (showSDoc $ ppr slotIGraph)
       print $ "graph before procPointMap: " ++ (showSDoc $ ppr g)
       procPointMap <- run $ procPointAnalysis procPoints g
       let areaMap = layout procPoints slotEnv g
       g  <- run $ manifestSP procPoints procPointMap areaMap g
       procPointMap <- run $ procPointAnalysis procPoints g
       gs <- run $ splitAtProcPoints l callPPs procPoints procPointMap slotEnv areaMap
                                     (CmmProc h l args g)
       return gs
       --return $ [CmmProc h l args (runTx cmmCfgOptsZ g)]
  where dflags = hsc_dflags hsc_env
        dump f txt g = dumpIfSet_dyn dflags f txt (ppr g)
        run = runFuelIO (hsc_OptFuel hsc_env)
        dual_rewrite flag txt pass g =
          do dump flag ("Pre " ++ txt)  g
             g <- run $ pass g
             dump flag ("Post " ++ txt) $ g
             return g
