module CmmCPSZ (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  -- Well, sort of.
  protoCmmCPSZ
) where

import BlockId
import Cmm
import CmmCommonBlockElimZ
import CmmContFlowOpt
import CmmProcPointZ
import CmmSpillReload
import CmmTx
import DFMonad
import PprCmmZ()
import ZipCfg hiding (zip, unzip)
import ZipCfgCmmRep

import DynFlags
import ErrUtils
import FiniteMap
import HscTypes
import Monad
import Outputable
import UniqSupply

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
        tops <- mapM (cpsTop hsc_env) tops
        dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "Post CPS Cmm" (ppr (Cmm tops))
        return $ Cmm tops

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
The identity and the last pass are stored in
mutable reference cells in an 'HscEnv' and are
global to one compiler session.
-}

cpsTop :: HscEnv -> CmmTopZ -> IO CmmTopZ
cpsTop _ p@(CmmData {}) = return p
cpsTop hsc_env (CmmProc h l args g) =
    do dump Opt_D_dump_cmmz "Pre Proc Points Added"  g
       let callPPs = callProcPoints g
       g <- return $ map_nodes id NotSpillOrReload id g
               -- Change types of middle nodes to allow spill/reload
       g     <- dual_rewrite Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion callPPs) g
       (varSlots, g) <- trim g >>= return . elimSpillAndReload emptyFM
       procPoints <- run $ minimalProcPointSet callPPs (runTx cmmCfgOptsZ g)
       g <- run $ addProcPointProtocols callPPs procPoints g
       dump Opt_D_dump_cmmz "Post Proc Points Added" g
       g <- return $ map_nodes id NotSpillOrReload id g
               -- Change types of middle nodes to allow spill/reload
       g     <- dual_rewrite Opt_D_dump_cmmz "spills and reloads"
                             (dualLivenessWithInsertion procPoints) g
                    -- Insert spills at defns; reloads at return points
       g     <- run $ insertLateReloads' g -- Duplicate reloads just before uses
       dump Opt_D_dump_cmmz "Post late reloads" g
       g     <- trim g >>= dual_rewrite Opt_D_dump_cmmz "Dead Assignment Elimination"
                                        (removeDeadAssignmentsAndReloads procPoints)
                    -- Remove redundant reloads (and any other redundant asst)
       (_, g) <- trim g >>= return . elimSpillAndReload varSlots
       gs    <- run $ splitAtProcPoints args l procPoints g
       gs `seq` dump Opt_D_dump_cmmz "Pre common block elimination" g
       g     <- return $ elimCommonBlocks g
       dump Opt_D_dump_cmmz "Post common block elimination" g
       return $ CmmProc h l args (runTx cmmCfgOptsZ g)
  where dflags = hsc_dflags hsc_env
        dump f txt g = dumpIfSet_dyn dflags f txt (ppr g)
        run = runFuelIO (hsc_OptFuel hsc_env)
        dual_rewrite flag txt pass g =
          do dump flag ("Pre " ++ txt)  g
             g <- run $ pass (graphOfLGraph g) >>= lGraphOfGraph
             dump flag ("Post " ++ txt) $ g
             return $ graphOfLGraph g
        trim (Graph (ZLast (LastOther (LastBranch id))) blocks) = return $ LGraph id blocks
        trim (Graph tail blocks) =
          do entry <- liftM BlockId $ run $ getUniqueM
             return $ LGraph entry (insertBlock (Block entry tail) blocks)
