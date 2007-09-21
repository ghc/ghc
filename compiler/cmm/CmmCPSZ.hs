
module CmmCPSZ (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  -- Well, sort of.
  protoCmmCPSZ
) where

import Cmm
import CmmContFlowOpt
import CmmProcPointZ
import CmmSpillReload
import CmmTx
import DFMonad
import PprCmmZ()
import ZipCfg hiding (zip, unzip)
import ZipCfgCmmRep
import ZipDataflow0

import DynFlags
import ErrUtils
import Outputable
import UniqSupply

import Data.IORef

-----------------------------------------------------------------------------
-- |Top level driver for the CPS pass
-----------------------------------------------------------------------------
protoCmmCPSZ :: DynFlags -- ^ Dynamic flags: -dcmm-lint -ddump-cps-cmm
       -> CmmZ     -- ^ Input C-- with Proceedures
       -> IO CmmZ  -- ^ Output CPS transformed C--
protoCmmCPSZ dflags (Cmm tops)
  = do	{ showPass dflags "CPSZ"
        ; u <- mkSplitUniqSupply 'p'
        ; pass_ref <- newIORef "unoptimized program" -- XXX see [Note global fuel]
        ; fuel_ref <- newIORef (tankFilledTo maxBound) -- XXX see [Note global fuel]
        ; let txtops = initUs_ u $ mapM cpsTop tops
        ; tops <- runFuelIO pass_ref fuel_ref (sequence txtops)
	; dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (ppr (Cmm tops))
        ; return $ Cmm tops
        }

{- [Note global fuel]
~~~~~~~~~~~~~~~~~~~~~
In a correct world, the identity and the last pass would be stored in
mutable reference cells associated with an 'HscEnv' and would be
global to one compiler session.  Unfortunately the 'HscEnv' is not
plumbed sufficiently close to this function; only the DynFlags are
plumbed here.  One day the plumbing will be extended, in which case
this pass will use the global 'pass_ref' and 'fuel_ref' instead of the
bogus facsimiles in place here.
-}

cpsTop :: CmmTopZ -> UniqSM (FuelMonad CmmTopZ)
cpsTop p@(CmmData {}) = return (return p)
cpsTop (CmmProc h l args g) =
    let procPoints = minimalProcPointSet (runTx cmmCfgOptsZ g)
        g' = addProcPointProtocols procPoints args g
        g'' = map_nodes id NotSpillOrReload id g'
    in do { u1 <- getUs; u2 <- getUs; u3 <- getUs
          ; entry <- getUniqueUs >>= return . BlockId
          ; return $ 
              do { g <- return g''
                 ; g <- dual_rewrite u1 dualLivenessWithInsertion g
                 ; g <- insertLateReloads' u2 (extend g)
                 ; g <- dual_rewrite u3 removeDeadAssignmentsAndReloads (trim entry g)
                 ; return $ CmmProc h l args $ map_nodes id spillAndReloadComments id g
                 }
          }
  where dual_rewrite u pass g = runDFM u dualLiveLattice $ b_rewrite pass g
        extend (LGraph eid blocks) = Graph (ZLast $ mkBranchNode eid) blocks
        trim _ (Graph (ZLast (LastOther (LastBranch id))) blocks) = LGraph id blocks
        trim e (Graph tail blocks) = LGraph e (insertBlock (Block e tail) blocks)
