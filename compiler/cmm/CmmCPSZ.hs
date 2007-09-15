
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
import DynFlags
import ErrUtils
import Outputable
import PprCmmZ()
import UniqSupply
import ZipCfg hiding (zip, unzip)
import ZipCfgCmmRep
import ZipDataflow

-----------------------------------------------------------------------------
-- |Top level driver for the CPS pass
-----------------------------------------------------------------------------
protoCmmCPSZ :: DynFlags -- ^ Dynamic flags: -dcmm-lint -ddump-cps-cmm
       -> CmmZ     -- ^ Input C-- with Proceedures
       -> IO CmmZ  -- ^ Output CPS transformed C--
protoCmmCPSZ dflags (Cmm tops)
  = do	{ showPass dflags "CPSZ"
        ; u <- mkSplitUniqSupply 'p'
        ; let txtops = initUs_ u $ mapM cpsTop tops
        ; let pgm = Cmm $ runDFTx maxBound $ sequence txtops
           --- XXX calling runDFTx is totally bogus
	; dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (ppr pgm)
        ; return pgm
        }

cpsTop :: CmmTopZ -> UniqSM (DFTx CmmTopZ)
cpsTop p@(CmmData {}) = return $ return p
cpsTop (CmmProc h l args g) =
    let procPoints = minimalProcPointSet (runTx cmmCfgOptsZ g)
        g' = addProcPointProtocols procPoints args g
        g'' = map_nodes id NotSpillOrReload id g'
    in do g <- dual_rewrite dualLivenessWithInsertion g''
          g <- return (g >>= insertLateReloads)
          u <- getUs
          let g' = g >>= (initUs_ u . dual_rewrite removeDeadAssignmentsAndReloads)
          return $ do g <- g' >>= return . map_nodes id spillAndReloadComments id
                      return $ CmmProc h l args g
  where dual_rewrite pass g =
            do us <- getUs
               return $ runDFM us dualLiveLattice $ b_rewrite pass g
