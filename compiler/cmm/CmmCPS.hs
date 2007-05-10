module CmmCPS (cmmCPS) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import Dataflow (mapCmmTop, onBasicBlock, cmmLivenessComment, cmmLiveness)

import DynFlags
import ErrUtils
import Maybes
import Outputable

import Monad
import IO

cmmCPS :: DynFlags
       -> [Cmm]                 -- C-- with Proceedures
       -> IO [Cmm]		-- Output: CPS transformed C--

cmmCPS dflags abstractC = do
  when (dopt Opt_DoCmmLinting dflags) $
       do showPass dflags "CmmLint"
	  case firstJust $ map cmmLint abstractC of
	    Just err -> do printDump err
			   ghcExit dflags 1
	    Nothing  -> return ()
  showPass dflags "CPS"
  -- continuationC <- return abstractC
  continuationC <- return $ map (mapCmmTop (onBasicBlock (\bs -> map (cmmLivenessComment (cmmLiveness bs)) bs))) abstractC

  dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms continuationC)
  -- TODO: add option to dump Cmm to file
  return continuationC
