\section[FamInst]{The @FamInst@ type: family instance heads}

\begin{code}
module FamInst ( 
        tcExtendLocalFamInstEnv
    ) where

#include "HsVersions.h"

import HscTypes   ( ExternalPackageState(..) )
import FamInstEnv ( FamInstEnv, FamInst(..), famInstTyCon, extendFamInstEnv,
		    pprFamInst, pprFamInsts )
import TcMType	  ( tcInstSkolType )
import TcType	  ( SkolemInfo(..), tcSplitTyConApp )
import TcRnMonad  ( TcM, TcGblEnv(..), setGblEnv, getGblEnv, foldlM,
		    setSrcSpan, addErr, getEps )
import TyCon      ( tyConFamInst_maybe )
import Type	  ( mkTyConApp )
import Name	  ( getSrcLoc )
import SrcLoc	  ( mkSrcSpan )
import Outputable
\end{code}


%************************************************************************
%*									*
	Extending the family instance environment
%*									*
%************************************************************************

\begin{code}

-- Add new locally-defined family instances
tcExtendLocalFamInstEnv :: [FamInst] -> TcM a -> TcM a
tcExtendLocalFamInstEnv fam_insts thing_inside
 = do { env <- getGblEnv
      ; inst_env' <- foldlM addLocalFamInst (tcg_fam_inst_env env) fam_insts
      ; let env' = env { tcg_fam_insts    = fam_insts ++ tcg_fam_insts env,
			 tcg_fam_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }


-- Check that the proposed new instance is OK, 
-- and then add it to the home inst env
addLocalFamInst :: FamInstEnv -> FamInst -> TcM FamInstEnv
addLocalFamInst home_fie famInst
  = do	{ 	-- To instantiate the family instance type, extend the instance
		-- envt with completely fresh template variables
		-- This is important because the template variables must
		-- not overlap with anything in the things being looked up
		-- (since we do unification).  
		-- We use tcInstSkolType because we don't want to allocate
		-- fresh *meta* type variables.  
	  let tycon = famInstTyCon famInst
	      ty    = case tyConFamInst_maybe tycon of
		        Nothing        -> panic "FamInst.addLocalFamInst"
			Just (tc, tys) -> tc `mkTyConApp` tys
	; (tvs', theta', tau') <- tcInstSkolType (FamInstSkol tycon) ty

	; let	(fam, tys') = tcSplitTyConApp tau'

		-- Load imported instances, so that we report
		-- overlaps correctly
	; eps <- getEps
	; let inst_envs = (eps_fam_inst_env eps, home_fie)

{- !!!TODO: Need to complete this:
		-- Check for overlapping instance decls
	; let { (matches, _) = lookupFamInstEnv inst_envs fam tys'
	      ;	dup_ispecs = [ dup_ispec   --!!!adapt
			     | (_, dup_ispec) <- matches
			     , let (_,_,_,dup_tys) = instanceHead dup_ispec
			     , isJust (tcMatchTys (mkVarSet tvs') tys' dup_tys)] }
		-- Find memebers of the match list which ispec itself matches.
		-- If the match is 2-way, it's a duplicate
	; case dup_ispecs of
	    dup_ispec : _ -> dupInstErr famInst dup_ispec
	    []            -> return ()
 -}

		-- OK, now extend the envt
	; return (extendFamInstEnv home_fie famInst) }

overlapErr famInst dupFamInst
  = addFamInstLoc famInst $
    addErr (hang (ptext SLIT("Overlapping family instance declarations:"))
	       2 (pprFamInsts [famInst, dupFamInst]))

addFamInstLoc famInst thing_inside
  = setSrcSpan (mkSrcSpan loc loc) thing_inside
  where
    loc = getSrcLoc famInst
\end{code}
