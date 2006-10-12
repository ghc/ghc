The @FamInst@ type: family instance heads

\begin{code}
module FamInst ( 
        tcExtendLocalFamInstEnv
    ) where

#include "HsVersions.h"

import HscTypes
import FamInstEnv
import TcMType
import TcType
import TcRnMonad
import TyCon
import Type
import Name
import SrcLoc
import Outputable

import Monad
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
	; (tvs', _, tau') <- tcInstSkolType (FamInstSkol tycon) ty

	; let (fam, tys') = tcSplitTyConApp tau'

		-- Load imported instances, so that we report
		-- overlaps correctly
	; eps <- getEps
	; let inst_envs = (eps_fam_inst_env eps, home_fie)

		-- Check for conflicting instance decls
	; let { matches   = lookupFamInstEnvUnify inst_envs fam tys'
	      ;	conflicts = [ conflictingFamInst
			    | match@(_, conflictingFamInst) <- matches
			    , conflicting fam tys' tycon match 
			    ]
              }
	; unless (null conflicts) $
	    conflictInstErr famInst (head conflicts)

		-- OK, now extend the envt
	; return (extendFamInstEnv home_fie famInst) 
        }
  where
    -- In the case of data/newtype instances, any overlap is a conflicts (as
    -- these instances imply injective type mappings).
    conflicting _   _    tycon _                 | isAlgTyCon tycon = True
    conflicting fam tys' tycon (subst, cFamInst) | otherwise	  =
      panic "FamInst.addLocalFamInst: overlap check for indexed synonyms is still missing"

conflictInstErr famInst conflictingFamInst
  = addFamInstLoc famInst $
    addErr (hang (ptext SLIT("Conflicting family instance declarations:"))
	       2 (pprFamInsts [famInst, conflictingFamInst]))

addFamInstLoc famInst thing_inside
  = setSrcSpan (mkSrcSpan loc loc) thing_inside
  where
    loc = getSrcLoc famInst
\end{code}
