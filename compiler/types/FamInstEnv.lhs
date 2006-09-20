\section[FamInstEnv]{Type checked family instance declarations}

\begin{code}
module FamInstEnv (
	FamInst(..), famInstTyCon, extractFamInsts,
	pprFamInst, pprFamInstHdr, pprFamInsts, 
	{-famInstHead, mkLocalFamInst, mkImportedFamInst-}

	FamInstEnv, emptyFamInstEnv, extendFamInstEnv, extendFamInstEnvList, 
	famInstEnvElts, familyInstances,
	{-lookupFamInstEnv-}
    ) where

#include "HsVersions.h"

import TcType		( Type )
import Type		( TyThing (ATyCon), pprParendType )
import TyCon		( TyCon, isDataTyCon, isNewTyCon, isSynTyCon, 
			  tyConName, tyConTyVars, tyConFamInst_maybe )
import VarSet		( TyVarSet, mkVarSet )
import Name		( Name, getOccName, NamedThing(..), getSrcLoc )
import OccName		( parenSymOcc )
import SrcLoc		( pprDefnLoc )
import UniqFM		( UniqFM, lookupUFM, emptyUFM, addToUFM_C, eltsUFM )
import Outputable

import Monad		( mzero )
\end{code}


%************************************************************************
%*									*
\subsection{Type checked family instance heads}
%*									*
%************************************************************************

\begin{code}
data FamInst 
  = FamInst { fi_fam   :: Name		-- Family name
	    , fi_tvs   :: TyVarSet	-- Template tyvars for full match
	    , fi_tys   :: [Type]	-- Full arg types

	    , fi_tycon :: TyCon		-- Representation tycon
	    }

-- Obtain the representation tycon of a family instance.
--
famInstTyCon :: FamInst -> TyCon
famInstTyCon = fi_tycon

-- Extract all family instances.
--
extractFamInsts :: [TyThing] -> [FamInst]
extractFamInsts tythings
  = do { ATyCon tycon <- tythings
       ; case tyConFamInst_maybe tycon of
           Nothing         -> mzero
	   Just (fam, tys) -> 
	     return $ FamInst { fi_fam   = tyConName fam
			      , fi_tvs   = mkVarSet . tyConTyVars $ tycon
			      , fi_tys   = tys
			      , fi_tycon = tycon
			      }
       }
\end{code}

\begin{code}
instance NamedThing FamInst where
   getName = getName . fi_tycon

instance Outputable FamInst where
   ppr = pprFamInst

-- Prints the FamInst as a family instance declaration
pprFamInst :: FamInst -> SDoc
pprFamInst famInst
  = hang (pprFamInstHdr famInst)
	2 (ptext SLIT("--") <+> (pprDefnLoc (getSrcLoc famInst)))

pprFamInstHdr :: FamInst -> SDoc
pprFamInstHdr (FamInst {fi_fam = fam, fi_tys = tys, fi_tycon = tycon})
  = pprTyConSort <+> pprHead
  where
    pprHead = parenSymOcc (getOccName fam) (ppr fam) <+> 
	      sep (map pprParendType tys)
    pprTyConSort | isDataTyCon tycon = ptext SLIT("data instance")
		 | isNewTyCon  tycon = ptext SLIT("newtype instance")
		 | isSynTyCon  tycon = ptext SLIT("type instance")
		 | otherwise	     = panic "FamInstEnv.pprFamInstHdr"

pprFamInsts :: [FamInst] -> SDoc
pprFamInsts finsts = vcat (map pprFamInst finsts)
\end{code}


%************************************************************************
%*									*
		FamInstEnv
%*									*
%************************************************************************

InstEnv maps a family name to the list of known instances for that family.

\begin{code}
type FamInstEnv = UniqFM [FamInst]	-- Maps a family to its instances

-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--	of a range value of the map (so we can safely unify them)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = emptyUFM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts = concat . eltsUFM

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get env = case lookupUFM env fam of
		Just insts -> insts
		Nothing	   -> []

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv inst_env ins_item@(FamInst {fi_fam = cls_nm})
  = addToUFM_C add inst_env cls_nm [ins_item]
  where
    add items _ = ins_item:items
\end{code}		      

