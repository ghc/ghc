%
% (c) The University of Glasgow 2006
%

FamInstEnv: Type checked family instance declarations

\begin{code}
module FamInstEnv (
	FamInst(..), famInstTyCon, famInstTyVars, 
	pprFamInst, pprFamInstHdr, pprFamInsts, 
	famInstHead, mkLocalFamInst, mkImportedFamInst,

	FamInstEnvs, FamInstEnv, emptyFamInstEnv, 
	extendFamInstEnv, extendFamInstEnvList, 
	famInstEnvElts, familyInstances,

	lookupFamInstEnv, lookupFamInstEnvUnify
    ) where

#include "HsVersions.h"

import InstEnv
import Unify
import TcGadt
import TcType
import Type
import TyCon
import VarSet
import Var
import Name
import OccName
import SrcLoc
import UniqFM
import Outputable

import Maybe
\end{code}


%************************************************************************
%*									*
\subsection{Type checked family instance heads}
%*									*
%************************************************************************

\begin{code}
data FamInst 
  = FamInst { fi_fam   :: Name		-- Family name

		-- Used for "rough matching"; same idea as for class instances
	    , fi_tcs   :: [Maybe Name]	-- Top of type args

		-- Used for "proper matching"; ditto
	    , fi_tvs   :: TyVarSet	-- Template tyvars for full match
	    , fi_tys   :: [Type]	-- Full arg types

	    , fi_tycon :: TyCon		-- Representation tycon
	    }

-- Obtain the representation tycon of a family instance.
--
famInstTyCon :: FamInst -> TyCon
famInstTyCon = fi_tycon

famInstTyVars = fi_tvs
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

famInstHead :: FamInst -> ([TyVar], TyCon, [Type])
famInstHead (FamInst {fi_tycon = tycon})
  = case tyConFamInst_maybe tycon of
      Nothing         -> panic "FamInstEnv.famInstHead"
      Just (fam, tys) -> (tyConTyVars tycon, fam, tys)

-- Make a family instance representation from a tycon.  This is used for local
-- instances, where we can safely pull on the tycon.
--
mkLocalFamInst :: TyCon -> FamInst
mkLocalFamInst tycon
  = case tyConFamInst_maybe tycon of
           Nothing         -> panic "FamInstEnv.mkLocalFamInst"
	   Just (fam, tys) -> 
	     FamInst {
	       fi_fam   = tyConName fam,
	       fi_tcs   = roughMatchTcs tys,
	       fi_tvs   = mkVarSet . tyConTyVars $ tycon,
	       fi_tys   = tys,
	       fi_tycon = tycon
	     }

-- Make a family instance representation from the information found in an
-- unterface file.  In particular, we get the rough match info from the iface
-- (instead of computing it here).
--
mkImportedFamInst :: Name -> [Maybe Name] -> TyCon -> FamInst
mkImportedFamInst fam mb_tcs tycon
  = FamInst {
      fi_fam   = fam,
      fi_tcs   = mb_tcs,
      fi_tvs   = mkVarSet . tyConTyVars $ tycon,
      fi_tys   = case tyConFamInst_maybe tycon of
		   Nothing       -> panic "FamInstEnv.mkImportedFamInst"
		   Just (_, tys) -> tys,
      fi_tycon = tycon
    }
\end{code}


%************************************************************************
%*									*
		FamInstEnv
%*									*
%************************************************************************

InstEnv maps a family name to the list of known instances for that family.

\begin{code}
type FamInstEnv = UniqFM FamilyInstEnv	-- Maps a family to its instances

type FamInstEnvs = (FamInstEnv, FamInstEnv)
 	-- External package inst-env, Home-package inst-env

data FamilyInstEnv
  = FamIE [FamInst]	-- The instances for a particular family, in any order
  	  Bool 		-- True <=> there is an instance of form T a b c
			-- 	If *not* then the common case of looking up
			--	(T a b c) can fail immediately

-- INVARIANTS:
--  * The fs_tvs are distinct in each FamInst
--	of a range value of the map (so we can safely unify them)

emptyFamInstEnv :: FamInstEnv
emptyFamInstEnv = emptyUFM

famInstEnvElts :: FamInstEnv -> [FamInst]
famInstEnvElts fi = [elt | FamIE elts _ <- eltsUFM fi, elt <- elts]

familyInstances :: (FamInstEnv, FamInstEnv) -> TyCon -> [FamInst]
familyInstances (pkg_fie, home_fie) fam
  = get home_fie ++ get pkg_fie
  where
    get env = case lookupUFM env fam of
		Just (FamIE insts _) -> insts
		Nothing	             -> []

extendFamInstEnvList :: FamInstEnv -> [FamInst] -> FamInstEnv
extendFamInstEnvList inst_env fis = foldl extendFamInstEnv inst_env fis

extendFamInstEnv :: FamInstEnv -> FamInst -> FamInstEnv
extendFamInstEnv inst_env ins_item@(FamInst {fi_fam = cls_nm, fi_tcs = mb_tcs})
  = addToUFM_C add inst_env cls_nm (FamIE [ins_item] ins_tyvar)
  where
    add (FamIE items tyvar) _ = FamIE (ins_item:items)
				      (ins_tyvar || tyvar)
    ins_tyvar = not (any isJust mb_tcs)
\end{code}

%************************************************************************
%*									*
\subsection{Looking up a family instance}
%*									*
%************************************************************************

@lookupFamInstEnvExact@ looks up in a @FamInstEnv@ using an exact match.
This is used when we want the @TyCon@ of a particular family instance (e.g.,
during deriving classes).

\begin{code}
{-		NOT NEEDED ANY MORE
lookupFamInstEnvExact :: (FamInstEnv		-- External package inst-env
		         ,FamInstEnv)		-- Home-package inst-env
		      -> TyCon -> [Type]	-- What we are looking for
	              -> Maybe FamInst
lookupFamInstEnvExact (pkg_ie, home_ie) fam tys
  = home_matches `mplus` pkg_matches
  where
    rough_tcs    = roughMatchTcs tys
    all_tvs      = all isNothing rough_tcs
    home_matches = lookup home_ie 
    pkg_matches  = lookup pkg_ie  

    --------------
    lookup env = case lookupUFM env fam of
		   Nothing -> Nothing		-- No instances for this class
		   Just (FamIE insts has_tv_insts)
		       -- Short cut for common case:
		       --   The thing we are looking up is of form (C a
		       --   b c), and the FamIE has no instances of
		       --   that form, so don't bother to search 
		     | all_tvs && not has_tv_insts -> Nothing
		     | otherwise                   -> find insts

    --------------
    find [] = Nothing
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tys = tpl_tys }) : rest)
	-- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

        -- Proper check
      | tcEqTypes tpl_tys tys
      = Just item

        -- No match => try next
      | otherwise
      = find rest
-}
\end{code}

@lookupFamInstEnv@ looks up in a @FamInstEnv@, using a one-way match.
Multiple matches are only possible in case of type families (not data
families), and then, it doesn't matter which match we choose (as the
instances are guaranteed confluent).

\begin{code}
lookupFamInstEnv :: FamInstEnvs
	         -> TyCon -> [Type]		-- What we are looking for
	         -> [(TvSubst, FamInst)] 	-- Successful matches
lookupFamInstEnv (pkg_ie, home_ie) fam tys
  = home_matches ++ pkg_matches
  where
    rough_tcs    = roughMatchTcs tys
    all_tvs      = all isNothing rough_tcs
    home_matches = lookup home_ie 
    pkg_matches  = lookup pkg_ie  

    --------------
    lookup env = case lookupUFM env fam of
		   Nothing -> []	-- No instances for this class
		   Just (FamIE insts has_tv_insts)
		       -- Short cut for common case:
		       --   The thing we are looking up is of form (C a
		       --   b c), and the FamIE has no instances of
		       --   that form, so don't bother to search 
		     | all_tvs && not has_tv_insts -> []
		     | otherwise                   -> find insts

    --------------
    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs, 
			  fi_tys = tpl_tys, fi_tycon = tycon }) : rest)
	-- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

        -- Proper check
      | Just subst <- tcMatchTys tpl_tvs tpl_tys tys
      = (subst, item) : find rest

        -- No match => try next
      | otherwise
      = find rest
\end{code}

While @lookupFamInstEnv@ uses a one-way match, the next function
@lookupFamInstEnvUnify@ uses two-way matching (ie, unification).  This is
needed to check for overlapping instances.

For class instances, these two variants of lookup are combined into one
function (cf, @InstEnv@).  We don't do that for family instances as the
results of matching and unification are used in two different contexts.
Moreover, matching is the wildly more frequently used operation in the case of
indexed synonyms and we don't want to slow that down by needless unification.

\begin{code}
lookupFamInstEnvUnify :: (FamInstEnv, FamInstEnv) -> TyCon -> [Type]
	              -> [(TvSubst, FamInst)]
lookupFamInstEnvUnify (pkg_ie, home_ie) fam tys
  = home_matches ++ pkg_matches
  where
    rough_tcs    = roughMatchTcs tys
    all_tvs      = all isNothing rough_tcs
    home_matches = lookup home_ie 
    pkg_matches  = lookup pkg_ie  

    --------------
    lookup env = case lookupUFM env fam of
		   Nothing -> []	-- No instances for this class
		   Just (FamIE insts has_tv_insts)
		       -- Short cut for common case:
		       --   The thing we are looking up is of form (C a
		       --   b c), and the FamIE has no instances of
		       --   that form, so don't bother to search 
		     | all_tvs && not has_tv_insts -> []
		     | otherwise                   -> find insts

    --------------
    find [] = []
    find (item@(FamInst { fi_tcs = mb_tcs, fi_tvs = tpl_tvs, 
			  fi_tys = tpl_tys, fi_tycon = tycon }) : rest)
	-- Fast check for no match, uses the "rough match" fields
      | instanceCantMatch rough_tcs mb_tcs
      = find rest

      | otherwise
      = ASSERT2( tyVarsOfTypes tys `disjointVarSet` tpl_tvs,
		 (ppr fam <+> ppr tys <+> ppr all_tvs) $$
		 (ppr tycon <+> ppr tpl_tvs <+> ppr tpl_tys)
		)
		-- Unification will break badly if the variables overlap
		-- They shouldn't because we allocate separate uniques for them
        case tcUnifyTys bind_fn tpl_tys tys of
	    Just subst -> (subst, item) : find rest
	    Nothing    -> find rest

-- See explanation at @InstEnv.bind_fn@.
--
bind_fn tv | isTcTyVar tv && isExistentialTyVar tv = Skolem
	   | otherwise	 		 	   = BindMe
\end{code}
