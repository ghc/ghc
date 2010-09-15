%
% (c) The University of Glasgow 2006
%

FamInstEnv: Type checked family instance declarations

\begin{code}
module FamInstEnv (
	FamInst(..), famInstTyCon, famInstTyVars, 
	pprFamInst, pprFamInstHdr, pprFamInsts, 
	famInstHead, mkLocalFamInst, mkImportedFamInst,

	FamInstEnvs, FamInstEnv, emptyFamInstEnv, emptyFamInstEnvs, 
	extendFamInstEnv, extendFamInstEnvList, 
	famInstEnvElts, familyInstances,

	lookupFamInstEnv, lookupFamInstEnvConflicts,
	
	-- Normalisation
	topNormaliseType
    ) where

#include "HsVersions.h"

import InstEnv
import Unify
import Type
import TypeRep
import TyCon
import Coercion
import VarSet
import Var
import Name
import UniqFM
import Outputable
import Maybes
import Util
import FastString
\end{code}


%************************************************************************
%*									*
\subsection{Type checked family instance heads}
%*									*
%************************************************************************

\begin{code}
data FamInst 
  = FamInst { fi_fam   :: Name		-- Family name
		-- INVARIANT: fi_fam = case tyConFamInst_maybe fi_tycon of
		--			   Just (tc, tys) -> tc

		-- Used for "rough matching"; same idea as for class instances
	    , fi_tcs   :: [Maybe Name]	-- Top of type args
		-- INVARIANT: fi_tcs = roughMatchTcs fi_tys

		-- Used for "proper matching"; ditto
	    , fi_tvs   :: TyVarSet	-- Template tyvars for full match
	    , fi_tys   :: [Type]	-- Full arg types
		-- INVARIANT: fi_tvs = tyConTyVars fi_tycon
		--	      fi_tys = case tyConFamInst_maybe fi_tycon of
		--			   Just (_, tys) -> tys

	    , fi_tycon :: TyCon		-- Representation tycon
	    }

-- Obtain the representation tycon of a family instance.
--
famInstTyCon :: FamInst -> TyCon
famInstTyCon = fi_tycon

famInstTyVars :: FamInst -> TyVarSet
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
	2 (ptext (sLit "--") <+> pprNameLoc (getName famInst))

pprFamInstHdr :: FamInst -> SDoc
pprFamInstHdr (FamInst {fi_tycon = rep_tc})
  = pprTyConSort <+> pp_instance <+> pprHead
  where
    Just (fam_tc, tys) = tyConFamInst_maybe rep_tc 
    
    -- For *associated* types, say "type T Int = blah" 
    -- For *top level* type instances, say "type instance T Int = blah"
    pp_instance 
      | isTyConAssoc fam_tc = empty
      | otherwise           = ptext (sLit "instance")

    pprHead = pprTypeApp fam_tc tys
    pprTyConSort | isDataTyCon     rep_tc = ptext (sLit "data")
		 | isNewTyCon      rep_tc = ptext (sLit "newtype")
		 | isSynTyCon      rep_tc = ptext (sLit "type")
		 | isAbstractTyCon rep_tc = ptext (sLit "data")
		 | otherwise	          = panic "FamInstEnv.pprFamInstHdr"

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

emptyFamInstEnvs :: (FamInstEnv, FamInstEnv)
emptyFamInstEnvs = (emptyFamInstEnv, emptyFamInstEnv)

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
		Looking up a family instance
%*									*
%************************************************************************

@lookupFamInstEnv@ looks up in a @FamInstEnv@, using a one-way match.
Multiple matches are only possible in case of type families (not data
families), and then, it doesn't matter which match we choose (as the
instances are guaranteed confluent).

We return the matching family instances and the type instance at which it
matches.  For example, if we lookup 'T [Int]' and have a family instance

  data instance T [a] = ..

desugared to

  data :R42T a = ..
  coe :Co:R42T a :: T [a] ~ :R42T a

we return the matching instance '(FamInst{.., fi_tycon = :R42T}, Int)'.

\begin{code}
type FamInstMatch = (FamInst, [Type])           -- Matching type instance
  -- See Note [Over-saturated matches]

lookupFamInstEnv
    :: FamInstEnvs
    -> TyCon -> [Type]		-- What we are looking for
    -> [FamInstMatch] 	        -- Successful matches

lookupFamInstEnv
   = lookup_fam_inst_env match True
   where
     match _ tpl_tvs tpl_tys tys = tcMatchTys tpl_tvs tpl_tys tys

lookupFamInstEnvConflicts
    :: FamInstEnvs
    -> FamInst		-- Putative new instance
    -> [TyVar]		-- Unique tyvars, matching arity of FamInst
    -> [FamInstMatch] 	-- Conflicting matches
-- E.g. when we are about to add
--    f : type instance F [a] = a->a
-- we do (lookupFamInstConflicts f [b])
-- to find conflicting matches
-- The skolem tyvars are needed because we don't have a 
-- unique supply to hand

lookupFamInstEnvConflicts envs fam_inst skol_tvs
  = lookup_fam_inst_env my_unify False envs fam tys'
  where
    inst_tycon = famInstTyCon fam_inst
    (fam, tys) = expectJust "FamInstEnv.lookuFamInstEnvConflicts"
    	       	            (tyConFamInst_maybe inst_tycon)
    skol_tys = mkTyVarTys skol_tvs
    tys'     = substTys (zipTopTvSubst (tyConTyVars inst_tycon) skol_tys) tys
        -- In example above,   fam tys' = F [b]   

    my_unify old_fam_inst tpl_tvs tpl_tys match_tys
       = ASSERT2( tyVarsOfTypes tys `disjointVarSet` tpl_tvs,
		  (ppr fam <+> ppr tys) $$
		  (ppr tpl_tvs <+> ppr tpl_tys) )
		-- Unification will break badly if the variables overlap
		-- They shouldn't because we allocate separate uniques for them
         case tcUnifyTys instanceBindFun tpl_tys match_tys of
	      Just subst | conflicting old_fam_inst subst -> Just subst
	      _other	   	              	          -> Nothing

      -- - In the case of data family instances, any overlap is fundamentally a
      --   conflict (as these instances imply injective type mappings).
      -- - In the case of type family instances, overlap is admitted as long as
      --   the right-hand sides of the overlapping rules coincide under the
      --   overlap substitution.  We require that they are syntactically equal;
      --   anything else would be difficult to test for at this stage.
    conflicting old_fam_inst subst 
      | isAlgTyCon fam = True
      | otherwise      = not (old_rhs `tcEqType` new_rhs)
      where
        old_tycon = famInstTyCon old_fam_inst
        old_tvs   = tyConTyVars old_tycon
        old_rhs   = mkTyConApp old_tycon  (substTyVars subst old_tvs)
        new_rhs   = mkTyConApp inst_tycon (substTyVars subst skol_tvs)
\end{code}

While @lookupFamInstEnv@ uses a one-way match, the next function
@lookupFamInstEnvConflicts@ uses two-way matching (ie, unification).  This is
needed to check for overlapping instances.

For class instances, these two variants of lookup are combined into one
function (cf, @InstEnv@).  We don't do that for family instances as the
results of matching and unification are used in two different contexts.
Moreover, matching is the wildly more frequently used operation in the case of
indexed synonyms and we don't want to slow that down by needless unification.

\begin{code}
------------------------------------------------------------
-- Might be a one-way match or a unifier
type MatchFun =  FamInst		-- The FamInst template
     	      -> TyVarSet -> [Type]	--   fi_tvs, fi_tys of that FamInst
	      -> [Type]			-- Target to match against
	      -> Maybe TvSubst

type OneSidedMatch = Bool     -- Are optimisations that are only valid for
                              -- one sided matches allowed?

lookup_fam_inst_env 	      -- The worker, local to this module
    :: MatchFun
    -> OneSidedMatch
    -> FamInstEnvs
    -> TyCon -> [Type]		-- What we are looking for
    -> [FamInstMatch] 	        -- Successful matches
lookup_fam_inst_env match_fun one_sided (pkg_ie, home_ie) fam tys
  | not (isFamilyTyCon fam) 
  = []
  | otherwise
  = ASSERT( n_tys >= arity )	-- Family type applications must be saturated
    home_matches ++ pkg_matches
  where
    home_matches = lookup home_ie 
    pkg_matches  = lookup pkg_ie  

    -- See Note [Over-saturated matches]
    arity = tyConArity fam
    n_tys = length tys
    extra_tys = drop arity tys
    (match_tys, add_extra_tys) 
       | arity > n_tys = (take arity tys, \res_tys -> res_tys ++ extra_tys)
       | otherwise     = (tys,            \res_tys -> res_tys)
       	 -- The second case is the common one, hence functional representation

    --------------
    rough_tcs = roughMatchTcs match_tys
    all_tvs   = all isNothing rough_tcs && one_sided

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
      | Just subst <- match_fun item tpl_tvs tpl_tys match_tys
      = (item, add_extra_tys $ substTyVars subst (tyConTyVars tycon)) : find rest

        -- No match => try next
      | otherwise
      = find rest
\end{code}

Note [Over-saturated matches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's ok to look up an over-saturated type constructor.  E.g.
     type family F a :: * -> *
     type instance F (a,b) = Either (a->b)

The type instance gives rise to a newtype TyCon (at a higher kind
which you can't do in Haskell!):
     newtype FPair a b = FP (Either (a->b))

Then looking up (F (Int,Bool) Char) will return a FamInstMatch 
     (FPair, [Int,Bool,Char])

The "extra" type argument [Char] just stays on the end.




%************************************************************************
%*									*
		Looking up a family instance
%*									*
%************************************************************************

\begin{code}
topNormaliseType :: FamInstEnvs
		 -> Type
	   	 -> Maybe (Coercion, Type)

-- Get rid of *outermost* (or toplevel) 
--	* type functions 
--	* newtypes
-- using appropriate coercions.
-- By "outer" we mean that toplevelNormaliseType guarantees to return
-- a type that does not have a reducible redex (F ty1 .. tyn) as its
-- outermost form.  It *can* return something like (Maybe (F ty)), where
-- (F ty) is a redex.

-- Its a bit like Type.repType, but handles type families too

topNormaliseType env ty
  = go [] ty
  where
    go :: [TyCon] -> Type -> Maybe (Coercion, Type)
    go rec_nts ty | Just ty' <- coreView ty 	-- Expand synonyms
	= go rec_nts ty'	

    go rec_nts (TyConApp tc tys)		-- Expand newtypes
	| Just co_con <- newTyConCo_maybe tc	-- See Note [Expanding newtypes]
	= if tc `elem` rec_nts 			--  in Type.lhs
	  then Nothing
	  else let nt_co = mkTyConApp co_con tys
	       in add_co nt_co rec_nts' nt_rhs
	where
	  nt_rhs = newTyConInstRhs tc tys
	  rec_nts' | isRecursiveTyCon tc = tc:rec_nts
		   | otherwise		 = rec_nts

    go rec_nts (TyConApp tc tys)		-- Expand open tycons
	| isFamilyTyCon tc
	, (ACo co, ty) <- normaliseTcApp env tc tys
	= 	-- The ACo says "something happened"
		-- Note that normaliseType fully normalises, but it has do to so
		-- to be sure that 
	   add_co co rec_nts ty

    go _ _ = Nothing

    add_co co rec_nts ty 
	= case go rec_nts ty of
		Nothing 	-> Just (co, ty)
		Just (co', ty') -> Just (mkTransCoercion co co', ty')
	 

---------------
normaliseTcApp :: FamInstEnvs -> TyCon -> [Type] -> (CoercionI, Type)
normaliseTcApp env tc tys
  = let	-- First normalise the arg types so that they'll match 
	-- when we lookup in in the instance envt
	(cois, ntys) = mapAndUnzip (normaliseType env) tys
	tycon_coi    = mkTyConAppCoI tc cois
    in 	-- Now try the top-level redex
    case lookupFamInstEnv env tc ntys of
		-- A matching family instance exists
	[(fam_inst, tys)] -> (fix_coi, nty)
	    where
		rep_tc         = famInstTyCon fam_inst
		co_tycon       = expectJust "lookupFamInst" (tyConFamilyCoercion_maybe rep_tc)
		co	       = mkTyConApp co_tycon tys
		first_coi      = mkTransCoI tycon_coi (ACo co)
		(rest_coi,nty) = normaliseType env (mkTyConApp rep_tc tys)
		fix_coi        = mkTransCoI first_coi rest_coi

		-- No unique matching family instance exists;
		-- we do not do anything
	_ -> (tycon_coi, TyConApp tc ntys)
---------------
normaliseType :: FamInstEnvs 		-- environment with family instances
	      -> Type  			-- old type
	      -> (CoercionI, Type)	-- (coercion,new type), where
					-- co :: old-type ~ new_type
-- Normalise the input type, by eliminating *all* type-function redexes
-- Returns with IdCo if nothing happens

normaliseType env ty 
  | Just ty' <- coreView ty = normaliseType env ty' 
normaliseType env (TyConApp tc tys)
  = normaliseTcApp env tc tys
normaliseType env (AppTy ty1 ty2)
  = let (coi1,nty1) = normaliseType env ty1
        (coi2,nty2) = normaliseType env ty2
    in  (mkAppTyCoI coi1 coi2, AppTy nty1 nty2)
normaliseType env (FunTy ty1 ty2)
  = let (coi1,nty1) = normaliseType env ty1
        (coi2,nty2) = normaliseType env ty2
    in  (mkFunTyCoI coi1 coi2, FunTy nty1 nty2)
normaliseType env (ForAllTy tyvar ty1)
  = let (coi,nty1) = normaliseType env ty1
    in  (mkForAllTyCoI tyvar coi, ForAllTy tyvar nty1)
normaliseType _   ty@(TyVarTy _)
  = (IdCo ty,ty)
normaliseType env (PredTy predty)
  = normalisePred env predty

---------------
normalisePred :: FamInstEnvs -> PredType -> (CoercionI,Type)
normalisePred env (ClassP cls tys)
  =	let (cois,tys') = mapAndUnzip (normaliseType env) tys
	in  (mkClassPPredCoI cls cois, PredTy $ ClassP cls tys')
normalisePred env (IParam ipn ty)
  = 	let (coi,ty') = normaliseType env ty
	in  (mkIParamPredCoI ipn coi, PredTy $ IParam ipn ty')
normalisePred env (EqPred ty1 ty2)
  = 	let (coi1,ty1') = normaliseType env ty1
            (coi2,ty2') = normaliseType env ty2
	in  (mkEqPredCoI coi1 coi2, PredTy $ EqPred ty1' ty2')
\end{code}
