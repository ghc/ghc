%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Utility functions on @Core@ syntax

\begin{code}
module CoreSubst (
	-- * Main data types
	Subst, TvSubstEnv, IdSubstEnv, InScopeSet,

        -- ** Substituting into expressions and related types
	deShadowBinds,
	substTy, substExpr, substSpec, substWorker,
	lookupIdSubst, lookupTvSubst, 

        -- ** Operations on substitutions
	emptySubst, mkEmptySubst, mkSubst, substInScope, isEmptySubst, 
 	extendIdSubst, extendIdSubstList, extendTvSubst, extendTvSubstList,
	extendSubst, extendSubstList, zapSubstEnv,
	extendInScope, extendInScopeList, extendInScopeIds, 
	isInScope,

	-- ** Substituting and cloning binders
	substBndr, substBndrs, substRecBndrs,
	cloneIdBndr, cloneIdBndrs, cloneRecIdBndrs
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreUtils

import qualified Type
import Type     ( Type, TvSubst(..), TvSubstEnv )
import VarSet
import VarEnv
import Id
import Var      ( Var, TyVar, setVarUnique )
import IdInfo
import Unique
import UniqSupply
import Maybes
import Outputable
import PprCore		()		-- Instances
import FastString

import Data.List
\end{code}


%************************************************************************
%*									*
\subsection{Substitutions}
%*									*
%************************************************************************

\begin{code}
-- | A substitution environment, containing both 'Id' and 'TyVar' substitutions.
--
-- Some invariants apply to how you use the substitution:
--
-- 1. #in_scope_invariant# The in-scope set contains at least those 'Id's and 'TyVar's that will be in scope /after/
-- applying the substitution to a term. Precisely, the in-scope set must be a superset of the free vars of the
-- substitution range that might possibly clash with locally-bound variables in the thing being substituted in.
--
-- 2. #apply_once# You may apply the substitution only /once/
--
-- There are various ways of setting up the in-scope set such that the first of these invariants hold:
--
-- * Arrange that the in-scope set really is all the things in scope
--
-- * Arrange that it's the free vars of the range of the substitution
--
-- * Make it empty, if you know that all the free vars of the substitution are fresh, and hence can't possibly clash
data Subst 
  = Subst InScopeSet  -- Variables in in scope (both Ids and TyVars) /after/
                      -- applying the substitution
          IdSubstEnv  -- Substitution for Ids
          TvSubstEnv  -- Substitution for TyVars

	-- INVARIANT 1: See #in_scope_invariant#
	-- This is what lets us deal with name capture properly
	-- It's a hard invariant to check...
	--
	-- INVARIANT 2: The substitution is apply-once; see Note [Apply once] with
	--		Types.TvSubstEnv
	--
	-- INVARIANT 3: See Note [Extending the Subst]
\end{code}

Note [Extending the Subst]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For a core Subst, which binds Ids as well, we make a different choice for Ids
than we do for TyVars.  

For TyVars, see Note [Extending the TvSubst] with Type.TvSubstEnv

For Ids, we have a different invariant
	The IdSubstEnv is extended *only* when the Unique on an Id changes
	Otherwise, we just extend the InScopeSet

In consequence:

* In substIdBndr, we extend the IdSubstEnv only when the unique changes

* If the TvSubstEnv and IdSubstEnv are both empty, substExpr does nothing
  (Note that the above rule for substIdBndr maintains this property.  If
   the incoming envts are both empty, then substituting the type and
   IdInfo can't change anything.)

* In lookupIdSubst, we *must* look up the Id in the in-scope set, because
  it may contain non-trivial changes.  Example:
	(/\a. \x:a. ...x...) Int
  We extend the TvSubstEnv with [a |-> Int]; but x's unique does not change
  so we only extend the in-scope set.  Then we must look up in the in-scope
  set when we find the occurrence of x.

* The requirement to look up the Id in the in-scope set means that we
  must NOT take no-op short cut in the case the substitution is empty.
  We must still look up every Id in the in-scope set.

* (However, we don't need to do so for expressions found in the IdSubst
  itself, whose range is assumed to be correct wrt the in-scope set.)

Why do we make a different choice for the IdSubstEnv than the TvSubstEnv?

* For Ids, we change the IdInfo all the time (e.g. deleting the
  unfolding), and adding it back later, so using the TyVar convention
  would entail extending the substitution almost all the time

* The simplifier wants to look up in the in-scope set anyway, in case it 
  can see a better unfolding from an enclosing case expression

* For TyVars, only coercion variables can possibly change, and they are 
  easy to spot

\begin{code}
-- | An environment for substituting for 'Id's
type IdSubstEnv = IdEnv CoreExpr

----------------------------
isEmptySubst :: Subst -> Bool
isEmptySubst (Subst _ id_env tv_env) = isEmptyVarEnv id_env && isEmptyVarEnv tv_env

emptySubst :: Subst
emptySubst = Subst emptyInScopeSet emptyVarEnv emptyVarEnv

mkEmptySubst :: InScopeSet -> Subst
mkEmptySubst in_scope = Subst in_scope emptyVarEnv emptyVarEnv

mkSubst :: InScopeSet -> TvSubstEnv -> IdSubstEnv -> Subst
mkSubst in_scope tvs ids = Subst in_scope ids tvs

-- getTvSubst :: Subst -> TvSubst
-- getTvSubst (Subst in_scope _ tv_env) = TvSubst in_scope tv_env

-- getTvSubstEnv :: Subst -> TvSubstEnv
-- getTvSubstEnv (Subst _ _ tv_env) = tv_env
-- 
-- setTvSubstEnv :: Subst -> TvSubstEnv -> Subst
-- setTvSubstEnv (Subst in_scope ids _) tvs = Subst in_scope ids tvs

-- | Find the in-scope set: see "CoreSubst#in_scope_invariant"
substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _ _) = in_scope

-- | Remove all substitutions for 'Id's and 'Var's that might have been built up
-- while preserving the in-scope set
zapSubstEnv :: Subst -> Subst
zapSubstEnv (Subst in_scope _ _) = Subst in_scope emptyVarEnv emptyVarEnv

-- | Add a substitution for an 'Id' to the 'Subst': you must ensure that the in-scope set is
-- such that the "CoreSubst#in_scope_invariant" is true after extending the substitution like this
extendIdSubst :: Subst -> Id -> CoreExpr -> Subst
-- ToDo: add an ASSERT that fvs(subst-result) is already in the in-scope set
extendIdSubst (Subst in_scope ids tvs) v r = Subst in_scope (extendVarEnv ids v r) tvs

-- | Adds multiple 'Id' substitutions to the 'Subst': see also 'extendIdSubst'
extendIdSubstList :: Subst -> [(Id, CoreExpr)] -> Subst
extendIdSubstList (Subst in_scope ids tvs) prs = Subst in_scope (extendVarEnvList ids prs) tvs

-- | Add a substitution for a 'TyVar' to the 'Subst': you must ensure that the in-scope set is
-- such that the "CoreSubst#in_scope_invariant" is true after extending the substitution like this
extendTvSubst :: Subst -> TyVar -> Type -> Subst
extendTvSubst (Subst in_scope ids tvs) v r = Subst in_scope ids (extendVarEnv tvs v r) 

-- | Adds multiple 'TyVar' substitutions to the 'Subst': see also 'extendTvSubst'
extendTvSubstList :: Subst -> [(TyVar,Type)] -> Subst
extendTvSubstList (Subst in_scope ids tvs) prs = Subst in_scope ids (extendVarEnvList tvs prs)

-- | Add a substitution for a 'TyVar' or 'Id' as appropriate to the 'Var' being added. See also
-- 'extendIdSubst' and 'extendTvSubst'
extendSubst :: Subst -> Var -> CoreArg -> Subst
extendSubst (Subst in_scope ids tvs) tv (Type ty)
  = ASSERT( isTyVar tv ) Subst in_scope ids (extendVarEnv tvs tv ty)
extendSubst (Subst in_scope ids tvs) id expr
  = ASSERT( isId id ) Subst in_scope (extendVarEnv ids id expr) tvs

-- | Add a substitution for a 'TyVar' or 'Id' as appropriate to all the 'Var's being added. See also 'extendSubst'
extendSubstList :: Subst -> [(Var,CoreArg)] -> Subst
extendSubstList subst []	      = subst
extendSubstList subst ((var,rhs):prs) = extendSubstList (extendSubst subst var rhs) prs

-- | Find the substitution for an 'Id' in the 'Subst'
lookupIdSubst :: Subst -> Id -> CoreExpr
lookupIdSubst (Subst in_scope ids _) v
  | not (isLocalId v) = Var v
  | Just e  <- lookupVarEnv ids       v = e
  | Just v' <- lookupInScope in_scope v = Var v'
	-- Vital! See Note [Extending the Subst]
  | otherwise = WARN( True, ptext (sLit "CoreSubst.lookupIdSubst") <+> ppr v ) 
		Var v

-- | Find the substitution for a 'TyVar' in the 'Subst'
lookupTvSubst :: Subst -> TyVar -> Type
lookupTvSubst (Subst _ _ tvs) v = lookupVarEnv tvs v `orElse` Type.mkTyVarTy v

------------------------------
isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _ _) = v `elemInScopeSet` in_scope

-- | Add the 'Var' to the in-scope set: as a side effect, removes any existing substitutions for it
extendInScope :: Subst -> Var -> Subst
extendInScope (Subst in_scope ids tvs) v
  = Subst (in_scope `extendInScopeSet` v) 
	  (ids `delVarEnv` v) (tvs `delVarEnv` v)

-- | Add the 'Var's to the in-scope set: see also 'extendInScope'
extendInScopeList :: Subst -> [Var] -> Subst
extendInScopeList (Subst in_scope ids tvs) vs
  = Subst (in_scope `extendInScopeSetList` vs) 
	  (ids `delVarEnvList` vs) (tvs `delVarEnvList` vs)

-- | Optimized version of 'extendInScopeList' that can be used if you are certain 
-- all the things being added are 'Id's and hence none are 'TyVar's
extendInScopeIds :: Subst -> [Id] -> Subst
extendInScopeIds (Subst in_scope ids tvs) vs 
  = Subst (in_scope `extendInScopeSetList` vs) 
	  (ids `delVarEnvList` vs) tvs
\end{code}

Pretty printing, for debugging only

\begin{code}
instance Outputable Subst where
  ppr (Subst in_scope ids tvs) 
	=  ptext (sLit "<InScope =") <+> braces (fsep (map ppr (varEnvElts (getInScopeVars in_scope))))
	$$ ptext (sLit " IdSubst   =") <+> ppr ids
	$$ ptext (sLit " TvSubst   =") <+> ppr tvs
 	 <> char '>'
\end{code}


%************************************************************************
%*									*
	Substituting expressions
%*									*
%************************************************************************

\begin{code}
-- | Apply a substititon to an entire 'CoreExpr'. Rememeber, you may only 
-- apply the substitution /once/: see "CoreSubst#apply_once"
--
-- Do *not* attempt to short-cut in the case of an empty substitution!
-- See Note [Extending the Subst]
substExpr :: Subst -> CoreExpr -> CoreExpr
substExpr subst expr
  = go expr
  where
    go (Var v)	       = lookupIdSubst subst v 
    go (Type ty)       = Type (substTy subst ty)
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Note note e)   = Note (go_note note) (go e)
    go (Cast e co)     = Cast (go e) (substTy subst co)
    go (Lam bndr body) = Lam bndr' (substExpr subst' body)
		       where
			 (subst', bndr') = substBndr subst bndr

    go (Let bind body) = Let bind' (substExpr subst' body)
		       where
			 (subst', bind') = substBind subst bind

    go (Case scrut bndr ty alts) = Case (go scrut) bndr' (substTy subst ty) (map (go_alt subst') alts)
			         where
			  	 (subst', bndr') = substBndr subst bndr

    go_alt subst (con, bndrs, rhs) = (con, bndrs', substExpr subst' rhs)
				 where
				   (subst', bndrs') = substBndrs subst bndrs

    go_note note	     = note

-- | Apply a substititon to an entire 'CoreBind', additionally returning an updated 'Subst'
-- that should be used by subsequent substitutons.
substBind :: Subst -> CoreBind -> (Subst, CoreBind)
substBind subst (NonRec bndr rhs) = (subst', NonRec bndr' (substExpr subst rhs))
				  where
				    (subst', bndr') = substBndr subst bndr

substBind subst (Rec pairs) = (subst', Rec pairs')
			    where
				(subst', bndrs') = substRecBndrs subst (map fst pairs)
				pairs'	= bndrs' `zip` rhss'
				rhss'	= map (substExpr subst' . snd) pairs
\end{code}

\begin{code}
-- | De-shadowing the program is sometimes a useful pre-pass. It can be done simply
-- by running over the bindings with an empty substitution, becuase substitution
-- returns a result that has no-shadowing guaranteed.
--
-- (Actually, within a single /type/ there might still be shadowing, because 
-- 'substTy' is a no-op for the empty substitution, but that's probably OK.)
deShadowBinds :: [CoreBind] -> [CoreBind]
deShadowBinds binds = snd (mapAccumL substBind emptySubst binds)
\end{code}


%************************************************************************
%*									*
	Substituting binders
%*									*
%************************************************************************

Remember that substBndr and friends are used when doing expression
substitution only.  Their only business is substitution, so they
preserve all IdInfo (suitably substituted).  For example, we *want* to
preserve occ info in rules.

\begin{code}
-- | Substitutes a 'Var' for another one according to the 'Subst' given, returning
-- the result and an updated 'Subst' that should be used by subsequent substitutons.
-- 'IdInfo' is preserved by this process, although it is substituted into appropriately.
substBndr :: Subst -> Var -> (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = substTyVarBndr subst bndr
  | otherwise     = substIdBndr subst subst bndr

-- | Applies 'substBndr' to a number of 'Var's, accumulating a new 'Subst' left-to-right
substBndrs :: Subst -> [Var] -> (Subst, [Var])
substBndrs subst bndrs = mapAccumL substBndr subst bndrs

-- | Substitute in a mutually recursive group of 'Id's
substRecBndrs :: Subst -> [Id] -> (Subst, [Id])
substRecBndrs subst bndrs 
  = (new_subst, new_bndrs)
  where		-- Here's the reason we need to pass rec_subst to subst_id
    (new_subst, new_bndrs) = mapAccumL (substIdBndr new_subst) subst bndrs
\end{code}


\begin{code}
substIdBndr :: Subst		-- ^ Substitution to use for the IdInfo
	    -> Subst -> Id 	-- ^ Substitition and Id to transform
	    -> (Subst, Id)	-- ^ Transformed pair
				-- NB: unfolding may be zapped

substIdBndr rec_subst subst@(Subst in_scope env tvs) old_id
  = (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
  where
    id1 = uniqAway in_scope old_id	-- id1 is cloned if necessary
    id2 | no_type_change = id1
	| otherwise	 = setIdType id1 (substTy subst old_ty)

    old_ty = idType old_id
    no_type_change = isEmptyVarEnv tvs || 
                     isEmptyVarSet (Type.tyVarsOfType old_ty)

	-- new_id has the right IdInfo
	-- The lazy-set is because we're in a loop here, with 
	-- rec_subst, when dealing with a mutually-recursive group
    new_id = maybeModifyIdInfo mb_new_info id2
    mb_new_info = substIdInfo rec_subst id2 (idInfo id2)
	-- NB: unfolding info may be zapped

	-- Extend the substitution if the unique has changed
	-- See the notes with substTyVarBndr for the delVarEnv
    new_env | no_change = delVarEnv env old_id
	    | otherwise = extendVarEnv env old_id (Var new_id)

    no_change = id1 == old_id
	-- See Note [Extending the Subst]
	-- it's /not/ necessary to check mb_new_info and no_type_change
\end{code}

Now a variant that unconditionally allocates a new unique.
It also unconditionally zaps the OccInfo.

\begin{code}
-- | Very similar to 'substBndr', but it always allocates a new 'Unique' for
-- each variable in its output and removes all 'IdInfo'
cloneIdBndr :: Subst -> UniqSupply -> Id -> (Subst, Id)
cloneIdBndr subst us old_id
  = clone_id subst subst (old_id, uniqFromSupply us)

-- | Applies 'cloneIdBndr' to a number of 'Id's, accumulating a final
-- substitution from left to right
cloneIdBndrs :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
cloneIdBndrs subst us ids
  = mapAccumL (clone_id subst) subst (ids `zip` uniqsFromSupply us)

-- | Clone a mutually recursive group of 'Id's
cloneRecIdBndrs :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
cloneRecIdBndrs subst us ids
  = (subst', ids')
  where
    (subst', ids') = mapAccumL (clone_id subst') subst
			       (ids `zip` uniqsFromSupply us)

-- Just like substIdBndr, except that it always makes a new unique
-- It is given the unique to use
clone_id    :: Subst			-- Substitution for the IdInfo
	    -> Subst -> (Id, Unique)	-- Substitition and Id to transform
	    -> (Subst, Id)		-- Transformed pair

clone_id rec_subst subst@(Subst in_scope env tvs) (old_id, uniq)
  = (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
  where
    id1	    = setVarUnique old_id uniq
    id2     = substIdType subst id1
    new_id  = maybeModifyIdInfo (substIdInfo rec_subst id2 (idInfo old_id)) id2
    new_env = extendVarEnv env old_id (Var new_id)
\end{code}


%************************************************************************
%*									*
		Types
%*									*
%************************************************************************

For types we just call the corresponding function in Type, but we have
to repackage the substitution, from a Subst to a TvSubst

\begin{code}
substTyVarBndr :: Subst -> TyVar -> (Subst, TyVar)
substTyVarBndr (Subst in_scope id_env tv_env) tv
  = case Type.substTyVarBndr (TvSubst in_scope tv_env) tv of
	(TvSubst in_scope' tv_env', tv') 
	   -> (Subst in_scope' id_env tv_env', tv')

-- | See 'Type.substTy'
substTy :: Subst -> Type -> Type 
substTy (Subst in_scope _id_env tv_env) ty
  = Type.substTy (TvSubst in_scope tv_env) ty
\end{code}


%************************************************************************
%*									*
\section{IdInfo substitution}
%*									*
%************************************************************************

\begin{code}
substIdType :: Subst -> Id -> Id
substIdType subst@(Subst _ _ tv_env) id
  | isEmptyVarEnv tv_env || isEmptyVarSet (Type.tyVarsOfType old_ty) = id
  | otherwise	= setIdType id (substTy subst old_ty)
		-- The tyVarsOfType is cheaper than it looks
		-- because we cache the free tyvars of the type
		-- in a Note in the id's type itself
  where
    old_ty = idType id

------------------
-- | Substitute into some 'IdInfo' with regard to the supplied new 'Id'.
-- Always zaps the unfolding, to save substitution work
substIdInfo :: Subst -> Id -> IdInfo -> Maybe IdInfo
substIdInfo subst new_id info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setSpecInfo`   	  substSpec subst new_id old_rules
			       `setWorkerInfo` 	  substWorker subst old_wrkr
			       `setUnfoldingInfo` noUnfolding)
  where
    old_rules 	  = specInfo info
    old_wrkr  	  = workerInfo info
    nothing_to_do = isEmptySpecInfo old_rules &&
		    not (workerExists old_wrkr) &&
		    not (hasUnfolding (unfoldingInfo info))
    

------------------
-- | Substitutes for the 'Id's within the 'WorkerInfo'
substWorker :: Subst -> WorkerInfo -> WorkerInfo
	-- Seq'ing on the returned WorkerInfo is enough to cause all the 
	-- substitutions to happen completely

substWorker _ NoWorker
  = NoWorker
substWorker subst (HasWorker w a)
  = case lookupIdSubst subst w of
	Var w1 -> HasWorker w1 a
	other  -> WARN( not (exprIsTrivial other), text "CoreSubst.substWorker:" <+> ppr w )
		  NoWorker	-- Worker has got substituted away altogether
				-- (This can happen if it's trivial, 
				--  via postInlineUnconditionally, hence warning)

------------------
-- | Substitutes for the 'Id's within the 'WorkerInfo' given the new function 'Id'
substSpec :: Subst -> Id -> SpecInfo -> SpecInfo
substSpec subst new_fn (SpecInfo rules rhs_fvs)
  = seqSpecInfo new_rules `seq` new_rules
  where
    new_name = idName new_fn
    new_rules = SpecInfo (map do_subst rules) (substVarSet subst rhs_fvs)

    do_subst rule@(BuiltinRule {}) = rule
    do_subst rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs })
	= rule { ru_bndrs = bndrs', 
		 ru_fn = new_name, 	-- Important: the function may have changed its name!
		 ru_args  = map (substExpr subst') args,
		 ru_rhs   = substExpr subst' rhs }
	where
	  (subst', bndrs') = substBndrs subst bndrs

------------------
substVarSet :: Subst -> VarSet -> VarSet
substVarSet subst fvs 
  = foldVarSet (unionVarSet . subst_fv subst) emptyVarSet fvs
  where
    subst_fv subst fv 
	| isId fv   = exprFreeVars (lookupIdSubst subst fv)
	| otherwise = Type.tyVarsOfType (lookupTvSubst subst fv)
\end{code}
