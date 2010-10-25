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
	deShadowBinds, substSpec, substRulesForImportedIds,
	substTy, substExpr, substExprSC, substBind, substBindSC,
        substUnfolding, substUnfoldingSC,
	substUnfoldingSource, lookupIdSubst, lookupTvSubst, substIdOcc,

        -- ** Operations on substitutions
	emptySubst, mkEmptySubst, mkSubst, mkOpenSubst, substInScope, isEmptySubst, 
 	extendIdSubst, extendIdSubstList, extendTvSubst, extendTvSubstList,
	extendSubst, extendSubstList, zapSubstEnv,
	extendInScope, extendInScopeList, extendInScopeIds, 
	isInScope,

	-- ** Substituting and cloning binders
	substBndr, substBndrs, substRecBndrs,
	cloneIdBndr, cloneIdBndrs, cloneRecIdBndrs,

	-- ** Simple expression optimiser
	simpleOptPgm, simpleOptExpr
    ) where

#include "HsVersions.h"

import CoreSyn
import CoreFVs
import CoreUtils
import PprCore
import OccurAnal( occurAnalyseExpr, occurAnalysePgm )

import qualified Type
import Type     ( Type, TvSubst(..), TvSubstEnv )
import Coercion	   ( isIdentityCoercion )
import OptCoercion ( optCoercion )
import VarSet
import VarEnv
import Id
import Name	( Name )
import Var      ( Var, TyVar, setVarUnique )
import IdInfo
import Unique
import UniqSupply
import Maybes
import ErrUtils
import DynFlags   ( DynFlags, DynFlag(..) )
import BasicTypes ( isAlwaysActive )
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

* If the TvSubstEnv and IdSubstEnv are both empty, substExpr would be a
  no-op, so substExprSC ("short cut") does nothing.

  However, substExpr still goes ahead and substitutes.  Reason: we may
  want to replace existing Ids with new ones from the in-scope set, to
  avoid space leaks.

* In substIdBndr, we extend the IdSubstEnv only when the unique changes

* In lookupIdSubst, we *must* look up the Id in the in-scope set, because
  it may contain non-trivial changes.  Example:
	(/\a. \x:a. ...x...) Int
  We extend the TvSubstEnv with [a |-> Int]; but x's unique does not change
  so we only extend the in-scope set.  Then we must look up in the in-scope
  set when we find the occurrence of x.

* The requirement to look up the Id in the in-scope set means that we
  must NOT take no-op short cut when the IdSubst is empty.
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
  = ASSERT( isTyCoVar tv ) Subst in_scope ids (extendVarEnv tvs tv ty)
extendSubst (Subst in_scope ids tvs) id expr
  = ASSERT( isId id ) Subst in_scope (extendVarEnv ids id expr) tvs

-- | Add a substitution for a 'TyVar' or 'Id' as appropriate to all the 'Var's being added. See also 'extendSubst'
extendSubstList :: Subst -> [(Var,CoreArg)] -> Subst
extendSubstList subst []	      = subst
extendSubstList subst ((var,rhs):prs) = extendSubstList (extendSubst subst var rhs) prs

-- | Find the substitution for an 'Id' in the 'Subst'
lookupIdSubst :: SDoc -> Subst -> Id -> CoreExpr
lookupIdSubst doc (Subst in_scope ids _) v
  | not (isLocalId v) = Var v
  | Just e  <- lookupVarEnv ids       v = e
  | Just v' <- lookupInScope in_scope v = Var v'
	-- Vital! See Note [Extending the Subst]
  | otherwise = WARN( True, ptext (sLit "CoreSubst.lookupIdSubst") <+> ppr v $$ ppr in_scope $$ doc) 
		Var v

-- | Find the substitution for a 'TyVar' in the 'Subst'
lookupTvSubst :: Subst -> TyVar -> Type
lookupTvSubst (Subst _ _ tvs) v = lookupVarEnv tvs v `orElse` Type.mkTyVarTy v

-- | Simultaneously substitute for a bunch of variables
--   No left-right shadowing
--   ie the substitution for   (\x \y. e) a1 a2
--      so neither x nor y scope over a1 a2
mkOpenSubst :: InScopeSet -> [(Var,CoreArg)] -> Subst
mkOpenSubst in_scope pairs = Subst in_scope
	    	          	   (mkVarEnv [(id,e)  | (id, e) <- pairs, isId id])
			  	   (mkVarEnv [(tv,ty) | (tv, Type ty) <- pairs])

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
substExprSC :: SDoc -> Subst -> CoreExpr -> CoreExpr
substExprSC _doc subst orig_expr
  | isEmptySubst subst = orig_expr
  | otherwise          = -- pprTrace "enter subst-expr" (doc $$ ppr orig_expr) $
                         subst_expr subst orig_expr

substExpr :: SDoc -> Subst -> CoreExpr -> CoreExpr
substExpr _doc subst orig_expr = subst_expr subst orig_expr

subst_expr :: Subst -> CoreExpr -> CoreExpr
subst_expr subst expr
  = go expr
  where
    go (Var v)	       = lookupIdSubst (text "subst_expr") subst v 
    go (Type ty)       = Type (substTy subst ty)
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Note note e)   = Note (go_note note) (go e)
    go (Cast e co) 
      | isIdentityCoercion co' = go e
      | otherwise              = Cast (go e) co'
      where
        co' = optCoercion (getTvSubst subst) co
	-- Optimise coercions as we go; this is good, for example
	-- in the RHS of rules, which are only substituted in

    go (Lam bndr body) = Lam bndr' (subst_expr subst' body)
		       where
			 (subst', bndr') = substBndr subst bndr

    go (Let bind body) = Let bind' (subst_expr subst' body)
		       where
			 (subst', bind') = substBind subst bind

    go (Case scrut bndr ty alts) = Case (go scrut) bndr' (substTy subst ty) (map (go_alt subst') alts)
			         where
			  	 (subst', bndr') = substBndr subst bndr

    go_alt subst (con, bndrs, rhs) = (con, bndrs', subst_expr subst' rhs)
				 where
				   (subst', bndrs') = substBndrs subst bndrs

    go_note note	     = note

-- | Apply a substititon to an entire 'CoreBind', additionally returning an updated 'Subst'
-- that should be used by subsequent substitutons.
substBind, substBindSC :: Subst -> CoreBind -> (Subst, CoreBind)

substBindSC subst bind 	  -- Short-cut if the substitution is empty
  | not (isEmptySubst subst)
  = substBind subst bind
  | otherwise
  = case bind of
       NonRec bndr rhs -> (subst', NonRec bndr' rhs)
          where
      	    (subst', bndr') = substBndr subst bndr
       Rec pairs -> (subst', Rec (bndrs' `zip` rhss'))
          where
            (bndrs, rhss)    = unzip pairs
      	    (subst', bndrs') = substRecBndrs subst bndrs
      	    rhss' | isEmptySubst subst' = rhss
                  | otherwise	        = map (subst_expr subst') rhss

substBind subst (NonRec bndr rhs) = (subst', NonRec bndr' (subst_expr subst rhs))
				  where
				    (subst', bndr') = substBndr subst bndr

substBind subst (Rec pairs) = (subst', Rec (bndrs' `zip` rhss'))
			    where
                                (bndrs, rhss)    = unzip pairs
				(subst', bndrs') = substRecBndrs subst bndrs
				rhss' = map (subst_expr subst') rhss
\end{code}

\begin{code}
-- | De-shadowing the program is sometimes a useful pre-pass. It can be done simply
-- by running over the bindings with an empty substitution, becuase substitution
-- returns a result that has no-shadowing guaranteed.
--
-- (Actually, within a single /type/ there might still be shadowing, because 
-- 'substTy' is a no-op for the empty substitution, but that's probably OK.)
--
-- [Aug 09] This function is not used in GHC at the moment, but seems so 
--          short and simple that I'm going to leave it here
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
  | isTyCoVar bndr  = substTyVarBndr subst bndr
  | otherwise       = substIdBndr (text "var-bndr") subst subst bndr

-- | Applies 'substBndr' to a number of 'Var's, accumulating a new 'Subst' left-to-right
substBndrs :: Subst -> [Var] -> (Subst, [Var])
substBndrs subst bndrs = mapAccumL substBndr subst bndrs

-- | Substitute in a mutually recursive group of 'Id's
substRecBndrs :: Subst -> [Id] -> (Subst, [Id])
substRecBndrs subst bndrs 
  = (new_subst, new_bndrs)
  where		-- Here's the reason we need to pass rec_subst to subst_id
    (new_subst, new_bndrs) = mapAccumL (substIdBndr (text "rec-bndr") new_subst) subst bndrs
\end{code}


\begin{code}
substIdBndr :: SDoc 
            -> Subst		-- ^ Substitution to use for the IdInfo
	    -> Subst -> Id 	-- ^ Substitition and Id to transform
	    -> (Subst, Id)	-- ^ Transformed pair
				-- NB: unfolding may be zapped

substIdBndr _doc rec_subst subst@(Subst in_scope env tvs) old_id
  = -- pprTrace "substIdBndr" (doc $$ ppr old_id $$ ppr in_scope) $
    (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
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
-- each variable in its output.  It substitutes the IdInfo though.
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
substTy subst ty = Type.substTy (getTvSubst subst) ty

getTvSubst :: Subst -> TvSubst
getTvSubst (Subst in_scope _id_env tv_env) = TvSubst in_scope tv_env
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
substIdInfo :: Subst -> Id -> IdInfo -> Maybe IdInfo
substIdInfo subst new_id info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setSpecInfo`   	  substSpec subst new_id old_rules
			       `setUnfoldingInfo` substUnfolding subst old_unf)
  where
    old_rules 	  = specInfo info
    old_unf	  = unfoldingInfo info
    nothing_to_do = isEmptySpecInfo old_rules && isClosedUnfolding old_unf
    

------------------
-- | Substitutes for the 'Id's within an unfolding
substUnfolding, substUnfoldingSC :: Subst -> Unfolding -> Unfolding
	-- Seq'ing on the returned Unfolding is enough to cause
	-- all the substitutions to happen completely

substUnfoldingSC subst unf 	 -- Short-cut version
  | isEmptySubst subst = unf
  | otherwise          = substUnfolding subst unf

substUnfolding subst (DFunUnfolding ar con args)
  = DFunUnfolding ar con (map (substExpr (text "dfun-unf") subst) args)

substUnfolding subst unf@(CoreUnfolding { uf_tmpl = tmpl, uf_src = src })
	-- Retain an InlineRule!
  | not (isStableSource src)  -- Zap an unstable unfolding, to save substitution work
  = NoUnfolding
  | otherwise                 -- But keep a stable one!
  = seqExpr new_tmpl `seq` 
    new_src `seq`
    unf { uf_tmpl = new_tmpl, uf_src = new_src }
  where
    new_tmpl = substExpr (text "subst-unf") subst tmpl
    new_src  = substUnfoldingSource subst src

substUnfolding _ unf = unf	-- NoUnfolding, OtherCon

-------------------
substUnfoldingSource :: Subst -> UnfoldingSource -> UnfoldingSource
substUnfoldingSource (Subst in_scope ids _) (InlineWrapper wkr)
  | Just wkr_expr <- lookupVarEnv ids wkr 
  = case wkr_expr of
      Var w1 -> InlineWrapper w1
      _other -> -- WARN( True, text "Interesting! CoreSubst.substWorker1:" <+> ppr wkr 
                --             <+> ifPprDebug (equals <+> ppr wkr_expr) )   
			      -- Note [Worker inlining]
                InlineStable  -- It's not a wrapper any more, but still inline it!

  | Just w1  <- lookupInScope in_scope wkr = InlineWrapper w1
  | otherwise = -- WARN( True, text "Interesting! CoreSubst.substWorker2:" <+> ppr wkr )
    	      	-- This can legitimately happen.  The worker has been inlined and
		-- dropped as dead code, because we don't treat the UnfoldingSource
		-- as an "occurrence".
                -- Note [Worker inlining]
      	        InlineStable

substUnfoldingSource _ src = src

------------------
substIdOcc :: Subst -> Id -> Id
-- These Ids should not be substituted to non-Ids
substIdOcc subst v = case lookupIdSubst (text "substIdOcc") subst v of
	   	        Var v' -> v'
			other  -> pprPanic "substIdOcc" (vcat [ppr v <+> ppr other, ppr subst])

------------------
-- | Substitutes for the 'Id's within the 'WorkerInfo' given the new function 'Id'
substSpec :: Subst -> Id -> SpecInfo -> SpecInfo
substSpec subst new_id (SpecInfo rules rhs_fvs)
  = seqSpecInfo new_spec `seq` new_spec
  where
    subst_ru_fn = const (idName new_id)
    new_spec = SpecInfo (map (substRule subst subst_ru_fn) rules)
                         (substVarSet subst rhs_fvs)

------------------
substRulesForImportedIds :: Subst -> [CoreRule] -> [CoreRule]
substRulesForImportedIds subst rules 
  = map (substRule subst not_needed) rules
  where
    not_needed name = pprPanic "substRulesForImportedIds" (ppr name)

------------------
substRule :: Subst -> (Name -> Name) -> CoreRule -> CoreRule

-- The subst_ru_fn argument is applied to substitute the ru_fn field
-- of the rule:
--    - Rules for *imported* Ids never change ru_fn
--    - Rules for *local* Ids are in the IdInfo for that Id,
--      and the ru_fn field is simply replaced by the new name 
--	of the Id

substRule _ _ rule@(BuiltinRule {}) = rule
substRule subst subst_ru_fn rule@(Rule { ru_bndrs = bndrs, ru_args = args
                                       , ru_fn = fn_name, ru_rhs = rhs
                                       , ru_local = is_local })
  = rule { ru_bndrs = bndrs', 
	   ru_fn    = if is_local 
                      	then subst_ru_fn fn_name 
                      	else fn_name,
	   ru_args  = map (substExpr (text "subst-rule" <+> ppr fn_name) subst') args,
	   ru_rhs   = substExpr (text "subst-rule" <+> ppr fn_name) subst' rhs }
  where
    (subst', bndrs') = substBndrs subst bndrs

------------------
substVarSet :: Subst -> VarSet -> VarSet
substVarSet subst fvs 
  = foldVarSet (unionVarSet . subst_fv subst) emptyVarSet fvs
  where
    subst_fv subst fv 
	| isId fv   = exprFreeVars (lookupIdSubst (text "substVarSet") subst fv)
	| otherwise = Type.tyVarsOfType (lookupTvSubst subst fv)
\end{code}

Note [Worker inlining]
~~~~~~~~~~~~~~~~~~~~~~
A worker can get sustituted away entirely.
	- it might be trivial
	- it might simply be very small
We do not treat an InlWrapper as an 'occurrence' in the occurence 
analyser, so it's possible that the worker is not even in scope any more.

In all all these cases we simply drop the special case, returning to
InlVanilla.  The WARN is just so I can see if it happens a lot.


%************************************************************************
%*									*
	The Very Simple Optimiser
%*									*
%************************************************************************

\begin{code}
simpleOptExpr :: CoreExpr -> CoreExpr
-- Do simple optimisation on an expression
-- The optimisation is very straightforward: just
-- inline non-recursive bindings that are used only once, 
-- or where the RHS is trivial
--
-- The result is NOT guaranteed occurence-analysed, becuase
-- in  (let x = y in ....) we substitute for x; so y's occ-info
-- may change radically

simpleOptExpr expr
  = -- pprTrace "simpleOptExpr" (ppr init_subst $$ ppr expr)
    simple_opt_expr init_subst (occurAnalyseExpr expr)
  where
    init_subst = mkEmptySubst (mkInScopeSet (exprFreeVars expr))
	-- It's potentially important to make a proper in-scope set
	-- Consider  let x = ..y.. in \y. ...x...
	-- Then we should remember to clone y before substituting
	-- for x.  It's very unlikely to occur, because we probably
	-- won't *be* substituting for x if it occurs inside a
	-- lambda.  
	--
	-- It's a bit painful to call exprFreeVars, because it makes
	-- three passes instead of two (occ-anal, and go)

----------------------
simpleOptPgm :: DynFlags -> [CoreBind] -> [CoreRule] -> IO ([CoreBind], [CoreRule])
simpleOptPgm dflags binds rules
  = do { dumpIfSet_dyn dflags Opt_D_dump_occur_anal "Occurrence analysis"
		       (pprCoreBindings occ_anald_binds);

       ; return (reverse binds', substRulesForImportedIds subst' rules) }
  where
    occ_anald_binds  = occurAnalysePgm binds rules
    (subst', binds') = foldl do_one (emptySubst, []) occ_anald_binds
                       
    do_one (subst, binds') bind 
      = case simple_opt_bind subst bind of
          (subst', Nothing)    -> (subst', binds')
          (subst', Just bind') -> (subst', bind':binds')

----------------------
type InVar   = Var
type OutVar  = Var
type InId    = Id
type OutId   = Id
type InExpr  = CoreExpr
type OutExpr = CoreExpr

-- In these functions the substitution maps InVar -> OutExpr

----------------------
simple_opt_expr :: Subst -> InExpr -> OutExpr
simple_opt_expr subst expr
  = go expr
  where
    go (Var v)          = lookupIdSubst (text "simpleOptExpr") subst v
    go (App e1 e2)      = simple_app subst e1 [go e2]
    go (Type ty)        = Type (substTy subst ty)
    go (Lit lit)        = Lit lit
    go (Note note e)    = Note note (go e)
    go (Cast e co)      | isIdentityCoercion co' = go e
       	                | otherwise              = Cast (go e) co' 
                        where
                          co' = substTy subst co

    go (Let bind body) = case simple_opt_bind subst bind of
                           (subst', Nothing)   -> simple_opt_expr subst' body
                           (subst', Just bind) -> Let bind (simple_opt_expr subst' body)

    go lam@(Lam {})     = go_lam [] subst lam
    go (Case e b ty as) = Case (go e) b' (substTy subst ty)
       			       (map (go_alt subst') as)
       		        where
       		  	  (subst', b') = subst_opt_bndr subst b

    ----------------------
    go_alt subst (con, bndrs, rhs) 
      = (con, bndrs', simple_opt_expr subst' rhs)
      where
	(subst', bndrs') = subst_opt_bndrs subst bndrs

    ----------------------
    -- go_lam tries eta reduction
    go_lam bs' subst (Lam b e) 
       = go_lam (b':bs') subst' e
       where
         (subst', b') = subst_opt_bndr subst b
    go_lam bs' subst e 
       | Just etad_e <- tryEtaReduce bs e' = etad_e
       | otherwise                         = mkLams bs e'
       where
         bs = reverse bs'
         e' = simple_opt_expr subst e

----------------------
-- simple_app collects arguments for beta reduction
simple_app :: Subst -> InExpr -> [OutExpr] -> CoreExpr
simple_app subst (App e1 e2) as   
  = simple_app subst e1 (simple_opt_expr subst e2 : as)
simple_app subst (Lam b e) (a:as) 
  = case maybe_substitute subst b a of
      Just ext_subst -> simple_app ext_subst e as
      Nothing        -> Let (NonRec b2 a) (simple_app subst' e as)
  where
    (subst', b') = subst_opt_bndr subst b
    b2 = add_info subst' b b'
simple_app subst e as
  = foldl App (simple_opt_expr subst e) as

----------------------
simple_opt_bind :: Subst -> CoreBind -> (Subst, Maybe CoreBind)
simple_opt_bind subst (Rec prs)
  = (subst'', Just (Rec (reverse rev_prs')))
  where
    (subst', bndrs')    = subst_opt_bndrs subst (map fst prs)
    (subst'', rev_prs') = foldl do_pr (subst', []) (prs `zip` bndrs')
    do_pr (subst, prs) ((b,r), b') 
       = case maybe_substitute subst b r2 of
           Just subst' -> (subst', prs)
    	   Nothing     -> (subst,  (b2,r2):prs)
       where
         b2 = add_info subst b b'
         r2 = simple_opt_expr subst r

simple_opt_bind subst (NonRec b r)
  = case maybe_substitute subst b r' of
      Just ext_subst -> (ext_subst, Nothing)
      Nothing        -> (subst', Just (NonRec b2 r'))
  where
    r' = simple_opt_expr subst r
    (subst', b') = subst_opt_bndr subst b
    b2 = add_info subst' b b'

----------------------
maybe_substitute :: Subst -> InVar -> OutExpr -> Maybe Subst
    -- (maybe_substitute subst in_var out_rhs)  
    --   either extends subst with (in_var -> out_rhs)
    --   or     returns Nothing
maybe_substitute subst b r
  | Type ty <- r 	-- let a::* = TYPE ty in <body>
  = ASSERT( isTyCoVar b )
    Just (extendTvSubst subst b ty)

  | isId b		-- let x = e in <body>
  , safe_to_inline (idOccInfo b) 
  , isAlwaysActive (idInlineActivation b)	-- Note [Inline prag in simplOpt]
  , not (isStableUnfolding (idUnfolding b))
  , not (isExportedId b)
  = Just (extendIdSubst subst b r)
  
  | otherwise
  = Nothing
  where
	-- Unconditionally safe to inline
    safe_to_inline :: OccInfo -> Bool
    safe_to_inline (IAmALoopBreaker {})     = False
    safe_to_inline IAmDead                  = True
    safe_to_inline (OneOcc in_lam one_br _) = (not in_lam && one_br) || exprIsTrivial r
    safe_to_inline NoOccInfo                = exprIsTrivial r

----------------------
subst_opt_bndr :: Subst -> InVar -> (Subst, OutVar)
subst_opt_bndr subst bndr
  | isTyCoVar bndr  = substTyVarBndr subst bndr
  | otherwise       = subst_opt_id_bndr subst bndr

subst_opt_id_bndr :: Subst -> InId -> (Subst, OutId)
-- Nuke all fragile IdInfo, unfolding, and RULES; 
--    it gets added back later by add_info
-- Rather like SimplEnv.substIdBndr
--
-- It's important to zap fragile OccInfo (which CoreSubst.SubstIdBndr 
-- carefully does not do) because simplOptExpr invalidates it

subst_opt_id_bndr subst@(Subst in_scope id_subst tv_subst) old_id
  = (Subst new_in_scope new_id_subst tv_subst, new_id)
  where
    id1	   = uniqAway in_scope old_id
    id2    = setIdType id1 (substTy subst (idType old_id))
    new_id = zapFragileIdInfo id2	-- Zaps rules, worker-info, unfolding
					-- and fragile OccInfo
    new_in_scope = in_scope `extendInScopeSet` new_id

	-- Extend the substitution if the unique has changed,
	-- or there's some useful occurrence information
	-- See the notes with substTyVarBndr for the delSubstEnv
    new_id_subst | new_id /= old_id
	         = extendVarEnv id_subst old_id (Var new_id)
	         | otherwise 
	         = delVarEnv id_subst old_id

----------------------
subst_opt_bndrs :: Subst -> [InVar] -> (Subst, [OutVar])
subst_opt_bndrs subst bndrs
  = mapAccumL subst_opt_bndr subst bndrs

----------------------
add_info :: Subst -> InVar -> OutVar -> OutVar
add_info subst old_bndr new_bndr 
 | isTyCoVar old_bndr = new_bndr
 | otherwise          = maybeModifyIdInfo mb_new_info new_bndr
 where
   mb_new_info = substIdInfo subst new_bndr (idInfo old_bndr)
\end{code}

Note [Inline prag in simplOpt]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If there's an INLINE/NOINLINE pragma that restricts the phase in 
which the binder can be inlined, we don't inline here; after all,
we don't know what phase we're in.  Here's an example

  foo :: Int -> Int -> Int
  {-# INLINE foo #-}
  foo m n = inner m
     where
       {-# INLINE [1] inner #-}
       inner m = m+n

  bar :: Int -> Int
  bar n = foo n 1

When inlining 'foo' in 'bar' we want the let-binding for 'inner' 
to remain visible until Phase 1

