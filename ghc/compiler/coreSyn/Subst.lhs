%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module Subst (
	-- In-scope set
	InScopeSet, emptyInScopeSet,
	lookupInScope, setInScope, extendInScope, extendInScopes, isInScope, modifyInScope,

	-- Substitution stuff
	Subst, TyVarSubst, IdSubst,
	emptySubst, mkSubst, substEnv, substInScope,
	lookupSubst, lookupIdSubst, isEmptySubst, extendSubst, extendSubstList,
	zapSubstEnv, setSubstEnv, 

	bindSubst, unBindSubst, bindSubstList, unBindSubstList,

	-- Binders
	substBndr, substBndrs, substTyVar, substId, substIds,
	substAndCloneId, substAndCloneIds,

	-- Type stuff
	mkTyVarSubst, mkTopTyVarSubst, 
	substTy, substTheta,

	-- Expression stuff
	substExpr, substIdInfo
    ) where

#include "HsVersions.h"

import CoreSyn		( Expr(..), Bind(..), Note(..), CoreExpr, CoreBndr,
			  CoreRules(..), CoreRule(..), 
			  emptyCoreRules, isEmptyCoreRules, seqRules
			)
import CoreFVs		( exprFreeVars )
import TypeRep		( Type(..), TyNote(..), 
			)  -- friend
import Type		( ThetaType,
			  tyVarsOfType, tyVarsOfTypes, mkAppTy
			)
import VarSet
import VarEnv
import Var		( setVarUnique, isId )
import Id		( idType, setIdType, getIdOccInfo, zapFragileIdInfo )
import Name		( isLocallyDefined )
import IdInfo		( IdInfo, isFragileOccInfo,
			  specInfo, setSpecInfo, 
			  workerExists, workerInfo, setWorkerInfo, WorkerInfo
			)
import UniqSupply	( UniqSupply, uniqFromSupply, splitUniqSupply )
import Var		( Var, IdOrTyVar, Id, TyVar, isTyVar )
import Outputable
import Util		( mapAccumL, foldl2, seqList, ($!) )
\end{code}

%************************************************************************
%*									*
\subsection{Substitutions}
%*									*
%************************************************************************

\begin{code}
type InScopeSet = VarEnv Var

data Subst = Subst InScopeSet		-- In scope
		   SubstEnv		-- Substitution itself
	-- INVARIANT 1: The (domain of the) in-scope set is a superset
	-- 	        of the free vars of the range of the substitution
	--		that might possibly clash with locally-bound variables
	--		in the thing being substituted in.
	-- This is what lets us deal with name capture properly
	-- It's a hard invariant to check...
	-- There are various ways of causing it to happen:
	--	- arrange that the in-scope set really is all the things in scope
	--	- arrange that it's the free vars of the range of the substitution
	--	- make it empty because all the free vars of the subst are fresh,
	--		and hence can't possibly clash.a
	--
	-- INVARIANT 2: No variable is both in scope and in the domain of the substitution
	--		Equivalently, the substitution is idempotent
	--

type IdSubst    = Subst
\end{code}

The general plan about the substitution and in-scope set for Ids is as follows

* substId always adds new_id to the in-scope set.
  new_id has a correctly-substituted type, but all its fragile IdInfo has been zapped.
  That is added back in later.  So new_id is the minimal thing it's 
  correct to substitute.

* substId adds a binding (DoneVar new_id occ) to the substitution if 
	EITHER the Id's unique has changed
	OR     the Id has interesting occurrence information
  Note, though that the substitution isn't necessarily extended
  if the type changes.  Why not?  Because of the next point:

* We *always, always* finish by looking up in the in-scope set 
  any variable that doesn't get a DoneEx or DoneVar hit in the substitution.
  Reason: so that we never finish up with a "old" Id in the result.  
  An old Id might point to an old unfolding and so on... which gives a space leak.

  [The DoneEx and DoneVar hits map to "new" stuff.]

* It follows that substExpr must not do a no-op if the substitution is empty.
  substType is free to do so, however.

* When we come to a let-binding (say) we generate new IdInfo, including an
  unfolding, attach it to the binder, and add this newly adorned binder to
  the in-scope set.  So all subsequent occurrences of the binder will get mapped
  to the full-adorned binder, which is also the one put in the binding site.

* The in-scope "set" usually maps x->x; we use it simply for its domain.
  But sometimes we have two in-scope Ids that are synomyms, and should
  map to the same target:  x->x, y->x.  Notably:
	case y of x { ... }
  That's why the "set" is actually a VarEnv Var

\begin{code}
emptyInScopeSet :: InScopeSet
emptyInScopeSet = emptyVarSet

add_in_scope :: InScopeSet -> Var -> InScopeSet
add_in_scope in_scope v = extendVarEnv in_scope v v
\end{code}



\begin{code}
isEmptySubst :: Subst -> Bool
isEmptySubst (Subst _ env) = isEmptySubstEnv env

emptySubst :: Subst
emptySubst = Subst emptyInScopeSet emptySubstEnv

mkSubst :: InScopeSet -> SubstEnv -> Subst
mkSubst in_scope env = Subst in_scope env

substEnv :: Subst -> SubstEnv
substEnv (Subst _ env) = env

substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _) = in_scope

zapSubstEnv :: Subst -> Subst
zapSubstEnv (Subst in_scope env) = Subst in_scope emptySubstEnv

extendSubst :: Subst -> Var -> SubstResult -> Subst
extendSubst (Subst in_scope env) v r = Subst in_scope (extendSubstEnv env v r)

extendSubstList :: Subst -> [Var] -> [SubstResult] -> Subst
extendSubstList (Subst in_scope env) v r = Subst in_scope (extendSubstEnvList env v r)

lookupSubst :: Subst -> Var -> Maybe SubstResult
lookupSubst (Subst _ env) v = lookupSubstEnv env v

lookupIdSubst :: Subst -> Id -> SubstResult
-- Does the lookup in the in-scope set too
lookupIdSubst (Subst in_scope env) v
  = case lookupSubstEnv env v of
	Just (DoneId v' occ) -> case lookupVarEnv in_scope v' of
				  Just v'' -> DoneId v'' occ
				  Nothing  -> DoneId v' occ
	Just res	     -> res
	Nothing  	     -> DoneId v' (getIdOccInfo v')
			     where
				    v' = case lookupVarEnv in_scope v of
					   Just v' -> v'
					   Nothing -> v

lookupInScope :: Subst -> Var -> Maybe Var
lookupInScope (Subst in_scope _) v = lookupVarEnv in_scope v

isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _) = v `elemVarEnv` in_scope

extendInScope :: Subst -> Var -> Subst
extendInScope (Subst in_scope env) v = Subst (in_scope `add_in_scope` v) env

modifyInScope :: Subst -> Var -> Var -> Subst
modifyInScope (Subst in_scope env) old_v new_v = Subst (extendVarEnv in_scope old_v new_v) env
	-- make old_v map to new_v

extendInScopes :: Subst -> [Var] -> Subst
extendInScopes (Subst in_scope env) vs = Subst (foldl add_in_scope in_scope vs) env

-------------------------------
bindSubst :: Subst -> Var -> Var -> Subst
-- Extend with a substitution, v1 -> Var v2
-- and extend the in-scopes with v2
bindSubst (Subst in_scope env) old_bndr new_bndr
  = Subst (in_scope `add_in_scope` new_bndr)
	  (extendSubstEnv env old_bndr subst_result)
  where
    subst_result | isId old_bndr = DoneEx (Var new_bndr)
		 | otherwise	 = DoneTy (TyVarTy new_bndr)

unBindSubst :: Subst -> Var -> Var -> Subst
-- Reverse the effect of bindSubst
-- If old_bndr was already in the substitution, this doesn't quite work
unBindSubst (Subst in_scope env) old_bndr new_bndr
  = Subst (in_scope `delVarEnv` new_bndr) (delSubstEnv env old_bndr)

-- And the "List" forms
bindSubstList :: Subst -> [Var] -> [Var] -> Subst
bindSubstList subst old_bndrs new_bndrs
  = foldl2 bindSubst subst old_bndrs new_bndrs

unBindSubstList :: Subst -> [Var] -> [Var] -> Subst
unBindSubstList subst old_bndrs new_bndrs
  = foldl2 unBindSubst subst old_bndrs new_bndrs


-------------------------------
setInScope :: Subst 	-- Take env part from here
	   -> InScopeSet
	   -> Subst
setInScope (Subst in_scope1 env1) in_scope2
  = Subst in_scope2 env1

setSubstEnv :: Subst 		-- Take in-scope part from here
	    -> SubstEnv		-- ... and env part from here
	    -> Subst
setSubstEnv (Subst in_scope1 _) env2 = Subst in_scope1 env2
\end{code}


%************************************************************************
%*									*
\subsection{Type substitution}
%*									*
%************************************************************************

\begin{code}
type TyVarSubst    = Subst	-- TyVarSubst are expected to have range elements
	-- (We could have a variant of Subst, but it doesn't seem worth it.)

-- mkTyVarSubst generates the in-scope set from
-- the types given; but it's just a thunk so with a bit of luck
-- it'll never be evaluated
mkTyVarSubst :: [TyVar] -> [Type] -> Subst
mkTyVarSubst tyvars tys = Subst (tyVarsOfTypes tys) (zip_ty_env tyvars tys emptySubstEnv)

-- mkTopTyVarSubst is called when doing top-level substitutions.
-- Here we expect that the free vars of the range of the
-- substitution will be empty.
mkTopTyVarSubst :: [TyVar] -> [Type] -> Subst
mkTopTyVarSubst tyvars tys = Subst emptyInScopeSet (zip_ty_env tyvars tys emptySubstEnv)

zip_ty_env []       []       env = env
zip_ty_env (tv:tvs) (ty:tys) env = zip_ty_env tvs tys (extendSubstEnv env tv (DoneTy ty))
\end{code}

substTy works with general Substs, so that it can be called from substExpr too.

\begin{code}
substTy :: Subst -> Type  -> Type
substTy subst ty | isEmptySubst subst = ty
	         | otherwise	      = subst_ty subst ty

substTheta :: TyVarSubst -> ThetaType -> ThetaType
substTheta subst theta
  | isEmptySubst subst = theta
  | otherwise	       = [(clas, map (subst_ty subst) tys) | (clas, tys) <- theta]

subst_ty subst ty
   = go ty
  where
    go (TyConApp tc tys)	  = let args = map go tys
				    in  args `seqList` TyConApp tc args
    go (NoteTy (SynNote ty1) ty2) = NoteTy (SynNote $! (go ty1)) $! (go ty2)
    go (NoteTy (FTVNote _) ty2)   = go ty2		-- Discard the free tyvar note
    go (FunTy arg res)   	  = (FunTy $! (go arg)) $! (go res)
    go (NoteTy (UsgNote usg)  ty2) = (NoteTy $! UsgNote usg) $! go ty2  	-- Keep usage annot
    go (NoteTy (UsgForAll uv) ty2) = (NoteTy $! UsgForAll uv) $! go ty2  	-- Keep uvar bdr
    go (AppTy fun arg)   	  = mkAppTy (go fun) $! (go arg)
    go ty@(TyVarTy tv)   	  = case (lookupSubst subst tv) of
	       				Nothing 	   -> ty
       					Just (DoneTy ty')  -> ty'
					
    go (ForAllTy tv ty)		  = case substTyVar subst tv of
					(subst', tv') -> ForAllTy tv' $! (subst_ty subst' ty)
\end{code}

Here is where we invent a new binder if necessary.

\begin{code}
substTyVar :: Subst -> TyVar -> (Subst, TyVar)	
substTyVar subst@(Subst in_scope env) old_var
  | old_var == new_var	-- No need to clone
			-- But we *must* zap any current substitution for the variable.
			--  For example:
			--	(\x.e) with id_subst = [x |-> e']
			-- Here we must simply zap the substitution for x
			--
			-- The new_id isn't cloned, but it may have a different type
			-- etc, so we must return it, not the old id
  = (Subst (in_scope `add_in_scope` new_var)
	   (delSubstEnv env old_var),
     new_var)

  | otherwise	-- The new binder is in scope so
		-- we'd better rename it away from the in-scope variables
		-- Extending the substitution to do this renaming also
		-- has the (correct) effect of discarding any existing
		-- substitution for that variable
  = (Subst (in_scope `add_in_scope` new_var) 
	   (extendSubstEnv env old_var (DoneTy (TyVarTy new_var))),
     new_var)
  where
    new_var = uniqAway in_scope old_var
	-- The uniqAway part makes sure the new variable is not already in scope
\end{code}


%************************************************************************
%*									*
\section{Expression substitution}
%*									*
%************************************************************************

This expression substituter deals correctly with name capture.

BUT NOTE that substExpr silently discards the
	unfolding, and
	spec env
IdInfo attached to any binders in the expression.  It's quite
tricky to do them 'right' in the case of mutually recursive bindings,
and so far has proved unnecessary.

\begin{code}
substExpr :: Subst -> CoreExpr -> CoreExpr
substExpr subst expr
	-- NB: we do not do a no-op when the substitution is empty,
	-- because we always want to substitute the variables in the
	-- in-scope set for their occurrences.  Why?
	-- 	(a) because they may contain more information
	--	(b) because leaving an un-substituted Id might cause
	--	    a space leak (its unfolding might point to an old version
	--	    of its right hand side).

  = go expr
  where
    go (Var v) = -- See the notes at the top, with the Subst data type declaration
		 case lookupIdSubst subst v of
	
		    ContEx env' e' -> substExpr (setSubstEnv subst env') e'
		    DoneId v _	   -> Var v
		    DoneEx e'      -> e'

    go (Type ty)      = Type (go_ty ty)
    go (Con con args) = Con con (map go args)
    go (App fun arg)  = App (go fun) (go arg)
    go (Note note e)  = Note (go_note note) (go e)

    go (Lam bndr body) = Lam bndr' (substExpr subst' body)
		       where
			 (subst', bndr') = substBndr subst bndr

    go (Let (NonRec bndr rhs) body) = Let (NonRec bndr' (go rhs)) (substExpr subst' body)
				    where
				      (subst', bndr') = substBndr subst bndr

    go (Let (Rec pairs) body) = Let (Rec pairs') (substExpr subst' body)
			      where
				(subst', bndrs') = substBndrs subst (map fst pairs)
				pairs'	= bndrs' `zip` rhss'
				rhss'	= map (substExpr subst' . snd) pairs

    go (Case scrut bndr alts) = Case (go scrut) bndr' (map (go_alt subst') alts)
			      where
				(subst', bndr') = substBndr subst bndr

    go_alt subst (con, bndrs, rhs) = (con, bndrs', substExpr subst' rhs)
				 where
				   (subst', bndrs') = substBndrs subst bndrs

    go_note (Coerce ty1 ty2) = Coerce (go_ty ty1) (go_ty ty2)
    go_note note	     = note

    go_ty ty = substTy subst ty

\end{code}

Substituting in binders is a rather tricky part of the whole compiler.

When we hit a binder we may need to
  (a) apply the the type envt (if non-empty) to its type
  (c) give it a new unique to avoid name clashes

\begin{code}
substBndr :: Subst -> IdOrTyVar -> (Subst, IdOrTyVar)
substBndr subst bndr
  | isTyVar bndr  = substTyVar subst bndr
  | otherwise     = substId    subst bndr

substBndrs :: Subst -> [IdOrTyVar] -> (Subst, [IdOrTyVar])
substBndrs subst bndrs = mapAccumL substBndr subst bndrs


substIds :: Subst -> [Id] -> (Subst, [Id])
substIds subst bndrs = mapAccumL substId subst bndrs

substId :: Subst -> Id -> (Subst, Id)
	-- Returns an Id with empty IdInfo
	-- See the notes with the Subst data type decl at the
	-- top of this module

substId subst@(Subst in_scope env) old_id
  = (Subst (in_scope `add_in_scope` new_id) new_env, new_id)
  where
    id_ty    = idType old_id
    occ_info = getIdOccInfo old_id

       -- id1 has its type zapped
    id1 |  noTypeSubst env
	|| isEmptyVarSet (tyVarsOfType id_ty) = old_id
			-- The tyVarsOfType is cheaper than it looks
			-- because we cache the free tyvars of the type
			-- in a Note in the id's type itself
        | otherwise  = setIdType old_id (substTy subst id_ty)

	-- id2 has its IdInfo zapped
    id2 = zapFragileIdInfo id1

	-- new_id is cloned if necessary
    new_id = uniqAway in_scope id2

	-- Extend the substitution if the unique has changed,
	-- or there's some useful occurrence information
	-- See the notes with substTyVar for the delSubstEnv
    new_env | new_id /= old_id || isFragileOccInfo occ_info 
	    = extendSubstEnv env old_id (DoneId new_id occ_info)
	    | otherwise 
	    = delSubstEnv env old_id
\end{code}

Now a variant that unconditionally allocates a new unique.

\begin{code}
substAndCloneIds :: Subst -> UniqSupply -> [Id] -> (Subst, UniqSupply, [Id])
substAndCloneIds subst us [] = (subst, us, [])
substAndCloneIds subst us (b:bs) = case substAndCloneId  subst  us  b  of { (subst1, us1, b') ->
				   case substAndCloneIds subst1 us1 bs of { (subst2, us2, bs') ->
				   (subst2, us2, (b':bs')) }}
					
substAndCloneId :: Subst -> UniqSupply -> Id -> (Subst, UniqSupply, Id)
substAndCloneId subst@(Subst in_scope env) us old_id
  = (Subst (in_scope `add_in_scope` new_id) 
	   (extendSubstEnv env old_id (DoneEx (Var new_id))),
     new_us,
     new_id)
  where
    id_ty    = idType old_id
    id1 | noTypeSubst env || isEmptyVarSet (tyVarsOfType id_ty) = old_id
        | otherwise 						= setIdType old_id (substTy subst id_ty)

    id2 	 = zapFragileIdInfo id1
    new_id	 = setVarUnique id2 (uniqFromSupply us1)
    (us1,new_us) = splitUniqSupply us
\end{code}


%************************************************************************
%*									*
\section{IdInfo substitution}
%*									*
%************************************************************************

\begin{code}
substIdInfo :: Subst 
	    -> IdInfo		-- Get un-substituted ones from here
	    -> IdInfo		-- Substitute it and add it to here
	    -> IdInfo		-- To give this
	-- Seq'ing on the returned IdInfo is enough to cause all the 
	-- substitutions to happen completely

substIdInfo subst old_info new_info
  = info2
  where 
    info1 | isEmptyCoreRules old_rules = new_info
	  | otherwise		       = new_info `setSpecInfo` new_rules
			-- setSpecInfo does a seq
	  where
	    new_rules = substRules subst old_rules
 
    info2 | not (workerExists old_wrkr) = info1
	  | otherwise			= info1 `setWorkerInfo` new_wrkr
			-- setWorkerInfo does a seq
	  where
	    new_wrkr = substWorker subst old_wrkr

    old_rules = specInfo   old_info
    old_wrkr  = workerInfo old_info

substWorker :: Subst -> WorkerInfo -> WorkerInfo
	-- Seq'ing on the returned WorkerInfo is enough to cause all the 
	-- substitutions to happen completely

substWorker subst Nothing
  = Nothing
substWorker subst (Just w)
  = case lookupSubst subst w of
	Nothing -> Just w
	Just (DoneId w1 _)     -> Just w1
	Just (DoneEx (Var w1)) -> Just w1
	Just (DoneEx other)    -> WARN( True, text "substWorker: DoneEx" <+> ppr w )
				  Nothing	-- Worker has got substituted away altogether
	Just (ContEx se1 e)    -> WARN( True, text "substWorker: ContEx" <+> ppr w )
				  Nothing	-- Ditto
			
substRules :: Subst -> CoreRules -> CoreRules
	-- Seq'ing on the returned CoreRules is enough to cause all the 
	-- substitutions to happen completely

substRules subst rules
 | isEmptySubst subst = rules

substRules subst (Rules rules rhs_fvs)
  = seqRules new_rules `seq` new_rules
  where
    new_rules = Rules (map do_subst rules)
		      (subst_fvs (substEnv subst) rhs_fvs)

    do_subst rule@(BuiltinRule _) = rule
    do_subst (Rule name tpl_vars lhs_args rhs)
	= Rule name tpl_vars' 
	       (map (substExpr subst') lhs_args)
	       (substExpr subst' rhs)
	where
	  (subst', tpl_vars') = substBndrs subst tpl_vars

    subst_fvs se fvs
	= foldVarSet (unionVarSet . subst_fv) emptyVarSet rhs_fvs
	where
	  subst_fv fv = case lookupSubstEnv se fv of
				Nothing			  -> unitVarSet fv
				Just (DoneId fv' _)	  -> unitVarSet fv'
				Just (DoneEx expr)	  -> exprFreeVars expr
				Just (DoneTy ty)  	  -> tyVarsOfType ty 
				Just (ContEx se' expr) -> subst_fvs se' (exprFreeVars expr)
\end{code}
