%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module CoreSubst (
	-- Substitution stuff
	Subst, TvSubstEnv, IdSubstEnv, InScopeSet,

	deShadowBinds,
	substTy, substExpr, substSpec, substWorker,
	lookupIdSubst, lookupTvSubst, 

	emptySubst, mkEmptySubst, mkSubst, substInScope, isEmptySubst, 
 	extendIdSubst, extendIdSubstList, extendTvSubst, extendTvSubstList,
	extendInScope, extendInScopeIds,
	isInScope,

	-- Binders
	substBndr, substBndrs, substRecBndrs,
	cloneIdBndr, cloneIdBndrs, cloneRecIdBndrs
    ) where

#include "HsVersions.h"

import CoreSyn		( Expr(..), Bind(..), Note(..), CoreExpr, CoreBind,
			  CoreRule(..), hasUnfolding, noUnfolding
			)
import CoreFVs		( exprFreeVars )
import CoreUtils	( exprIsTrivial )

import qualified Type	( substTy, substTyVarBndr )
import Type		( Type, tyVarsOfType, TvSubstEnv, TvSubst(..), mkTyVarTy )
import VarSet
import VarEnv
import Var		( setVarUnique, isId )
import Id		( idType, setIdType, maybeModifyIdInfo, isLocalId )
import IdInfo		( IdInfo, SpecInfo(..), specInfo, setSpecInfo, isEmptySpecInfo,
			  unfoldingInfo, setUnfoldingInfo, seqSpecInfo,
			  WorkerInfo(..), workerExists, workerInfo, setWorkerInfo
			)
import Unique		( Unique )
import UniqSupply	( UniqSupply, uniqFromSupply, uniqsFromSupply )
import Var		( Var, Id, TyVar, isTyVar )
import Maybes		( orElse )
import Outputable
import PprCore		()		-- Instances
import Util		( mapAccumL )
import FastTypes
\end{code}


%************************************************************************
%*									*
\subsection{Substitutions}
%*									*
%************************************************************************

\begin{code}
data Subst 
  = Subst InScopeSet	-- Variables in in scope (both Ids and TyVars)
	  IdSubstEnv	-- Substitution for Ids
	  TvSubstEnv	-- Substitution for TyVars

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
	-- INVARIANT 2: The substitution is apply-once; see notes with
	--		Types.TvSubstEnv

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

substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _ _) = in_scope

-- zapSubstEnv :: Subst -> Subst
-- zapSubstEnv (Subst in_scope _ _) = Subst in_scope emptyVarEnv emptyVarEnv

-- ToDo: add an ASSERT that fvs(subst-result) is already in the in-scope set
extendIdSubst :: Subst -> Id -> CoreExpr -> Subst
extendIdSubst (Subst in_scope ids tvs) v r = Subst in_scope (extendVarEnv ids v r) tvs

extendIdSubstList :: Subst -> [(Id, CoreExpr)] -> Subst
extendIdSubstList (Subst in_scope ids tvs) prs = Subst in_scope (extendVarEnvList ids prs) tvs

extendTvSubst :: Subst -> TyVar -> Type -> Subst
extendTvSubst (Subst in_scope ids tvs) v r = Subst in_scope ids (extendVarEnv tvs v r) 

extendTvSubstList :: Subst -> [(TyVar,Type)] -> Subst
extendTvSubstList (Subst in_scope ids tvs) prs = Subst in_scope ids (extendVarEnvList tvs prs)

lookupIdSubst :: Subst -> Id -> CoreExpr
lookupIdSubst (Subst in_scope ids tvs) v 
  | not (isLocalId v) = Var v
  | otherwise
  = case lookupVarEnv ids v of {
	Just e  -> e ;
	Nothing -> 	
    case lookupInScope in_scope v of {
	-- Watch out!  Must get the Id from the in-scope set,
	-- because its type there may differ
	Just v  -> Var v ;
	Nothing -> WARN( True, ptext SLIT("CoreSubst.lookupIdSubst") <+> ppr v ) 
		   Var v
    }}

lookupTvSubst :: Subst -> TyVar -> Type
lookupTvSubst (Subst _ ids tvs) v = lookupVarEnv tvs v `orElse` mkTyVarTy v

------------------------------
isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _ _) = v `elemInScopeSet` in_scope

extendInScope :: Subst -> Var -> Subst
extendInScope (Subst in_scope ids tvs) v
  = Subst (in_scope `extendInScopeSet` v) 
	  (ids `delVarEnv` v) (tvs `delVarEnv` v)

extendInScopeIds :: Subst -> [Id] -> Subst
extendInScopeIds (Subst in_scope ids tvs) vs 
  = Subst (in_scope `extendInScopeSetList` vs) 
	  (ids `delVarEnvList` vs) tvs
\end{code}

Pretty printing, for debugging only

\begin{code}
instance Outputable Subst where
  ppr (Subst in_scope ids tvs) 
	=  ptext SLIT("<InScope =") <+> braces (fsep (map ppr (varEnvElts (getInScopeVars in_scope))))
	$$ ptext SLIT(" IdSubst   =") <+> ppr ids
	$$ ptext SLIT(" TvSubst   =") <+> ppr tvs
 	 <> char '>'
\end{code}


%************************************************************************
%*									*
	Substituting expressions
%*									*
%************************************************************************

\begin{code}
substExpr :: Subst -> CoreExpr -> CoreExpr
substExpr subst expr
  = go expr
  where
    go (Var v)	       = lookupIdSubst subst v 
    go (Type ty)       = Type (substTy subst ty)
    go (Lit lit)       = Lit lit
    go (App fun arg)   = App (go fun) (go arg)
    go (Note note e)   = Note (go_note note) (go e)
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

    go_note (Coerce ty1 ty2) = Coerce (substTy subst ty1) (substTy subst ty2)
    go_note note	     = note

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

De-shadowing the program is sometimes a useful pre-pass.  It can be done simply
by running over the bindings with an empty substitution, becuase substitution
returns a result that has no-shadowing guaranteed.

(Actually, within a single *type* there might still be shadowing, because 
substType is a no-op for the empty substitution, but that's OK.)

\begin{code}
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
substBndr :: Subst -> Var -> (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = substTyVarBndr subst bndr
  | otherwise     = substIdBndr subst subst bndr

substBndrs :: Subst -> [Var] -> (Subst, [Var])
substBndrs subst bndrs = mapAccumL substBndr subst bndrs

substRecBndrs :: Subst -> [Id] -> (Subst, [Id])
-- Substitute a mutually recursive group
substRecBndrs subst bndrs 
  = (new_subst, new_bndrs)
  where		-- Here's the reason we need to pass rec_subst to subst_id
    (new_subst, new_bndrs) = mapAccumL (substIdBndr new_subst) subst bndrs
\end{code}


\begin{code}
substIdBndr :: Subst		-- Substitution to use for the IdInfo
	    -> Subst -> Id 	-- Substitition and Id to transform
	    -> (Subst, Id)	-- Transformed pair

substIdBndr rec_subst subst@(Subst in_scope env tvs) old_id
  = (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
  where
    id1 = uniqAway in_scope old_id	-- id1 is cloned if necessary
    id2 = substIdType subst id1		-- id2 has its type zapped

	-- new_id has the right IdInfo
	-- The lazy-set is because we're in a loop here, with 
	-- rec_subst, when dealing with a mutually-recursive group
    new_id = maybeModifyIdInfo (substIdInfo rec_subst) id2

	-- Extend the substitution if the unique has changed
	-- See the notes with substTyVarBndr for the delVarEnv
    new_env | new_id /= old_id  = extendVarEnv env old_id (Var new_id)
	    | otherwise         = delVarEnv env old_id
\end{code}

Now a variant that unconditionally allocates a new unique.
It also unconditionally zaps the OccInfo.

\begin{code}
cloneIdBndr :: Subst -> UniqSupply -> Id -> (Subst, Id)
cloneIdBndr subst us old_id
  = clone_id subst subst (old_id, uniqFromSupply us)

cloneIdBndrs :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
cloneIdBndrs subst us ids
  = mapAccumL (clone_id subst) subst (ids `zip` uniqsFromSupply us)

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
    new_id  = maybeModifyIdInfo (substIdInfo rec_subst) id2
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

substTy :: Subst -> Type -> Type 
substTy (Subst in_scope id_env tv_env) ty 
  = Type.substTy (TvSubst in_scope tv_env) ty
\end{code}


%************************************************************************
%*									*
\section{IdInfo substitution}
%*									*
%************************************************************************

\begin{code}
substIdType :: Subst -> Id -> Id
substIdType subst@(Subst in_scope id_env tv_env) id
  | isEmptyVarEnv tv_env || isEmptyVarSet (tyVarsOfType old_ty) = id
  | otherwise	= setIdType id (substTy subst old_ty)
		-- The tyVarsOfType is cheaper than it looks
		-- because we cache the free tyvars of the type
		-- in a Note in the id's type itself
  where
    old_ty = idType id

------------------
substIdInfo :: Subst -> IdInfo -> Maybe IdInfo
-- Always zaps the unfolding, to save substitution work
substIdInfo  subst info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setSpecInfo`   	  substSpec  subst old_rules
			       `setWorkerInfo` 	  substWorker subst old_wrkr
			       `setUnfoldingInfo` noUnfolding)
  where
    old_rules 	  = specInfo info
    old_wrkr  	  = workerInfo info
    nothing_to_do = isEmptySpecInfo old_rules &&
		    not (workerExists old_wrkr) &&
		    not (hasUnfolding (unfoldingInfo info))
    

------------------
substWorker :: Subst -> WorkerInfo -> WorkerInfo
	-- Seq'ing on the returned WorkerInfo is enough to cause all the 
	-- substitutions to happen completely

substWorker subst NoWorker
  = NoWorker
substWorker subst (HasWorker w a)
  = case lookupIdSubst subst w of
	Var w1 -> HasWorker w1 a
	other  -> WARN( not (exprIsTrivial other), text "CoreSubst.substWorker:" <+> ppr w )
		  NoWorker	-- Worker has got substituted away altogether
				-- (This can happen if it's trivial, 
				--  via postInlineUnconditionally, hence warning)

------------------
substSpec :: Subst -> SpecInfo -> SpecInfo

substSpec subst spec@(SpecInfo rules rhs_fvs)
  | isEmptySubst subst
  = spec
  | otherwise
  = seqSpecInfo new_rules `seq` new_rules
  where
    new_rules = SpecInfo (map do_subst rules) (substVarSet subst rhs_fvs)

    do_subst rule@(BuiltinRule {}) = rule
    do_subst rule@(Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs })
	= rule { ru_bndrs = bndrs',
		 ru_args  = map (substExpr subst') args,
		 ru_rhs   = substExpr subst' rhs }
	where
	  (subst', bndrs') = substBndrs subst bndrs

------------------
substVarSet subst fvs 
  = foldVarSet (unionVarSet . subst_fv subst) emptyVarSet fvs
  where
    subst_fv subst fv 
	| isId fv   = exprFreeVars (lookupIdSubst subst fv)
	| otherwise = tyVarsOfType (lookupTvSubst subst fv)
\end{code}
