%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module Subst (
	-- Substitution stuff
	IdSubstEnv, SubstResult(..),

	Subst, emptySubst, mkSubst, substInScope, substTy,
	lookupIdSubst, lookupTvSubst, isEmptySubst, 
 	extendIdSubst, extendIdSubstList, extendTvSubst, extendTvSubstList,
	zapSubstEnv, setSubstEnv, 
	getTvSubst, getTvSubstEnv, setTvSubstEnv, 

	bindSubst, unBindSubst, bindSubstList, unBindSubstList,

	-- Binders
	simplBndr, simplBndrs, simplLetId, simplLamBndr, simplIdInfo,
	substAndCloneId, substAndCloneIds, substAndCloneRecIds,

	setInScope, setInScopeSet, 
	extendInScope, extendInScopeIds,
	isInScope, modifyInScope,

	-- Expression stuff
	substExpr, substRules, substId
    ) where

#include "HsVersions.h"

import CoreSyn		( Expr(..), Bind(..), Note(..), CoreExpr,
			  CoreRules(..), CoreRule(..), 
			  isEmptyCoreRules, seqRules, hasUnfolding, noUnfolding, hasSomeUnfolding,
			  Unfolding(..)
			)
import CoreFVs		( exprFreeVars )
import CoreUtils	( exprIsTrivial )

import qualified Type	( substTy )
import Type		( Type, tyVarsOfType, mkTyVarTy,
			  TvSubstEnv, TvSubst(..), substTyVar )
import VarSet
import VarEnv
import Var		( setVarUnique, isId, mustHaveLocalBinding )
import Id		( idType, idInfo, setIdInfo, setIdType, 
			  idUnfolding, setIdUnfolding,
			  idOccInfo, maybeModifyIdInfo )
import IdInfo		( IdInfo, vanillaIdInfo,
			  occInfo, isFragileOcc, setOccInfo, 
			  specInfo, setSpecInfo, 
			  setArityInfo, unknownArity, arityInfo,
			  unfoldingInfo, setUnfoldingInfo,
			  WorkerInfo(..), workerExists, workerInfo, setWorkerInfo, WorkerInfo
			)
import BasicTypes	( OccInfo(..) )
import Unique		( Unique )
import UniqSupply	( UniqSupply, uniqFromSupply, uniqsFromSupply )
import Var		( Var, Id, TyVar, isTyVar )
import Outputable
import PprCore		()		-- Instances
import Util		( mapAccumL, foldl2 )
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
	-- INVARIANT 2: No variable is both in scope and in the domain of the substitution
	--		Equivalently, the substitution is idempotent
	--	[Sep 2000: Lies, all lies.  The substitution now does contain
	--		   mappings x77 -> DoneId x77 occ
	--		   to record x's occurrence information.]
	-- 	[Also watch out: the substitution can contain x77 -> DoneEx (Var x77)
	--	 Consider let x = case k of I# x77 -> ... in
	--		  let y = case k of I# x77 -> ... in ...
	--	 and suppose the body is strict in both x and y.  Then the simplifier
	--	 will pull the first (case k) to the top; so the second (case k) will
	--	 cancel out, mapping x77 to, well, x77!  But one is an in-Id and the 
	--	 other is an out-Id. So the substitution is idempotent in the sense
	--	 that we *must not* repeatedly apply it.]


type IdSubstEnv = IdEnv SubstResult

data SubstResult
  = DoneEx CoreExpr		-- Completed term
  | DoneId Id OccInfo		-- Completed term variable, with occurrence info;
				-- only used by the simplifier
  | ContEx Subst CoreExpr  	-- A suspended substitution
\end{code}

The general plan about the substitution and in-scope set for Ids is as follows

* substId always adds new_id to the in-scope set.
  new_id has a correctly-substituted type, occ info

* substId adds a binding (DoneId new_id occ) to the substitution if 
	EITHER the Id's unique has changed
	OR     the Id has interesting occurrence information
  So in effect you can only get to interesting occurrence information
  by looking up the *old* Id; it's not really attached to the new id
  at all.

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
isEmptySubst :: Subst -> Bool
isEmptySubst (Subst _ id_env tv_env) = isEmptyVarEnv id_env && isEmptyVarEnv tv_env

emptySubst :: Subst
emptySubst = Subst emptyInScopeSet emptyVarEnv emptyVarEnv

mkSubst :: InScopeSet -> Subst
mkSubst in_scope = Subst in_scope emptyVarEnv emptyVarEnv

getTvSubst :: Subst -> TvSubst
getTvSubst (Subst in_scope _ tv_env) = TvSubst in_scope tv_env

getTvSubstEnv :: Subst -> TvSubstEnv
getTvSubstEnv (Subst _ _ tv_env) = tv_env

setTvSubstEnv :: Subst -> TvSubstEnv -> Subst
setTvSubstEnv (Subst in_scope ids _) tvs = Subst in_scope ids tvs



substInScope :: Subst -> InScopeSet
substInScope (Subst in_scope _ _) = in_scope

zapSubstEnv :: Subst -> Subst
zapSubstEnv (Subst in_scope _ _) = Subst in_scope emptyVarEnv emptyVarEnv

-- ToDo: add an ASSERT that fvs(subst-result) is already in the in-scope set
extendIdSubst :: Subst -> Id -> SubstResult -> Subst
extendIdSubst (Subst in_scope ids tvs) v r = Subst in_scope (extendVarEnv ids v r) tvs

extendIdSubstList :: Subst -> [(Id, SubstResult)] -> Subst
extendIdSubstList (Subst in_scope ids tvs) prs = Subst in_scope (extendVarEnvList ids prs) tvs

extendTvSubst :: Subst -> TyVar -> Type -> Subst
extendTvSubst (Subst in_scope ids tvs) v r = Subst in_scope ids (extendVarEnv tvs v r) 

extendTvSubstList :: Subst -> [(TyVar,Type)] -> Subst
extendTvSubstList (Subst in_scope ids tvs) prs = Subst in_scope ids (extendVarEnvList tvs prs)

lookupIdSubst :: Subst -> Id -> Maybe SubstResult
lookupIdSubst (Subst in_scope ids tvs) v = lookupVarEnv ids v

lookupTvSubst :: Subst -> TyVar -> Maybe Type
lookupTvSubst (Subst _ ids tvs) v = lookupVarEnv tvs v

------------------------------
isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _ _) = v `elemInScopeSet` in_scope

modifyInScope :: Subst -> Var -> Var -> Subst
modifyInScope (Subst in_scope ids tvs) old_v new_v 
  = Subst (modifyInScopeSet in_scope old_v new_v) ids tvs
	-- make old_v map to new_v

extendInScope :: Subst -> Var -> Subst
extendInScope (Subst in_scope ids tvs) v
  = Subst (in_scope `extendInScopeSet` v) 
	  (ids `delVarEnv` v) (tvs `delVarEnv` v)

extendInScopeIds :: Subst -> [Id] -> Subst
extendInScopeIds (Subst in_scope ids tvs) vs 
  = Subst (in_scope `extendInScopeSetList` vs) 
	  (ids `delVarEnvList` vs) tvs

-------------------------------
bindSubst :: Subst -> Var -> Var -> Subst
-- Extend with a substitution, v1 -> Var v2
-- and extend the in-scopes with v2
bindSubst (Subst in_scope ids tvs) old_bndr new_bndr
  | isId old_bndr
  = Subst (in_scope `extendInScopeSet` new_bndr)
	  (extendVarEnv ids old_bndr (DoneEx (Var new_bndr)))
	  tvs
  | otherwise
  = Subst (in_scope `extendInScopeSet` new_bndr)
	  ids
	  (extendVarEnv tvs old_bndr (mkTyVarTy new_bndr))

unBindSubst :: Subst -> Var -> Var -> Subst
-- Reverse the effect of bindSubst
-- If old_bndr was already in the substitution, this doesn't quite work
unBindSubst (Subst in_scope ids tvs) old_bndr new_bndr
  = Subst (in_scope `delInScopeSet` new_bndr)
	  (delVarEnv ids old_bndr) 
	  (delVarEnv tvs old_bndr)

-- And the "List" forms
bindSubstList :: Subst -> [Var] -> [Var] -> Subst
bindSubstList subst old_bndrs new_bndrs
  = foldl2 bindSubst subst old_bndrs new_bndrs

unBindSubstList :: Subst -> [Var] -> [Var] -> Subst
unBindSubstList subst old_bndrs new_bndrs
  = foldl2 unBindSubst subst old_bndrs new_bndrs


-------------------------------
setInScopeSet :: Subst -> InScopeSet -> Subst
setInScopeSet (Subst _ ids tvs) in_scope
  = Subst in_scope ids tvs 

setInScope :: Subst 	-- Take env part from here
	   -> Subst	-- Take in-scope part from here
	   -> Subst
setInScope (Subst _ ids tvs) (Subst in_scope _ _)
  = Subst in_scope ids tvs 

setSubstEnv :: Subst 	-- Take in-scope part from here
	    -> Subst	-- ... and env part from here
	    -> Subst
setSubstEnv s1 s2 = setInScope s2 s1
\end{code}

Pretty printing, for debugging only

\begin{code}
instance Outputable SubstResult where
  ppr (DoneEx e)   = ptext SLIT("DoneEx") <+> ppr e
  ppr (DoneId v _) = ptext SLIT("DoneId") <+> ppr v
  ppr (ContEx _ e) = ptext SLIT("ContEx") <+> ppr e

instance Outputable Subst where
  ppr (Subst in_scope ids tvs) 
	=  ptext SLIT("<InScope =") <+> braces (fsep (map ppr (varEnvElts (getInScopeVars in_scope))))
	$$ ptext SLIT(" IdSubst   =") <+> ppr ids
	$$ ptext SLIT(" TvSubst   =") <+> ppr tvs
 	 <> char '>'
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
    go (Var v) = case substId subst v of
		    ContEx env' e' -> substExpr (setSubstEnv subst env') e'
		    DoneId v _	   -> Var v
		    DoneEx e'      -> e'

    go (Type ty)      = Type (go_ty ty)
    go (Lit lit)      = Lit lit
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
				(subst', bndrs') = substRecBndrs subst (map fst pairs)
				pairs'	= bndrs' `zip` rhss'
				rhss'	= map (substExpr subst' . snd) pairs
    go (Case scrut bndr ty alts) = Case (go scrut) bndr' (go_ty ty) (map (go_alt subst') alts)
			         where
			  	 (subst', bndr') = substBndr subst bndr

    go_alt subst (con, bndrs, rhs) = (con, bndrs', substExpr subst' rhs)
				 where
				   (subst', bndrs') = substBndrs subst bndrs

    go_note (Coerce ty1 ty2) = Coerce (go_ty ty1) (go_ty ty2)
    go_note note	     = note

    go_ty ty = substTy subst ty

substId :: Subst -> Id -> SubstResult
substId (Subst in_scope ids tvs) v 
  = case lookupVarEnv ids v of
	Just (DoneId v occ) -> DoneId (lookup v) occ
	Just res	    -> res
	Nothing		    -> let v' = lookup v
			       in DoneId v' (idOccInfo v')
		-- Note [idOccInfo] 
		-- We don't use DoneId for LoopBreakers, so the idOccInfo is
		-- very important!  If isFragileOcc returned True for
		-- loop breakers we could avoid this call, but at the expense
		-- of adding more to the substitution, and building new Ids
		-- in substId a bit more often than really necessary
  where
	-- Get the most up-to-date thing from the in-scope set
	-- Even though it isn't in the substitution, it may be in
	-- the in-scope set with a different type (we only use the
	-- substitution if the unique changes).
    lookup v = case lookupInScope in_scope v of
		 Just v' -> v'
		 Nothing -> WARN( mustHaveLocalBinding v, ppr v ) v


substTy :: Subst -> Type -> Type 
substTy subst ty = Type.substTy (getTvSubst subst) ty
\end{code}


%************************************************************************
%*									*
\section{Substituting an Id binder}
%*									*
%************************************************************************

\begin{code}
-- simplBndr and simplLetId are used by the simplifier

simplBndr :: Subst -> Var -> (Subst, Var)
-- Used for lambda and case-bound variables
-- Clone Id if necessary, substitute type
-- Return with IdInfo already substituted, but (fragile) occurrence info zapped
-- The substitution is extended only if the variable is cloned, because
-- we *don't* need to use it to track occurrence info.
simplBndr subst bndr
  | isTyVar bndr  = subst_tv subst bndr
  | otherwise     = subst_id False subst subst bndr

simplBndrs :: Subst -> [Var] -> (Subst, [Var])
simplBndrs subst bndrs = mapAccumL simplBndr subst bndrs

simplLamBndr :: Subst -> Var -> (Subst, Var)
-- Used for lambda binders.  These sometimes have unfoldings added by
-- the worker/wrapper pass that must be preserved, becuase they can't
-- be reconstructed from context.  For example:
--	f x = case x of (a,b) -> fw a b x
--	fw a b x{=(a,b)} = ...
-- The "{=(a,b)}" is an unfolding we can't reconstruct otherwise.
simplLamBndr subst bndr
  | not (isId bndr && hasSomeUnfolding old_unf)
  = simplBndr subst bndr	-- Normal case
  | otherwise
  = (subst', bndr' `setIdUnfolding` substUnfolding subst old_unf)
  where
    old_unf = idUnfolding bndr
    (subst', bndr') = subst_id False subst subst bndr
		

simplLetId :: Subst -> Id -> (Subst, Id)
-- Clone Id if necessary
-- Substitute its type
-- Return an Id with completely zapped IdInfo
-- 	[A subsequent substIdInfo will restore its IdInfo]
-- Augment the subtitution 
--	if the unique changed, *or* 
--	if there's interesting occurrence info

simplLetId subst@(Subst in_scope env tvs) old_id
  = (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
  where
    old_info = idInfo old_id
    id1	    = uniqAway in_scope old_id
    id2     = substIdType subst id1
    new_id  = setIdInfo id2 vanillaIdInfo

	-- Extend the substitution if the unique has changed,
	-- or there's some useful occurrence information
	-- See the notes with substTyVar for the delSubstEnv
    occ_info = occInfo old_info
    new_env | new_id /= old_id || isFragileOcc occ_info
	    = extendVarEnv env old_id (DoneId new_id occ_info)
	    | otherwise 
	    = delVarEnv env old_id

simplIdInfo :: Subst -> IdInfo -> IdInfo
  -- Used by the simplifier to compute new IdInfo for a let(rec) binder,
  -- subsequent to simplLetId having zapped its IdInfo
simplIdInfo subst old_info
  = case substIdInfo False subst old_info of 
	Just new_info -> new_info
	Nothing       -> old_info
\end{code}

\begin{code}
-- substBndr and friends are used when doing expression substitution only
-- In this case we can *preserve* occurrence information, and indeed we *want*
-- to do so else lose useful occ info in rules. 

substBndr :: Subst -> Var -> (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = subst_tv subst bndr
  | otherwise     = subst_id True {- keep fragile info -} subst subst bndr

substBndrs :: Subst -> [Var] -> (Subst, [Var])
substBndrs subst bndrs = mapAccumL substBndr subst bndrs

substRecBndrs :: Subst -> [Id] -> (Subst, [Id])
-- Substitute a mutually recursive group
substRecBndrs subst bndrs 
  = (new_subst, new_bndrs)
  where
	-- Here's the reason we need to pass rec_subst to subst_id
    (new_subst, new_bndrs) = mapAccumL (subst_id True {- keep fragile info -} new_subst) 
			 	       subst bndrs
\end{code}


\begin{code}
subst_tv :: Subst -> TyVar -> (Subst, TyVar)
-- Unpackage and re-package for substTyVar
subst_tv (Subst in_scope id_env tv_env) tv
  = case substTyVar (TvSubst in_scope tv_env) tv of
	(TvSubst in_scope' tv_env', tv') 
	   -> (Subst in_scope' id_env tv_env', tv')

subst_id :: Bool		-- True <=> keep fragile info
	 -> Subst		-- Substitution to use for the IdInfo
	 -> Subst -> Id 	-- Substitition and Id to transform
	 -> (Subst, Id)		-- Transformed pair

-- Returns with:
--	* Unique changed if necessary
--	* Type substituted
-- 	* Unfolding zapped
--	* Rules, worker, lbvar info all substituted 
--	* Occurrence info zapped if is_fragile_occ returns True
--	* The in-scope set extended with the returned Id
--	* The substitution extended with a DoneId if unique changed
--	  In this case, the var in the DoneId is the same as the
--	  var returned

subst_id keep_fragile rec_subst subst@(Subst in_scope env tvs) old_id
  = (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
  where
	-- id1 is cloned if necessary
    id1 = uniqAway in_scope old_id

	-- id2 has its type zapped
    id2 = substIdType subst id1

	-- new_id has the right IdInfo
	-- The lazy-set is because we're in a loop here, with 
	-- rec_subst, when dealing with a mutually-recursive group
    new_id = maybeModifyIdInfo (substIdInfo keep_fragile rec_subst) id2

	-- Extend the substitution if the unique has changed
	-- See the notes with substTyVar for the delSubstEnv
    new_env | new_id /= old_id
	    = extendVarEnv env old_id (DoneId new_id (idOccInfo old_id))
	    | otherwise 
	    = delVarEnv env old_id
\end{code}

Now a variant that unconditionally allocates a new unique.
It also unconditionally zaps the OccInfo.

\begin{code}
subst_clone_id :: Subst			-- Substitution to use (lazily) for the rules and worker
	       -> Subst -> (Id, Unique)	-- Substitition and Id to transform
	       -> (Subst, Id)		-- Transformed pair

subst_clone_id rec_subst subst@(Subst in_scope env tvs) (old_id, uniq)
  = (Subst (in_scope `extendInScopeSet` new_id) new_env tvs, new_id)
  where
    id1	 = setVarUnique old_id uniq
    id2  = substIdType subst id1

    new_id  = maybeModifyIdInfo (substIdInfo False rec_subst) id2
    new_env = extendVarEnv env old_id (DoneId new_id NoOccInfo)

substAndCloneIds :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
substAndCloneIds subst us ids
  = mapAccumL (subst_clone_id subst) subst (ids `zip` uniqsFromSupply us)

substAndCloneRecIds :: Subst -> UniqSupply -> [Id] -> (Subst, [Id])
substAndCloneRecIds subst us ids
  = (subst', ids')
  where
    (subst', ids') = mapAccumL (subst_clone_id subst') subst
			       (ids `zip` uniqsFromSupply us)

substAndCloneId :: Subst -> UniqSupply -> Id -> (Subst, Id)
substAndCloneId subst us old_id
  = subst_clone_id subst subst (old_id, uniqFromSupply us)
\end{code}


%************************************************************************
%*									*
\section{IdInfo substitution}
%*									*
%************************************************************************

\begin{code}
substIdInfo :: Bool	-- True <=> keep even fragile info
	    -> Subst 
	    -> IdInfo
	    -> Maybe IdInfo
-- The keep_fragile flag is True when we are running a simple expression
-- substitution that preserves all structure, so that arity and occurrence
-- info are unaffected.  The False state is used more often.
--
-- Substitute the 
--	rules
--	worker info
-- Zap the unfolding 
-- If keep_fragile then
--	keep OccInfo
--	keep Arity
-- else
--	keep only 'robust' OccInfo
--	zap Arity
-- 
-- Seq'ing on the returned IdInfo is enough to cause all the 
-- substitutions to happen completely

substIdInfo keep_fragile subst info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setOccInfo`    	  (if keep_occ then old_occ else NoOccInfo)
			       `setArityInfo`     (if keep_arity then old_arity else unknownArity)
			       `setSpecInfo`   	  substRules  subst old_rules
			       `setWorkerInfo` 	  substWorker subst old_wrkr
			       `setUnfoldingInfo` noUnfolding)
			-- setSpecInfo does a seq
			-- setWorkerInfo does a seq
  where
    nothing_to_do = keep_occ && keep_arity &&
		    isEmptyCoreRules old_rules &&
		    not (workerExists old_wrkr) &&
		    not (hasUnfolding (unfoldingInfo info))
    
    keep_occ   = keep_fragile || not (isFragileOcc old_occ)
    keep_arity = keep_fragile || old_arity == unknownArity
    old_arity = arityInfo info
    old_occ   = occInfo info
    old_rules = specInfo info
    old_wrkr  = workerInfo info

------------------
substIdType :: Subst -> Id -> Id
substIdType subst@(Subst in_scope id_env tv_env) id
  | isEmptyVarEnv tv_env || isEmptyVarSet (tyVarsOfType old_ty) = id
  | otherwise	= setIdType id (Type.substTy (TvSubst in_scope tv_env) old_ty)
		-- The tyVarsOfType is cheaper than it looks
		-- because we cache the free tyvars of the type
		-- in a Note in the id's type itself
  where
    old_ty = idType id

------------------
substWorker :: Subst -> WorkerInfo -> WorkerInfo
	-- Seq'ing on the returned WorkerInfo is enough to cause all the 
	-- substitutions to happen completely

substWorker subst NoWorker
  = NoWorker
substWorker subst (HasWorker w a)
  = case substId subst w of
	DoneId w1 _     -> HasWorker w1 a
	DoneEx (Var w1) -> HasWorker w1 a
	DoneEx other    -> WARN( not (exprIsTrivial other), text "substWorker: DoneEx" <+> ppr w )
			   NoWorker	-- Worker has got substituted away altogether
						-- This can happen if it's trivial, 
						-- via postInlineUnconditionally
	ContEx se1 e    -> WARN( True, text "substWorker: ContEx" <+> ppr w <+> ppr e)
			   NoWorker	-- Ditto
			
------------------
substUnfolding subst NoUnfolding     		 = NoUnfolding
substUnfolding subst (OtherCon cons) 		 = OtherCon cons
substUnfolding subst (CompulsoryUnfolding rhs)   = CompulsoryUnfolding (substExpr subst rhs)
substUnfolding subst (CoreUnfolding rhs t v w g) = CoreUnfolding (substExpr subst rhs) t v w g

------------------
substRules :: Subst -> CoreRules -> CoreRules
	-- Seq'ing on the returned CoreRules is enough to cause all the 
	-- substitutions to happen completely

substRules subst rules
 | isEmptySubst subst = rules

substRules subst (Rules rules rhs_fvs)
  = seqRules new_rules `seq` new_rules
  where
    new_rules = Rules (map do_subst rules) (substVarSet subst rhs_fvs)

    do_subst rule@(BuiltinRule _ _) = rule
    do_subst (Rule name act tpl_vars lhs_args rhs)
	= Rule name act tpl_vars' 
	       (map (substExpr subst') lhs_args)
	       (substExpr subst' rhs)
	where
	  (subst', tpl_vars') = substBndrs subst tpl_vars

------------------
substVarSet subst fvs 
  = foldVarSet (unionVarSet . subst_fv subst) emptyVarSet fvs
  where
    subst_fv subst fv 
	| isId fv = case substId subst fv of
			DoneId fv' _    -> unitVarSet fv'
			DoneEx expr     -> exprFreeVars expr
			ContEx se' expr -> substVarSet (setSubstEnv subst se') (exprFreeVars expr)
	| otherwise = case lookupTvSubst subst fv of
			    Nothing -> unitVarSet fv
			    Just ty -> substVarSet subst (tyVarsOfType ty)
\end{code}
