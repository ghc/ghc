%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module Subst (
	-- In-scope set
	InScopeSet, emptyInScopeSet, mkInScopeSet,
	extendInScopeSet, extendInScopeSetList,
	lookupInScope, elemInScopeSet, uniqAway,


	-- Substitution stuff
	Subst, TyVarSubst, IdSubst,
	emptySubst, mkSubst, substEnv, substInScope,
	lookupSubst, lookupIdSubst, isEmptySubst, extendSubst, extendSubstList,
	zapSubstEnv, setSubstEnv, 
	setInScope, 
	extendInScope, extendInScopeList, extendNewInScope, extendNewInScopeList, 
	isInScope, modifyInScope,

	bindSubst, unBindSubst, bindSubstList, unBindSubstList,

	-- Binders
	simplBndr, simplBndrs, simplLetId, simplLamBndr, simplIdInfo,
	substAndCloneId, substAndCloneIds, substAndCloneRecIds,

	-- Type stuff
	mkTyVarSubst, mkTopTyVarSubst, 
	substTyWith, substTy, substTheta, deShadowTy,

	-- Expression stuff
	substExpr, substRules
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_PprStyle_Debug )
import CoreSyn		( Expr(..), Bind(..), Note(..), CoreExpr,
			  CoreRules(..), CoreRule(..), 
			  isEmptyCoreRules, seqRules, hasUnfolding, noUnfolding, hasSomeUnfolding,
			  Unfolding(..)
			)
import CoreFVs		( exprFreeVars )
import TypeRep		( Type(..), TyNote(..) )  -- friend
import Type		( ThetaType, SourceType(..), PredType,
			  tyVarsOfType, tyVarsOfTypes, mkAppTy, 
			)
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
			  WorkerInfo(..), workerExists, workerInfo, setWorkerInfo, WorkerInfo,
                          lbvarInfo, LBVarInfo(..), setLBVarInfo, hasNoLBVarInfo
			)
import BasicTypes	( OccInfo(..) )
import Unique		( Unique, Uniquable(..), deriveUnique )
import UniqSet		( elemUniqSet_Directly )
import UniqSupply	( UniqSupply, uniqFromSupply, uniqsFromSupply )
import Var		( Var, Id, TyVar, isTyVar )
import Outputable
import PprCore		()		-- Instances
import UniqFM		( ufmToList )	-- Yuk (add a new op to VarEnv)
import Util		( mapAccumL, foldl2, seqList )
import FastTypes
\end{code}


%************************************************************************
%*									*
\subsection{The in-scope set}
%*									*
%************************************************************************

\begin{code}
data InScopeSet = InScope (VarEnv Var) FastInt
	-- The Int# is a kind of hash-value used by uniqAway
	-- For example, it might be the size of the set
	-- INVARIANT: it's not zero; we use it as a multiplier in uniqAway

emptyInScopeSet :: InScopeSet
emptyInScopeSet = InScope emptyVarSet 1#

mkInScopeSet :: VarEnv Var -> InScopeSet
mkInScopeSet in_scope = InScope in_scope 1#

extendInScopeSet :: InScopeSet -> Var -> InScopeSet
extendInScopeSet (InScope in_scope n) v = InScope (extendVarEnv in_scope v v) (n +# 1#)

extendInScopeSetList :: InScopeSet -> [Var] -> InScopeSet
extendInScopeSetList (InScope in_scope n) vs
   = InScope (foldl (\s v -> extendVarEnv s v v) in_scope vs)
		    (n +# iUnbox (length vs))

modifyInScopeSet :: InScopeSet -> Var -> Var -> InScopeSet
-- Exploit the fact that the in-scope "set" is really a map
-- 	Make old_v map to new_v
modifyInScopeSet (InScope in_scope n) old_v new_v = InScope (extendVarEnv in_scope old_v new_v) (n +# 1#)

delInScopeSet :: InScopeSet -> Var -> InScopeSet
delInScopeSet (InScope in_scope n) v = InScope (in_scope `delVarEnv` v) n

elemInScopeSet :: Var -> InScopeSet -> Bool
elemInScopeSet v (InScope in_scope n) = v `elemVarEnv` in_scope

lookupInScope :: InScopeSet -> Var -> Var
-- It's important to look for a fixed point
-- When we see (case x of y { I# v -> ... })
-- we add  [x -> y] to the in-scope set (Simplify.simplCaseBinder).
-- When we lookup up an occurrence of x, we map to y, but then
-- we want to look up y in case it has acquired more evaluation information by now.
lookupInScope (InScope in_scope n) v 
  = go v
  where
    go v = case lookupVarEnv in_scope v of
		Just v' | v == v'   -> v'	-- Reached a fixed point
			| otherwise -> go v'
		Nothing		    -> WARN( mustHaveLocalBinding v, ppr v )
				       v
\end{code}

\begin{code}
uniqAway :: InScopeSet -> Var -> Var
-- (uniqAway in_scope v) finds a unique that is not used in the
-- in-scope set, and gives that to v.  It starts with v's current unique, of course,
-- in the hope that it won't have to change it, nad thereafter uses a combination
-- of that and the hash-code found in the in-scope set
uniqAway (InScope set n) var
  | not (var `elemVarSet` set) = var				-- Nothing to do
  | otherwise		       = try 1#
  where
    orig_unique = getUnique var
    try k 
#ifdef DEBUG
	  | k ># 1000#
	  = pprPanic "uniqAway loop:" (ppr (iBox k) <+> text "tries" <+> ppr var <+> int (iBox n)) 
#endif			    
	  | uniq `elemUniqSet_Directly` set = try (k +# 1#)
#ifdef DEBUG
	  | opt_PprStyle_Debug && k ># 3#
	  = pprTrace "uniqAway:" (ppr (iBox k) <+> text "tries" <+> ppr var <+> int (iBox n)) 
	    setVarUnique var uniq
#endif			    
	  | otherwise = setVarUnique var uniq
	  where
	    uniq = deriveUnique orig_unique (iBox (n *# k))
\end{code}


%************************************************************************
%*									*
\subsection{Substitutions}
%*									*
%************************************************************************

\begin{code}
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

type IdSubst    = Subst
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

-- ToDo: add an ASSERT that fvs(subst-result) is already in the in-scope set
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
	Just (DoneId v' occ) -> DoneId (lookupInScope in_scope v') occ
	Just res	     -> res
	Nothing  	     -> DoneId v' (idOccInfo v')
				-- We don't use DoneId for LoopBreakers, so the idOccInfo is
				-- very important!  If isFragileOcc returned True for
				-- loop breakers we could avoid this call, but at the expense
				-- of adding more to the substitution, and building new Ids
				-- in substId a bit more often than really necessary
			     where
				    v' = lookupInScope in_scope v

isInScope :: Var -> Subst -> Bool
isInScope v (Subst in_scope _) = v `elemInScopeSet` in_scope

modifyInScope :: Subst -> Var -> Var -> Subst
modifyInScope (Subst in_scope env) old_v new_v = Subst (modifyInScopeSet in_scope old_v new_v) env
	-- make old_v map to new_v

extendInScope :: Subst -> Var -> Subst
	-- Add a new variable as in-scope
	-- Remember to delete any existing binding in the substitution!
extendInScope (Subst in_scope env) v = Subst (in_scope `extendInScopeSet` v)
					     (env `delSubstEnv` v)

extendInScopeList :: Subst -> [Var] -> Subst
extendInScopeList (Subst in_scope env) vs = Subst (extendInScopeSetList in_scope vs)
						  (delSubstEnvList env vs)

-- The "New" variants are guaranteed to be adding freshly-allocated variables
-- It's not clear that the gain (not needing to delete it from the substitution)
-- is worth the extra proof obligation
extendNewInScope :: Subst -> Var -> Subst
extendNewInScope (Subst in_scope env) v = Subst (in_scope `extendInScopeSet` v) env

extendNewInScopeList :: Subst -> [Var] -> Subst
extendNewInScopeList (Subst in_scope env) vs = Subst (in_scope `extendInScopeSetList` vs) env

-------------------------------
bindSubst :: Subst -> Var -> Var -> Subst
-- Extend with a substitution, v1 -> Var v2
-- and extend the in-scopes with v2
bindSubst (Subst in_scope env) old_bndr new_bndr
  = Subst (in_scope `extendInScopeSet` new_bndr)
	  (extendSubstEnv env old_bndr subst_result)
  where
    subst_result | isId old_bndr = DoneEx (Var new_bndr)
		 | otherwise	 = DoneTy (TyVarTy new_bndr)

unBindSubst :: Subst -> Var -> Var -> Subst
-- Reverse the effect of bindSubst
-- If old_bndr was already in the substitution, this doesn't quite work
unBindSubst (Subst in_scope env) old_bndr new_bndr
  = Subst (in_scope `delInScopeSet` new_bndr) (delSubstEnv env old_bndr)

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

Pretty printing, for debugging only

\begin{code}
instance Outputable SubstResult where
  ppr (DoneEx e)   = ptext SLIT("DoneEx") <+> ppr e
  ppr (DoneId v _) = ptext SLIT("DoneId") <+> ppr v
  ppr (ContEx _ e) = ptext SLIT("ContEx") <+> ppr e
  ppr (DoneTy t)   = ptext SLIT("DoneTy") <+> ppr t

instance Outputable SubstEnv where
  ppr se = brackets (fsep (punctuate comma (map ppr_elt (ufmToList (substEnvEnv se)))))
	where
	   ppr_elt (uniq,sr) = ppr uniq <+> ptext SLIT("->") <+> ppr sr

instance Outputable Subst where
  ppr (Subst (InScope in_scope _) se) 
	=  ptext SLIT("<InScope =") <+> braces   (fsep (map ppr (rngVarEnv in_scope)))
	$$ ptext SLIT(" Subst   =") <+> ppr se <> char '>'
\end{code}

%************************************************************************
%*									*
\subsection{Type substitution}
%*									*
%************************************************************************

\begin{code}
type TyVarSubst = Subst	-- TyVarSubst are expected to have range elements
	-- (We could have a variant of Subst, but it doesn't seem worth it.)

-- mkTyVarSubst generates the in-scope set from
-- the types given; but it's just a thunk so with a bit of luck
-- it'll never be evaluated
mkTyVarSubst :: [TyVar] -> [Type] -> Subst
mkTyVarSubst tyvars tys = Subst (mkInScopeSet (tyVarsOfTypes tys)) 
				(zipTyEnv tyvars tys)

-- mkTopTyVarSubst is called when doing top-level substitutions.
-- Here we expect that the free vars of the range of the
-- substitution will be empty.
mkTopTyVarSubst :: [TyVar] -> [Type] -> Subst
mkTopTyVarSubst tyvars tys = Subst emptyInScopeSet (zipTyEnv tyvars tys)

zipTyEnv tyvars tys
#ifdef DEBUG
  | length tyvars /= length tys
  = pprTrace "mkTopTyVarSubst" (ppr tyvars $$ ppr tys) emptySubstEnv
  | otherwise
#endif
  = zip_ty_env tyvars tys emptySubstEnv

-- Later substitutions in the list over-ride earlier ones
zip_ty_env []       []       env = env
zip_ty_env (tv:tvs) (ty:tys) env = zip_ty_env tvs tys (extendSubstEnv env tv (DoneTy ty))
	-- There used to be a special case for when 
	--	ty == TyVarTy tv
	-- (a not-uncommon case) in which case the substitution was dropped.
	-- But the type-tidier changes the print-name of a type variable without
	-- changing the unique, and that led to a bug.   Why?  Pre-tidying, we had 
	-- a type {Foo t}, where Foo is a one-method class.  So Foo is really a newtype.
	-- And it happened that t was the type variable of the class.  Post-tiding, 
	-- it got turned into {Foo t2}.  The ext-core printer expanded this using
	-- sourceTypeRep, but that said "Oh, t == t2" because they have the same unique,
	-- and so generated a rep type mentioning t not t2.  
	--
	-- Simplest fix is to nuke the "optimisation"
\end{code}

substTy works with general Substs, so that it can be called from substExpr too.

\begin{code}
substTyWith :: [TyVar] -> [Type] -> Type -> Type
substTyWith tvs tys = substTy (mkTyVarSubst tvs tys)

substTy :: Subst -> Type  -> Type
substTy subst ty | isEmptySubst subst = ty
	         | otherwise	      = subst_ty subst ty

deShadowTy :: Type -> Type		-- Remove any shadowing from the type
deShadowTy ty = subst_ty emptySubst ty

substTheta :: TyVarSubst -> ThetaType -> ThetaType
substTheta subst theta
  | isEmptySubst subst = theta
  | otherwise	       = map (substPred subst) theta

substPred :: TyVarSubst -> PredType -> PredType
substPred = substSourceType

substSourceType subst (IParam n ty)     = IParam n (subst_ty subst ty)
substSourceType subst (ClassP clas tys) = ClassP clas (map (subst_ty subst) tys)
substSourceType subst (NType  tc   tys) = NType  tc   (map (subst_ty subst) tys)

subst_ty subst ty
   = go ty
  where
    go (TyConApp tc tys)	   = let args = map go tys
				     in  args `seqList` TyConApp tc args

    go (SourceTy p)  		   = SourceTy $! (substSourceType subst p)

    go (NoteTy (SynNote ty1) ty2)  = NoteTy (SynNote $! (go ty1)) $! (go ty2)
    go (NoteTy (FTVNote _) ty2)    = go ty2		-- Discard the free tyvar note

    go (FunTy arg res)   	   = (FunTy $! (go arg)) $! (go res)
    go (AppTy fun arg)   	   = mkAppTy (go fun) $! (go arg)
    go ty@(TyVarTy tv)   	   = case (lookupSubst subst tv) of
	       				Nothing 	   -> ty
       					Just (DoneTy ty')  -> ty'
					
    go (ForAllTy tv ty)		   = case substTyVar subst tv of
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
  = (Subst (in_scope `extendInScopeSet` new_var)
	   (delSubstEnv env old_var),
     new_var)

  | otherwise	-- The new binder is in scope so
		-- we'd better rename it away from the in-scope variables
		-- Extending the substitution to do this renaming also
		-- has the (correct) effect of discarding any existing
		-- substitution for that variable
  = (Subst (in_scope `extendInScopeSet` new_var) 
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
  | isTyVar bndr  = substTyVar subst bndr
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

simplLetId subst@(Subst in_scope env) old_id
  = (Subst (in_scope `extendInScopeSet` new_id) new_env, new_id)
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
	    = extendSubstEnv env old_id (DoneId new_id occ_info)
	    | otherwise 
	    = delSubstEnv env old_id

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
-- to do so else lose useful occ info in rules.  Hence the calls to 
-- simpl_id with keepOccInfo

substBndr :: Subst -> Var -> (Subst, Var)
substBndr subst bndr
  | isTyVar bndr  = substTyVar subst bndr
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

keepOccInfo occ = False	-- Never fragile
\end{code}


\begin{code}
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

subst_id keep_fragile rec_subst subst@(Subst in_scope env) old_id
  = (Subst (in_scope `extendInScopeSet` new_id) new_env, new_id)
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
	    = extendSubstEnv env old_id (DoneId new_id (idOccInfo old_id))
	    | otherwise 
	    = delSubstEnv env old_id
\end{code}

Now a variant that unconditionally allocates a new unique.
It also unconditionally zaps the OccInfo.

\begin{code}
subst_clone_id :: Subst			-- Substitution to use (lazily) for the rules and worker
	       -> Subst -> (Id, Unique)	-- Substitition and Id to transform
	       -> (Subst, Id)		-- Transformed pair

subst_clone_id rec_subst subst@(Subst in_scope env) (old_id, uniq)
  = (Subst (in_scope `extendInScopeSet` new_id) new_env, new_id)
  where
    id1	 = setVarUnique old_id uniq
    id2  = substIdType subst id1

    new_id  = maybeModifyIdInfo (substIdInfo False rec_subst) id2
    new_env = extendSubstEnv env old_id (DoneId new_id NoOccInfo)

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
substAndCloneId subst@(Subst in_scope env) us old_id
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
--	LBVar info
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
substIdType subst@(Subst in_scope env) id
  |  noTypeSubst env || isEmptyVarSet (tyVarsOfType old_ty) = id
  | otherwise						    = setIdType id (substTy subst old_ty)
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
  = case lookupIdSubst subst w of
	(DoneId w1 _)     -> HasWorker w1 a
	(DoneEx (Var w1)) -> HasWorker w1 a
	(DoneEx other)    -> WARN( True, text "substWorker: DoneEx" <+> ppr w )
				  NoWorker	-- Worker has got substituted away altogether
	(ContEx se1 e)    -> WARN( True, text "substWorker: ContEx" <+> ppr w <+> ppr e)
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
    subst_fv subst fv = case lookupIdSubst subst fv of
			    DoneId fv' _    -> unitVarSet fv'
			    DoneEx expr	    -> exprFreeVars expr
			    DoneTy ty  	    -> tyVarsOfType ty 
			    ContEx se' expr -> substVarSet (setSubstEnv subst se') (exprFreeVars expr)
\end{code}
