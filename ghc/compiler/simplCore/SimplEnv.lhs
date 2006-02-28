%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplEnv (
	InId, InBind, InExpr, InAlt, InArg, InType, InBinder,
	OutId, OutTyVar, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBinder,

	-- The simplifier mode
	setMode, getMode, 

	-- Switch checker
	SwitchChecker, SwitchResult(..), getSwitchChecker, getSimplIntSwitch,
	isAmongSimpl, intSwitchSet, switchIsOn,

	setEnclosingCC, getEnclosingCC,

	-- Environments
	SimplEnv, mkSimplEnv, extendIdSubst, extendTvSubst, 
	zapSubstEnv, setSubstEnv, 
	getInScope, setInScope, setInScopeSet, modifyInScope, addNewInScopeIds,
	getRules, refineSimplEnv,

	SimplSR(..), mkContEx, substId, 

	simplNonRecBndr, simplRecBndrs, simplLamBndr, simplLamBndrs, 
 	simplBinder, simplBinders, 
	substExpr, substTy,

	-- Floats
	FloatsWith, FloatsWithExpr,
  	Floats, emptyFloats, isEmptyFloats, unitFloat, addFloats, flattenFloats,
	allLifted, wrapFloats, floatBinds,
	addAuxiliaryBind,
    ) where

#include "HsVersions.h"

import SimplMonad	
import Id		( Id, idType, idOccInfo, idUnfolding, setIdUnfolding )
import IdInfo		( IdInfo, vanillaIdInfo, occInfo, setOccInfo, specInfo, setSpecInfo,
			  arityInfo, setArityInfo, workerInfo, setWorkerInfo, 
			  unfoldingInfo, setUnfoldingInfo, isEmptySpecInfo,
			  unknownArity, workerExists
			    )
import CoreSyn
import Unify		( TypeRefinement )
import Rules		( RuleBase )
import CoreUtils	( needsCaseBinding )
import CostCentre	( CostCentreStack, subsumedCCS )
import Var	
import VarEnv
import VarSet		( isEmptyVarSet )
import OrdList

import qualified CoreSubst	( Subst, mkSubst, substExpr, substSpec, substWorker )
import qualified Type		( substTy, substTyVarBndr )

import Type             ( Type, TvSubst(..), TvSubstEnv, composeTvSubst,
			  isUnLiftedType, seqType, tyVarsOfType )
import BasicTypes	( OccInfo(..), isFragileOcc )
import DynFlags		( SimplifierMode(..) )
import Util		( mapAccumL )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InBinder  = CoreBndr
type InId      = Id			-- Not yet cloned
type InType    = Type			-- Ditto
type InBind    = CoreBind
type InExpr    = CoreExpr
type InAlt     = CoreAlt
type InArg     = CoreArg

type OutBinder  = CoreBndr
type OutId	= Id			-- Cloned
type OutTyVar	= TyVar			-- Cloned
type OutType	= Type			-- Cloned
type OutBind	= CoreBind
type OutExpr	= CoreExpr
type OutAlt	= CoreAlt
type OutArg	= CoreArg
\end{code}

%************************************************************************
%*									*
\subsubsection{The @SimplEnv@ type}
%*									*
%************************************************************************


\begin{code}
data SimplEnv
  = SimplEnv {
	seMode 	    :: SimplifierMode,
	seChkr      :: SwitchChecker,
	seCC        :: CostCentreStack,	-- The enclosing CCS (when profiling)

	-- Rules from other modules
	seExtRules  :: RuleBase,

	-- The current set of in-scope variables
	-- They are all OutVars, and all bound in this module
	seInScope   :: InScopeSet,	-- OutVars only

	-- The current substitution
	seTvSubst   :: TvSubstEnv,	-- InTyVar |--> OutType
	seIdSubst   :: SimplIdSubst	-- InId    |--> OutExpr
    }

type SimplIdSubst = IdEnv SimplSR	-- IdId |--> OutExpr

data SimplSR
  = DoneEx OutExpr		-- Completed term
  | DoneId OutId OccInfo	-- Completed term variable, with occurrence info
  | ContEx TvSubstEnv	 	-- A suspended substitution
	   SimplIdSubst
	   InExpr 	 
\end{code}


seInScope: 
	The in-scope part of Subst includes *all* in-scope TyVars and Ids
	The elements of the set may have better IdInfo than the
	occurrences of in-scope Ids, and (more important) they will
	have a correctly-substituted type.  So we use a lookup in this
	set to replace occurrences

	The Ids in the InScopeSet are replete with their Rules,
	and as we gather info about the unfolding of an Id, we replace
	it in the in-scope set.  

	The in-scope set is actually a mapping OutVar -> OutVar, and
	in case expressions we sometimes bind 

seIdSubst:
	The substitution is *apply-once* only, because InIds and OutIds can overlap.
	For example, we generally omit mappings 
		a77 -> a77
	from the substitution, when we decide not to clone a77, but it's quite 
	legitimate to put the mapping in the substitution anyway.
	
	Indeed, we do so when we want to pass fragile OccInfo to the
	occurrences of the variable; we add a substitution
		x77 -> DoneId x77 occ
	to record x's occurrence information.]

	Furthermore, consider 
		let x = case k of I# x77 -> ... in
		let y = case k of I# x77 -> ... in ...
	and suppose the body is strict in both x and y.  Then the simplifier
	will pull the first (case k) to the top; so the second (case k) will
	cancel out, mapping x77 to, well, x77!  But one is an in-Id and the 
	other is an out-Id. 

	Of course, the substitution *must* applied! Things in its domain 
	simply aren't necessarily bound in the result.

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


Note [GADT type refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come to a GADT pattern match that refines the in-scope types, we
  a) Refine the types of the Ids in the in-scope set, seInScope.  
     For exmaple, consider
	data T a where
		Foo :: T (Bool -> Bool)

	(\ (x::T a) (y::a) -> case x of { Foo -> y True }

     Technically this is well-typed, but exprType will barf on the
     (y True) unless we refine the type on y's occurrence.

  b) Refine the range of the type substitution, seTvSubst. 
     Very similar reason to (a).

  NB: we don't refine the range of the SimplIdSubst, because it's always
  interpreted relative to the seInScope (see substId)

For (b) we need to be a little careful.  Specifically, we compose the refinement 
with the type substitution.  Suppose 
  The substitution was	  [a->b, b->a]
  and the refinement was  [b->Int]
  Then we want [a->Int, b->a]

But also if
  The substitution was	  [a->b]
  and the refinement was  [b->Int]
  Then we want [a->Int, b->Int]
	becuase b might be both an InTyVar and OutTyVar


\begin{code}
mkSimplEnv :: SimplifierMode -> SwitchChecker -> RuleBase -> SimplEnv
mkSimplEnv mode switches rules
  = SimplEnv { seChkr = switches, seCC = subsumedCCS, 
	       seMode = mode, seInScope = emptyInScopeSet, 
	       seExtRules = rules,
	       seTvSubst = emptyVarEnv, seIdSubst = emptyVarEnv }
	-- The top level "enclosing CC" is "SUBSUMED".

---------------------
getSwitchChecker :: SimplEnv -> SwitchChecker
getSwitchChecker env = seChkr env

---------------------
getMode :: SimplEnv -> SimplifierMode
getMode env = seMode env

setMode :: SimplifierMode -> SimplEnv -> SimplEnv
setMode mode env = env { seMode = mode }

---------------------
getEnclosingCC :: SimplEnv -> CostCentreStack
getEnclosingCC env = seCC env

setEnclosingCC :: SimplEnv -> CostCentreStack -> SimplEnv
setEnclosingCC env cc = env {seCC = cc}

---------------------
extendIdSubst :: SimplEnv -> Id -> SimplSR -> SimplEnv
extendIdSubst env@(SimplEnv {seIdSubst = subst}) var res
  = env {seIdSubst = extendVarEnv subst var res}

extendTvSubst :: SimplEnv -> TyVar -> Type -> SimplEnv
extendTvSubst env@(SimplEnv {seTvSubst = subst}) var res
  = env {seTvSubst = extendVarEnv subst var res}

---------------------
getInScope :: SimplEnv -> InScopeSet
getInScope env = seInScope env

setInScopeSet :: SimplEnv -> InScopeSet -> SimplEnv
setInScopeSet env in_scope = env {seInScope = in_scope}

setInScope :: SimplEnv -> SimplEnv -> SimplEnv
setInScope env env_with_in_scope = setInScopeSet env (getInScope env_with_in_scope)

addNewInScopeIds :: SimplEnv -> [CoreBndr] -> SimplEnv
	-- The new Ids are guaranteed to be freshly allocated
addNewInScopeIds env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst }) vs
  = env { seInScope = in_scope `extendInScopeSetList` vs,
	  seIdSubst = id_subst `delVarEnvList` vs }
	-- Why delete?  Consider 
	--	let x = a*b in (x, \x -> x+3)
	-- We add [x |-> a*b] to the substitution, but we must
	-- *delete* it from the substitution when going inside
	-- the (\x -> ...)!

modifyInScope :: SimplEnv -> CoreBndr -> CoreBndr -> SimplEnv
modifyInScope env@(SimplEnv {seInScope = in_scope}) v v'
  = env {seInScope = modifyInScopeSet in_scope v v'}

---------------------
zapSubstEnv :: SimplEnv -> SimplEnv
zapSubstEnv env = env {seTvSubst = emptyVarEnv, seIdSubst = emptyVarEnv}

setSubstEnv :: SimplEnv -> TvSubstEnv -> SimplIdSubst -> SimplEnv
setSubstEnv env tvs ids = env { seTvSubst = tvs, seIdSubst = ids }

mkContEx :: SimplEnv -> InExpr -> SimplSR
mkContEx (SimplEnv { seTvSubst = tvs, seIdSubst = ids }) e = ContEx tvs ids e

isEmptySimplSubst :: SimplEnv -> Bool
isEmptySimplSubst (SimplEnv { seTvSubst = tvs, seIdSubst = ids })
  = isEmptyVarEnv tvs && isEmptyVarEnv ids

---------------------
getRules :: SimplEnv -> RuleBase
getRules = seExtRules
\end{code}

		GADT stuff

Given an idempotent substitution, generated by the unifier, use it to 
refine the environment

\begin{code}
refineSimplEnv :: SimplEnv -> TypeRefinement -> SimplEnv
-- The TvSubstEnv is the refinement, and it refines OutTyVars into OutTypes
refineSimplEnv env@(SimplEnv { seTvSubst = tv_subst, seInScope = in_scope })
	       (refine_tv_subst, all_bound_here)
  = env { seTvSubst = composeTvSubst in_scope refine_tv_subst tv_subst,
	  seInScope = in_scope' }
  where
    in_scope' 
	| all_bound_here = in_scope
		-- The tvs are the tyvars bound here.  If only they 
		-- are refined, there's no need to do anything 
	| otherwise = mapInScopeSet refine_id in_scope

    refine_id v 	-- Only refine its type; any rules will get
			-- refined if they are used (I hope)
	| isId v    = setIdType v (Type.substTy refine_subst (idType v))
	| otherwise = v
    refine_subst = TvSubst in_scope refine_tv_subst
\end{code}

%************************************************************************
%*									*
		Substitution of Vars
%*									*
%************************************************************************


\begin{code}
substId :: SimplEnv -> Id -> SimplSR
substId (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v 
  | not (isLocalId v) 
  = DoneId v NoOccInfo
  | otherwise	-- A local Id
  = case lookupVarEnv ids v of
	Just (DoneId v occ) -> DoneId (refine v) occ
	Just res	    -> res
	Nothing		    -> let v' = refine v
			       in DoneId v' (idOccInfo v')
		-- We don't put LoopBreakers in the substitution (unless then need
		-- to be cloned for name-clash rasons), so the idOccInfo is
		-- very important!  If isFragileOcc returned True for
		-- loop breakers we could avoid this call, but at the expense
		-- of adding more to the substitution, and building new Ids
		-- a bit more often than really necessary
  where
	-- Get the most up-to-date thing from the in-scope set
	-- Even though it isn't in the substitution, it may be in
	-- the in-scope set with a different type (we only use the
	-- substitution if the unique changes).
    refine v = case lookupInScope in_scope v of
		 Just v' -> v'
		 Nothing -> WARN( True, ppr v ) v	-- This is an error!
\end{code}


%************************************************************************
%*									*
\section{Substituting an Id binder}
%*									*
%************************************************************************


These functions are in the monad only so that they can be made strict via seq.

\begin{code}
simplBinders, simplLamBndrs
	:: SimplEnv -> [InBinder] -> SimplM (SimplEnv, [OutBinder])
simplBinders  env bndrs = mapAccumLSmpl simplBinder  env bndrs
simplLamBndrs env bndrs = mapAccumLSmpl simplLamBndr env bndrs

-------------
simplBinder :: SimplEnv -> InBinder -> SimplM (SimplEnv, OutBinder)
-- Used for lambda and case-bound variables
-- Clone Id if necessary, substitute type
-- Return with IdInfo already substituted, but (fragile) occurrence info zapped
-- The substitution is extended only if the variable is cloned, because
-- we *don't* need to use it to track occurrence info.
simplBinder env bndr
  | isTyVar bndr  = do	{ let (env', tv) = substTyVarBndr env bndr
			; seqTyVar tv `seq` return (env', tv) }
  | otherwise     = do	{ let (env', id) = substIdBndr env bndr
			; seqId id `seq` return (env', id) }

-------------
simplLamBndr :: SimplEnv -> Var -> SimplM (SimplEnv, Var)
-- Used for lambda binders.  These sometimes have unfoldings added by
-- the worker/wrapper pass that must be preserved, becuase they can't
-- be reconstructed from context.  For example:
--	f x = case x of (a,b) -> fw a b x
--	fw a b x{=(a,b)} = ...
-- The "{=(a,b)}" is an unfolding we can't reconstruct otherwise.
simplLamBndr env bndr
  | not (isId bndr && hasSomeUnfolding old_unf) = simplBinder env bndr	-- Normal case
  | otherwise					= seqId id2 `seq` return (env', id2)
  where
    old_unf = idUnfolding bndr
    (env', id1) = substIdBndr env bndr
    id2 = id1 `setIdUnfolding` substUnfolding env old_unf

--------------
substIdBndr :: SimplEnv -> Id 	-- Substitition and Id to transform
	    -> (SimplEnv, Id)	-- Transformed pair

-- Returns with:
--	* Unique changed if necessary
--	* Type substituted
-- 	* Unfolding zapped
--	* Rules, worker, lbvar info all substituted 
--	* Fragile occurrence info zapped
--	* The in-scope set extended with the returned Id
--	* The substitution extended with a DoneId if unique changed
--	  In this case, the var in the DoneId is the same as the
--	  var returned

substIdBndr env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst})
	    old_id
  = (env { seInScope = in_scope `extendInScopeSet` new_id,
	   seIdSubst = new_subst }, new_id)
  where
	-- id1 is cloned if necessary
    id1 = uniqAway in_scope old_id

	-- id2 has its type zapped
    id2 = substIdType env id1

	-- new_id has the final IdInfo
    subst  = mkCoreSubst env
    new_id = maybeModifyIdInfo (substIdInfo subst) id2

	-- Extend the substitution if the unique has changed
	-- See the notes with substTyVarBndr for the delSubstEnv
    new_subst | new_id /= old_id
	      = extendVarEnv id_subst old_id (DoneId new_id (idOccInfo old_id))
	      | otherwise 
	      = delVarEnv id_subst old_id
\end{code}


\begin{code}
seqTyVar :: TyVar -> ()
seqTyVar b = b `seq` ()

seqId :: Id -> ()
seqId id = seqType (idType id)	`seq`
	   idInfo id		`seq`
	   ()

seqIds :: [Id] -> ()
seqIds []       = ()
seqIds (id:ids) = seqId id `seq` seqIds ids
\end{code}


%************************************************************************
%*									*
		Let bindings
%*									*
%************************************************************************

Simplifying let binders
~~~~~~~~~~~~~~~~~~~~~~~
Rename the binders if necessary, and substitute their IdInfo,
and re-attach it.  The resulting binders therefore have all
their RULES, which is important in a mutually recursive group

We must transfer the IdInfo of the original binder to the new binder.
This is crucial, to preserve
	strictness
	rules
	worker info
etc.  To do this we must apply the current substitution, 
which incorporates earlier substitutions in this very letrec group.

NB 1. We do this *before* processing the RHS of the binder, so that
its substituted rules are visible in its own RHS.
This is important.  Manuel found cases where he really, really
wanted a RULE for a recursive function to apply in that function's
own right-hand side.

NB 2: We do not transfer the arity (see Subst.substIdInfo)
The arity of an Id should not be visible
in its own RHS, else we eta-reduce
	f = \x -> f x
to
	f = f
which isn't sound.  And it makes the arity in f's IdInfo greater than
the manifest arity, which isn't good.
The arity will get added later.

NB 3: It's important that we *do* transer the loop-breaker OccInfo,
because that's what stops the Id getting inlined infinitely, in the body
of the letrec.

NB 4: does no harm for non-recursive bindings

\begin{code}
simplNonRecBndr :: SimplEnv -> InBinder -> SimplM (SimplEnv, OutBinder)
simplNonRecBndr env id
  = do	{ let subst = mkCoreSubst env
	      (env1, id1) = substLetIdBndr subst env id
	; seqId id1 `seq` return (env1, id1) }

---------------
simplRecBndrs :: SimplEnv -> [InBinder] -> SimplM (SimplEnv, [OutBinder])
simplRecBndrs env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst }) ids
  = do	{ let 	-- Notice the knot here; we need the result to make 
		-- a substitution for the IdInfo.  c.f. CoreSubst.substIdBndr
	       (env1, ids1) = mapAccumL (substLetIdBndr subst) env ids
	       subst = mkCoreSubst env1
	; seqIds ids1 `seq` return (env1, ids1) }

---------------
substLetIdBndr :: CoreSubst.Subst	-- Substitution to use for the IdInfo (knot-tied)
	       -> SimplEnv -> InBinder 	-- Env and binder to transform
	       -> (SimplEnv, OutBinder)
-- C.f. CoreSubst.substIdBndr
-- Clone Id if necessary, substitute its type
-- Return an Id with completely zapped IdInfo
-- 	[A subsequent substIdInfo will restore its IdInfo]
-- Augment the subtitution 
--	if the unique changed, *or* 
--	if there's interesting occurrence info
--
-- The difference between SimplEnv.substIdBndr above is
--	a) the rec_subst
--	b) the hackish "interesting occ info" part (due to vanish)

substLetIdBndr rec_subst env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst }) old_id
  = (env { seInScope = in_scope `extendInScopeSet` new_id, 
	   seIdSubst = new_subst }, new_id)
  where
    id1	   = uniqAway in_scope old_id
    id2    = substIdType env id1
    new_id = maybeModifyIdInfo (substIdInfo rec_subst) id2

	-- Extend the substitution if the unique has changed,
	-- or there's some useful occurrence information
	-- See the notes with substTyVarBndr for the delSubstEnv
    occ_info = occInfo (idInfo old_id)
    new_subst | new_id /= old_id || isFragileOcc occ_info
	      = extendVarEnv id_subst old_id (DoneId new_id occ_info)
	      | otherwise 
	      = delVarEnv id_subst old_id

substIdInfo :: CoreSubst.Subst -> IdInfo -> Maybe IdInfo
-- Substitute the 
--	rules
--	worker info
-- Zap the unfolding 
-- Keep only 'robust' OccInfo
-- Zap Arity
-- 
-- Seq'ing on the returned IdInfo is enough to cause all the 
-- substitutions to happen completely

substIdInfo subst info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setOccInfo`    	  (if keep_occ then old_occ else NoOccInfo)
			       `setArityInfo`     (if keep_arity then old_arity else unknownArity)
			       `setSpecInfo`   	  CoreSubst.substSpec   subst old_rules
			       `setWorkerInfo` 	  CoreSubst.substWorker subst old_wrkr
			       `setUnfoldingInfo` noUnfolding)
			-- setSpecInfo does a seq
			-- setWorkerInfo does a seq
  where
    nothing_to_do = keep_occ && keep_arity &&
		    isEmptySpecInfo old_rules &&
		    not (workerExists old_wrkr) &&
		    not (hasUnfolding (unfoldingInfo info))
    
    keep_occ   = not (isFragileOcc old_occ)
    keep_arity = old_arity == unknownArity
    old_arity = arityInfo info
    old_occ   = occInfo info
    old_rules = specInfo info
    old_wrkr  = workerInfo info

------------------
substIdType :: SimplEnv -> Id -> Id
substIdType env@(SimplEnv { seInScope = in_scope,  seTvSubst = tv_env}) id
  | isEmptyVarEnv tv_env || isEmptyVarSet (tyVarsOfType old_ty) = id
  | otherwise	= setIdType id (Type.substTy (TvSubst in_scope tv_env) old_ty)
		-- The tyVarsOfType is cheaper than it looks
		-- because we cache the free tyvars of the type
		-- in a Note in the id's type itself
  where
    old_ty = idType id

------------------
substUnfolding env NoUnfolding     	       = NoUnfolding
substUnfolding env (OtherCon cons) 	       = OtherCon cons
substUnfolding env (CompulsoryUnfolding rhs)   = CompulsoryUnfolding (substExpr env rhs)
substUnfolding env (CoreUnfolding rhs t v w g) = CoreUnfolding (substExpr env rhs) t v w g
\end{code}


%************************************************************************
%*									*
		Impedence matching to type substitution
%*									*
%************************************************************************

\begin{code}
substTy :: SimplEnv -> Type -> Type 
substTy (SimplEnv { seInScope = in_scope, seTvSubst = tv_env }) ty
  = Type.substTy (TvSubst in_scope tv_env) ty

substTyVarBndr :: SimplEnv -> TyVar -> (SimplEnv, TyVar)
substTyVarBndr env@(SimplEnv { seInScope = in_scope, seTvSubst = tv_env }) tv
  = case Type.substTyVarBndr (TvSubst in_scope tv_env) tv of
	(TvSubst in_scope' tv_env', tv') 
	   -> (env { seInScope = in_scope', seTvSubst = tv_env'}, tv')

-- When substituting in rules etc we can get CoreSubst to do the work
-- But CoreSubst uses a simpler form of IdSubstEnv, so we must impedence-match
-- here.  I think the this will not usually result in a lot of work;
-- the substitutions are typically small, and laziness will avoid work in many cases.

mkCoreSubst  :: SimplEnv -> CoreSubst.Subst
mkCoreSubst (SimplEnv { seInScope = in_scope, seTvSubst = tv_env, seIdSubst = id_env })
  = mk_subst tv_env id_env
  where
    mk_subst tv_env id_env = CoreSubst.mkSubst in_scope tv_env (mapVarEnv fiddle id_env)

    fiddle (DoneEx e)       = e
    fiddle (DoneId v occ)   = Var v
    fiddle (ContEx tv id e) = CoreSubst.substExpr (mk_subst tv id) e

substExpr :: SimplEnv -> CoreExpr -> CoreExpr
substExpr env expr
  | isEmptySimplSubst env = expr
  | otherwise	          = CoreSubst.substExpr (mkCoreSubst env) expr
\end{code}


%************************************************************************
%*									*
\subsection{Floats}
%*									*
%************************************************************************

\begin{code}
type FloatsWithExpr = FloatsWith OutExpr
type FloatsWith a   = (Floats, a)
	-- We return something equivalent to (let b in e), but
	-- in pieces to avoid the quadratic blowup when floating 
	-- incrementally.  Comments just before simplExprB in Simplify.lhs

data Floats = Floats (OrdList OutBind) 
		     InScopeSet		-- Environment "inside" all the floats
		     Bool		-- True <=> All bindings are lifted

allLifted :: Floats -> Bool
allLifted (Floats _ _ is_lifted) = is_lifted

wrapFloats :: Floats -> OutExpr -> OutExpr
wrapFloats (Floats bs _ _) body = foldrOL Let body bs

isEmptyFloats :: Floats -> Bool
isEmptyFloats (Floats bs _ _) = isNilOL bs 

floatBinds :: Floats -> [OutBind]
floatBinds (Floats bs _ _) = fromOL bs

flattenFloats :: Floats -> Floats
-- Flattens into a single Rec group
flattenFloats (Floats bs is is_lifted) 
  = ASSERT2( is_lifted, ppr (fromOL bs) )
    Floats (unitOL (Rec (flattenBinds (fromOL bs)))) is is_lifted
\end{code}

\begin{code}
emptyFloats :: SimplEnv -> Floats
emptyFloats env = Floats nilOL (getInScope env) True

unitFloat :: SimplEnv -> OutId -> OutExpr -> Floats
-- A single non-rec float; extend the in-scope set
unitFloat env var rhs = Floats (unitOL (NonRec var rhs))
			       (extendInScopeSet (getInScope env) var)
			       (not (isUnLiftedType (idType var)))

addFloats :: SimplEnv -> Floats 
	  -> (SimplEnv -> SimplM (FloatsWith a))
	  -> SimplM (FloatsWith a)
addFloats env (Floats b1 is1 l1) thing_inside
  | isNilOL b1 
  = thing_inside env
  | otherwise
  = thing_inside (setInScopeSet env is1)	`thenSmpl` \ (Floats b2 is2 l2, res) ->
    returnSmpl (Floats (b1 `appOL` b2) is2 (l1 && l2), res)

addLetBind :: OutBind -> Floats -> Floats
addLetBind bind (Floats binds in_scope lifted) 
  = Floats (bind `consOL` binds) in_scope (lifted && is_lifted_bind bind)

is_lifted_bind (Rec _)      = True
is_lifted_bind (NonRec b r) = not (isUnLiftedType (idType b))

-- addAuxiliaryBind 	* takes already-simplified things (bndr and rhs)
--			* extends the in-scope env
--			* assumes it's a let-bindable thing
addAuxiliaryBind :: SimplEnv -> OutBind
		 -> (SimplEnv -> SimplM (FloatsWith a))
	 	 -> SimplM (FloatsWith a)
	-- Extends the in-scope environment as well as wrapping the bindings
addAuxiliaryBind env bind thing_inside
  = ASSERT( case bind of { NonRec b r -> not (needsCaseBinding (idType b) r) ; Rec _ -> True } )
    thing_inside (addNewInScopeIds env (bindersOf bind))	`thenSmpl` \ (floats, x) ->
    returnSmpl (addLetBind bind floats, x)
\end{code}


