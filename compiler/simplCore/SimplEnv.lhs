%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplEnv (
	InId, InBind, InExpr, InAlt, InArg, InType, InBndr,
	OutId, OutTyVar, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBndr,
        InCoercion, OutCoercion,

	isStrictBndr,

	-- The simplifier mode
	setMode, getMode, 

	-- Switch checker
	SwitchChecker, SwitchResult(..), getSwitchChecker, getSimplIntSwitch,
	isAmongSimpl, intSwitchSet, switchIsOn,

	setEnclosingCC, getEnclosingCC,

	-- Environments
	SimplEnv(..), pprSimplEnv,	-- Temp not abstract
	mkSimplEnv, extendIdSubst, SimplEnv.extendTvSubst, 
	zapSubstEnv, setSubstEnv, 
	getInScope, setInScope, setInScopeSet, modifyInScope, addNewInScopeIds,
	getRules, 

	SimplSR(..), mkContEx, substId, lookupRecBndr,

	simplNonRecBndr, simplRecBndrs, simplLamBndr, simplLamBndrs, 
 	simplBinder, simplBinders, addLetIdInfo,
	substExpr, substTy, 

	-- Floats
  	Floats, emptyFloats, isEmptyFloats, addNonRec, addFloats, 
	wrapFloats, floatBinds, setFloats, canFloat, zapFloats, addRecFloats,
	getFloats
    ) where

#include "HsVersions.h"

import SimplMonad	
import IdInfo
import CoreSyn
import Rules
import CoreUtils
import CoreFVs
import CostCentre
import Var
import VarEnv
import VarSet
import OrdList
import Id
import NewDemand
import qualified CoreSubst	( Subst, mkSubst, substExpr, substSpec, substWorker )
import qualified Type		( substTy, substTyVarBndr )
import Type hiding		( substTy, substTyVarBndr )
import Coercion
import BasicTypes	
import DynFlags
import Util
import UniqFM
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-types]{Type declarations}
%*									*
%************************************************************************

\begin{code}
type InBndr     = CoreBndr
type InId       = Id			-- Not yet cloned
type InType     = Type			-- Ditto
type InBind     = CoreBind
type InExpr     = CoreExpr
type InAlt      = CoreAlt
type InArg      = CoreArg
type InCoercion = Coercion

type OutBndr     = CoreBndr
type OutId	 = Id			-- Cloned
type OutTyVar	 = TyVar		-- Cloned
type OutType	 = Type			-- Cloned
type OutCoercion = Coercion
type OutBind	 = CoreBind
type OutExpr	 = CoreExpr
type OutAlt	 = CoreAlt
type OutArg	 = CoreArg
\end{code}

\begin{code}
isStrictBndr :: Id -> Bool
isStrictBndr bndr
  = ASSERT2( isId bndr, ppr bndr )
    isStrictDmd (idNewDemandInfo bndr) || isStrictType (idType bndr)
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
		-- Includes all variables bound by seFloats
	seFloats    :: Floats,
		-- See Note [Simplifier floats]

	-- The current substitution
	seTvSubst   :: TvSubstEnv,	-- InTyVar |--> OutType
	seIdSubst   :: SimplIdSubst	-- InId    |--> OutExpr

    }

pprSimplEnv :: SimplEnv -> SDoc
-- Used for debugging; selective
pprSimplEnv env
  = vcat [ptext SLIT("TvSubst:") <+> ppr (seTvSubst env),
 	  ptext SLIT("IdSubst:") <+> ppr (seIdSubst env) ]

type SimplIdSubst = IdEnv SimplSR	-- IdId |--> OutExpr
	-- See Note [Extending the Subst] in CoreSubst

data SimplSR
  = DoneEx OutExpr		-- Completed term
  | DoneId OutId		-- Completed term variable
  | ContEx TvSubstEnv	 	-- A suspended substitution
	   SimplIdSubst
	   InExpr 	 

instance Outputable SimplSR where
  ppr (DoneEx e) = ptext SLIT("DoneEx") <+> ppr e
  ppr (DoneId v) = ptext SLIT("DoneId") <+> ppr v
  ppr (ContEx tv id e) = vcat [ptext SLIT("ContEx") <+> ppr e {-,
				ppr (filter_env tv), ppr (filter_env id) -}]
	-- where
	-- fvs = exprFreeVars e
	-- filter_env env = filterVarEnv_Directly keep env
	-- keep uniq _ = uniq `elemUFM_Directly` fvs
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

	Furthermore, consider 
		let x = case k of I# x77 -> ... in
		let y = case k of I# x77 -> ... in ...
	and suppose the body is strict in both x and y.  Then the simplifier
	will pull the first (case k) to the top; so the second (case k) will
	cancel out, mapping x77 to, well, x77!  But one is an in-Id and the 
	other is an out-Id. 

	Of course, the substitution *must* applied! Things in its domain 
	simply aren't necessarily bound in the result.

* substId adds a binding (DoneId new_id) to the substitution if 
	the Id's unique has changed


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
mkSimplEnv :: SimplifierMode -> SwitchChecker -> RuleBase -> SimplEnv
mkSimplEnv mode switches rules
  = SimplEnv { seChkr = switches, seCC = subsumedCCS, 
	       seMode = mode, seInScope = emptyInScopeSet, 
	       seExtRules = rules, seFloats = emptyFloats,
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
-- Set the in-scope set, and *zap* the floats
setInScope env env_with_scope
  = env { seInScope = seInScope env_with_scope,
	  seFloats = emptyFloats }

setFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Set the in-scope set *and* the floats
setFloats env env_with_floats
  = env { seInScope = seInScope env_with_floats,
	  seFloats  = seFloats  env_with_floats }

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



%************************************************************************
%*									*
\subsection{Floats}
%*									*
%************************************************************************

Note [Simplifier floats]
~~~~~~~~~~~~~~~~~~~~~~~~~
The Floats is a bunch of bindings, classified by a FloatFlag.

  NonRec x (y:ys)	FltLifted
  Rec [(x,rhs)]		FltLifted
  NonRec x# (y +# 3)	FltOkSpec
  NonRec x# (a /# b)	FltCareful
  NonRec x* (f y)	FltCareful	-- Might fail or diverge
  NonRec x# (f y)	FltCareful	-- Might fail or diverge
			  (where f :: Int -> Int#)

\begin{code}
data Floats = Floats (OrdList OutBind) FloatFlag
	-- See Note [Simplifier floats]

data FloatFlag
  = FltLifted 	-- All bindings are lifted and lazy
		--  Hence ok to float to top level, or recursive

  | FltOkSpec	-- All bindings are FltLifted *or* 
		--	strict (perhaps because unlifted, 
		--	perhaps because of a strict binder),
		--	  *and* ok-for-speculation
		--  Hence ok to float out of the RHS 
		--  of a lazy non-recursive let binding
		--  (but not to top level, or into a rec group)

  | FltCareful	-- At least one binding is strict (or unlifted)
		--	and not guaranteed cheap
		--	Do not float these bindings out of a lazy let

instance Outputable Floats where
  ppr (Floats binds ff) = ppr ff $$ ppr (fromOL binds)

instance Outputable FloatFlag where
  ppr FltLifted = ptext SLIT("FltLifted")
  ppr FltOkSpec = ptext SLIT("FltOkSpec")
  ppr FltCareful = ptext SLIT("FltCareful")
   
andFF :: FloatFlag -> FloatFlag -> FloatFlag
andFF FltCareful _ 	    = FltCareful
andFF FltOkSpec  FltCareful = FltCareful
andFF FltOkSpec  flt	    = FltOkSpec
andFF FltLifted  flt	    = flt

classifyFF :: CoreBind -> FloatFlag
classifyFF (Rec _) = FltLifted
classifyFF (NonRec bndr rhs) 
  | not (isStrictBndr bndr)  = FltLifted
  | exprOkForSpeculation rhs = FltOkSpec
  | otherwise		     = FltCareful

canFloat :: TopLevelFlag -> RecFlag -> Bool -> SimplEnv -> Bool
canFloat lvl rec str (SimplEnv {seFloats = Floats _ ff}) 
  = canFloatFlt lvl rec str ff

canFloatFlt :: TopLevelFlag -> RecFlag -> Bool -> FloatFlag -> Bool
canFloatFlt lvl rec str FltLifted  = True
canFloatFlt lvl rec str FltOkSpec  = isNotTopLevel lvl && isNonRec rec
canFloatFlt lvl rec str FltCareful = str && isNotTopLevel lvl && isNonRec rec
\end{code}


\begin{code}
emptyFloats :: Floats
emptyFloats = Floats nilOL FltLifted

unitFloat :: OutBind -> Floats
-- A single-binding float
unitFloat bind = Floats (unitOL bind) (classifyFF bind)

addNonRec :: SimplEnv -> OutId -> OutExpr -> SimplEnv
-- Add a non-recursive binding and extend the in-scope set
-- The latter is important; the binder may already be in the
-- in-scope set (although it might also have been created with newId)
-- but it may now have more IdInfo
addNonRec env id rhs
  = env { seFloats = seFloats env `addFlts` unitFloat (NonRec id rhs),
	  seInScope = extendInScopeSet (seInScope env) id }

addFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Add the floats for env2 to env1; 
-- *plus* the in-scope set for env2, which is bigger 
-- than that for env1
addFloats env1 env2 
  = env1 {seFloats = seFloats env1 `addFlts` seFloats env2,
	  seInScope = seInScope env2 }

addFlts :: Floats -> Floats -> Floats
addFlts (Floats bs1 l1) (Floats bs2 l2)
  = Floats (bs1 `appOL` bs2) (l1 `andFF` l2)

zapFloats :: SimplEnv -> SimplEnv
zapFloats env = env { seFloats = emptyFloats }

addRecFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Flattens the floats from env2 into a single Rec group,
-- prepends the floats from env1, and puts the result back in env2
-- This is all very specific to the way recursive bindings are
-- handled; see Simplify.simplRecBind
addRecFloats env1 env2@(SimplEnv {seFloats = Floats bs ff})
  = ASSERT2( case ff of { FltLifted -> True; other -> False }, ppr (fromOL bs) )
    env2 {seFloats = seFloats env1 `addFlts` unitFloat (Rec (flattenBinds (fromOL bs)))}

wrapFloats :: SimplEnv -> OutExpr -> OutExpr
wrapFloats env expr = wrapFlts (seFloats env) expr

wrapFlts :: Floats -> OutExpr -> OutExpr
-- Wrap the floats around the expression, using case-binding where necessary
wrapFlts (Floats bs _) body = foldrOL wrap body bs
  where
    wrap (Rec prs)    body = Let (Rec prs) body
    wrap (NonRec b r) body = bindNonRec b r body

getFloats :: SimplEnv -> [CoreBind]
getFloats (SimplEnv {seFloats = Floats bs _}) = fromOL bs

isEmptyFloats :: SimplEnv -> Bool
isEmptyFloats env = isEmptyFlts (seFloats env)

isEmptyFlts :: Floats -> Bool
isEmptyFlts (Floats bs _) = isNilOL bs 

floatBinds :: Floats -> [OutBind]
floatBinds (Floats bs _) = fromOL bs
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
  = DoneId v
  | otherwise	-- A local Id
  = case lookupVarEnv ids v of
	Just (DoneId v) -> DoneId (refine in_scope v)
	Just res	-> res
	Nothing		-> DoneId (refine in_scope v)
  where

	-- Get the most up-to-date thing from the in-scope set
	-- Even though it isn't in the substitution, it may be in
	-- the in-scope set with better IdInfo
refine in_scope v = case lookupInScope in_scope v of
			 Just v' -> v'
			 Nothing -> WARN( True, ppr v ) v	-- This is an error!

lookupRecBndr :: SimplEnv -> Id -> Id
-- Look up an Id which has been put into the envt by simplRecBndrs,
-- but where we have not yet done its RHS
lookupRecBndr (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v
  = case lookupVarEnv ids v of
	Just (DoneId v) -> v
	Just res	-> pprPanic "lookupRecBndr" (ppr v)
	Nothing		-> refine in_scope v
\end{code}


%************************************************************************
%*									*
\section{Substituting an Id binder}
%*									*
%************************************************************************


These functions are in the monad only so that they can be made strict via seq.

\begin{code}
simplBinders, simplLamBndrs
	:: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplBinders  env bndrs = mapAccumLSmpl simplBinder  env bndrs
simplLamBndrs env bndrs = mapAccumLSmpl simplLamBndr env bndrs

-------------
simplBinder :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
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
--
-- Exactly like CoreSubst.substIdBndr, except that the type of id_subst differs

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
    new_id = maybeModifyIdInfo (substIdInfo subst (idInfo old_id)) id2

	-- Extend the substitution if the unique has changed
	-- See the notes with substTyVarBndr for the delSubstEnv
	-- Also see Note [Extending the Subst] in CoreSubst
    new_subst | new_id /= old_id
	      = extendVarEnv id_subst old_id (DoneId new_id)
	      | otherwise 
	      = delVarEnv id_subst old_id
\end{code}

\begin{code}
------------------------------------
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
Rename the binders if necessary, 

\begin{code}
simplNonRecBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
simplNonRecBndr env id
  = do	{ let (env1, id1) = substLetIdBndr env id
	; seqId id1 `seq` return (env1, id1) }

---------------
simplRecBndrs :: SimplEnv -> [InBndr] -> SimplM SimplEnv
simplRecBndrs env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst }) ids
  = do	{ let (env1, ids1) = mapAccumL substLetIdBndr env ids
	; seqIds ids1 `seq` return env1 }

---------------
substLetIdBndr :: SimplEnv -> InBndr 	-- Env and binder to transform
	       -> (SimplEnv, OutBndr)
-- C.f. substIdBndr above
-- Clone Id if necessary, substitute its type
-- Return an Id with completely zapped IdInfo
-- 	[addLetIdInfo, below, will restore its IdInfo]
-- Augment the subtitution 
--	if the unique changed, *or* 
--	if there's interesting occurrence info

substLetIdBndr env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst }) old_id
  = (env { seInScope = in_scope `extendInScopeSet` new_id, 
	   seIdSubst = new_subst }, new_id)
  where
    id1	   = uniqAway in_scope old_id
    id2    = substIdType env id1
    new_id = setIdInfo id2 vanillaIdInfo

	-- Extend the substitution if the unique has changed,
	-- or there's some useful occurrence information
	-- See the notes with substTyVarBndr for the delSubstEnv
    new_subst | new_id /= old_id
	      = extendVarEnv id_subst old_id (DoneId new_id)
	      | otherwise 
	      = delVarEnv id_subst old_id
\end{code}

Add IdInfo back onto a let-bound Id
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

NB 2: ARITY.  We *do* transfer the arity.  This is important, so that
the arity of an Id is visible in its own RHS.  For example:
	f = \x. ....g (\y. f y)....
We can eta-reduce the arg to g, becuase f is a value.  But that 
needs to be visible.  

This interacts with the 'state hack' too:
	f :: Bool -> IO Int
	f = \x. case x of 
		  True  -> f y
		  False -> \s -> ...
Can we eta-expand f?  Only if we see that f has arity 1, and then we 
take advantage of the 'state hack' on the result of
(f y) :: State# -> (State#, Int) to expand the arity one more.

There is a disadvantage though.  Making the arity visible in the RHA
allows us to eta-reduce
	f = \x -> f x
to
	f = f
which technically is not sound.   This is very much a corner case, so
I'm not worried about it.  Another idea is to ensure that f's arity 
never decreases; its arity started as 1, and we should never eta-reduce
below that.

NB 3: OccInfo.  It's important that we *do* transer the loop-breaker
OccInfo, because that's what stops the Id getting inlined infinitely,
in the body of the letrec.

NB 4: does no harm for non-recursive bindings

NB 5: we can't do the addLetIdInfo part before *all* the RHSs because
	rec { f = g
	      h = ...
		RULE h Int = f
	}
Here, we'll do postInlineUnconditionally on f, and we must "see" that 
when substituting in h's RULE.  

\begin{code}
addLetIdInfo :: SimplEnv -> InBndr -> OutBndr -> (SimplEnv, OutBndr)
addLetIdInfo env in_id out_id
  = (modifyInScope env out_id final_id, final_id)
  where
    final_id = out_id `setIdInfo` new_info
    subst = mkCoreSubst env
    old_info = idInfo in_id
    new_info = case substIdInfo subst old_info of
	 	  Nothing       -> old_info
		  Just new_info -> new_info

substIdInfo :: CoreSubst.Subst -> IdInfo -> Maybe IdInfo
-- Substitute the 
--	rules
--	worker info
-- Zap the unfolding 
-- Keep only 'robust' OccInfo
--	     arity
-- 
-- Seq'ing on the returned IdInfo is enough to cause all the 
-- substitutions to happen completely

substIdInfo subst info
  | nothing_to_do = Nothing
  | otherwise     = Just (info `setOccInfo`    	  (if keep_occ then old_occ else NoOccInfo)
			       `setSpecInfo`   	  CoreSubst.substSpec   subst old_rules
			       `setWorkerInfo` 	  CoreSubst.substWorker subst old_wrkr
			       `setUnfoldingInfo` noUnfolding)
			-- setSpecInfo does a seq
			-- setWorkerInfo does a seq
  where
    nothing_to_do = keep_occ && 
		    isEmptySpecInfo old_rules &&
		    not (workerExists old_wrkr) &&
		    not (hasUnfolding (unfoldingInfo info))
    
    keep_occ  = not (isFragileOcc old_occ)
    old_occ   = occInfo info
    old_rules = specInfo info
    old_wrkr  = workerInfo info

------------------
substIdType :: SimplEnv -> Id -> Id
substIdType env@(SimplEnv { seInScope = in_scope,  seTvSubst = tv_env}) id
  | isEmptyVarEnv tv_env || isEmptyVarSet (tyVarsOfType old_ty) = id
  | otherwise	= Id.setIdType id (Type.substTy (TvSubst in_scope tv_env) old_ty)
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
    fiddle (DoneId v)       = Var v
    fiddle (ContEx tv id e) = CoreSubst.substExpr (mk_subst tv id) e

substExpr :: SimplEnv -> CoreExpr -> CoreExpr
substExpr env expr
  | isEmptySimplSubst env = expr
  | otherwise	          = CoreSubst.substExpr (mkCoreSubst env) expr
\end{code}

