%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplMonad]{The simplifier Monad}

\begin{code}
module SimplEnv (
        InId, InBind, InExpr, InAlt, InArg, InType, InBndr, InVar,
        OutId, OutTyVar, OutBind, OutExpr, OutAlt, OutArg, OutType, OutBndr, OutVar,
        InCoercion, OutCoercion,

        -- The simplifier mode
        setMode, getMode, updMode,

        -- Environments
        SimplEnv(..), StaticEnv, pprSimplEnv,   -- Temp not abstract
        mkSimplEnv, extendIdSubst, SimplEnv.extendTvSubst, SimplEnv.extendCvSubst,
        zapSubstEnv, setSubstEnv,
        getInScope, setInScope, setInScopeSet, modifyInScope, addNewInScopeIds,
        getSimplRules,

        SimplSR(..), mkContEx, substId, lookupRecBndr,

        simplNonRecBndr, simplRecBndrs, simplLamBndr, simplLamBndrs,
        simplBinder, simplBinders, addBndrRules,
        substExpr, substTy, substTyVar, getTvSubst,
        getCvSubst, substCo, substCoVar,
        mkCoreSubst,

        -- Floats
        Floats, emptyFloats, isEmptyFloats, addNonRec, addFloats, extendFloats,
        wrapFloats, floatBinds, setFloats, zapFloats, addRecFloats,
        doFloatFromRhs, getFloatBinds, getFloats, mapFloats
    ) where

#include "HsVersions.h"

import SimplMonad
import CoreMonad        ( SimplifierMode(..) )
import IdInfo
import CoreSyn
import CoreUtils
import Var
import VarEnv
import VarSet
import OrdList
import Id
import MkCore
import TysWiredIn
import qualified CoreSubst
import qualified Type
import Type hiding              ( substTy, substTyVarBndr, substTyVar )
import qualified Coercion
import Coercion hiding          ( substCo, substTy, substCoVar, substCoVarBndr, substTyVarBndr )
import BasicTypes
import MonadUtils
import Outputable
import FastString
import Util

import Data.List
\end{code}

%************************************************************************
%*                                                                      *
\subsection[Simplify-types]{Type declarations}
%*                                                                      *
%************************************************************************

\begin{code}
type InBndr     = CoreBndr
type InVar      = Var                   -- Not yet cloned
type InId       = Id                    -- Not yet cloned
type InType     = Type                  -- Ditto
type InBind     = CoreBind
type InExpr     = CoreExpr
type InAlt      = CoreAlt
type InArg      = CoreArg
type InCoercion = Coercion

type OutBndr     = CoreBndr
type OutVar      = Var                  -- Cloned
type OutId       = Id                   -- Cloned
type OutTyVar    = TyVar                -- Cloned
type OutType     = Type                 -- Cloned
type OutCoercion = Coercion
type OutBind     = CoreBind
type OutExpr     = CoreExpr
type OutAlt      = CoreAlt
type OutArg      = CoreArg
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{The @SimplEnv@ type}
%*                                                                      *
%************************************************************************


\begin{code}
data SimplEnv
  = SimplEnv {
     ----------- Static part of the environment -----------
     -- Static in the sense of lexically scoped,
     -- wrt the original expression

        seMode      :: SimplifierMode,

        -- The current substitution
        seTvSubst   :: TvSubstEnv,      -- InTyVar |--> OutType
        seCvSubst   :: CvSubstEnv,      -- InCoVar |--> OutCoercion
        seIdSubst   :: SimplIdSubst,    -- InId    |--> OutExpr

     ----------- Dynamic part of the environment -----------
     -- Dynamic in the sense of describing the setup where
     -- the expression finally ends up

        -- The current set of in-scope variables
        -- They are all OutVars, and all bound in this module
        seInScope   :: InScopeSet,      -- OutVars only
                -- Includes all variables bound by seFloats
        seFloats    :: Floats
                -- See Note [Simplifier floats]
    }

type StaticEnv = SimplEnv       -- Just the static part is relevant

pprSimplEnv :: SimplEnv -> SDoc
-- Used for debugging; selective
pprSimplEnv env
  = vcat [ptext (sLit "TvSubst:") <+> ppr (seTvSubst env),
          ptext (sLit "IdSubst:") <+> ppr (seIdSubst env),
          ptext (sLit "InScope:") <+> vcat (map ppr_one in_scope_vars)
    ]
  where
   in_scope_vars = varEnvElts (getInScopeVars (seInScope env))
   ppr_one v | isId v = ppr v <+> ppr (idUnfolding v)
             | otherwise = ppr v

type SimplIdSubst = IdEnv SimplSR       -- IdId |--> OutExpr
        -- See Note [Extending the Subst] in CoreSubst

data SimplSR
  = DoneEx OutExpr              -- Completed term
  | DoneId OutId                -- Completed term variable
  | ContEx TvSubstEnv           -- A suspended substitution
           CvSubstEnv
           SimplIdSubst
           InExpr

instance Outputable SimplSR where
  ppr (DoneEx e) = ptext (sLit "DoneEx") <+> ppr e
  ppr (DoneId v) = ptext (sLit "DoneId") <+> ppr v
  ppr (ContEx _tv _cv _id e) = vcat [ptext (sLit "ContEx") <+> ppr e {-,
                                ppr (filter_env tv), ppr (filter_env id) -}]
        -- where
        -- fvs = exprFreeVars e
        -- filter_env env = filterVarEnv_Directly keep env
        -- keep uniq _ = uniq `elemUFM_Directly` fvs
\end{code}

Note [SimplEnv invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        The substitution is *apply-once* only, because InIds and OutIds
        can overlap.
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
  if the type of the Id changes.  Why not?  Because of the next point:

* We *always, always* finish by looking up in the in-scope set
  any variable that doesn't get a DoneEx or DoneVar hit in the substitution.
  Reason: so that we never finish up with a "old" Id in the result.
  An old Id might point to an old unfolding and so on... which gives a space
  leak.

  [The DoneEx and DoneVar hits map to "new" stuff.]

* It follows that substExpr must not do a no-op if the substitution is empty.
  substType is free to do so, however.

* When we come to a let-binding (say) we generate new IdInfo, including an
  unfolding, attach it to the binder, and add this newly adorned binder to
  the in-scope set.  So all subsequent occurrences of the binder will get
  mapped to the full-adorned binder, which is also the one put in the
  binding site.

* The in-scope "set" usually maps x->x; we use it simply for its domain.
  But sometimes we have two in-scope Ids that are synomyms, and should
  map to the same target:  x->x, y->x.  Notably:
        case y of x { ... }
  That's why the "set" is actually a VarEnv Var


\begin{code}
mkSimplEnv :: SimplifierMode -> SimplEnv
mkSimplEnv mode
  = SimplEnv { seMode = mode
             , seInScope = init_in_scope
             , seFloats = emptyFloats
             , seTvSubst = emptyVarEnv
             , seCvSubst = emptyVarEnv
             , seIdSubst = emptyVarEnv }
        -- The top level "enclosing CC" is "SUBSUMED".

init_in_scope :: InScopeSet
init_in_scope = mkInScopeSet (unitVarSet (mkWildValBinder unitTy))
              -- See Note [WildCard binders]
\end{code}

Note [WildCard binders]
~~~~~~~~~~~~~~~~~~~~~~~
The program to be simplified may have wild binders
    case e of wild { p -> ... }
We want to *rename* them away, so that there are no
occurrences of 'wild-id' (with wildCardKey).  The easy
way to do that is to start of with a representative
Id in the in-scope set

There can be be *occurrences* of wild-id.  For example,
MkCore.mkCoreApp transforms
   e (a /# b)   -->   case (a /# b) of wild { DEFAULT -> e wild }
This is ok provided 'wild' isn't free in 'e', and that's the delicate
thing. Generally, you want to run the simplifier to get rid of the
wild-ids before doing much else.

It's a very dark corner of GHC.  Maybe it should be cleaned up.

\begin{code}
getMode :: SimplEnv -> SimplifierMode
getMode env = seMode env

setMode :: SimplifierMode -> SimplEnv -> SimplEnv
setMode mode env = env { seMode = mode }

updMode :: (SimplifierMode -> SimplifierMode) -> SimplEnv -> SimplEnv
updMode upd env = env { seMode = upd (seMode env) }

---------------------
extendIdSubst :: SimplEnv -> Id -> SimplSR -> SimplEnv
extendIdSubst env@(SimplEnv {seIdSubst = subst}) var res
  = ASSERT2( isId var && not (isCoVar var), ppr var )
    env {seIdSubst = extendVarEnv subst var res}

extendTvSubst :: SimplEnv -> TyVar -> Type -> SimplEnv
extendTvSubst env@(SimplEnv {seTvSubst = subst}) var res
  = env {seTvSubst = extendVarEnv subst var res}

extendCvSubst :: SimplEnv -> CoVar -> Coercion -> SimplEnv
extendCvSubst env@(SimplEnv {seCvSubst = subst}) var res
  = env {seCvSubst = extendVarEnv subst var res}

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
        --      let x = a*b in (x, \x -> x+3)
        -- We add [x |-> a*b] to the substitution, but we must
        -- _delete_ it from the substitution when going inside
        -- the (\x -> ...)!

modifyInScope :: SimplEnv -> CoreBndr -> SimplEnv
-- The variable should already be in scope, but
-- replace the existing version with this new one
-- which has more information
modifyInScope env@(SimplEnv {seInScope = in_scope}) v
  = env {seInScope = extendInScopeSet in_scope v}

---------------------
zapSubstEnv :: SimplEnv -> SimplEnv
zapSubstEnv env = env {seTvSubst = emptyVarEnv, seCvSubst = emptyVarEnv, seIdSubst = emptyVarEnv}

setSubstEnv :: SimplEnv -> TvSubstEnv -> CvSubstEnv -> SimplIdSubst -> SimplEnv
setSubstEnv env tvs cvs ids = env { seTvSubst = tvs, seCvSubst = cvs, seIdSubst = ids }

mkContEx :: SimplEnv -> InExpr -> SimplSR
mkContEx (SimplEnv { seTvSubst = tvs, seCvSubst = cvs, seIdSubst = ids }) e = ContEx tvs cvs ids e
\end{code}



%************************************************************************
%*                                                                      *
\subsection{Floats}
%*                                                                      *
%************************************************************************

Note [Simplifier floats]
~~~~~~~~~~~~~~~~~~~~~~~~~
The Floats is a bunch of bindings, classified by a FloatFlag.

  NonRec x (y:ys)       FltLifted
  Rec [(x,rhs)]         FltLifted

  NonRec x# (y +# 3)    FltOkSpec   -- Unboxed, but ok-for-spec'n

  NonRec x# (a /# b)    FltCareful
  NonRec x* (f y)       FltCareful  -- Strict binding; might fail or diverge
  NonRec x# (f y)       FltCareful  -- Unboxed binding: might fail or diverge
                                    --  (where f :: Int -> Int#)

\begin{code}
data Floats = Floats (OrdList OutBind) FloatFlag
        -- See Note [Simplifier floats]

data FloatFlag
  = FltLifted   -- All bindings are lifted and lazy
                --  Hence ok to float to top level, or recursive

  | FltOkSpec   -- All bindings are FltLifted *or*
                --      strict (perhaps because unlifted,
                --      perhaps because of a strict binder),
                --        *and* ok-for-speculation
                --  Hence ok to float out of the RHS
                --  of a lazy non-recursive let binding
                --  (but not to top level, or into a rec group)

  | FltCareful  -- At least one binding is strict (or unlifted)
                --      and not guaranteed cheap
                --      Do not float these bindings out of a lazy let

instance Outputable Floats where
  ppr (Floats binds ff) = ppr ff $$ ppr (fromOL binds)

instance Outputable FloatFlag where
  ppr FltLifted = ptext (sLit "FltLifted")
  ppr FltOkSpec = ptext (sLit "FltOkSpec")
  ppr FltCareful = ptext (sLit "FltCareful")

andFF :: FloatFlag -> FloatFlag -> FloatFlag
andFF FltCareful _          = FltCareful
andFF FltOkSpec  FltCareful = FltCareful
andFF FltOkSpec  _          = FltOkSpec
andFF FltLifted  flt        = flt

classifyFF :: CoreBind -> FloatFlag
classifyFF (Rec _) = FltLifted
classifyFF (NonRec bndr rhs)
  | not (isStrictId bndr)    = FltLifted
  | exprOkForSpeculation rhs = FltOkSpec
  | otherwise                = FltCareful

doFloatFromRhs :: TopLevelFlag -> RecFlag -> Bool -> OutExpr -> SimplEnv -> Bool
-- If you change this function look also at FloatIn.noFloatFromRhs
doFloatFromRhs lvl rec str rhs (SimplEnv {seFloats = Floats fs ff})
  =  not (isNilOL fs) && want_to_float && can_float
  where
     want_to_float = isTopLevel lvl || exprIsCheap rhs || exprIsExpandable rhs 
                     -- See Note [Float when cheap or expandable]
     can_float = case ff of
                   FltLifted  -> True
                   FltOkSpec  -> isNotTopLevel lvl && isNonRec rec
                   FltCareful -> isNotTopLevel lvl && isNonRec rec && str
\end{code}

Note [Float when cheap or expandable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to float a let from a let if the residual RHS is
   a) cheap, such as (\x. blah)
   b) expandable, such as (f b) if f is CONLIKE
But there are 
  - cheap things that are not expandable (eg \x. expensive)
  - expandable things that are not cheap (eg (f b) where b is CONLIKE)
so we must take the 'or' of the two.

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
  = id `seq`   -- This seq forces the Id, and hence its IdInfo,
               -- and hence any inner substitutions
    env { seFloats = seFloats env `addFlts` unitFloat (NonRec id rhs),
          seInScope = extendInScopeSet (seInScope env) id }

mapFloats :: SimplEnv -> ((Id,CoreExpr) -> (Id,CoreExpr)) -> SimplEnv
mapFloats env@SimplEnv { seFloats = Floats fs ff } fun
   = env { seFloats = Floats (mapOL app fs) ff }
   where
     app (NonRec b e) = case fun (b,e) of (b',e') -> NonRec b' e'
     app (Rec bs)     = Rec (map fun bs)

extendFloats :: SimplEnv -> OutBind -> SimplEnv
-- Add these bindings to the floats, and extend the in-scope env too
extendFloats env bind
  = env { seFloats  = seFloats env `addFlts` unitFloat bind,
          seInScope = extendInScopeSetList (seInScope env) bndrs }
  where
    bndrs = bindersOf bind

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
  = ASSERT2( case ff of { FltLifted -> True; _ -> False }, ppr (fromOL bs) )
    env2 {seFloats = seFloats env1 `addFlts` unitFloat (Rec (flattenBinds (fromOL bs)))}

wrapFloats :: SimplEnv -> OutExpr -> OutExpr
wrapFloats env expr = wrapFlts (seFloats env) expr

wrapFlts :: Floats -> OutExpr -> OutExpr
-- Wrap the floats around the expression, using case-binding where necessary
wrapFlts (Floats bs _) body = foldrOL wrap body bs
  where
    wrap (Rec prs)    body = Let (Rec prs) body
    wrap (NonRec b r) body = bindNonRec b r body

getFloatBinds :: SimplEnv -> [CoreBind]
getFloatBinds env = floatBinds (seFloats env)

getFloats :: SimplEnv -> Floats
getFloats env = seFloats env

isEmptyFloats :: SimplEnv -> Bool
isEmptyFloats env = isEmptyFlts (seFloats env)

isEmptyFlts :: Floats -> Bool
isEmptyFlts (Floats bs _) = isNilOL bs

floatBinds :: Floats -> [OutBind]
floatBinds (Floats bs _) = fromOL bs
\end{code}


%************************************************************************
%*                                                                      *
                Substitution of Vars
%*                                                                      *
%************************************************************************

Note [Global Ids in the substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We look up even a global (eg imported) Id in the substitution. Consider
   case X.g_34 of b { (a,b) ->  ... case X.g_34 of { (p,q) -> ...} ... }
The binder-swap in the occurrence analyser will add a binding
for a LocalId version of g (with the same unique though):
   case X.g_34 of b { (a,b) ->  let g_34 = b in
                                ... case X.g_34 of { (p,q) -> ...} ... }
So we want to look up the inner X.g_34 in the substitution, where we'll
find that it has been substituted by b.  (Or conceivably cloned.)

\begin{code}
substId :: SimplEnv -> InId -> SimplSR
-- Returns DoneEx only on a non-Var expression
substId (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v
  = case lookupVarEnv ids v of  -- Note [Global Ids in the substitution]
        Nothing               -> DoneId (refine in_scope v)
        Just (DoneId v)       -> DoneId (refine in_scope v)
        Just (DoneEx (Var v)) -> DoneId (refine in_scope v)
        Just res              -> res    -- DoneEx non-var, or ContEx

        -- Get the most up-to-date thing from the in-scope set
        -- Even though it isn't in the substitution, it may be in
        -- the in-scope set with better IdInfo
refine :: InScopeSet -> Var -> Var
refine in_scope v
  | isLocalId v = case lookupInScope in_scope v of
                  Just v' -> v'
                  Nothing -> WARN( True, ppr v ) v  -- This is an error!
  | otherwise = v

lookupRecBndr :: SimplEnv -> InId -> OutId
-- Look up an Id which has been put into the envt by simplRecBndrs,
-- but where we have not yet done its RHS
lookupRecBndr (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v
  = case lookupVarEnv ids v of
        Just (DoneId v) -> v
        Just _ -> pprPanic "lookupRecBndr" (ppr v)
        Nothing -> refine in_scope v
\end{code}


%************************************************************************
%*                                                                      *
\section{Substituting an Id binder}
%*                                                                      *
%************************************************************************


These functions are in the monad only so that they can be made strict via seq.

\begin{code}
simplBinders, simplLamBndrs
        :: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplBinders  env bndrs = mapAccumLM simplBinder  env bndrs
simplLamBndrs env bndrs = mapAccumLM simplLamBndr env bndrs

-------------
simplBinder :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- Used for lambda and case-bound variables
-- Clone Id if necessary, substitute type
-- Return with IdInfo already substituted, but (fragile) occurrence info zapped
-- The substitution is extended only if the variable is cloned, because
-- we *don't* need to use it to track occurrence info.
simplBinder env bndr
  | isTyVar bndr  = do  { let (env', tv) = substTyVarBndr env bndr
                        ; seqTyVar tv `seq` return (env', tv) }
  | otherwise     = do  { let (env', id) = substIdBndr env bndr
                        ; seqId id `seq` return (env', id) }

-------------
simplLamBndr :: SimplEnv -> Var -> SimplM (SimplEnv, Var)
-- Used for lambda binders.  These sometimes have unfoldings added by
-- the worker/wrapper pass that must be preserved, because they can't
-- be reconstructed from context.  For example:
--      f x = case x of (a,b) -> fw a b x
--      fw a b x{=(a,b)} = ...
-- The "{=(a,b)}" is an unfolding we can't reconstruct otherwise.
simplLamBndr env bndr
  | isId bndr && hasSomeUnfolding old_unf = seqId id2 `seq` return (env2, id2)  -- Special case
  | otherwise                             = simplBinder env bndr                -- Normal case
  where
    old_unf = idUnfolding bndr
    (env1, id1) = substIdBndr env bndr
    id2  = id1 `setIdUnfolding` substUnfolding env old_unf
    env2 = modifyInScope env1 id2

---------------
simplNonRecBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- A non-recursive let binder
simplNonRecBndr env id
  = do  { let (env1, id1) = substIdBndr env id
        ; seqId id1 `seq` return (env1, id1) }

---------------
simplRecBndrs :: SimplEnv -> [InBndr] -> SimplM SimplEnv
-- Recursive let binders
simplRecBndrs env@(SimplEnv {}) ids
  = do  { let (env1, ids1) = mapAccumL substIdBndr env ids
        ; seqIds ids1 `seq` return env1 }

---------------
substIdBndr :: SimplEnv -> InBndr -> (SimplEnv, OutBndr)
-- Might be a coercion variable
substIdBndr env bndr
  | isCoVar bndr  = substCoVarBndr env bndr
  | otherwise     = substNonCoVarIdBndr env bndr

---------------
substNonCoVarIdBndr
   :: SimplEnv
   -> InBndr    -- Env and binder to transform
   -> (SimplEnv, OutBndr)
-- Clone Id if necessary, substitute its type
-- Return an Id with its
--      * Type substituted
--      * UnfoldingInfo, Rules, WorkerInfo zapped
--      * Fragile OccInfo (only) zapped: Note [Robust OccInfo]
--      * Robust info, retained especially arity and demand info,
--         so that they are available to occurrences that occur in an
--         earlier binding of a letrec
--
-- For the robust info, see Note [Arity robustness]
--
-- Augment the substitution  if the unique changed
-- Extend the in-scope set with the new Id
--
-- Similar to CoreSubst.substIdBndr, except that
--      the type of id_subst differs
--      all fragile info is zapped
substNonCoVarIdBndr env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst })
                    old_id
  = ASSERT2( not (isCoVar old_id), ppr old_id )
    (env { seInScope = in_scope `extendInScopeSet` new_id,
           seIdSubst = new_subst }, new_id)
  where
    id1    = uniqAway in_scope old_id
    id2    = substIdType env id1
    new_id = zapFragileIdInfo id2       -- Zaps rules, worker-info, unfolding
                                        -- and fragile OccInfo

        -- Extend the substitution if the unique has changed,
        -- or there's some useful occurrence information
        -- See the notes with substTyVarBndr for the delSubstEnv
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
seqId id = seqType (idType id)  `seq`
           idInfo id            `seq`
           ()

seqIds :: [Id] -> ()
seqIds []       = ()
seqIds (id:ids) = seqId id `seq` seqIds ids
\end{code}


Note [Arity robustness]
~~~~~~~~~~~~~~~~~~~~~~~
We *do* transfer the arity from from the in_id of a let binding to the
out_id.  This is important, so that the arity of an Id is visible in
its own RHS.  For example:
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

There is a disadvantage though.  Making the arity visible in the RHS
allows us to eta-reduce
        f = \x -> f x
to
        f = f
which technically is not sound.   This is very much a corner case, so
I'm not worried about it.  Another idea is to ensure that f's arity
never decreases; its arity started as 1, and we should never eta-reduce
below that.


Note [Robust OccInfo]
~~~~~~~~~~~~~~~~~~~~~
It's important that we *do* retain the loop-breaker OccInfo, because
that's what stops the Id getting inlined infinitely, in the body of
the letrec.


Note [Rules in a letrec]
~~~~~~~~~~~~~~~~~~~~~~~~
After creating fresh binders for the binders of a letrec, we
substitute the RULES and add them back onto the binders; this is done
*before* processing any of the RHSs.  This is important.  Manuel found
cases where he really, really wanted a RULE for a recursive function
to apply in that function's own right-hand side.

See Note [Loop breaking and RULES] in OccAnal.


\begin{code}
addBndrRules :: SimplEnv -> InBndr -> OutBndr -> (SimplEnv, OutBndr)
-- Rules are added back in to to the bin
addBndrRules env in_id out_id
  | isEmptySpecInfo old_rules = (env, out_id)
  | otherwise = (modifyInScope env final_id, final_id)
  where
    subst     = mkCoreSubst (text "local rules") env
    old_rules = idSpecialisation in_id
    new_rules = CoreSubst.substSpec subst out_id old_rules
    final_id  = out_id `setIdSpecialisation` new_rules
\end{code}


%************************************************************************
%*                                                                      *
                Impedence matching to type substitution
%*                                                                      *
%************************************************************************

\begin{code}
getTvSubst :: SimplEnv -> TvSubst
getTvSubst (SimplEnv { seInScope = in_scope, seTvSubst = tv_env })
  = mkTvSubst in_scope tv_env

getCvSubst :: SimplEnv -> CvSubst
getCvSubst (SimplEnv { seInScope = in_scope, seTvSubst = tv_env, seCvSubst = cv_env })
  = CvSubst in_scope tv_env cv_env

substTy :: SimplEnv -> Type -> Type
substTy env ty = Type.substTy (getTvSubst env) ty

substTyVar :: SimplEnv -> TyVar -> Type
substTyVar env tv = Type.substTyVar (getTvSubst env) tv

substTyVarBndr :: SimplEnv -> TyVar -> (SimplEnv, TyVar)
substTyVarBndr env tv
  = case Type.substTyVarBndr (getTvSubst env) tv of
        (TvSubst in_scope' tv_env', tv')
           -> (env { seInScope = in_scope', seTvSubst = tv_env' }, tv')

substCoVar :: SimplEnv -> CoVar -> Coercion
substCoVar env tv = Coercion.substCoVar (getCvSubst env) tv

substCoVarBndr :: SimplEnv -> CoVar -> (SimplEnv, CoVar)
substCoVarBndr env cv
  = case Coercion.substCoVarBndr (getCvSubst env) cv of
        (CvSubst in_scope' tv_env' cv_env', cv')
           -> (env { seInScope = in_scope', seTvSubst = tv_env', seCvSubst = cv_env' }, cv')

substCo :: SimplEnv -> Coercion -> Coercion
substCo env co = Coercion.substCo (getCvSubst env) co

-- When substituting in rules etc we can get CoreSubst to do the work
-- But CoreSubst uses a simpler form of IdSubstEnv, so we must impedence-match
-- here.  I think the this will not usually result in a lot of work;
-- the substitutions are typically small, and laziness will avoid work in many cases.

mkCoreSubst  :: SDoc -> SimplEnv -> CoreSubst.Subst
mkCoreSubst doc (SimplEnv { seInScope = in_scope, seTvSubst = tv_env, seCvSubst = cv_env, seIdSubst = id_env })
  = mk_subst tv_env cv_env id_env
  where
    mk_subst tv_env cv_env id_env = CoreSubst.mkSubst in_scope tv_env cv_env (mapVarEnv fiddle id_env)

    fiddle (DoneEx e)          = e
    fiddle (DoneId v)          = Var v
    fiddle (ContEx tv cv id e) = CoreSubst.substExpr (text "mkCoreSubst" <+> doc) (mk_subst tv cv id) e
                                                -- Don't shortcut here

------------------
substIdType :: SimplEnv -> Id -> Id
substIdType (SimplEnv { seInScope = in_scope,  seTvSubst = tv_env }) id
  | isEmptyVarEnv tv_env || isEmptyVarSet (tyVarsOfType old_ty) = id
  | otherwise = Id.setIdType id (Type.substTy (TvSubst in_scope tv_env) old_ty)
                -- The tyVarsOfType is cheaper than it looks
                -- because we cache the free tyvars of the type
                -- in a Note in the id's type itself
  where
    old_ty = idType id

------------------
substExpr :: SDoc -> SimplEnv -> CoreExpr -> CoreExpr
substExpr doc env
  = CoreSubst.substExpr (text "SimplEnv.substExpr1" <+> doc)
                        (mkCoreSubst (text "SimplEnv.substExpr2" <+> doc) env)
  -- Do *not* short-cut in the case of an empty substitution
  -- See Note [SimplEnv invariants]

substUnfolding :: SimplEnv -> Unfolding -> Unfolding
substUnfolding env unf = CoreSubst.substUnfolding (mkCoreSubst (text "subst-unfolding") env) unf
  -- Do *not* short-cut in the case of an empty substitution
  -- See Note [SimplEnv invariants]
\end{code}

