{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[SimplMonad]{The simplifier Monad}
-}

{-# LANGUAGE CPP #-}

module SimplEnv (
        -- * The simplifier mode
        setMode, getMode, updMode,

        -- * Environments
        SimplEnv(..), StaticEnv, pprSimplEnv,   -- Temp not abstract
        mkSimplEnv, extendIdSubst,
        SimplEnv.extendTvSubst, SimplEnv.extendCvSubst,
        zapSubstEnv, setSubstEnv,
        getInScope, setInScopeAndZapFloats,
        setInScopeSet, modifyInScope, addNewInScopeIds,
        getSimplRules,

        -- * Substitution results
        SimplSR(..), mkContEx, substId, lookupRecBndr, refineFromInScope,
        isJoinIdInEnv_maybe,

        -- * Simplifying 'Id' binders
        simplNonRecBndr, simplNonRecJoinBndr, simplRecBndrs, simplRecJoinBndrs,
        simplBinder, simplBinders,
        substTy, substTyVar, getTCvSubst,
        substCo, substCoVar,

        -- * Floats
        Floats, emptyFloats, isEmptyFloats,
        addNonRec, addFloats, extendFloats,
        wrapFloats, setFloats, zapFloats, addRecFloats, mapFloats,
        doFloatFromRhs, getFloatBinds,

        JoinFloats, emptyJoinFloats, isEmptyJoinFloats,
        wrapJoinFloats, zapJoinFloats, restoreJoinFloats, getJoinFloatBinds,
    ) where

#include "HsVersions.h"

import SimplMonad
import CoreMonad                ( SimplifierMode(..) )
import CoreSyn
import CoreUtils
import Var
import VarEnv
import VarSet
import OrdList
import Id
import MkCore                   ( mkWildValBinder )
import TysWiredIn
import qualified Type
import Type hiding              ( substTy, substTyVar, substTyVarBndr )
import qualified Coercion
import Coercion hiding          ( substCo, substCoVar, substCoVarBndr )
import BasicTypes
import MonadUtils
import Outputable
import Util
import UniqFM                   ( pprUniqFM )

import Data.List

{-
************************************************************************
*                                                                      *
\subsubsection{The @SimplEnv@ type}
*                                                                      *
************************************************************************
-}

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
        seFloats    :: Floats,
                -- See Note [Simplifier floats]
        seJoinFloats :: JoinFloats
                -- Handled separately; they don't go very far
    }

type StaticEnv = SimplEnv       -- Just the static part is relevant

pprSimplEnv :: SimplEnv -> SDoc
-- Used for debugging; selective
pprSimplEnv env
  = vcat [text "TvSubst:" <+> ppr (seTvSubst env),
          text "CvSubst:" <+> ppr (seCvSubst env),
          text "IdSubst:" <+> id_subst_doc,
          text "InScope:" <+> in_scope_vars_doc
    ]
  where
   id_subst_doc = pprUniqFM ppr_id_subst (seIdSubst env)
   ppr_id_subst (m_ar, sr) = arity_part <+> ppr sr
     where arity_part = case m_ar of Just ar -> brackets $
                                                  text "join" <+> int ar
                                     Nothing -> empty

   in_scope_vars_doc = pprVarSet (getInScopeVars (seInScope env))
                                 (vcat . map ppr_one)
   ppr_one v | isId v = ppr v <+> ppr (idUnfolding v)
             | otherwise = ppr v

type SimplIdSubst = IdEnv (Maybe JoinArity, SimplSR) -- IdId |--> OutExpr
        -- See Note [Extending the Subst] in CoreSubst
        -- See Note [Join arity in SimplIdSubst]

-- | A substitution result.
data SimplSR
  = DoneEx OutExpr              -- Completed term
  | DoneId OutId                -- Completed term variable
  | ContEx TvSubstEnv           -- A suspended substitution
           CvSubstEnv
           SimplIdSubst
           InExpr

instance Outputable SimplSR where
  ppr (DoneEx e) = text "DoneEx" <+> ppr e
  ppr (DoneId v) = text "DoneId" <+> ppr v
  ppr (ContEx _tv _cv _id e) = vcat [text "ContEx" <+> ppr e {-,
                                ppr (filter_env tv), ppr (filter_env id) -}]
        -- where
        -- fvs = exprFreeVars e
        -- filter_env env = filterVarEnv_Directly keep env
        -- keep uniq _ = uniq `elemUFM_Directly` fvs

{-
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

Note [Join arity in SimplIdSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We have to remember which incoming variables are join points (the occurrences
may not be marked correctly yet; we're in change of propagating the change if
OccurAnal makes something a join point). Normally the in-scope set is where we
keep the latest information, but the in-scope set tracks only OutVars; if a
binding is unconditionally inlined, it never makes it into the in-scope set,
and we need to know at the occurrence site that the variable is a join point so
that we know to drop the context. Thus we remember which join points we're
substituting. Clumsily, finding whether an InVar is a join variable may require
looking in both the substitution *and* the in-scope set (see
'isJoinIdInEnv_maybe').
-}

mkSimplEnv :: SimplifierMode -> SimplEnv
mkSimplEnv mode
  = SimplEnv { seMode = mode
             , seInScope = init_in_scope
             , seFloats = emptyFloats
             , seJoinFloats = emptyJoinFloats
             , seTvSubst = emptyVarEnv
             , seCvSubst = emptyVarEnv
             , seIdSubst = emptyVarEnv }
        -- The top level "enclosing CC" is "SUBSUMED".

init_in_scope :: InScopeSet
init_in_scope = mkInScopeSet (unitVarSet (mkWildValBinder unitTy))
              -- See Note [WildCard binders]

{-
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
-}

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
    env { seIdSubst = extendVarEnv subst var (isJoinId_maybe var, res) }

extendTvSubst :: SimplEnv -> TyVar -> Type -> SimplEnv
extendTvSubst env@(SimplEnv {seTvSubst = tsubst}) var res
  = ASSERT( isTyVar var )
    env {seTvSubst = extendVarEnv tsubst var res}

extendCvSubst :: SimplEnv -> CoVar -> Coercion -> SimplEnv
extendCvSubst env@(SimplEnv {seCvSubst = csubst}) var co
  = ASSERT( isCoVar var )
    env {seCvSubst = extendVarEnv csubst var co}

---------------------
getInScope :: SimplEnv -> InScopeSet
getInScope env = seInScope env

setInScopeSet :: SimplEnv -> InScopeSet -> SimplEnv
setInScopeSet env in_scope = env {seInScope = in_scope}

setInScopeAndZapFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Set the in-scope set, and *zap* the floats
setInScopeAndZapFloats env env_with_scope
  = env { seInScope    = seInScope env_with_scope,
          seFloats     = emptyFloats,
          seJoinFloats = emptyJoinFloats }

setFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Set the in-scope set *and* the floats
setFloats env env_with_floats
  = env { seInScope    = seInScope env_with_floats,
          seFloats     = seFloats  env_with_floats,
          seJoinFloats = seJoinFloats env_with_floats }

restoreJoinFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Put back floats previously zapped
-- Unlike 'setFloats', does *not* update the in-scope set, since the right-hand
-- env is assumed to be *older*
restoreJoinFloats env old_env
  = env { seJoinFloats = seJoinFloats old_env }

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

{-
************************************************************************
*                                                                      *
\subsection{Floats}
*                                                                      *
************************************************************************

Note [Simplifier floats]
~~~~~~~~~~~~~~~~~~~~~~~~~
The Floats is a bunch of bindings, classified by a FloatFlag.

* All of them satisfy the let/app invariant

Examples

  NonRec x (y:ys)       FltLifted
  Rec [(x,rhs)]         FltLifted

  NonRec x* (p:q)       FltOKSpec   -- RHS is WHNF.  Question: why not FltLifted?
  NonRec x# (y +# 3)    FltOkSpec   -- Unboxed, but ok-for-spec'n

  NonRec x* (f y)       FltCareful  -- Strict binding; might fail or diverge

Can't happen:
  NonRec x# (a /# b)    -- Might fail; does not satisfy let/app
  NonRec x# (f y)       -- Might diverge; does not satisfy let/app
-}

data Floats = Floats (OrdList OutBind) FloatFlag
        -- See Note [Simplifier floats]

type JoinFloats = OrdList OutBind

data FloatFlag
  = FltLifted   -- All bindings are lifted and lazy *or*
                --     consist of a single primitive string literal
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
  ppr FltLifted = text "FltLifted"
  ppr FltOkSpec = text "FltOkSpec"
  ppr FltCareful = text "FltCareful"

andFF :: FloatFlag -> FloatFlag -> FloatFlag
andFF FltCareful _          = FltCareful
andFF FltOkSpec  FltCareful = FltCareful
andFF FltOkSpec  _          = FltOkSpec
andFF FltLifted  flt        = flt

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

{-
Note [Float when cheap or expandable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to float a let from a let if the residual RHS is
   a) cheap, such as (\x. blah)
   b) expandable, such as (f b) if f is CONLIKE
But there are
  - cheap things that are not expandable (eg \x. expensive)
  - expandable things that are not cheap (eg (f b) where b is CONLIKE)
so we must take the 'or' of the two.
-}

emptyFloats :: Floats
emptyFloats = Floats nilOL FltLifted

emptyJoinFloats :: JoinFloats
emptyJoinFloats = nilOL

unitFloat :: OutBind -> Floats
-- This key function constructs a singleton float with the right form
unitFloat bind = ASSERT(all (not . isJoinId) (bindersOf bind))
                 Floats (unitOL bind) (flag bind)
  where
    flag (Rec {})                = FltLifted
    flag (NonRec bndr rhs)
      | not (isStrictId bndr)    = FltLifted
      | exprIsLiteralString rhs  = FltLifted
          -- String literals can be floated freely.
          -- See Note [CoreSyn top-level string ltierals] in CoreSyn.
      | exprOkForSpeculation rhs = FltOkSpec  -- Unlifted, and lifted but ok-for-spec (eg HNF)
      | otherwise                = ASSERT2( not (isUnliftedType (idType bndr)), ppr bndr )
                                   FltCareful
      -- Unlifted binders can only be let-bound if exprOkForSpeculation holds

unitJoinFloat :: OutBind -> JoinFloats
unitJoinFloat bind = ASSERT(all isJoinId (bindersOf bind))
                     unitOL bind

addNonRec :: SimplEnv -> OutId -> OutExpr -> SimplEnv
-- Add a non-recursive binding and extend the in-scope set
-- The latter is important; the binder may already be in the
-- in-scope set (although it might also have been created with newId)
-- but it may now have more IdInfo
addNonRec env id rhs
  = id `seq`   -- This seq forces the Id, and hence its IdInfo,
               -- and hence any inner substitutions
    env { seFloats = floats',
          seJoinFloats = jfloats',
          seInScope = extendInScopeSet (seInScope env) id }
  where
    bind = NonRec id rhs

    floats'  | isJoinId id = seFloats env
             | otherwise   = seFloats env `addFlts` unitFloat bind
    jfloats' | isJoinId id = seJoinFloats env `addJoinFlts` unitJoinFloat bind
             | otherwise   = seJoinFloats env

extendFloats :: SimplEnv -> OutBind -> SimplEnv
-- Add these bindings to the floats, and extend the in-scope env too
extendFloats env bind
  = ASSERT(all (not . isJoinId) (bindersOf bind))
    env { seFloats  = floats',
          seJoinFloats = jfloats',
          seInScope = extendInScopeSetList (seInScope env) bndrs }
  where
    bndrs = bindersOf bind

    floats'  | isJoinBind bind = seFloats env
             | otherwise       = seFloats env `addFlts` unitFloat bind
    jfloats' | isJoinBind bind = seJoinFloats env `addJoinFlts`
                                   unitJoinFloat bind
             | otherwise       = seJoinFloats env

addFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Add the floats for env2 to env1;
-- *plus* the in-scope set for env2, which is bigger
-- than that for env1
addFloats env1 env2
  = env1 {seFloats = seFloats env1 `addFlts` seFloats env2,
          seJoinFloats = seJoinFloats env1 `addJoinFlts` seJoinFloats env2,
          seInScope = seInScope env2 }

addFlts :: Floats -> Floats -> Floats
addFlts (Floats bs1 l1) (Floats bs2 l2)
  = Floats (bs1 `appOL` bs2) (l1 `andFF` l2)

addJoinFlts :: JoinFloats -> JoinFloats -> JoinFloats
addJoinFlts = appOL

zapFloats :: SimplEnv -> SimplEnv
zapFloats env = env { seFloats = emptyFloats
                    , seJoinFloats = emptyJoinFloats }

zapJoinFloats :: SimplEnv -> SimplEnv
zapJoinFloats env = env { seJoinFloats = emptyJoinFloats }

addRecFloats :: SimplEnv -> SimplEnv -> SimplEnv
-- Flattens the floats from env2 into a single Rec group,
-- prepends the floats from env1, and puts the result back in env2
-- This is all very specific to the way recursive bindings are
-- handled; see Simplify.simplRecBind
addRecFloats env1 env2@(SimplEnv {seFloats = Floats bs ff
                                 ,seJoinFloats = jbs })
  = ASSERT2( case ff of { FltLifted -> True; _ -> False }, ppr (fromOL bs) )
    env2 {seFloats = seFloats env1 `addFlts` floats'
         ,seJoinFloats = seJoinFloats env1 `addJoinFlts` jfloats'}
  where
    floats'  | isNilOL bs  = emptyFloats
             | otherwise   = unitFloat (Rec (flattenBinds (fromOL bs)))
    jfloats' | isNilOL jbs = emptyJoinFloats
             | otherwise   = unitJoinFloat (Rec (flattenBinds (fromOL jbs)))

wrapFloats :: SimplEnv -> OutExpr -> OutExpr
-- Wrap the floats around the expression; they should all
-- satisfy the let/app invariant, so mkLets should do the job just fine
wrapFloats env@(SimplEnv {seFloats = Floats bs _}) body
  = foldrOL Let (wrapJoinFloats env body) bs
      -- Note: Always safe to put the joins on the inside since the values
      -- can't refer to them

wrapJoinFloats :: SimplEnv -> OutExpr -> OutExpr
wrapJoinFloats (SimplEnv {seJoinFloats = jbs}) body
  = foldrOL Let body jbs

getFloatBinds :: SimplEnv -> [CoreBind]
getFloatBinds env@(SimplEnv {seFloats = Floats bs _})
  = fromOL bs ++ getJoinFloatBinds env

getJoinFloatBinds :: SimplEnv -> [CoreBind]
getJoinFloatBinds (SimplEnv {seJoinFloats = jbs})
  = fromOL jbs

isEmptyFloats :: SimplEnv -> Bool
isEmptyFloats env@(SimplEnv {seFloats = Floats bs _})
  = isNilOL bs && isEmptyJoinFloats env

isEmptyJoinFloats :: SimplEnv -> Bool
isEmptyJoinFloats (SimplEnv {seJoinFloats = jbs})
  = isNilOL jbs

mapFloats :: SimplEnv -> ((Id,CoreExpr) -> (Id,CoreExpr)) -> SimplEnv
mapFloats env@SimplEnv { seFloats = Floats fs ff, seJoinFloats = jfs } fun
   = env { seFloats = Floats (mapOL app fs) ff
         , seJoinFloats = mapOL app jfs }
   where
    app (NonRec b e) = case fun (b,e) of (b',e') -> NonRec b' e'
    app (Rec bs)     = Rec (map fun bs)

{-
************************************************************************
*                                                                      *
                Substitution of Vars
*                                                                      *
************************************************************************

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
-}

substId :: SimplEnv -> InId -> SimplSR
-- Returns DoneEx only on a non-Var expression
substId (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v
  = case snd <$> lookupVarEnv ids v of  -- Note [Global Ids in the substitution]
        Nothing               -> DoneId (refineFromInScope in_scope v)
        Just (DoneId v)       -> DoneId (refineFromInScope in_scope v)
        Just (DoneEx (Var v)) -> DoneId (refineFromInScope in_scope v)
        Just res              -> res    -- DoneEx non-var, or ContEx

        -- Get the most up-to-date thing from the in-scope set
        -- Even though it isn't in the substitution, it may be in
        -- the in-scope set with better IdInfo

isJoinIdInEnv_maybe :: SimplEnv -> InId -> Maybe JoinArity
isJoinIdInEnv_maybe (SimplEnv { seInScope = inScope, seIdSubst = ids }) v
  | not (isLocalId v)                         = Nothing
  | Just (m_ar, _) <- lookupVarEnv ids v      = m_ar
  | Just v'        <- lookupInScope inScope v = isJoinId_maybe v'
  | otherwise                                 = WARN( True , ppr v )
                                                isJoinId_maybe v

refineFromInScope :: InScopeSet -> Var -> Var
refineFromInScope in_scope v
  | isLocalId v = case lookupInScope in_scope v of
                  Just v' -> v'
                  Nothing -> WARN( True, ppr v ) v  -- This is an error!
  | otherwise = v

lookupRecBndr :: SimplEnv -> InId -> OutId
-- Look up an Id which has been put into the envt by simplRecBndrs,
-- but where we have not yet done its RHS
lookupRecBndr (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v
  = case lookupVarEnv ids v of
        Just (_, DoneId v) -> v
        Just _ -> pprPanic "lookupRecBndr" (ppr v)
        Nothing -> refineFromInScope in_scope v

{-
************************************************************************
*                                                                      *
\section{Substituting an Id binder}
*                                                                      *
************************************************************************


These functions are in the monad only so that they can be made strict via seq.
-}

simplBinders :: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplBinders  env bndrs = mapAccumLM simplBinder  env bndrs

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
  | otherwise     = do  { let (env', id) = substIdBndr Nothing env bndr
                        ; seqId id `seq` return (env', id) }

---------------
simplNonRecBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- A non-recursive let binder
simplNonRecBndr env id
  = do  { let (env1, id1) = substIdBndr Nothing env id
        ; seqId id1 `seq` return (env1, id1) }

---------------
simplNonRecJoinBndr :: SimplEnv -> OutType -> InBndr
                    -> SimplM (SimplEnv, OutBndr)
-- A non-recursive let binder for a join point; context being pushed inward may
-- change the type
simplNonRecJoinBndr env res_ty id
  = do  { let (env1, id1) = substIdBndr (Just res_ty) env id
        ; seqId id1 `seq` return (env1, id1) }

---------------
simplRecBndrs :: SimplEnv -> [InBndr] -> SimplM SimplEnv
-- Recursive let binders
simplRecBndrs env@(SimplEnv {}) ids
  = ASSERT(all (not . isJoinId) ids)
    do  { let (env1, ids1) = mapAccumL (substIdBndr Nothing) env ids
        ; seqIds ids1 `seq` return env1 }

---------------
simplRecJoinBndrs :: SimplEnv -> OutType -> [InBndr] -> SimplM SimplEnv
-- Recursive let binders for join points; context being pushed inward may
-- change types
simplRecJoinBndrs env@(SimplEnv {}) res_ty ids
  = ASSERT(all isJoinId ids)
    do  { let (env1, ids1) = mapAccumL (substIdBndr (Just res_ty)) env ids
        ; seqIds ids1 `seq` return env1 }

---------------
substIdBndr :: Maybe OutType -> SimplEnv -> InBndr -> (SimplEnv, OutBndr)
-- Might be a coercion variable
substIdBndr new_res_ty env bndr
  | isCoVar bndr  = substCoVarBndr env bndr
  | otherwise     = substNonCoVarIdBndr new_res_ty env bndr

---------------
substNonCoVarIdBndr
   :: Maybe OutType -- New result type, if a join binder
   -> SimplEnv
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
substNonCoVarIdBndr new_res_ty
                    env@(SimplEnv { seInScope = in_scope
                                  , seIdSubst = id_subst })
                    old_id
  = ASSERT2( not (isCoVar old_id), ppr old_id )
    (env { seInScope = in_scope `extendInScopeSet` new_id,
           seIdSubst = new_subst }, new_id)
  where
    id1    = uniqAway in_scope old_id
    id2    = substIdType env id1
    id3    | Just res_ty <- new_res_ty
           = id2 `setIdType` setJoinResTy (idJoinArity id2) res_ty (idType id2)
           | otherwise
           = id2
    new_id = zapFragileIdInfo id3       -- Zaps rules, worker-info, unfolding
                                        -- and fragile OccInfo

        -- Extend the substitution if the unique has changed,
        -- or there's some useful occurrence information
        -- See the notes with substTyVarBndr for the delSubstEnv
    new_subst | new_id /= old_id
              = extendVarEnv id_subst old_id
                             (isJoinId_maybe new_id, DoneId new_id)
              | otherwise
              = delVarEnv id_subst old_id

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

{-
Note [Arity robustness]
~~~~~~~~~~~~~~~~~~~~~~~
We *do* transfer the arity from from the in_id of a let binding to the
out_id.  This is important, so that the arity of an Id is visible in
its own RHS.  For example:
        f = \x. ....g (\y. f y)....
We can eta-reduce the arg to g, because f is a value.  But that
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
-}


{-
************************************************************************
*                                                                      *
                Impedance matching to type substitution
*                                                                      *
************************************************************************
-}

getTCvSubst :: SimplEnv -> TCvSubst
getTCvSubst (SimplEnv { seInScope = in_scope, seTvSubst = tv_env
                      , seCvSubst = cv_env })
  = mkTCvSubst in_scope (tv_env, cv_env)

substTy :: SimplEnv -> Type -> Type
substTy env ty = Type.substTy (getTCvSubst env) ty

substTyVar :: SimplEnv -> TyVar -> Type
substTyVar env tv = Type.substTyVar (getTCvSubst env) tv

substTyVarBndr :: SimplEnv -> TyVar -> (SimplEnv, TyVar)
substTyVarBndr env tv
  = case Type.substTyVarBndr (getTCvSubst env) tv of
        (TCvSubst in_scope' tv_env' cv_env', tv')
           -> (env { seInScope = in_scope', seTvSubst = tv_env', seCvSubst = cv_env' }, tv')

substCoVar :: SimplEnv -> CoVar -> Coercion
substCoVar env tv = Coercion.substCoVar (getTCvSubst env) tv

substCoVarBndr :: SimplEnv -> CoVar -> (SimplEnv, CoVar)
substCoVarBndr env cv
  = case Coercion.substCoVarBndr (getTCvSubst env) cv of
        (TCvSubst in_scope' tv_env' cv_env', cv')
           -> (env { seInScope = in_scope', seTvSubst = tv_env', seCvSubst = cv_env' }, cv')

substCo :: SimplEnv -> Coercion -> Coercion
substCo env co = Coercion.substCo (getTCvSubst env) co

------------------
substIdType :: SimplEnv -> Id -> Id
substIdType (SimplEnv { seInScope = in_scope, seTvSubst = tv_env, seCvSubst = cv_env }) id
  |  (isEmptyVarEnv tv_env && isEmptyVarEnv cv_env)
  || noFreeVarsOfType old_ty
  = id
  | otherwise = Id.setIdType id (Type.substTy (TCvSubst in_scope tv_env cv_env) old_ty)
                -- The tyCoVarsOfType is cheaper than it looks
                -- because we cache the free tyvars of the type
                -- in a Note in the id's type itself
  where
    old_ty = idType id
