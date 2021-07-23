{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[GHC.Core.Opt.Simplify.Monad]{The simplifier Monad}
-}



module GHC.Core.Opt.Simplify.Env (
        -- * The simplifier mode
        setMode, getMode, updMode, seDynFlags, seUnfoldingOpts, seLogger,

        -- * Environments
        SimplEnv(..), pprSimplEnv,   -- Temp not abstract
        mkSimplEnv, extendIdSubst,
        extendTvSubst, extendCvSubst,
        zapSubstEnv, setSubstEnv, bumpCaseDepth,
        getInScope, setInScopeFromE, setInScopeFromF,
        setInScopeSet, modifyInScope, addNewInScopeIds,
        getSimplRules,

        -- * Substitution results
        SimplSR(..), mkContEx, substId, lookupRecBndr,

        -- * Simplifying 'Id' binders
        simplNonRecBndr, simplNonRecJoinBndr, simplRecBndrs, simplRecJoinBndrs,
        simplBinder, simplBinders,
        substTy, substTyVar, getTCvSubst,
        substCo, substCoVar,

        -- * Floats
        SimplFloats(..), emptyFloats, mkRecFloats,
        mkFloatBind, addLetFloats, addJoinFloats, addFloats,
        extendFloats, wrapFloats,
        isEmptyFloats, isEmptyJoinFloats, isEmptyLetFloats,
        doFloatFromRhs, getTopFloatBinds,

        -- * LetFloats
        LetFloats, letFloatBinds, emptyLetFloats, unitLetFloat,
        addLetFlts,  mapLetFloats,

        -- * JoinFloats
        JoinFloat, JoinFloats, emptyJoinFloats,
        wrapJoinFloats, wrapJoinFloatsX, unitJoinFloat, addJoinFlts
    ) where

import GHC.Prelude

import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Opt.Monad        ( SimplMode(..) )
import GHC.Core
import GHC.Core.Utils
import GHC.Core.Multiplicity     ( scaleScaled )
import GHC.Core.Unfold
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Data.OrdList
import GHC.Types.Id as Id
import GHC.Core.Make            ( mkWildValBinder )
import GHC.Driver.Session       ( DynFlags )
import GHC.Builtin.Types
import GHC.Core.TyCo.Rep        ( TyCoBinder(..) )
import qualified GHC.Core.Type as Type
import GHC.Core.Type hiding     ( substTy, substTyVar, substTyVarBndr, extendTvSubst, extendCvSubst )
import qualified GHC.Core.Coercion as Coercion
import GHC.Core.Coercion hiding ( substCo, substCoVar, substCoVarBndr )
import GHC.Types.Basic
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Types.Unique.FM      ( pprUniqFM )

import Data.List (mapAccumL)

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

        seMode      :: !SimplMode

        -- The current substitution
      , seTvSubst   :: TvSubstEnv      -- InTyVar |--> OutType
      , seCvSubst   :: CvSubstEnv      -- InCoVar |--> OutCoercion
      , seIdSubst   :: SimplIdSubst    -- InId    |--> OutExpr

     ----------- Dynamic part of the environment -----------
     -- Dynamic in the sense of describing the setup where
     -- the expression finally ends up

        -- The current set of in-scope variables
        -- They are all OutVars, and all bound in this module
      , seInScope   :: !InScopeSet       -- OutVars only

      , seCaseDepth :: !Int  -- Depth of multi-branch case alternatives
    }

data SimplFloats
  = SimplFloats
      { -- Ordinary let bindings
        sfLetFloats  :: LetFloats
                -- See Note [LetFloats]

        -- Join points
      , sfJoinFloats :: JoinFloats
                -- Handled separately; they don't go very far
                -- We consider these to be /inside/ sfLetFloats
                -- because join points can refer to ordinary bindings,
                -- but not vice versa

        -- Includes all variables bound by sfLetFloats and
        -- sfJoinFloats, plus at least whatever is in scope where
        -- these bindings land up.
      , sfInScope :: InScopeSet  -- All OutVars
      }

instance Outputable SimplFloats where
  ppr (SimplFloats { sfLetFloats = lf, sfJoinFloats = jf, sfInScope = is })
    = text "SimplFloats"
      <+> braces (vcat [ text "lets: " <+> ppr lf
                       , text "joins:" <+> ppr jf
                       , text "in_scope:" <+> ppr is ])

emptyFloats :: SimplEnv -> SimplFloats
emptyFloats env
  = SimplFloats { sfLetFloats  = emptyLetFloats
                , sfJoinFloats = emptyJoinFloats
                , sfInScope    = seInScope env }

isEmptyFloats :: SimplFloats -> Bool
isEmptyFloats (SimplFloats { sfLetFloats = lf, sfJoinFloats = jf })
  = isEmptyLetFloats lf && isEmptyJoinFloats jf

pprSimplEnv :: SimplEnv -> SDoc
-- Used for debugging; selective
pprSimplEnv env
  = vcat [text "TvSubst:" <+> ppr (seTvSubst env),
          text "CvSubst:" <+> ppr (seCvSubst env),
          text "IdSubst:" <+> id_subst_doc,
          text "InScope:" <+> in_scope_vars_doc
    ]
  where
   id_subst_doc = pprUniqFM ppr (seIdSubst env)
   in_scope_vars_doc = pprVarSet (getInScopeVars (seInScope env))
                                 (vcat . map ppr_one)
   ppr_one v | isId v = ppr v <+> ppr (idUnfolding v)
             | otherwise = ppr v

type SimplIdSubst = IdEnv SimplSR -- IdId |--> OutExpr
        -- See Note [Extending the Subst] in GHC.Core.Subst

-- | A substitution result.
data SimplSR
  = DoneEx OutExpr (Maybe JoinArity)
       -- If  x :-> DoneEx e ja   is in the SimplIdSubst
       -- then replace occurrences of x by e
       -- and  ja = Just a <=> x is a join-point of arity a
       -- See Note [Join arity in SimplIdSubst]


  | DoneId OutId
       -- If  x :-> DoneId v   is in the SimplIdSubst
       -- then replace occurrences of x by v
       -- and  v is a join-point of arity a
       --      <=> x is a join-point of arity a

  | ContEx TvSubstEnv                 -- A suspended substitution
           CvSubstEnv
           SimplIdSubst
           InExpr
      -- If   x :-> ContEx tv cv id e   is in the SimplISubst
      -- then replace occurrences of x by (subst (tv,cv,id) e)

instance Outputable SimplSR where
  ppr (DoneId v)    = text "DoneId" <+> ppr v
  ppr (DoneEx e mj) = text "DoneEx" <> pp_mj <+> ppr e
    where
      pp_mj = case mj of
                Nothing -> empty
                Just n  -> parens (int n)

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
We have to remember which incoming variables are join points: the occurrences
may not be marked correctly yet, and we're in change of propagating the change if
OccurAnal makes something a join point).

Normally the in-scope set is where we keep the latest information, but
the in-scope set tracks only OutVars; if a binding is unconditionally
inlined (via DoneEx), it never makes it into the in-scope set, and we
need to know at the occurrence site that the variable is a join point
so that we know to drop the context. Thus we remember which join
points we're substituting. -}

mkSimplEnv :: SimplMode -> SimplEnv
mkSimplEnv mode
  = SimplEnv { seMode      = mode
             , seInScope   = init_in_scope
             , seTvSubst   = emptyVarEnv
             , seCvSubst   = emptyVarEnv
             , seIdSubst   = emptyVarEnv
             , seCaseDepth = 0 }
        -- The top level "enclosing CC" is "SUBSUMED".

init_in_scope :: InScopeSet
init_in_scope = mkInScopeSet (unitVarSet (mkWildValBinder Many unitTy))
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

There can be *occurrences* of wild-id.  For example,
GHC.Core.Make.mkCoreApp transforms
   e (a /# b)   -->   case (a /# b) of wild { DEFAULT -> e wild }
This is ok provided 'wild' isn't free in 'e', and that's the delicate
thing. Generally, you want to run the simplifier to get rid of the
wild-ids before doing much else.

It's a very dark corner of GHC.  Maybe it should be cleaned up.
-}

getMode :: SimplEnv -> SimplMode
getMode env = seMode env

seDynFlags :: SimplEnv -> DynFlags
seDynFlags env = sm_dflags (seMode env)

seLogger :: SimplEnv -> Logger
seLogger env = sm_logger (seMode env)


seUnfoldingOpts :: SimplEnv -> UnfoldingOpts
seUnfoldingOpts env = sm_uf_opts (seMode env)


setMode :: SimplMode -> SimplEnv -> SimplEnv
setMode mode env = env { seMode = mode }

updMode :: (SimplMode -> SimplMode) -> SimplEnv -> SimplEnv
updMode upd env
  = -- Avoid keeping env alive in case inlining fails.
    let mode = upd $! (seMode env)
    in env { seMode = mode }

bumpCaseDepth :: SimplEnv -> SimplEnv
bumpCaseDepth env = env { seCaseDepth = seCaseDepth env + 1 }

---------------------
extendIdSubst :: SimplEnv -> Id -> SimplSR -> SimplEnv
extendIdSubst env@(SimplEnv {seIdSubst = subst}) var res
  = assertPpr (isId var && not (isCoVar var)) (ppr var) $
    env { seIdSubst = extendVarEnv subst var res }

extendTvSubst :: SimplEnv -> TyVar -> Type -> SimplEnv
extendTvSubst env@(SimplEnv {seTvSubst = tsubst}) var res
  = assertPpr (isTyVar var) (ppr var $$ ppr res) $
    env {seTvSubst = extendVarEnv tsubst var res}

extendCvSubst :: SimplEnv -> CoVar -> Coercion -> SimplEnv
extendCvSubst env@(SimplEnv {seCvSubst = csubst}) var co
  = assert (isCoVar var) $
    env {seCvSubst = extendVarEnv csubst var co}

---------------------
getInScope :: SimplEnv -> InScopeSet
getInScope env = seInScope env

setInScopeSet :: SimplEnv -> InScopeSet -> SimplEnv
setInScopeSet env in_scope = env {seInScope = in_scope}

setInScopeFromE :: SimplEnv -> SimplEnv -> SimplEnv
-- See Note [Setting the right in-scope set]
setInScopeFromE rhs_env here_env = rhs_env { seInScope = seInScope here_env }

setInScopeFromF :: SimplEnv -> SimplFloats -> SimplEnv
setInScopeFromF env floats = env { seInScope = sfInScope floats }

addNewInScopeIds :: SimplEnv -> [CoreBndr] -> SimplEnv
        -- The new Ids are guaranteed to be freshly allocated
addNewInScopeIds env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst }) vs
-- See Note [Bangs in the Simplifier]
  = let !in_scope1 = in_scope `extendInScopeSetList` vs
        !id_subst1 = id_subst `delVarEnvList` vs
    in
    env { seInScope = in_scope1,
          seIdSubst = id_subst1 }
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

{- Note [Setting the right in-scope set]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  \x. (let x = e in b) arg[x]
where the let shadows the lambda.  Really this means something like
  \x1. (let x2 = e in b) arg[x1]

- When we capture the 'arg' in an ApplyToVal continuation, we capture
  the environment, which says what 'x' is bound to, namely x1

- Then that continuation gets pushed under the let

- Finally we simplify 'arg'.  We want
     - the static, lexical environment binding x :-> x1
     - the in-scopeset from "here", under the 'let' which includes
       both x1 and x2

It's important to have the right in-scope set, else we may rename a
variable to one that is already in scope.  So we must pick up the
in-scope set from "here", but otherwise use the environment we
captured along with 'arg'.  This transfer of in-scope set is done by
setInScopeFromE.
-}

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
\subsection{LetFloats}
*                                                                      *
************************************************************************

Note [LetFloats]
~~~~~~~~~~~~~~~~
The LetFloats is a bunch of bindings, classified by a FloatFlag.

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

data LetFloats = LetFloats (OrdList OutBind) FloatFlag
                 -- See Note [LetFloats]

type JoinFloat  = OutBind
type JoinFloats = OrdList JoinFloat

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

instance Outputable LetFloats where
  ppr (LetFloats binds ff) = ppr ff $$ ppr (fromOL binds)

instance Outputable FloatFlag where
  ppr FltLifted  = text "FltLifted"
  ppr FltOkSpec  = text "FltOkSpec"
  ppr FltCareful = text "FltCareful"

andFF :: FloatFlag -> FloatFlag -> FloatFlag
andFF FltCareful _          = FltCareful
andFF FltOkSpec  FltCareful = FltCareful
andFF FltOkSpec  _          = FltOkSpec
andFF FltLifted  flt        = flt

doFloatFromRhs :: TopLevelFlag -> RecFlag -> Bool -> SimplFloats -> OutExpr -> Bool
-- If you change this function look also at FloatIn.noFloatFromRhs
doFloatFromRhs lvl rec str (SimplFloats { sfLetFloats = LetFloats fs ff }) rhs
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

emptyLetFloats :: LetFloats
emptyLetFloats = LetFloats nilOL FltLifted

isEmptyLetFloats :: LetFloats -> Bool
isEmptyLetFloats (LetFloats fs _) = isNilOL fs

emptyJoinFloats :: JoinFloats
emptyJoinFloats = nilOL

isEmptyJoinFloats :: JoinFloats -> Bool
isEmptyJoinFloats = isNilOL

unitLetFloat :: OutBind -> LetFloats
-- This key function constructs a singleton float with the right form
unitLetFloat bind = assert (all (not . isJoinId) (bindersOf bind)) $
                    LetFloats (unitOL bind) (flag bind)
  where
    flag (Rec {})                = FltLifted
    flag (NonRec bndr rhs)
      | not (isStrictId bndr)    = FltLifted
      | exprIsTickedString rhs   = FltLifted
          -- String literals can be floated freely.
          -- See Note [Core top-level string literals] in GHC.Core.
      | exprOkForSpeculation rhs = FltOkSpec  -- Unlifted, and lifted but ok-for-spec (eg HNF)
      | otherwise                = assertPpr (not (isUnliftedType (idType bndr))) (ppr bndr)
                                   FltCareful
      -- Unlifted binders can only be let-bound if exprOkForSpeculation holds

unitJoinFloat :: OutBind -> JoinFloats
unitJoinFloat bind = assert (all isJoinId (bindersOf bind)) $
                     unitOL bind

mkFloatBind :: SimplEnv -> OutBind -> (SimplFloats, SimplEnv)
-- Make a singleton SimplFloats, and
-- extend the incoming SimplEnv's in-scope set with its binders
-- These binders may already be in the in-scope set,
-- but may have by now been augmented with more IdInfo
mkFloatBind env bind
  = (floats, env { seInScope = in_scope' })
  where
    floats
      | isJoinBind bind
      = SimplFloats { sfLetFloats  = emptyLetFloats
                    , sfJoinFloats = unitJoinFloat bind
                    , sfInScope    = in_scope' }
      | otherwise
      = SimplFloats { sfLetFloats  = unitLetFloat bind
                    , sfJoinFloats = emptyJoinFloats
                    , sfInScope    = in_scope' }
    -- See Note [Bangs in the Simplifier]
    !in_scope' = seInScope env `extendInScopeSetBind` bind

extendFloats :: SimplFloats -> OutBind -> SimplFloats
-- Add this binding to the floats, and extend the in-scope env too
extendFloats (SimplFloats { sfLetFloats  = floats
                          , sfJoinFloats = jfloats
                          , sfInScope    = in_scope })
             bind
  | isJoinBind bind
  = SimplFloats { sfInScope    = in_scope'
                , sfLetFloats  = floats
                , sfJoinFloats = jfloats' }
  | otherwise
  = SimplFloats { sfInScope    = in_scope'
                , sfLetFloats  = floats'
                , sfJoinFloats = jfloats }
  where
    in_scope' = in_scope `extendInScopeSetBind` bind
    floats'   = floats  `addLetFlts`  unitLetFloat bind
    jfloats'  = jfloats `addJoinFlts` unitJoinFloat bind

addLetFloats :: SimplFloats -> LetFloats -> SimplFloats
-- Add the let-floats for env2 to env1;
-- *plus* the in-scope set for env2, which is bigger
-- than that for env1
addLetFloats floats let_floats
  = floats { sfLetFloats = sfLetFloats floats `addLetFlts` let_floats
           , sfInScope   = sfInScope floats `extendInScopeFromLF` let_floats }

extendInScopeFromLF :: InScopeSet -> LetFloats -> InScopeSet
extendInScopeFromLF in_scope (LetFloats binds _)
  = foldlOL extendInScopeSetBind in_scope binds

addJoinFloats :: SimplFloats -> JoinFloats -> SimplFloats
addJoinFloats floats join_floats
  = floats { sfJoinFloats = sfJoinFloats floats `addJoinFlts` join_floats
           , sfInScope    = foldlOL extendInScopeSetBind
                                    (sfInScope floats) join_floats }

extendInScopeSetBind :: InScopeSet -> CoreBind -> InScopeSet
extendInScopeSetBind in_scope bind
  = extendInScopeSetList in_scope (bindersOf bind)

addFloats :: SimplFloats -> SimplFloats -> SimplFloats
-- Add both let-floats and join-floats for env2 to env1;
-- *plus* the in-scope set for env2, which is bigger
-- than that for env1
addFloats (SimplFloats { sfLetFloats = lf1, sfJoinFloats = jf1 })
          (SimplFloats { sfLetFloats = lf2, sfJoinFloats = jf2, sfInScope = in_scope })
  = SimplFloats { sfLetFloats  = lf1 `addLetFlts` lf2
                , sfJoinFloats = jf1 `addJoinFlts` jf2
                , sfInScope    = in_scope }

addLetFlts :: LetFloats -> LetFloats -> LetFloats
addLetFlts (LetFloats bs1 l1) (LetFloats bs2 l2)
  = LetFloats (bs1 `appOL` bs2) (l1 `andFF` l2)

letFloatBinds :: LetFloats -> [CoreBind]
letFloatBinds (LetFloats bs _) = fromOL bs

addJoinFlts :: JoinFloats -> JoinFloats -> JoinFloats
addJoinFlts = appOL

mkRecFloats :: SimplFloats -> SimplFloats
-- Flattens the floats into a single Rec group,
-- They must either all be lifted LetFloats or all JoinFloats
mkRecFloats floats@(SimplFloats { sfLetFloats  = LetFloats bs _ff
                                , sfJoinFloats = jbs
                                , sfInScope    = in_scope })
  = assertPpr (isNilOL bs || isNilOL jbs) (ppr floats) $
    SimplFloats { sfLetFloats  = floats'
                , sfJoinFloats = jfloats'
                , sfInScope    = in_scope }
  where
    -- See Note [Bangs in the Simplifier]
    !floats'  | isNilOL bs  = emptyLetFloats
              | otherwise   = unitLetFloat (Rec (flattenBinds (fromOL bs)))
    !jfloats' | isNilOL jbs = emptyJoinFloats
              | otherwise   = unitJoinFloat (Rec (flattenBinds (fromOL jbs)))

wrapFloats :: SimplFloats -> OutExpr -> OutExpr
-- Wrap the floats around the expression; they should all
-- satisfy the let/app invariant, so mkLets should do the job just fine
wrapFloats (SimplFloats { sfLetFloats  = LetFloats bs _
                        , sfJoinFloats = jbs }) body
  = foldrOL Let (wrapJoinFloats jbs body) bs
     -- Note: Always safe to put the joins on the inside
     -- since the values can't refer to them

wrapJoinFloatsX :: SimplFloats -> OutExpr -> (SimplFloats, OutExpr)
-- Wrap the sfJoinFloats of the env around the expression,
-- and take them out of the SimplEnv
wrapJoinFloatsX floats body
  = ( floats { sfJoinFloats = emptyJoinFloats }
    , wrapJoinFloats (sfJoinFloats floats) body )

wrapJoinFloats :: JoinFloats -> OutExpr -> OutExpr
-- Wrap the sfJoinFloats of the env around the expression,
-- and take them out of the SimplEnv
wrapJoinFloats join_floats body
  = foldrOL Let body join_floats

getTopFloatBinds :: SimplFloats -> [CoreBind]
getTopFloatBinds (SimplFloats { sfLetFloats  = lbs
                              , sfJoinFloats = jbs})
  = assert (isNilOL jbs) $  -- Can't be any top-level join bindings
    letFloatBinds lbs

{-# INLINE mapLetFloats #-}
mapLetFloats :: LetFloats -> ((Id,CoreExpr) -> (Id,CoreExpr)) -> LetFloats
mapLetFloats (LetFloats fs ff) fun
   = LetFloats fs1 ff
   where
    app (NonRec b e) = case fun (b,e) of (b',e') -> NonRec b' e'
    app (Rec bs)     = Rec (strictMap fun bs)
    !fs1 = (mapOL' app fs) -- See Note [Bangs in the Simplifier]

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
  = case lookupVarEnv ids v of  -- Note [Global Ids in the substitution]
        Nothing               -> DoneId (refineFromInScope in_scope v)
        Just (DoneId v)       -> DoneId (refineFromInScope in_scope v)
        Just res              -> res    -- DoneEx non-var, or ContEx

        -- Get the most up-to-date thing from the in-scope set
        -- Even though it isn't in the substitution, it may be in
        -- the in-scope set with better IdInfo.
        --
        -- See also Note [In-scope set as a substitution] in GHC.Core.Opt.Simplify.

refineFromInScope :: InScopeSet -> Var -> Var
refineFromInScope in_scope v
  | isLocalId v = case lookupInScope in_scope v of
                  Just v' -> v'
                  Nothing -> pprPanic "refineFromInScope" (ppr in_scope $$ ppr v)
                             -- c.f #19074 for a subtle place where this went wrong
  | otherwise = v

lookupRecBndr :: SimplEnv -> InId -> OutId
-- Look up an Id which has been put into the envt by simplRecBndrs,
-- but where we have not yet done its RHS
lookupRecBndr (SimplEnv { seInScope = in_scope, seIdSubst = ids }) v
  = case lookupVarEnv ids v of
        Just (DoneId v) -> v
        Just _ -> pprPanic "lookupRecBndr" (ppr v)
        Nothing -> refineFromInScope in_scope v

{-
************************************************************************
*                                                                      *
\section{Substituting an Id binder}
*                                                                      *
************************************************************************


These functions are in the monad only so that they can be made strict via seq.

Note [Return type for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

   (join j :: Char -> Int -> Int) 77
   (     j x = \y. y + ord x    )
   (in case v of                )
   (     A -> j 'x'             )
   (     B -> j 'y'             )
   (     C -> <blah>            )

The simplifier pushes the "apply to 77" continuation inwards to give

   join j :: Char -> Int
        j x = (\y. y + ord x) 77
   in case v of
        A -> j 'x'
        B -> j 'y'
        C -> <blah> 77

Notice that the "apply to 77" continuation went into the RHS of the
join point.  And that meant that the return type of the join point
changed!!

That's why we pass res_ty into simplNonRecJoinBndr, and substIdBndr
takes a (Just res_ty) argument so that it knows to do the type-changing
thing.

See also Note [Scaling join point arguments].
-}

simplBinders :: SimplEnv -> [InBndr] -> SimplM (SimplEnv, [OutBndr])
simplBinders  !env bndrs = mapAccumLM simplBinder  env bndrs

-------------
simplBinder :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- Used for lambda and case-bound variables
-- Clone Id if necessary, substitute type
-- Return with IdInfo already substituted, but (fragile) occurrence info zapped
-- The substitution is extended only if the variable is cloned, because
-- we *don't* need to use it to track occurrence info.
simplBinder !env bndr
  | isTyVar bndr  = do  { let (env', tv) = substTyVarBndr env bndr
                        ; seqTyVar tv `seq` return (env', tv) }
  | otherwise     = do  { let (env', id) = substIdBndr env bndr
                        ; seqId id `seq` return (env', id) }

---------------
simplNonRecBndr :: SimplEnv -> InBndr -> SimplM (SimplEnv, OutBndr)
-- A non-recursive let binder
simplNonRecBndr !env id
  -- See Note [Bangs in the Simplifier]
  = do  { let (!env1, id1) = substIdBndr env id
        ; seqId id1 `seq` return (env1, id1) }

---------------
simplRecBndrs :: SimplEnv -> [InBndr] -> SimplM SimplEnv
-- Recursive let binders
simplRecBndrs env@(SimplEnv {}) ids
  -- See Note [Bangs in the Simplifier]
  = assert (all (not . isJoinId) ids) $
    do  { let (!env1, ids1) = mapAccumL substIdBndr env ids
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
-- Similar to GHC.Core.Subst.substIdBndr, except that
--      the type of id_subst differs
--      all fragile info is zapped
substNonCoVarIdBndr env id = subst_id_bndr env id (\x -> x)

-- Inline to make the (OutId -> OutId) function a known call.
-- This is especially important for `substNonCoVarIdBndr` which
-- passes an identity lambda.
{-# INLINE subst_id_bndr #-}
subst_id_bndr :: SimplEnv
              -> InBndr    -- Env and binder to transform
              -> (OutId -> OutId)  -- Adjust the type
              -> (SimplEnv, OutBndr)
subst_id_bndr env@(SimplEnv { seInScope = in_scope, seIdSubst = id_subst })
              old_id adjust_type
  = assertPpr (not (isCoVar old_id)) (ppr old_id)
    (env { seInScope = new_in_scope,
           seIdSubst = new_subst }, new_id)
    -- It's important that both seInScope and seIdSubst are updated with
    -- the new_id, /after/ applying adjust_type. That's why adjust_type
    -- is done here.  If we did adjust_type in simplJoinBndr (the only
    -- place that gives a non-identity adjust_type) we'd have to fiddle
    -- afresh with both seInScope and seIdSubst
  where
    -- See Note [Bangs in the Simplifier]
    !id1  = uniqAway in_scope old_id
    !id2  = substIdType env id1
    !id3  = zapFragileIdInfo id2       -- Zaps rules, worker-info, unfolding
                                      -- and fragile OccInfo
    !new_id = adjust_type id3

        -- Extend the substitution if the unique has changed,
        -- or there's some useful occurrence information
        -- See the notes with substTyVarBndr for the delSubstEnv
    !new_subst | new_id /= old_id
              = extendVarEnv id_subst old_id (DoneId new_id)
              | otherwise
              = delVarEnv id_subst old_id

    !new_in_scope = in_scope `extendInScopeSet` new_id

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
We *do* transfer the arity from the in_id of a let binding to the
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


{- *********************************************************************
*                                                                      *
                Join points
*                                                                      *
********************************************************************* -}

simplNonRecJoinBndr :: SimplEnv -> InBndr
                    -> Mult -> OutType
                    -> SimplM (SimplEnv, OutBndr)

-- A non-recursive let binder for a join point;
-- context being pushed inward may change the type
-- See Note [Return type for join points]
simplNonRecJoinBndr env id mult res_ty
  = do { let (env1, id1) = simplJoinBndr mult res_ty env id
       ; seqId id1 `seq` return (env1, id1) }

simplRecJoinBndrs :: SimplEnv -> [InBndr]
                  -> Mult -> OutType
                  -> SimplM SimplEnv
-- Recursive let binders for join points;
-- context being pushed inward may change types
-- See Note [Return type for join points]
simplRecJoinBndrs env@(SimplEnv {}) ids mult res_ty
  = assert (all isJoinId ids) $
    do  { let (env1, ids1) = mapAccumL (simplJoinBndr mult res_ty) env ids
        ; seqIds ids1 `seq` return env1 }

---------------
simplJoinBndr :: Mult -> OutType
              -> SimplEnv -> InBndr
              -> (SimplEnv, OutBndr)
simplJoinBndr mult res_ty env id
  = subst_id_bndr env id (adjustJoinPointType mult res_ty)

---------------
adjustJoinPointType :: Mult
                    -> Type     -- New result type
                    -> Id       -- Old join-point Id
                    -> Id       -- Adjusted jont-point Id
-- (adjustJoinPointType mult new_res_ty join_id) does two things:
--
--   1. Set the return type of the join_id to new_res_ty
--      See Note [Return type for join points]
--
--   2. Adjust the multiplicity of arrows in join_id's type, as
--      directed by 'mult'. See Note [Scaling join point arguments]
--
-- INVARIANT: If any of the first n binders are foralls, those tyvars
-- cannot appear in the original result type. See isValidJoinPointType.
adjustJoinPointType mult new_res_ty join_id
  = assert (isJoinId join_id) $
    setIdType join_id new_join_ty
  where
    orig_ar = idJoinArity join_id
    orig_ty = idType join_id

    new_join_ty = go orig_ar orig_ty :: Type

    go 0 _  = new_res_ty
    go n ty | Just (arg_bndr, res_ty) <- splitPiTy_maybe ty
            = mkPiTy (scale_bndr arg_bndr) $
              go (n-1) res_ty
            | otherwise
            = pprPanic "adjustJoinPointType" (ppr orig_ar <+> ppr orig_ty)

    -- See Note [Bangs in the Simplifier]
    scale_bndr (Anon af t) = Anon af $! (scaleScaled mult t)
    scale_bndr b@(Named _) = b

{- Note [Scaling join point arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a join point which is linear in its variable, in some context E:

E[join j :: a %1 -> a
       j x = x
  in case v of
       A -> j 'x'
       B -> <blah>]

The simplifier changes to:

join j :: a %1 -> a
     j x = E[x]
in case v of
     A -> j 'x'
     B -> E[<blah>]

If E uses its argument in a nonlinear way (e.g. a case['Many]), then
this is wrong: the join point has to change its type to a -> a.
Otherwise, we'd get a linearity error.

See also Note [Return type for join points] and Note [Join points and case-of-case].
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

substTy :: HasDebugCallStack => SimplEnv -> Type -> Type
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
  | (isEmptyVarEnv tv_env && isEmptyVarEnv cv_env)
    || no_free_vars
  = id
  | otherwise = Id.updateIdTypeAndMult (Type.substTyUnchecked subst) id
                -- The tyCoVarsOfType is cheaper than it looks
                -- because we cache the free tyvars of the type
                -- in a Note in the id's type itself
  where
    no_free_vars = noFreeVarsOfType old_ty && noFreeVarsOfType old_w
    subst = TCvSubst in_scope tv_env cv_env
    old_ty = idType id
    old_w  = varMult id
