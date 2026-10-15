{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[GHC.Core.Opt.Simplify.Monad]{The simplifier Monad}
-}



module GHC.Core.Opt.Simplify.Env (
        -- * The simplifier mode
        SimplMode(..), updMode, smPlatform,

        -- * Environments
        SimplEnv(..), pprSimplEnv,   -- Temp not abstract
        SimplPhase(..), isActive, simplStartPhase, simplEndPhase,
        seArityOpts, seCaseCase, seCaseFolding, seCaseMerge, seCastSwizzle,
        seDoEtaReduction, seEtaExpand, seFloatEnable, seInline, seNames,
        seOptCoercionOpts, sePhase, sePlatform, sePreInline,
        seRuleOpts, seRules, seUnfoldingOpts,
        mkSimplEnv, extendIdSubst, extendCvIdSubst,
        extendTvSubst, extendCvSubst,
        zapSubstEnv, setSubstEnv, bumpCaseDepth,
        getInScope, setInScopeFromE, setInScopeFromF,
        setInScopeSet, modifyInScope, addNewInScopeIds,
        getSimplRules, enterRecGroupRHSs,
        reSimplifying,

        SimplEnvIS,  checkSimplEnvIS, pprBadSimplEnvIS,

        -- * Substitution results
        SimplSR(..), substId, lookupRecBndr,

        -- * Simplifying 'Id' binders
        simplNonRecBndr, simplNonRecJoinBndr, simplRecBndrs, simplRecJoinBndrs,
        simplBinder, simplBinders,
        substTy, substTyVar, getFullSubst, getTCvSubst,
        substCo, substCoVar,

        -- * Floats
        SimplFloats(..), emptyFloats, isEmptyFloats, mkRecFloats,
        mkFloatBind, addLetFloat, addLetFloats, addCaseFloat, mkLetSimplFloats,
        addJoinFloats, addFloats, addFloatBindsF,
        extendFloats, isEmptyJoinFloats,
        doFloatFromRhs, getTopFloatBinds, onlyLazyLetFloats, doCaseToLet,

        -- * LetFloats
        LetFloats, FloatEnable(..), emptyLetFloats, mapLetFloats,

        -- * JoinFloats
        JoinFloat, JoinFloats, emptyJoinFloats,
        wrapJoinFloats, wrapJoinFloatsX, unitJoinFloat, addJoinFlts
    ) where

import GHC.Prelude

import GHC.Core.Coercion.Opt ( OptCoercionOpts )
import GHC.Core.FamInstEnv ( FamInstEnv )
import GHC.Core.Opt.Arity ( ArityOpts(..) )
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Rules.Config ( RuleOpts(..) )
import GHC.Core
import GHC.Core.Utils
import GHC.Core.Subst( substExprSC )
import GHC.Core.Unfold
import GHC.Core.TyCo.Subst (emptyIdSubstEnv, mkSubst)
import GHC.Core.Multiplicity( Scaled(..), mkMultMul )
import GHC.Core.Make            ( mkWildValBinder )
import GHC.Core.Type hiding     ( substTy, substTyVar, substTyVarBndr, substCo
                                , extendTvSubst, extendCvSubst )
import qualified GHC.Core.Coercion as Coercion
import GHC.Core.Coercion hiding ( substCo, substCoVar, substCoVarBndr )
import qualified GHC.Core.Type as Type

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Id as Id
import GHC.Types.InlinePragma ( ActivationGhc, CompilerPhase, isActiveInPhase )
import GHC.Types.Basic
import GHC.Types.Tickish ( tickishHasSoftScope )
import GHC.Types.Unique.FM      ( pprUniqFM )

import GHC.Data.OrdList
import GHC.Data.Graph.UnVar

import GHC.Builtin.WiredIn.Types
import GHC.Platform ( Platform )

import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

import Data.List ( intersperse, mapAccumL )

{-
************************************************************************
*                                                                      *
\subsubsection{The @SimplEnv@ type}
*                                                                      *
************************************************************************
-}

{-
Note [The environments of the Simplify pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions of the Simplify pass draw their contextual data from two
environments: `SimplEnv`, which is passed to the functions as an argument, and
`SimplTopEnv`, which is part of the `SimplM` monad. For both environments exist
corresponding configuration records `SimplMode` and `TopEnvConfig` respectively.
A configuration record denotes a unary datatype bundeling the various options
and switches we provide to control the behaviour of the respective part of the
Simplify pass. The value is provided by the driver using the functions found in
the GHC.Driver.Config.Core.Opt.Simplify module.

These configuration records are part in the environment to avoid needless
copying of their values. This raises the question which data value goes in which
of the four datatypes. For each value needed by the pass we ask the following
two questions:

 * Does the value only make sense in a monadic environment?

 * Is it part of the configuration of the pass and provided by the user or is it
   it an internal value?

Examples of values that make only sense in conjunction with `SimplM` are the
logger and the values related to counting. As it does not make sense to use them
in a pure function (the logger needs IO and counting needs access to the
accumulated counts in the monad) we want these to live in `SimplTopEnv`.
Other values, like the switches controlling the behaviour of the pass (e.g.
whether to do case merging or not) are perfectly usable in a non-monadic setting.
Indeed many of those are used in guard expressions and it would be cumbersome to
query them from the monadic environment and feed them to the pure functions as
an argument. Hence we conveniently store them in the `SpecEnv` environment which
can be passed to pure functions as a whole.

Now that we know in which of the two environments a particular value lives we
turn to the second question to determine if the value is part of the
configuration record embedded in the environment or if it is stored in an own
field in the environment type. Some values like the tick factor must be provided
from outside as we can neither derive it from other values provided to us nor
does a constant value make sense. Other values like the maximal number of ticks
are computed on environment initialization and we wish not to expose the field
to the "user" or the pass -- it is an internal value. Therefore the distinction
here is between "freely set by the caller" and "internally managed by the pass".

Note that it doesn't matter for the decision procedure wheter a value is altered
throughout an iteration of the Simplify pass: The fields sm_phase, sm_inline,
sm_rules, sm_cast_swizzle and sm_eta_expand are updated locally (See the
definitions of `updModeForStableUnfoldings` and `updModeForRule{LHS,RHS}` in
GHC.Core.Opt.Simplify.Utils) but they are still part of `SimplMode` as the
caller of the Simplify pass needs to provide the initial values for those fields.

The decision which value goes into which datatype can be summarized by the
following table:
                                 |          Usable in a           |
                                 | pure setting | monadic setting |
    |----------------------------|--------------|-----------------|
    | Set by user                | SimplMode    | TopEnvConfig    |
    | Computed on initialization | SimplEnv     | SimplTopEnv     |

Note [Inline depth]
~~~~~~~~~~~~~~~~~~~
When we inline an /already-simplified/ unfolding, we
* Zap the substitution environment; the inlined thing is an OutExpr
* Bump the seInlineDepth in the SimplEnv
Both these tasks are done in zapSubstEnv.

The seInlineDepth tells us how deep in inlining we are.  Currently,
seInlineDepth is used for just one purpose: when we encounter a
coercion we don't apply optCoercion to it if seInlineDepth>0.
Reason: it has already been optimised once, no point in doing so again.
-}

data SimplEnv
  = SimplEnv {
     ----------- Static part of the environment -----------
     -- Static in the sense of lexically scoped,
     -- wrt the original expression

        -- See Note [The environments of the Simplify pass]
        seMode      :: !SimplMode
      , seFamEnvs   :: !(FamInstEnv, FamInstEnv)

        -- The current substitution
      , seTvSubst   :: TvSubstEnv      -- InTyVar |--> OutType
      , seCvSubst   :: CvSubstEnv      -- InCoVar |--> OutCoercion
      , seIdSubst   :: SimplIdSubst    -- InId    |--> OutExpr

        -- | Fast OutVarSet tracking which recursive RHSs we are analysing.
        -- See Note [Eta reduction in recursive RHSs] in GHC.Core.Opt.Arity.
      , seRecIds :: !UnVarSet

     ----------- Dynamic part of the environment -----------
     -- Dynamic in the sense of describing the setup where
     -- the expression finally ends up

        -- The current set of in-scope variables
        -- They are all OutVars, and all bound in this module
      , seInScope   :: !InScopeSet       -- OutVars only

      , seCaseDepth   :: !Int  -- Depth of multi-branch case alternatives

      , seInlineDepth :: !Int  -- 0 initially, 1 when we inline an already-simplified
                               -- unfolding, and simplify again; and so on
                               -- See Note [Inline depth]
    }

type SimplEnvIS = SimplEnv
     -- Invariant: the substitution is empty
     -- We want this SimplEnv for its InScopeSet and flags

checkSimplEnvIS :: SimplEnvIS -> Bool
-- Check the invariant for SimplEnvIS
checkSimplEnvIS (SimplEnv { seIdSubst = id_env, seTvSubst = tv_env, seCvSubst = cv_env })
  = isEmptyVarEnv id_env && isEmptyVarEnv tv_env && isEmptyVarEnv cv_env

pprBadSimplEnvIS :: SimplEnvIS -> SDoc
-- Print a SimplEnv that fails checkSimplEnvIS
pprBadSimplEnvIS env = ppr (getFullSubst (seInScope env) env)

seArityOpts :: SimplEnv -> ArityOpts
seArityOpts env = sm_arity_opts (seMode env)

seCaseCase :: SimplEnv -> Bool
seCaseCase env = sm_case_case (seMode env)

seCaseFolding :: SimplEnv -> Bool
seCaseFolding env = sm_case_folding (seMode env)

seCaseMerge :: SimplEnv -> Bool
seCaseMerge env = sm_case_merge (seMode env)

seCastSwizzle :: SimplEnv -> Bool
seCastSwizzle env = sm_cast_swizzle (seMode env)

seDoEtaReduction :: SimplEnv -> Bool
seDoEtaReduction env = sm_do_eta_reduction (seMode env)

seEtaExpand :: SimplEnv -> Bool
seEtaExpand env = sm_eta_expand (seMode env)

seFloatEnable :: SimplEnv -> FloatEnable
seFloatEnable env = sm_float_enable (seMode env)

seInline :: SimplEnv -> Bool
seInline env = sm_inline (seMode env)

seNames :: SimplEnv -> [String]
seNames env = sm_names (seMode env)

seOptCoercionOpts :: SimplEnv -> OptCoercionOpts
seOptCoercionOpts env = sm_co_opt_opts (seMode env)

sePhase :: SimplEnv -> SimplPhase
sePhase env = sm_phase (seMode env)

sePlatform :: SimplEnv -> Platform
sePlatform env = smPlatform (seMode env)

sePreInline :: SimplEnv -> Bool
sePreInline env = sm_pre_inline (seMode env)

seRuleOpts :: SimplEnv -> RuleOpts
seRuleOpts env = sm_rule_opts (seMode env)

seRules :: SimplEnv -> Bool
seRules env = sm_rules (seMode env)

seUnfoldingOpts :: SimplEnv -> UnfoldingOpts
seUnfoldingOpts env = sm_uf_opts (seMode env)

-- See Note [The environments of the Simplify pass]
data SimplMode = SimplMode -- See comments in GHC.Core.Opt.Simplify.Monad
  { sm_phase        :: !SimplPhase    -- ^ The phase of the simplifier
  , sm_names        :: ![String]      -- ^ Name(s) of the phase
  , sm_rules        :: !Bool          -- ^ Whether RULES are enabled
  , sm_inline       :: !Bool          -- ^ Whether inlining is enabled
  , sm_eta_expand   :: !Bool          -- ^ Whether eta-expansion is enabled
  , sm_cast_swizzle :: !Bool          -- ^ Do we swizzle casts past lambdas?
  , sm_uf_opts      :: !UnfoldingOpts -- ^ Unfolding options
  , sm_case_case    :: !Bool          -- ^ Whether case-of-case is enabled
  , sm_pre_inline   :: !Bool          -- ^ Whether pre-inlining is enabled
  , sm_float_enable :: !FloatEnable   -- ^ Whether to enable floating out
  , sm_do_eta_reduction :: !Bool
  , sm_arity_opts :: !ArityOpts
  , sm_rule_opts :: !RuleOpts
  , sm_case_folding :: !Bool
  , sm_case_merge :: !Bool
  , sm_co_opt_opts :: !OptCoercionOpts -- ^ Coercion optimiser options
  }

-- | See Note [SimplPhase]
data SimplPhase
  -- | A simplifier phase: InitialPhase, Phase 2, Phase 1, Phase 0, FinalPhase
  -- NB: (SimplPhase p) is equivalent to (SimplPhaseRange p p)
  = SimplPhase CompilerPhase

  -- | Simplifying the RHS of a rule or of a stable unfolding: the range of
  -- phases of the activation of the rule/stable unfolding.
  --
  -- _Invariant:_ 'simplStartPhase' is not a later phase than 'simplEndPhase'.
  -- Equivalently, 'SimplPhaseRange' is always a non-empty interval of phases.
  --
  -- See Note [What is active in the RHS of a RULE or unfolding?]
  --     in GHC.Core.Opt.Simplify.Utils.
  | SimplPhaseRange CompilerPhase CompilerPhase

  deriving Eq

simplStartPhase :: SimplPhase -> CompilerPhase
simplStartPhase (SimplPhase p)        = p
simplStartPhase (SimplPhaseRange p _) = p

simplEndPhase :: SimplPhase -> CompilerPhase
simplEndPhase (SimplPhase p)        = p
simplEndPhase (SimplPhaseRange _ p) = p

instance Outputable SimplPhase where
  ppr (SimplPhase p) = ppr p
  ppr (SimplPhaseRange s e) = brackets $ ppr s <> ellipsis <> ppr e

-- | Is this activation active in this simplifier phase?
--
-- For a phase range, @isActive simpl_phase_range act@ is true if and only if
-- @act@ is active throughout the entire range, as per
-- Note [What is active in the RHS of a RULE or unfolding?]
-- in GHC.Core.Opt.Simplify.Utils.
--
-- See Note [SimplPhase].
isActive :: SimplPhase -> ActivationGhc -> Bool
isActive (SimplPhase p) act
  = isActiveInPhase p act
isActive (SimplPhaseRange start end) act
  = -- To check whether the activation is active throughout the whole phase range,
    -- it's sufficient to check the endpoints of the phase range, because an
    -- activation can never have gaps (all activations are phase intervals).
    isActiveInPhase start act && isActiveInPhase end act

{- Note [SimplPhase]
~~~~~~~~~~~~~~~~~~~~
In general, the simplifier is invoked in successive phases:

  InitialPhase, Phase 2, Phase 1, Phase 0, FinalPhase

This allows us to control which rules, specialisations and inlinings are
active at any given point. For example,

  {-# RULE "myRule" [1] lhs = rhs #-}

starts being active in Phase 1, and stays active thereafter. Thus it is active
in Phase 1, Phase 0, FinalPhase, but not active in InitialPhase or Phase 2.

This simplifier phase is stored in the sm_phase field of SimplMode, usin
the 'SimplPhase' constructor. This allows us to determine which rules/inlinings
are active.

When we invoke the simplifier on the RHS of a rule, such as 'rhs' above, instead
of setting the simplifier mode to a single phase, we use a phase range
corresponding to the range of phases in which the rule is active, with the
'SimplPhaseRange' constructor. This allows us to check whether other rules or
inlinings are active throughout the whole activation of the rule.
See Note [What is active in the RHS of a RULE or unfolding?] in GHC.Core.Opt.Simplify.Utils.
-}

instance Outputable SimplMode where
    ppr (SimplMode { sm_phase = phase , sm_names = ss
                   , sm_rules = r, sm_inline = i
                   , sm_cast_swizzle = cs
                   , sm_eta_expand = eta, sm_case_case = cc })
       = text "SimplMode" <+> braces (
         sep [ text "Phase =" <+> ppr phase <+>
               brackets (text (concat $ intersperse "," ss)) <> comma
             , pp_flag i   (text "inline") <> comma
             , pp_flag r   (text "rules") <> comma
             , pp_flag eta (text "eta-expand") <> comma
             , pp_flag cs (text "cast-swizzle") <> comma
             , pp_flag cc  (text "case-of-case") ])
         where
           pp_flag f s = ppUnless f (text "no") <+> s

smPlatform :: SimplMode -> Platform
smPlatform opts = roPlatform (sm_rule_opts opts)

data FloatEnable  -- Controls local let-floating
  = FloatDisabled      -- Do no local let-floating
  | FloatNestedOnly    -- Local let-floating for nested (NotTopLevel) bindings only
  | FloatEnabled       -- Do local let-floating on all bindings

{- Note [Local floating]
~~~~~~~~~~~~~~~~~~~~~~~~
The Simplifier can perform local let-floating: it floats let-bindings
out of the RHS of let-bindings.  See
  Let-floating: moving bindings to give faster programs (ICFP'96)
  https://www.microsoft.com/en-us/research/publication/let-floating-moving-bindings-to-give-faster-programs/

Here's an example
   x = let y = v+1 in (y,true)

The RHS of x is a thunk.  Much better to float that y-binding out to give
   y = v+1
   x = (y,true)

Not only have we avoided building a thunk, but any (case x of (p,q) -> ...) in
the scope of the x-binding can now be simplified.

This local let-floating is done in GHC.Core.Opt.Simplify.prepareBinding,
controlled by the predicate GHC.Core.Opt.Simplify.Env.doFloatFromRhs.

The `FloatEnable` data type controls where local let-floating takes place;
it allows you to specify that it should be done only for nested bindings;
or for top-level bindings as well; or not at all.

Note that all of this is quite separate from the global FloatOut pass;
see GHC.Core.Opt.FloatOut.

-}

data SimplFloats
  = SimplFloats
      { -- Ordinary let bindings, case-floats and tick-floats
        sfLetFloats  :: FloatBinds
                -- See Note [LetFloats]
                -- Invariant: every FloatLet satisfies the let-can-float invariant

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
      <+> braces (vcat [ text "lets: " <+> ppr (fromOL lf)
                       , text "joins:" <+> ppr jf
                       , text "in_scope:" <+> ppr is ])

emptyFloats :: SimplEnvIS -> SimplFloats
emptyFloats env
  = SimplFloats { sfLetFloats  = nilOL
                , sfJoinFloats = emptyJoinFloats
                , sfInScope    = seInScope env }

isEmptyFloats :: SimplFloats -> Bool
-- Precondition: used only when sfJoinFloats is empty
isEmptyFloats (SimplFloats { sfLetFloats  = fs
                           , sfJoinFloats = js })
  = assertPpr (isNilOL js) (ppr js ) $
    isNilOL fs

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
        -- See Note [Extending the IdSubstEnv] in GHC.Core.Subst

-- | A substitution result.
data SimplSR
  = DoneEx OutExpr JoinPointHood
       -- If  x :-> DoneEx e ja   is in the SimplIdSubst
       -- then replace occurrences of x by e
       -- and  ja = Just a <=> x is a join-point of arity a
       -- See Note [Join arity in SimplIdSubst]


  | DoneId OutId
       -- If  x :-> DoneId v   is in the SimplIdSubst
       -- then replace occurrences of x by v
       -- and  v is a join-point of arity a
       --      <=> x is a join-point of arity a

  | ContEx SimplEnv
           InExpr
           MOutCoercion  -- See Note [The sc_cast field of ApplyToVal]
      -- If   x :-> ContEx static_env e mco   is in the SimplISubst
      -- then replace occurrences of x by (subst static_env e) |> mco

instance Outputable SimplSR where
  ppr (DoneId v)    = text "DoneId" <+> ppr v
  ppr (DoneEx e mj) = text "DoneEx" <> pp_mj <+> ppr e
    where
      pp_mj = case mj of
                NotJoinPoint -> empty
                JoinPoint n  -> parens (int n)

  ppr (ContEx _env e _mco) = text "ContEx" <+> ppr e
                             -- ppr (filter_env tv), ppr (filter_env id)
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
  But sometimes we have two in-scope Ids that are synonyms, and should
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

mkSimplEnv :: SimplMode -> (FamInstEnv, FamInstEnv) -> SimplEnv
mkSimplEnv mode fam_envs
  = SimplEnv { seMode        = mode
             , seFamEnvs     = fam_envs
             , seInScope     = init_in_scope
             , seTvSubst     = emptyVarEnv
             , seCvSubst     = emptyVarEnv
             , seIdSubst     = emptyVarEnv
             , seRecIds      = emptyUnVarSet
             , seCaseDepth   = 0
             , seInlineDepth = 0 }
        -- The top level "enclosing CC" is "SUBSUMED".

init_in_scope :: InScopeSet
init_in_scope = mkInScopeSet (unitVarSet (mkWildValBinder ManyTy unitTy))
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

updMode :: (SimplMode -> SimplMode) -> SimplEnv -> SimplEnv
updMode upd env
  = -- Avoid keeping env alive in case inlining fails.
    let mode = upd $! (seMode env)
    in env { seMode = mode }

bumpCaseDepth :: SimplEnv -> SimplEnv
bumpCaseDepth env = env { seCaseDepth = seCaseDepth env + 1 }

reSimplifying :: SimplEnv -> Bool
reSimplifying (SimplEnv { seInlineDepth = n }) = n>0

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

extendCvIdSubst :: SimplEnv -> Id -> OutExpr -> SimplEnv
extendCvIdSubst env bndr (Coercion co) = extendCvSubst env bndr co
extendCvIdSubst env bndr rhs           = extendIdSubst env bndr (DoneEx rhs NotJoinPoint)

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

enterRecGroupRHSs :: SimplEnv -> [OutBndr] -> (SimplEnv -> SimplM (r, SimplEnv))
                  -> SimplM (r, SimplEnv)
enterRecGroupRHSs env bndrs k = do
  --pprTraceM "enterRecGroupRHSs" (ppr bndrs)
  (r, env'') <- k env{seRecIds = extendUnVarSetList bndrs (seRecIds env)}
  return (r, env''{seRecIds = seRecIds env})

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
-- See Note [Inline depth]
-- We call zapSubstEnv precisely when we are about to
-- simplify an already-simplified term
zapSubstEnv env@(SimplEnv { seInlineDepth = n })
  = env { seTvSubst = emptyVarEnv, seCvSubst = emptyVarEnv, seIdSubst = emptyVarEnv
        , seInlineDepth = n+1 }

setSubstEnv :: SimplEnv -> TvSubstEnv -> CvSubstEnv -> SimplIdSubst -> SimplEnv
setSubstEnv env tvs cvs ids = env { seTvSubst = tvs, seCvSubst = cvs, seIdSubst = ids }

{-
************************************************************************
*                                                                      *
\subsection{LetFloats}
*                                                                      *
************************************************************************

Note [LetFloats]
~~~~~~~~~~~~~~~~
The `sfLetFloats` field of SimplFloats is a bunch of FloatBinds.  A FloatBind
can take one of three forms: FloatLet, FloatCase, and FloatTick; see
Note [Case-floats] and Note [Tick-floats].

In contrast, the type `LetFloats` (= OrdList OutBind) has let-bindings only.
It is what prepareBinding, prepareRhs and makeTrivial return: they never
produce case-floats or tick-floats.  See Note [prepareBinding] in
GHC.Core.Opt.Simplify.Iteration.

When prepareBinding decides whether to float some let-bindings out of a RHS,
doFloatFromRhs classifies them with a FloatFlag; see the data type declaration
of `FloatFlag`, and Note [FloatFlag for FloatLet].  Case-floats and tick-floats
have no FloatFlag, because prepareBinding never floats them out of a RHS.

Why do we have FloatCase at all, rather than just let-bindings?

(LF1) exprIsConApp_maybe looks through single-alternative cases (and lets
  and ticks) and returns what it looked through as a [FloatBind].  The
  case-of-known-constructor code wants to add these straight to the
  Simplifier's floats, so the floats must be able to hold a case.

(LF2) A single-alternative case is returned as a FloatCase by
  rebuildSingleAltCase rather than as an expression, so that the lets in
  its alternative are not trapped inside it.  Example, with case-of-case
  switched off (e.g. in the Gentle phase):
     case (case f x s of (# s1, a #) ->
           let b = g a in
           let c = h b in
           (# s1, c #))
     of (# s2, r #) -> body
  We simplify the inner case with an empty continuation (see
  splitContForNoCaseCase), and get back
     floats: [case f x s of (# s1, a #), b = g a, c = h b]
     result: (# s1, c #)
  Now rebuildWithFloats can do case-of-known-constructor on the result
  directly, giving
     case f x s of (# s1, a #) ->
     let b = g a in
     let c = h b in
     body[s1/s2, c/r]
  If instead the inner case came back as a single expression, then
  exprIsConApp_maybe would have to walk down through the case and all the
  lets to find the (# s1, c #).  With a long chain of such nested binds
  (e.g. state-passing code, as in T18223) the scrutinee at each level
  contains all the lets from the levels inside it, so the repeated walks
  are quadratic (#27737).
  See Note [Floating single-alternative cases] in GHC.Core.Opt.Simplify.Iteration.

Note [FloatFlag for FloatLet]  -- see `getFF_Let`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Examples

    NonRec x (y:ys)       FltLifted
    Rec [(x,rhs)]         FltLifted

    NonRec x* (p:q)       FltOKSpec   -- RHS is WHNF.  Question: why not FltLifted?
    NonRec x# (y +# 3)    FltOkSpec   -- Unboxed, but ok-for-spec'n

    NonRec x* (f y)       FltCareful  -- Strict binding; might fail or diverge
    NonRec x# (a /# b)    FltCareful  -- Might fail; does not satisfy let-can-float invariant
    NonRec x# (f y)       FltCareful  -- Might diverge; does not satisfy let-can-float invariant

    NonRec s "foo"#       FltLifted   -- Literal string can go to top level

* (FFL1) Note that a FloatLet can still be FltCareful: the first FltCareful example
  above, NonRec x* (f y), where x* is lifted but has a strict demand
  (isStrUsedDmd).  That is a perfectly valid (lazy) let-binding, but its demand
  info is valid only where it is.  If we floated it out of the RHS of a lazy
  binding
       let v = let x* = f y in x*+1 in body
   ==> let x* = f y; v = x*+1 in body
  then x's demand info would be a lie, and CorePrep, which case-binds a let
  whose binder has a strict demand, would evaluate (f y) eagerly.  Hence
  FltCareful.  prepareBinding never floats a FltCareful binding out of a RHS,
  even the RHS of a strict binding; see onlyLazyLetFloats.

* (FFL2) A literal-string binding is marked FltLifted (a bit of a mis-nomer
  because it's not a lifted type) because it can go to top level.
  See Note [Core top-level string literals] in GHC.Core.

* A binding that does not satisfy the let-can-float invariant (the last two
  examples above) is represented by a FloatCase, not a FloatLet:
    FloatCase (a /# b) x# DEFAULT []
  That is done once and for all by `addLetFloat`, so a FloatLet in sfLetFloats
  is always a valid let-binding, and `simplWrapFloats` does not need to check.
  Such bindings used to arise from `makeTrivial`, because an argument need not
  satisfy the let-can-float invariant; but now makeTrivial declines to bind such
  an argument (see Note [Cannot trivialise] in GHC.Core.Opt.Simplify.Iteration).
  They can also arise because
  exprOkForSpeculation is conservative: e.g. it says False for (# s, x #)
  where x is lifted, because it checks the arguments against the
  (representation-polymorphic) type of the unboxed-tuple worker.

Note [Case-floats]
~~~~~~~~~~~~~~~~~~
As well as let-bindings, sfLetFloats can contain single-alternative case
floats (FloatCase), for example
  case e of b { (# a, c #) -> <hole> }
A case-float never floats out of the RHS of a let-binding: prepareBinding
floats only let-bindings.  See Note [prepareBinding] in
GHC.Core.Opt.Simplify.Iteration, which also describes an opportunity that
we lose as a result.

(FFC1) Invariant: a DEFAULT FloatCase never has a scrutinee for which doCaseToLet
  holds.  Such a case-float
       FloatCase e b DEFAULT []      -- where doCaseToLet e b
  should instead be the let-float  (b = e), which satisfies the let-can-float
  invariant.  It would not actually be /wrong/ to use the FloatCase, just less
  good: a let-float can be inlined, and can float out of the RHS of a let-binding
  (see Note [prepareBinding]).

  doCaseToLet is the same test that rebuildCase uses to turn a case into a let:
  roughly, the scrutinee is ok-for-speculation (if unlifted) or a HNF (if lifted).
  But it says No if `b` is a coercion variable, because a coercion variable can
  be let-bound only to a Coercion (Note [Core type and coercion invariant] in
  GHC.Core); so  FloatCase (eq_sel @k @t1 @t2 d) co DEFAULT []  is fine.

  We maintain the invariant thus:
    * addLetFloat makes a FloatCase only for a binding that needsCaseBinding,
      i.e. an unlifted one whose RHS is not ok-for-speculation.
    * rebuildCase turns  case e of b { DEFAULT -> body }  into a let when
      doCaseToLet holds.
    * But rebuildCase looks at the alternatives before prepareAlts prunes away
      impossible ones; e.g.  case x# of b { 1# -> e1; DEFAULT -> e2 }  may be
      pruned to a single DEFAULT alternative.  So rebuildSingleAltCase makes a
      let-float, rather than a case-float, for a DEFAULT alternative when
      doCaseToLet holds.
    * exprIsConApp_maybe makes DEFAULT case-floats (in GHC.Core.SimpleOpt) only
      for an unlifted argument that needsCaseBinding, or for a strict field
      whose argument is not a HNF (mkFieldSeqFloats).
  We check the invariant, with `okCaseFloat`, in addCaseFloat and addFloatBindsF.

  This invariant is only about DEFAULT case-floats.  A case-float with pattern
  binders, such as  case addIntC# x y of (# r, c #) -> <hole>,  cannot be a
  let-float even when its scrutinee is ok-for-speculation.

Note [Tick-floats]
~~~~~~~~~~~~~~~~~~
sfLetFloats can contain a FloatTick for soft-scoped ticks (only -- see the
invariant on FloatTick in GHC.Core); see simplTick.

Like a case-float, a tick-float never floats out of the RHS of a let-binding,
because prepareBinding floats only let-bindings.  For a /counting/ tick (e.g.
an HPC tick) that is essential: floating it out of a lazy RHS would change when
(and how often) it is counted, namely when the let is executed rather than when
the binder is evaluated.  For a /non-counting/ tick (e.g. a source note) it
would be fine, but we don't bother.
-}

type LetFloats = OrdList OutBind
  -- Let-bindings only: no case-floats or tick-floats
  -- Invariant: every binding satisfies the let-can-float invariant
  -- See Note [LetFloats]

type JoinFloat  = OutBind
type JoinFloats = OrdList JoinFloat

data FloatFlag
  = FltLifted   -- All bindings are lifted and lazy *or*
                --     consist of a single primitive string literal
                -- Hence ok to float to top level, or recursive
                -- NB: consequence: all bindings satisfy let-can-float invariant

  | FltOkSpec   -- All bindings are FltLifted *or*
                --      strict (perhaps because unlifted,
                --      perhaps because of a strict binder),
                --        *and* ok-for-speculation
                -- Hence ok to float out of the RHS
                -- of a lazy non-recursive let binding
                -- (but /not/ to top level, or into a rec group)
                -- NB: consequence: all bindings satisfy let-can-float invariant

  | FltCareful  -- At least one binding is strict (or unlifted)
                --      and not guaranteed cheap
                -- Never float these bindings out of a RHS
                -- NB: some bindings may not satisfy let-can-float

instance Outputable FloatFlag where
  ppr FltLifted  = text "FltLifted"
  ppr FltOkSpec  = text "FltOkSpec"
  ppr FltCareful = text "FltCareful"

andFF :: FloatFlag -> FloatFlag -> FloatFlag
andFF FltCareful _          = FltCareful
andFF FltOkSpec  FltCareful = FltCareful
andFF FltOkSpec  _          = FltOkSpec
andFF FltLifted  flt        = flt


doFloatFromRhs :: FloatEnable -> TopLevelFlag -> RecFlag
               -> LetFloats -> FloatFlag   -- The let-bindings and their FloatFlag,
                                           --   from onlyLazyLetFloats
               -> OutExpr -> Bool
-- If you change this function look also at FloatIn.noFloatIntoRhs
-- See Note [Floating a prefix of the floats] for a possible improvement
doFloatFromRhs fe lvl rec fs ff rhs
  = floatEnabled lvl fe
      && not (isNilOL fs)
      && want_to_float
      && can_float
  where
     want_to_float = isTopLevel lvl || exprIsExpandable rhs
                     -- See Note [Float when expandable]

     can_float = case ff of
                   FltLifted  -> True
                   FltOkSpec  -> isNotTopLevel lvl && isNonRec rec
                   FltCareful -> False   -- onlyLazyLetFloats never returns FltCareful

     -- Whether any floating is allowed by flags.
     floatEnabled :: TopLevelFlag -> FloatEnable -> Bool
     floatEnabled _ FloatDisabled = False
     floatEnabled lvl FloatNestedOnly = not (isTopLevel lvl)
     floatEnabled _ FloatEnabled = True

{- Note [Floating a prefix of the floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
prepareBinding floats all of the floats from a RHS, or none of them.  It floats
none if any of them is a case-float or tick-float (see Note [prepareBinding] in
GHC.Core.Opt.Simplify.Iteration), or if doFloatFromRhs says no; and doFloatFromRhs
gives a single FloatFlag to the whole group.  Consider (T5321FD)
     x = let v1 = \ds -> ... in
         ...
         let v200 = \ds -> ... in
         case sampleData of (a :* b) -> ...
where the single-alternative case has become a FloatCase; see
Note [Floating single-alternative cases] in GHC.Core.Opt.Simplify.Iteration.
Because of the FloatCase, none of these floats go to top level, even though the
lets in front of the case are all FltLifted.

Opportunity: when the group cannot float as a whole, float the longest prefix
of let-bindings that can, and wrap the rest back around the RHS.  Taking a
prefix is always fine, because later floats may mention the binders of earlier
ones, but not vice versa.  To keep the common path cheap, do the linear
traversal only when the group cannot float as a whole.

We tried this (#27737).  It did restore the top-level floating in T5321FD, but
made little difference to compile time, so we did not keep it.  If you try it
again, beware: the sfInScope of the floats describes the scope /inside/ all of
them, which is wrong for the prefix alone.  In T5321FD, inside the trapped
     case sampleData of (x :* xs) -> ...
binder-swap gives sampleData the unfolding (x :* xs); keeping that sfInScope
let the refined sampleData escape to top level, taking x and xs with it.  The
fix is to rebuild sfInScope from the in-scope set outside the RHS, extended
with the binders of the prefix.
-}

{- Note [Float when expandable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We float when that would leave us with a RHS that is /expandable/ (#26854).
The whole purpose of the local-let-floating is to end up with
   let x = rhs
where rhs has an expandable unfolding, so that `exprIsConApp_maybe` will look
inside `x`.

Historical aside:

    In the long-distant (2011) past, we made `want_to_float` true if
      EITHER exprIsExpandable OR exprIsCheap
    But that seems wrong: there is no point in floating for expressions that are
    cheap but not expandable:
      * It costs more to test
      * It gives no benefit
      * Very few expressions are cheap but not expandable (e.g. error calls)
    We justified this OR by saying that there may be:
      - cheap things that are not expandable (eg \x. expensive)
      - expandable things that are not cheap (eg (f b) where b is CONLIKE)
    But that's not true: (\x.expensive) is certainly expandable.

End of historial aside
-}

emptyLetFloats :: LetFloats
emptyLetFloats = nilOL

emptyJoinFloats :: JoinFloats
emptyJoinFloats = nilOL

isEmptyJoinFloats :: JoinFloats -> Bool
isEmptyJoinFloats = isNilOL

getFF_Let :: CoreBind -> FloatFlag
-- See Note [FloatFlag for FloatLet]
getFF_Let (Rec {})        = FltLifted
getFF_Let (NonRec bndr rhs)
  | not (isStrictId bndr)    = FltLifted   -- Lazy lifted let
  | exprIsTickedString rhs   = FltLifted   -- See (FFL2)
  | exprOkForSpeculation rhs = FltOkSpec   -- Unlifted, or strict lifted, and ok-for-spec (e.g. HNF)
  | otherwise                = FltCareful  -- See (FFL1) Strict, e.g. let x* = factorial y

addLetFloat :: SimplFloats -> OutBind -> SimplFloats
-- Add a non-join binding to the let-floats, and extend the in-scope set.
-- If the binding does not satisfy the let-can-float invariant, it
-- makes a FloatCase. See Note [LetFloats]
addLetFloat floats bind
  | NonRec bndr rhs <- bind
  , needsCaseBinding (idType bndr) rhs
  = -- This should never happen, but currently does because of #27910
    add_float floats (FloatCase rhs bndr DEFAULT [])

  | otherwise
  = assert (all (not . isJoinId) (bindersOf bind)) $
    add_float floats (FloatLet bind)

addCaseFloat :: SimplFloats -> OutExpr -> OutId -> AltCon -> [OutVar] -> SimplFloats
-- Add a single-alternative case float
--   case scrut of bndr { con bndrs -> <hole> }
-- and extend the in-scope set.  See Note [LetFloats]
addCaseFloat floats scrut bndr con bndrs
  = assertPpr (okCaseFloat flt) (ppr flt) $
    add_float floats flt
  where
    flt = FloatCase scrut bndr con bndrs

add_float :: SimplFloats -> FloatBind -> SimplFloats
-- Add the float /inside/ the existing let-floats
add_float floats@(SimplFloats { sfLetFloats = fs, sfInScope = in_scope }) flt
  = floats { sfLetFloats = fs `snocOL` flt
           , sfInScope   = in_scope `extendInScopeFromFB` unitOL flt }

okLetFloat :: OutBind -> Bool
-- Checks the let-can-float invariant; used only in assertions
okLetFloat (NonRec b r)
  | isId b, not (isCoVar b) = not (needsCaseBinding (idType b) r)
okLetFloat _                = True

okCaseFloat :: FloatBind -> Bool
-- Checks invariant (FFC1) in Note [Case-floats]
-- Used only in assertions
okCaseFloat (FloatCase scrut b DEFAULT _) = not (doCaseToLet scrut b)
okCaseFloat _                             = True

doCaseToLet :: OutExpr          -- Scrutinee
            -> Id               -- Case binder; can be InId or OutId
            -> Bool
-- The situation is         case scrut of b { DEFAULT -> body }
-- Can we transform thus?   let { b = scrut } in body
-- See also (FFC1) in Note [Case-floats]
doCaseToLet scrut case_bndr
  | isTyCoVar case_bndr    -- Respect GHC.Core
  = isTyCoArg scrut        -- Note [Core type and coercion invariant]

  | isUnliftedType (exprType scrut)
    -- We can call isUnliftedType here: scrutinees always have a fixed RuntimeRep (see FRRCase).
    -- Note however that we must check 'scrut' (which is an 'OutExpr') and not 'case_bndr'
    -- (which may be an 'InId'): see Note [Dark corner with representation polymorphism]
    -- in GHC.Core.Opt.Simplify.Iteration.
    -- Using `exprType` is typically cheap because `scrut` is typically a variable.
    -- We could instead use mightBeUnliftedType (idType case_bndr), but that hurts
    -- the brain more.  Consider that if this test ever turns out to be a perf
    -- problem (which seems unlikely).
  = exprOkForSpeculation scrut

  | otherwise  -- Scrut has a lifted type
  = exprIsHNF scrut
       --    || isStrUsedDmd (idDemandInfo case_bndr)
       -- We no longer look at the demand on the case binder
       -- See Note [Case-to-let for strictly-used binders] in GHC.Core.Opt.Simplify.Iteration

unitJoinFloat :: OutBind -> JoinFloats
unitJoinFloat bind = assert (all isJoinId (bindersOf bind)) $
                     unitOL bind

mkFloatBind :: SimplEnv -> OutBind -> (SimplFloats, SimplEnv)
-- Make a singleton SimplFloats, and
-- extend the incoming SimplEnv's in-scope set with its binders
-- These binders may already be in the in-scope set,
-- but may have by now been augmented with more IdInfo
mkFloatBind env bind
  = (floats, env { seInScope = sfInScope floats })
  where
    -- See Note [Bangs in the Simplifier]
    !floats = emptyFloats env `extendFloats` bind

extendFloats :: SimplFloats -> OutBind -> SimplFloats
-- Add this binding (join point or not) to the floats,
-- and extend the in-scope env too
extendFloats floats@(SimplFloats { sfJoinFloats = jfloats, sfInScope = in_scope }) bind
  | isJoinBind bind
  = floats { sfJoinFloats = jfloats `addJoinFlts` unitJoinFloat bind
           , sfInScope    = in_scope `extendInScopeSetBind` bind }
  | otherwise
  = floats `addLetFloat` bind

mkLetSimplFloats :: InScopeSet -> LetFloats -> SimplFloats
-- Make SimplFloats from some let-bindings.
-- Precondition: the InScopeSet already includes the binders of the let-bindings
mkLetSimplFloats in_scope binds
  = SimplFloats { sfLetFloats  = fmap FloatLet binds
                , sfJoinFloats = emptyJoinFloats
                , sfInScope    = in_scope }

addLetFloats :: SimplFloats -> LetFloats -> SimplFloats
-- Add the let-bindings /inside/ the existing floats,
-- and extend the in-scope set
-- No need for addLetFloat's needsCaseBinding test: LetFloats satisfy the
-- let-can-float invariant (see the declaration of LetFloats)
addLetFloats floats binds = foldlOL add floats binds
  where
    add fs bind = assertPpr (okLetFloat bind) (ppr bind) $
                  add_float fs (FloatLet bind)

addFloatBindsF :: FloatBinds -> SimplFloats -> SimplFloats
-- (addFloatBindsF new_fbs floats) adds `new_fbs` /outside/ `floats`;
-- i.e. `new_fbs` scope over `floats`
-- Precondition: every FloatLet in `new_fbs` satisfies the let-can-float
-- invariant (see the FloatBind declaration in GHC.Core), so we can add
-- them directly; see Note [LetFloats]
addFloatBindsF new_fbs floats@(SimplFloats { sfLetFloats = lf_fbs, sfInScope = in_scope })
  | isNilOL new_fbs
  = floats
  | otherwise
  = assertPpr (all ok_float new_fbs) (ppr new_fbs) $
    floats { sfLetFloats = new_fbs `appOL` lf_fbs
           , sfInScope   = in_scope `extendInScopeFromFB` new_fbs }
  where
    -- Check the invariants on FloatBind; see its declaration in GHC.Core
    ok_float (FloatLet bind)    = okLetFloat bind
    ok_float (FloatTick t)      = tickishHasSoftScope t
    ok_float flt@(FloatCase {}) = okCaseFloat flt

extendInScopeFromFB :: InScopeSet -> FloatBinds -> InScopeSet
extendInScopeFromFB in_scope binds
  = foldlOL extend in_scope binds
  where
    extend in_scope (FloatLet bind)        = in_scope `extendInScopeSetBind` bind
    extend in_scope (FloatCase _ b _ bs)   = in_scope `extendInScopeSetList` (b:bs)
    extend in_scope (FloatTick {})         = in_scope

addJoinFloats :: SimplFloats -> JoinFloats -> SimplFloats
addJoinFloats floats join_floats
  = floats { sfJoinFloats = sfJoinFloats floats `addJoinFlts` join_floats
           , sfInScope    = foldlOL extendInScopeSetBind
                                    (sfInScope floats) join_floats }

addFloats :: SimplFloats -> SimplFloats -> SimplFloats
-- Add both let-floats and join-floats for env2 to env1;
-- *plus* the in-scope set for env2, which is bigger
-- than that for env1
addFloats (SimplFloats { sfLetFloats = lf1, sfJoinFloats = jf1 })
          (SimplFloats { sfLetFloats = lf2, sfJoinFloats = jf2, sfInScope = in_scope })
  = SimplFloats { sfLetFloats  = lf1 `appOL` lf2
                , sfJoinFloats = jf1 `addJoinFlts` jf2
                , sfInScope    = in_scope }

onlyLazyLetFloats :: SimplFloats -> LetFloats -> Maybe (LetFloats, FloatFlag)
-- (onlyLazyLetFloats floats extra_binds) returns (Just (binds, ff)) if all the
-- let-floats in `floats`, followed by `extra_binds`, are let-bindings that we are
-- prepared to float out of a RHS: no case-floats or tick-floats, and no
-- FltCareful bindings.  The FloatFlag `ff` combines the flags of the `binds`,
-- so it is FltLifted or FltOkSpec.
-- Precondition: there are no join floats
-- See Note [prepareBinding] in GHC.Core.Opt.Simplify.Iteration
onlyLazyLetFloats (SimplFloats { sfLetFloats = fs, sfJoinFloats = jfs }) extra_binds
  = assertPpr (isNilOL jfs) (ppr jfs) $
    go FltLifted nilOL (fromOL fs)
  where
    go ff binds []                    = foldlOL add_extra (Just (binds, ff)) extra_binds
    go ff binds (FloatLet bind : rest)
      | FltCareful <- bind_ff         = Nothing
      | otherwise                     = go (ff `andFF` bind_ff) (binds `snocOL` bind) rest
      where
        bind_ff = getFF_Let bind     -- See Note [FloatFlag for FloatLet]
    go _ _ _                          = Nothing    -- Case-float or tick-float

    add_extra Nothing            _    = Nothing
    add_extra (Just (binds, ff)) bind
      | FltCareful <- bind_ff         = Nothing
      | otherwise                     = Just (binds `snocOL` bind, ff `andFF` bind_ff)
      where
        bind_ff = getFF_Let bind

letFloatBinds :: SimplFloats -> [OutBind]
-- The let-floats, as bindings
-- Precondition: there are no case-floats or tick-floats; true, for example,
--               for floats that have come out of a recursive or top-level RHS
--               (see Note [prepareBinding] in GHC.Core.Opt.Simplify.Iteration)
letFloatBinds (SimplFloats { sfLetFloats = fs }) = map get_bind (fromOL fs)
  where
    get_bind (FloatLet bind) = bind
    get_bind flt             = pprPanic "letFloatBinds" (ppr flt)

addJoinFlts :: JoinFloats -> JoinFloats -> JoinFloats
addJoinFlts = appOL

mkRecFloats :: SimplFloats -> SimplFloats
-- Flattens the floats into a single Rec group,
-- They must either all be lifted LetFloats or all JoinFloats
mkRecFloats floats@(SimplFloats { sfLetFloats  = bs
                                , sfJoinFloats = jbs
                                , sfInScope    = in_scope })
  = assertPpr (isNilOL bs || isNilOL jbs) (ppr floats) $
    SimplFloats { sfLetFloats  = floats'
                , sfJoinFloats = jfloats'
                , sfInScope    = in_scope }
  where
    -- See Note [Bangs in the Simplifier]
    !floats'  | isNilOL bs  = nilOL
              | otherwise   = unitOL (FloatLet (Rec (flattenBinds (letFloatBinds floats))))
    !jfloats' | isNilOL jbs = emptyJoinFloats
              | otherwise   = unitJoinFloat (Rec (flattenBinds (fromOL jbs)))

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
getTopFloatBinds floats@(SimplFloats { sfJoinFloats = jbs })
  = assert (isNilOL jbs) $  -- Can't be any top-level join bindings
    letFloatBinds floats

{-# INLINE mapLetFloats #-}
mapLetFloats :: LetFloats -> ((Id,CoreExpr) -> (Id,CoreExpr)) -> LetFloats
mapLetFloats binds fun
   = binds1
   where
    app (NonRec b e) = case fun (b,e) of (b',e') -> NonRec b' e'
    app (Rec bs)     = Rec (strictMap fun bs)
    !binds1 = mapOL' app binds -- See Note [Bangs in the Simplifier]

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
    !id3  = zapFragileIdInfo id2      -- Zaps rules, worker-info, unfolding
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
out_id so that its arity is visible in its RHS. Examples:

  * f = \x y. let g = \p q. f (p+q) in Just (...g..g...)
    Here we want to give `g` arity 3 and eta-expand. `findRhsArity` will have a
    hard time figuring that out when `f` only has arity 0 in its own RHS.
  * f = \x y. ....(f `seq` blah)....
    We want to drop the seq.
  * f = \x. g (\y. f y)
    You'd think we could eta-reduce `\y. f y` to `f` here. And indeed, that is true.
    Unfortunately, it is not sound in general to eta-reduce in f's RHS.
    Example: `f = \x. f x`. See Note [Eta reduction in recursive RHSs] for how
    we prevent that.

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
                    -> Id       -- Adjusted join-point Id
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
    join_arity = idJoinArity join_id
    orig_ty    = idType join_id
    res_torc   = typeTypeOrConstraint new_res_ty :: TypeOrConstraint

    new_join_ty = go join_arity orig_ty :: Type

    go :: JoinArity -> Type -> Type
    go n ty
      | n == 0
      = new_res_ty

      | Just (arg_bndr, body_ty) <- splitPiTy_maybe ty
      , let body_ty' = go (n-1) body_ty
      = case arg_bndr of
          Named b                          -> mkForAllTy b body_ty'
          Anon (Scaled arg_mult arg_ty) af -> mkFunTy af' arg_mult' arg_ty body_ty'
              where
                -- Using "!": See Note [Bangs in the Simplifier]
                -- mkMultMul: see Note [Scaling join point arguments]
                !arg_mult' = arg_mult `mkMultMul` mult

                -- the new_res_ty might be ConstraintLike while the original
                -- one was TypeLike.  So we may need to adjust the FunTyFlag.
                -- (see #23952)
                !af' = mkFunTyFlag (funTyFlagArgTypeOrConstraint af) res_torc

      | otherwise
      = pprPanic "adjustJoinPointType" (ppr join_arity <+> ppr orig_ty)

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

getTCvSubst :: SimplEnv -> Subst
getTCvSubst (SimplEnv { seInScope = in_scope, seTvSubst = tv_env, seCvSubst = cv_env })
  = mkSubst in_scope emptyVarEnv tv_env cv_env

getFullSubst :: InScopeSet -> SimplEnv -> Subst
getFullSubst in_scope (SimplEnv { seIdSubst = id_env, seTvSubst = tv_env, seCvSubst = cv_env })
  = mkSubst in_scope (mapVarEnv to_expr id_env) tv_env cv_env
  where
    to_expr :: SimplSR -> CoreExpr
    -- A tiresome impedence-matcher
    to_expr (DoneEx e _)       = e
    to_expr (DoneId v)         = Var v
    to_expr (ContEx env e mco) = mkCastMCo e' mco
      where
        e' = GHC.Core.Subst.substExprSC (getFullSubst in_scope env) e

substTy :: HasDebugCallStack => SimplEnv -> Type -> Type
substTy env ty = Type.substTy (getTCvSubst env) ty

substTyVar :: SimplEnv -> TyVar -> Type
substTyVar env tv = Type.substTyVar (getTCvSubst env) tv

substTyVarBndr :: SimplEnv -> TyVar -> (SimplEnv, TyVar)
substTyVarBndr env tv
  = case Type.substTyVarBndr (getTCvSubst env) tv of
        (Subst in_scope' _ tv_env' cv_env', tv')
           -> (env { seInScope = in_scope', seTvSubst = tv_env', seCvSubst = cv_env' }, tv')

substCoVar :: SimplEnv -> CoVar -> Coercion
substCoVar env tv = Coercion.substCoVar (getTCvSubst env) tv

substCoVarBndr :: SimplEnv -> CoVar -> (SimplEnv, CoVar)
substCoVarBndr env cv
  = case Coercion.substCoVarBndr (getTCvSubst env) cv of
        (Subst in_scope' _ tv_env' cv_env', cv')
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
    subst = Subst in_scope emptyIdSubstEnv tv_env cv_env
    old_ty = idType id
    old_w  = idMult id
