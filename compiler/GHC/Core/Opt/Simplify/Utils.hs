{-
(c) The AQUA Project, Glasgow University, 1993-1998

The simplifier utilities
-}



module GHC.Core.Opt.Simplify.Utils (
        -- Rebuilding
        rebuildLam, mkCase, prepareAlts,
        tryEtaExpandRhs, wantEtaExpansion,

        -- Inlining,
        preInlineUnconditionally, postInlineUnconditionally,
        activeRule,
        getUnfoldingInRuleMatch,
        updModeForStableUnfoldings, updModeForRules,

        -- The BindContext type
        BindContext(..), bindContextLevel,

        -- The continuation type
        SimplCont(..), DupFlag(..), FromWhat(..), StaticEnv,
        isSimplified, contIsStop,
        contIsDupable, contResultType, contHoleType, contHoleScaling,
        contIsTrivial, contArgs, contIsRhs,
        countArgs,
        mkBoringStop, mkRhsStop, mkLazyArgStop,
        interestingCallContext,

        -- ArgInfo
        ArgInfo(..), ArgSpec(..), RewriteCall(..), mkArgInfo,
        addValArgTo, addCastTo, addTyArgTo,
        argInfoExpr, argInfoAppArgs,
        pushSimplifiedArgs, pushSimplifiedRevArgs,
        isStrictArgInfo, lazyArgContext,

        abstractFloats,

        -- Utilities
        isExitJoinId
    ) where

import GHC.Prelude hiding (head, init, last, tail)
import qualified GHC.Prelude as Partial (head)

import GHC.Core
import GHC.Types.Literal ( isLitRubbish )
import GHC.Core.Opt.Simplify.Env
import GHC.Core.Opt.Simplify.Inline( smallEnoughToInline )
import GHC.Core.Opt.Stats ( Tick(..) )
import qualified GHC.Core.Subst
import GHC.Core.Ppr
import GHC.Core.TyCo.Ppr ( pprParendType )
import GHC.Core.FVs
import GHC.Core.Utils
import GHC.Core.Rules( RuleEnv, getRules )
import GHC.Core.Opt.Arity
import GHC.Core.Unfold
import GHC.Core.Unfold.Make
import GHC.Core.Opt.Simplify.Monad
import GHC.Core.Type     hiding( substTy )
import GHC.Core.Coercion hiding( substCo )
import GHC.Core.DataCon ( dataConWorkId, isNullaryRepDataCon )
import GHC.Core.Multiplicity
import GHC.Core.Opt.ConstantFold

import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Tickish
import GHC.Types.Demand
import GHC.Types.Var.Set
import GHC.Types.Basic

import GHC.Hs.InlinePragma
import GHC.Hs.Extension(GhcPass)

import GHC.Data.OrdList ( isNilOL )
import GHC.Data.FastString ( fsLit )

import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Monad    ( when )
import Data.List        ( sortBy )
import GHC.Types.Name.Env
import Data.Graph

{- *********************************************************************
*                                                                      *
                The BindContext type
*                                                                      *
********************************************************************* -}

-- What sort of binding is this? A let-binding or a join-binding?
data BindContext
  = BC_Let                 -- A regular let-binding
      TopLevelFlag RecFlag

  | BC_Join                -- A join point with continuation k
      RecFlag              -- See Note [Rules and unfolding for join points]
      SimplCont            -- in GHC.Core.Opt.Simplify

bindContextLevel :: BindContext -> TopLevelFlag
bindContextLevel (BC_Let top_lvl _) = top_lvl
bindContextLevel (BC_Join {})       = NotTopLevel

bindContextRec :: BindContext -> RecFlag
bindContextRec (BC_Let _ rec_flag)  = rec_flag
bindContextRec (BC_Join rec_flag _) = rec_flag

isJoinBC :: BindContext -> Bool
isJoinBC (BC_Let {})  = False
isJoinBC (BC_Join {}) = True


{- *********************************************************************
*                                                                      *
                The SimplCont and DupFlag types
*                                                                      *
************************************************************************

A SimplCont allows the simplifier to traverse the expression in a
zipper-like fashion.  The SimplCont represents the rest of the expression,
"above" the point of interest.

You can also think of a SimplCont as an "evaluation context", using
that term in the way it is used for operational semantics. This is the
way I usually think of it, For example you'll often see a syntax for
evaluation context looking like
        C ::= []  |  C e   |  case C of alts  |  C `cast` co
That's the kind of thing we are doing here, and I use that syntax in
the comments.


Key points:
  * A SimplCont describes a *strict* context (just like
    evaluation contexts do).  E.g. Just [] is not a SimplCont

  * A SimplCont describes a context that *does not* bind
    any variables.  E.g. \x. [] is not a SimplCont
-}

data SimplCont
  = Stop                -- ^ Stop[e] = e
        OutType         -- ^ Type of the <hole>
        CallCtxt        -- ^ Tells if there is something interesting about
                        --          the syntactic context, and hence the inliner
                        --          should be a bit keener (see interestingCallContext)
                        -- Specifically:
                        --     This is an argument of a function that has RULES
                        --     Inlining the call might allow the rule to fire
                        -- Never ValAppCxt (use ApplyToVal instead)
                        -- or CaseCtxt (use Select instead)
        SubDemand       -- ^ The evaluation context of e. Tells how e is evaluated.
                        -- This fuels eta-expansion or eta-reduction without looking
                        -- at lambda bodies, for example.
                        --
                        -- See Note [Eta reduction based on evaluation context]
                        -- The evaluation context for other SimplConts can be
                        -- reconstructed with 'contEvalContext'


  | CastIt              -- (CastIt co K)[e] = K[ e `cast` co ]
      { sc_co   :: OutCoercion  -- The coercion simplified
                                -- Invariant: never an identity coercion
      , sc_opt  :: Bool         -- True <=> sc_co has had optCoercion applied to it
                                --      See Note [Avoid re-simplifying coercions]
                                --      in GHC.Core.Opt.Simplify.Iteration
      , sc_cont :: SimplCont }

  | ApplyToVal         -- (ApplyToVal arg K)[e] = K[ e arg ]
      { sc_dup     :: DupFlag   -- See Note [DupFlag invariants]
      , sc_hole_ty :: OutType   -- Type of the function, presumably (forall a. blah)
                                -- See Note [The hole type in ApplyToTy]
      , sc_arg  :: InExpr       -- The argument,
      , sc_env  :: StaticEnv    -- see Note [StaticEnv invariant]
      , sc_cont :: SimplCont }

  | ApplyToTy          -- (ApplyToTy ty K)[e] = K[ e ty ]
      { sc_arg_ty  :: OutType     -- Argument type
      , sc_hole_ty :: OutType     -- Type of the function, presumably (forall a. blah)
                                  -- See Note [The hole type in ApplyToTy]
      , sc_cont    :: SimplCont }

  | Select             -- (Select alts K)[e] = K[ case e of alts ]
      { sc_dup  :: DupFlag        -- See Note [DupFlag invariants]
      , sc_bndr :: InId           -- case binder
      , sc_alts :: [InAlt]        -- Alternatives
      , sc_env  :: StaticEnv      -- See Note [StaticEnv invariant]
      , sc_cont :: SimplCont }

  -- The two strict forms have no DupFlag, because we never duplicate them
  | StrictBind          -- (StrictBind x b K)[e] = let x = e in K[b]
                        --       or, equivalently,  = K[ (\x.b) e ]
      { sc_dup   :: DupFlag        -- See Note [DupFlag invariants]
      , sc_from  :: FromWhat
      , sc_bndr  :: InId
      , sc_body  :: InExpr
      , sc_env   :: StaticEnv      -- Static env for both sc_bndr (stable unfolding thereof)
                                   -- and sc_body.  Also see Note [StaticEnv invariant]
      , sc_cont  :: SimplCont }

  | StrictArg           -- (StrictArg (f e1 ..en) K)[e] = K[ f e1 .. en e ]
      { sc_dup  :: DupFlag     -- Always Simplified or OkToDup
      , sc_fun  :: ArgInfo     -- Specifies f, e1..en, Whether f has rules, etc
                               --     plus demands and discount flags for *this* arg
                               --          and further args
                               --     So ai_dmds and ai_discs are never empty
      , sc_fun_ty :: OutType   -- Type of the function (f e1 .. en),
                               -- presumably (arg_ty -> res_ty)
                               -- where res_ty is expected by sc_cont
      , sc_cont :: SimplCont }

  | TickIt              -- (TickIt t K)[e] = K[ tick t e ]
        CoreTickish     -- Tick tickish <hole>
        SimplCont

type StaticEnv = SimplEnv       -- Just the static part is relevant

data FromWhat = FromLet | FromBeta Levity

-- See Note [DupFlag invariants]
data DupFlag = NoDup       -- Unsimplified, might be big
             | Simplified  -- Simplified
             | OkToDup     -- Simplified and small

isSimplified :: DupFlag -> Bool
isSimplified NoDup = False
isSimplified _     = True       -- Invariant: the subst-env is empty

perhapsSubstTy :: DupFlag -> StaticEnv -> Type -> Type
perhapsSubstTy dup env ty
  | isSimplified dup = ty
  | otherwise        = substTy env ty

{- Note [StaticEnv invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pair up an InExpr or InAlts with a StaticEnv, which establishes the
lexical scope for that InExpr.

When we simplify that InExpr/InAlts, we use
  - Its captured StaticEnv
  - Overriding its InScopeSet with the larger one at the
    simplification point.

Why override the InScopeSet?  Example:
      (let y = ey in f) ex
By the time we simplify ex, 'y' will be in scope.

However the InScopeSet in the StaticEnv is not irrelevant: it should
include all the free vars of applying the substitution to the InExpr.
Reason: contHoleType uses perhapsSubstTy to apply the substitution to
the expression, and that (rightly) gives ASSERT failures if the InScopeSet
isn't big enough.

Note [DupFlag invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~
In both ApplyToVal { se_dup = dup, se_env = env, se_cont = k}
   and  Select { se_dup = dup, se_env = env, se_cont = k}
the following invariants hold

  (a) if dup = OkToDup, then continuation k is also ok-to-dup
  (b) if dup = OkToDup or Simplified, the subst-env is empty,
               or at least is always ignored; the payload is
               already an OutThing
-}

instance Outputable DupFlag where
  ppr OkToDup    = text "ok"
  ppr NoDup      = text "nodup"
  ppr Simplified = text "simpl"

instance Outputable SimplCont where
  ppr (Stop ty interesting eval_sd)
    = text "Stop" <> brackets (sep $ punctuate comma pps) <+> ppr ty
    where
      pps = [ppr interesting] ++ [ppr eval_sd | eval_sd /= topSubDmd]
  ppr (CastIt { sc_co = co, sc_cont = cont })
    = (text "CastIt" <+> pprOptCo co) $$ ppr cont
  ppr (TickIt t cont)
    = (text "TickIt" <+> ppr t) $$ ppr cont
  ppr (ApplyToTy  { sc_arg_ty = ty, sc_cont = cont })
    = (text "ApplyToTy" <+> pprParendType ty) $$ ppr cont
  ppr (ApplyToVal { sc_arg = arg, sc_dup = dup, sc_cont = cont, sc_hole_ty = hole_ty })
    = (hang (text "ApplyToVal" <+> ppr dup <+> text "hole" <+> ppr hole_ty)
          2 (pprParendExpr arg))
      $$ ppr cont
  ppr (StrictBind { sc_bndr = b, sc_cont = cont })
    = (text "StrictBind" <+> ppr b) $$ ppr cont
  ppr (StrictArg { sc_fun = ai, sc_cont = cont })
    = (text "StrictArg" <+> ppr (ai_fun ai)) $$ ppr cont
  ppr (Select { sc_dup = dup, sc_bndr = bndr, sc_alts = alts, sc_cont = cont })
    = (text "Select" <+> ppr dup <+> ppr bndr) $$
      whenPprDebug (nest 2 $ ppr alts) $$ ppr cont


{- Note [The hole type in ApplyToTy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The sc_hole_ty field of ApplyToTy records the type of the "hole" in the
continuation.  It is absolutely necessary to compute contHoleType, but it is
not used for anything else (and hence may not be evaluated).

Why is it necessary for contHoleType?  Consider the continuation
     ApplyToType Int (Stop Int)
corresponding to
     (<hole> @Int) :: Int
What is the type of <hole>?  It could be (forall a. Int) or (forall a. a),
and there is no way to know which, so we must record it.

In a chain of applications  (f @t1 @t2 @t3) we'll lazily compute exprType
for (f @t1) and (f @t1 @t2), which is potentially non-linear; but it probably
doesn't matter because we'll never compute them all.

************************************************************************
*                                                                      *
                ArgInfo and ArgSpec
*                                                                      *
************************************************************************
-}

data ArgInfo
  = ArgInfo {
        ai_fun   :: OutId,      -- The function
        ai_args  :: [ArgSpec],  -- ...applied to these args (which are in *reverse* order)

        ai_rewrite :: RewriteCall,  -- What transformation to try next for this call
             -- See Note [Rewrite rules and inlining] in GHC.Core.Opt.Simplify.Iteration

        ai_encl :: Bool,        -- Flag saying whether this function
                                -- or an enclosing one has rules (recursively)
                                --      True => be keener to inline in all args

        ai_dmds :: [Demand],    -- Demands on remaining value arguments (beyond ai_args)
                                --   Usually infinite, but if it is finite it guarantees
                                --   that the function diverges after being given
                                --   that number of args

        ai_discs :: [Int]       -- Discounts for remaining value arguments (beyond ai_args)
                                --   non-zero => be keener to inline
                                --   Always infinite
    }

data RewriteCall  -- What rewriting to try next for this call
                  -- See Note [Rewrite rules and inlining] in GHC.Core.Opt.Simplify.Iteration
  = TryRules FullArgCount [CoreRule]
  | TryInlining
  | TryNothing

data ArgSpec
  = ValArg { as_dmd  :: Demand        -- Demand placed on this argument
           , as_arg  :: OutExpr       -- Apply to this (coercion or value); c.f. ApplyToVal
           , as_hole_ty :: OutType }  -- Type of the function (presumably t1 -> t2)

  | TyArg { as_arg_ty  :: OutType     -- Apply to this type; c.f. ApplyToTy
          , as_hole_ty :: OutType }   -- Type of the function (presumably forall a. blah)

  | CastBy OutCoercion                -- Cast by this; c.f. CastIt
                                      -- Coercion is optimised

instance Outputable ArgInfo where
  ppr (ArgInfo { ai_fun = fun, ai_args = args, ai_dmds = dmds })
    = text "ArgInfo" <+> braces
         (sep [ text "fun =" <+> ppr fun
              , text "dmds(first 10) =" <+> ppr (take 10 dmds)
              , text "args =" <+> ppr args ])

instance Outputable ArgSpec where
  ppr (ValArg { as_arg = arg })  = text "ValArg" <+> ppr arg
  ppr (TyArg { as_arg_ty = ty }) = text "TyArg" <+> ppr ty
  ppr (CastBy c)                 = text "CastBy" <+> ppr c

addValArgTo :: ArgInfo ->  OutExpr -> OutType -> ArgInfo
addValArgTo ai arg hole_ty
  | ArgInfo { ai_dmds = dmd:dmds, ai_discs = _:discs, ai_rewrite = rew } <- ai
      -- Pop the top demand and and discounts off
  , let arg_spec = ValArg { as_arg = arg, as_hole_ty = hole_ty, as_dmd = dmd }
  = ai { ai_args    = arg_spec : ai_args ai
       , ai_dmds    = dmds
       , ai_discs   = discs
       , ai_rewrite = decArgCount rew }
  | otherwise
  = pprPanic "addValArgTo" (ppr ai $$ ppr arg)
    -- There should always be enough demands and discounts

addTyArgTo :: ArgInfo -> OutType -> OutType -> ArgInfo
addTyArgTo ai arg_ty hole_ty = ai { ai_args    = arg_spec : ai_args ai
                                  , ai_rewrite = decArgCount (ai_rewrite ai) }
  where
    arg_spec = TyArg { as_arg_ty = arg_ty, as_hole_ty = hole_ty }

addCastTo :: ArgInfo -> OutCoercion -> ArgInfo
addCastTo ai co = ai { ai_args = CastBy co : ai_args ai }

isStrictArgInfo :: ArgInfo -> Bool
-- True if the function is strict in the next argument
isStrictArgInfo (ArgInfo { ai_dmds = dmds })
  | dmd:_ <- dmds = isStrUsedDmd dmd
  | otherwise     = False

argInfoAppArgs :: [ArgSpec] -> [OutExpr]
argInfoAppArgs []                              = []
argInfoAppArgs (CastBy {}                : _)  = []  -- Stop at a cast
argInfoAppArgs (ValArg { as_arg = arg }  : as) = arg     : argInfoAppArgs as
argInfoAppArgs (TyArg { as_arg_ty = ty } : as) = Type ty : argInfoAppArgs as

pushSimplifiedArgs, pushSimplifiedRevArgs
  :: SimplEnv
  -> [ArgSpec]   -- In normal, forward order for pushSimplifiedArgs,
                 -- in /reverse/ order for pushSimplifiedRevArgs
  -> SimplCont -> SimplCont
pushSimplifiedArgs    env args cont = foldr  (pushSimplifiedArg env)             cont args
pushSimplifiedRevArgs env args cont = foldl' (\k a -> pushSimplifiedArg env a k) cont args

pushSimplifiedArg :: SimplEnv -> ArgSpec -> SimplCont -> SimplCont
pushSimplifiedArg _env (TyArg { as_arg_ty = arg_ty, as_hole_ty = hole_ty }) cont
  = ApplyToTy  { sc_arg_ty = arg_ty, sc_hole_ty = hole_ty, sc_cont = cont }
pushSimplifiedArg env (ValArg { as_arg = arg, as_hole_ty = hole_ty }) cont
  = ApplyToVal { sc_arg = arg, sc_env = env, sc_dup = Simplified
                 -- The SubstEnv will be ignored since sc_dup=Simplified
               , sc_hole_ty = hole_ty, sc_cont = cont }
pushSimplifiedArg _ (CastBy c) cont
  = CastIt { sc_co = c, sc_cont = cont, sc_opt = True }

argInfoExpr :: OutId -> [ArgSpec] -> OutExpr
-- NB: the [ArgSpec] is reversed so that the first arg
-- in the list is the last one in the application
argInfoExpr fun rev_args
  = go rev_args
  where
    go []                              = Var fun
    go (ValArg { as_arg = arg }  : as) = go as `App` arg
    go (TyArg { as_arg_ty = ty } : as) = go as `App` Type ty
    go (CastBy co                : as) = mkCast (go as) co

decArgCount :: RewriteCall -> RewriteCall
decArgCount (TryRules n rules) = TryRules (n-1) rules
decArgCount rew                = rew

mkRewriteCall :: Id -> RuleEnv -> RewriteCall
-- See Note [Rewrite rules and inlining] in GHC.Core.Opt.Simplify.Iteration
-- We try to skip any unnecessary stages:
--    No rules     => skip TryRules
--    No unfolding => skip TryInlining
-- This skipping is "just" for efficiency.  But rebuildCall is
-- quite a heavy hammer, so skipping stages is a good plan.
-- And it's extremely simple to do.
mkRewriteCall fun rule_env
  | not (null rules) = TryRules n_required rules
  | canUnfold unf    = TryInlining
  | otherwise        = TryNothing
  where
    n_required = maximum (map ruleArity rules)
    rules = getRules rule_env fun
    unf   = idUnfolding fun

{-
************************************************************************
*                                                                      *
                Functions on SimplCont
*                                                                      *
************************************************************************
-}

mkBoringStop :: OutType -> SimplCont
mkBoringStop ty = Stop ty BoringCtxt topSubDmd

mkRhsStop :: OutType -> RecFlag -> Demand -> SimplCont
-- See Note [RHS of lets] in GHC.Core.Unfold
mkRhsStop ty is_rec bndr_dmd = Stop ty (RhsCtxt is_rec) (subDemandIfEvaluated bndr_dmd)

mkLazyArgStop :: OutType -> ArgInfo -> SimplCont
mkLazyArgStop ty fun_info = Stop ty (lazyArgContext fun_info) arg_sd
  where
    arg_sd = subDemandIfEvaluated (Partial.head (ai_dmds fun_info))

-------------------
contIsRhs :: SimplCont -> Maybe RecFlag
contIsRhs (Stop _ (RhsCtxt is_rec) _) = Just is_rec
contIsRhs (CastIt { sc_cont = k })    = contIsRhs k   -- For f = e |> co, treat e as Rhs context
contIsRhs _                           = Nothing

-------------------
contIsStop :: SimplCont -> Bool
contIsStop (Stop {}) = True
contIsStop _         = False

contIsDupable :: SimplCont -> Bool
contIsDupable (Stop {})                         = True
contIsDupable (ApplyToTy  { sc_cont = k })      = contIsDupable k
contIsDupable (ApplyToVal { sc_dup = OkToDup }) = True -- See Note [DupFlag invariants]
contIsDupable (Select { sc_dup = OkToDup })     = True -- ...ditto...
contIsDupable (StrictArg { sc_dup = OkToDup })  = True -- ...ditto...
contIsDupable (CastIt { sc_cont = k })          = contIsDupable k
contIsDupable _                                 = False

-------------------
contIsTrivial :: SimplCont -> Bool
contIsTrivial (Stop {})                                         = True
contIsTrivial (ApplyToTy { sc_cont = k })                       = contIsTrivial k
-- This one doesn't look right.  A value application is not trivial
-- contIsTrivial (ApplyToVal { sc_arg = Coercion _, sc_cont = k }) = contIsTrivial k
contIsTrivial (CastIt { sc_cont = k })                          = contIsTrivial k
contIsTrivial _                                                 = False

-------------------
contResultType :: SimplCont -> OutType
contResultType (Stop ty _ _)                = ty
contResultType (CastIt { sc_cont = k })     = contResultType k
contResultType (StrictBind { sc_cont = k }) = contResultType k
contResultType (StrictArg { sc_cont = k })  = contResultType k
contResultType (Select { sc_cont = k })     = contResultType k
contResultType (ApplyToTy  { sc_cont = k }) = contResultType k
contResultType (ApplyToVal { sc_cont = k }) = contResultType k
contResultType (TickIt _ k)                 = contResultType k

contHoleType :: SimplCont -> OutType
contHoleType (Stop ty _ _)                    = ty
contHoleType (TickIt _ k)                     = contHoleType k
contHoleType (CastIt { sc_co = co })          = coercionLKind co
contHoleType (StrictBind { sc_bndr = b, sc_dup = dup, sc_env = se })
  = perhapsSubstTy dup se (idType b)
contHoleType (StrictArg  { sc_fun_ty = ty })  = funArgTy ty
contHoleType (ApplyToTy  { sc_hole_ty = ty }) = ty  -- See Note [The hole type in ApplyToTy]
contHoleType (ApplyToVal { sc_hole_ty = ty }) = ty  -- See Note [The hole type in ApplyToTy]
contHoleType (Select { sc_dup = d, sc_bndr =  b, sc_env = se })
  = perhapsSubstTy d se (idType b)


-- Computes the multiplicity scaling factor at the hole. That is, in (case [] of
-- x ::(p) _ { … }) (respectively for arguments of functions), the scaling
-- factor is p. And in E[G[]], the scaling factor is the product of the scaling
-- factor of E and that of G.
--
-- The scaling factor at the hole of E[] is used to determine how a binder
-- should be scaled if it commutes with E. This appears, in particular, in the
-- case-of-case transformation.
contHoleScaling :: SimplCont -> Mult
contHoleScaling (Stop _ _ _) = OneTy
contHoleScaling (CastIt { sc_cont = k })
  = contHoleScaling k
contHoleScaling (StrictBind { sc_bndr = id, sc_cont = k })
  = idMult id `mkMultMul` contHoleScaling k
contHoleScaling (Select { sc_bndr = id, sc_cont = k })
  = idMult id `mkMultMul` contHoleScaling k
contHoleScaling (StrictArg { sc_fun_ty = fun_ty, sc_cont = k })
  = w `mkMultMul` contHoleScaling k
  where
    (w, _, _) = splitFunTy fun_ty
contHoleScaling (ApplyToTy { sc_cont = k }) = contHoleScaling k
contHoleScaling (ApplyToVal { sc_cont = k }) = contHoleScaling k
contHoleScaling (TickIt _ k) = contHoleScaling k

-------------------
countArgs :: SimplCont -> Int
-- Count all arguments, including types, coercions,
-- and other values; skipping over casts.
countArgs (ApplyToTy  { sc_cont = cont }) = 1 + countArgs cont
countArgs (ApplyToVal { sc_cont = cont }) = 1 + countArgs cont
countArgs (CastIt     { sc_cont = cont }) = countArgs cont
countArgs _                               = 0

countValArgs :: SimplCont -> Int
-- Count value arguments only
countValArgs (ApplyToTy  { sc_cont = cont }) = countValArgs cont
countValArgs (ApplyToVal { sc_cont = cont }) = 1 + countValArgs cont
countValArgs (CastIt     { sc_cont = cont }) = countValArgs cont
countValArgs _                               = 0

-------------------
contArgs :: SimplCont -> (Bool, [ArgSummary], SimplCont)
-- Summarises value args, discards type args and coercions
-- The returned continuation of the call is only used to
-- answer questions like "are you interesting?"
contArgs cont
  | lone cont = (True, [], cont)
  | otherwise = go [] cont
  where
    lone (ApplyToTy  {}) = False  -- See Note [Lone variables] in GHC.Core.Unfold
    lone (ApplyToVal {}) = False  -- NB: even a type application or cast
    lone (CastIt {})     = False  --     stops it being "lone"
    lone _               = True

    go args (ApplyToVal { sc_arg = arg, sc_env = se, sc_cont = k })
                                        = go (is_interesting arg se : args) k
    go args (ApplyToTy { sc_cont = k }) = go args k
    go args (CastIt { sc_cont = k })    = go args k
    go args k                           = (False, reverse args, k)

    is_interesting arg se = interestingArg se arg
                   -- Do *not* use short-cutting substitution here
                   -- because we want to get as much IdInfo as possible

-- | Describes how the 'SimplCont' will evaluate the hole as a 'SubDemand'.
-- This can be more insightful than the limited syntactic context that
-- 'SimplCont' provides, because the 'Stop' constructor might carry a useful
-- 'SubDemand'.
-- For example, when simplifying the argument `e` in `f e` and `f` has the
-- demand signature `<MP(S,A)>`, this function will give you back `P(S,A)` when
-- simplifying `e`.
--
-- PRECONDITION: Don't call with 'ApplyToVal'. We haven't thoroughly thought
-- about what to do then and no call sites so far seem to care.
contEvalContext :: SimplCont -> SubDemand
contEvalContext k = case k of
  Stop _ _ sd              -> sd
  TickIt _ k               -> contEvalContext k
  CastIt   { sc_cont = k } -> contEvalContext k
  ApplyToTy{ sc_cont = k } -> contEvalContext k
    --  ApplyToVal{sc_cont=k}      -> mkCalledOnceDmd $ contEvalContext k
    -- Not 100% sure that's correct, . Here's an example:
    --   f (e x) and f :: <SC(S,C(1,L))>
    -- then what is the evaluation context of 'e' when we simplify it? E.g.,
    --   simpl e (ApplyToVal x $ Stop "C(S,C(1,L))")
    -- then it *should* be "C(1,C(S,C(1,L))", so perhaps correct after all.
    -- But for now we just panic:
  ApplyToVal{}               -> pprPanic "contEvalContext" (ppr k)
  StrictArg{sc_fun=fun_info} -> subDemandIfEvaluated (Partial.head (ai_dmds fun_info))
  StrictBind{sc_bndr=bndr}   -> subDemandIfEvaluated (idDemandInfo bndr)
  Select{}                   -> topSubDmd
    -- Perhaps reconstruct the demand on the scrutinee by looking at field
    -- and case binder dmds, see addCaseBndrDmd. No priority right now.

-------------------
mkArgInfo :: SimplEnv -> RuleEnv -> Id -> SimplCont -> ArgInfo

mkArgInfo env rule_base fun cont
  | n_val_args < idArity fun            -- Note [Unsaturated functions]
  = ArgInfo { ai_fun = fun, ai_args = []
            , ai_rewrite = fun_rewrite
            , ai_encl = False
            , ai_dmds = vanilla_dmds
            , ai_discs = vanilla_discounts }
  | otherwise
  = ArgInfo { ai_fun   = fun
            , ai_args  = []
            , ai_rewrite = fun_rewrite
            , ai_encl  = fun_has_rules || contHasRules cont
            , ai_dmds  = add_type_strictness (idType fun) arg_dmds
            , ai_discs = arg_discounts }
  where
    n_val_args    = countValArgs cont
    fun_rewrite   = mkRewriteCall fun rule_base
    fun_has_rules = case fun_rewrite of
                      TryRules {} -> True
                      _           -> False

    vanilla_discounts, arg_discounts :: [Int]
    vanilla_discounts = repeat 0
    arg_discounts = case idUnfolding fun of
                        CoreUnfolding {uf_guidance = UnfIfGoodArgs {ug_args = discounts}}
                              -> discounts ++ vanilla_discounts
                        _     -> vanilla_discounts

    vanilla_dmds, arg_dmds :: [Demand]
    vanilla_dmds  = repeat topDmd

    arg_dmds
      | not (seInline env)
      = vanilla_dmds -- See Note [Do not expose strictness if sm_inline=False]
      | otherwise
      = -- add_type_str fun_ty $
        case splitDmdSig (idDmdSig fun) of
          (demands, result_info)
                | not (demands `lengthExceeds` n_val_args)
                ->      -- Enough args, use the strictness given.
                        -- For bottoming functions we used to pretend that the arg
                        -- is lazy, so that we don't treat the arg as an
                        -- interesting context.  This avoids substituting
                        -- top-level bindings for (say) strings into
                        -- calls to error.  But now we are more careful about
                        -- inlining lone variables, so its ok
                        -- (see GHC.Core.Op.Simplify.Utils.analyseCont)
                   if isDeadEndDiv result_info then
                        demands  -- Finite => result is bottom
                   else
                        demands ++ vanilla_dmds
               | otherwise
               -> warnPprTrace True "More demands than arity" (ppr fun <+> ppr (idArity fun)
                                <+> ppr n_val_args <+> ppr demands) $
                  vanilla_dmds      -- Not enough args, or no strictness

    add_type_strictness :: Type -> [Demand] -> [Demand]
    -- If the function arg types are strict, record that in the 'strictness bits'
    -- No need to instantiate because unboxed types (which dominate the strict
    --   types) can't instantiate type variables.
    -- add_type_strictness is done repeatedly (for each call);
    --   might be better once-for-all in the function
    -- But beware primops/datacons with no strictness

    add_type_strictness fun_ty dmds
      | null dmds = []

      | Just (_, fun_ty') <- splitForAllTyCoVar_maybe fun_ty
      = add_type_strictness fun_ty' dmds     -- Look through foralls

      | Just (_, _, arg_ty, fun_ty') <- splitFunTy_maybe fun_ty        -- Add strict-type info
      , dmd : rest_dmds <- dmds
      , let dmd'
             | definitelyUnliftedType arg_ty
             = strictifyDmd dmd
             | otherwise
             -- Something that's not definitely unlifted.
             -- If the type is representation-polymorphic, we can't know whether
             -- it's strict.
             = dmd
      = dmd' : add_type_strictness fun_ty' rest_dmds

      | otherwise
      = dmds

{- Note [Unsaturated functions]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (test eyeball/inline4)
        x = a:as
        y = f x
where f has arity 2.  Then we do not want to inline 'x', because
it'll just be floated out again.  Even if f has lots of discounts
on its first argument -- it must be saturated for these to kick in

Note [Do not expose strictness if sm_inline=False]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#15163 showed a case in which we had

  {-# INLINE [1] zip #-}
  zip = undefined

  {-# RULES "foo" forall as bs. stream (zip as bs) = ..blah... #-}

If we expose zip's bottoming nature when simplifying the LHS of the
RULE we get
  {-# RULES "foo" forall as bs.
                   stream (case zip of {}) = ..blah... #-}
discarding the arguments to zip.  Usually this is fine, but on the
LHS of a rule it's not, because 'as' and 'bs' are now not bound on
the LHS.

This is a pretty pathological example, so I'm not losing sleep over
it, but the simplest solution was to check sm_inline; if it is False,
which it is on the LHS of a rule (see updModeForRules), then don't
make use of the strictness info for the function.
-}


{-
************************************************************************
*                                                                      *
        Interesting arguments
*                                                                      *
************************************************************************

Note [Interesting call context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee that isn't just a seq, application etc.)
then we inline, otherwise we don't.

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

        let x = _coerce_ (T Int) Int (I# 3) in
        case _coerce_ Int (T Int) x of
                I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

        case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

Note [No case of case is boring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see
   case f x of <alts>

we'd usually treat the context as interesting, to encourage 'f' to
inline.  But if case-of-case is off, it's really not so interesting
after all, because we are unlikely to be able to push the case
expression into the branches of any case in f's unfolding.  So, to
reduce unnecessary code expansion, we just make the context look boring.
This made a small compile-time perf improvement in perf/compiler/T6048,
and it looks plausible to me.

Note [Seq is boring]
~~~~~~~~~~~~~~~~~~~~
Suppose
  f x = case v of
          True  -> Just x
          False -> Just (x-1)

Now consider these variants of
   case (f x) of ...

1. [Dead case binder]: inline f
      case f x of b{-dead-} { DEFAULT -> blah[no b] }
   Inlining (f x) will allow us to avoid ever allocating (Just x),
   since the case binder `b` is dead.  We will end up with a
     join point for blah, thus
         join j = blah in
         case v of { True -> j; False -> j }
   which will turn into (case v of DEFAULT -> blah)
   All good

2. [Live case binder, live alt binders]: inline f
       case f x of b { Just y -> blah[y,b] }
   Inlining (f x) will mean we still allocate (Just x),
   but we also get to bind `y` without fetching it out of the Just, thus
         join j y b = blah[y,b]
         case v of { True -> j x (Just x)
                   ; False -> let y = x-1 in j y (Just y) }
   Inlining (f x) has a small benefit, perhaps.
   (To T14955 it makes a surprisingly large difference of ~30% to inline here.)

3. [Live case binder, dead alt binders]: maybe don't inline f
      case f x of b { DEFAULT -> blah[b] }
   Inlining (f x) will still mean we allocate (Just x). We'd get:
         join j b = blah[b]
         case v of { True -> j (Just x); False -> j (Just (x-1)) }
   No new optimisations are revealed. Nothing is gained.
   (This is the situation in T22317.)

   A variant is when we have a data constructor with dead binders:
       case g x of b { (x{-dead-}, x{-dead-}) -> blah[b, no x, no y] }
   Instead of DEFAULT we have a single constructor alternative
   with all dead binders.  Again, no gain from inlining (f x)

4. [Live case binder, dead alt binders]: small f
   Suppose f is CPR'd, so it looks like
       f x = case $wf x of (# a #) -> Just a
   Then even in case (3) we want to inline:
      case f x of b { DEFAULT -> blah[b] }
   -->
      case $wf x of (# a #) ->
      let b = Just a in blah[b]
   This is very good; we now know a lot about `b` (instead of nothing)
   and `blah` might benefit.  Similarly if `f` has a join point
      f x = join $j y = Just y in ...
   Again the case (f x) is now consuming a constructor (Just y).

   This is very like the situation described in Note [RHS of lets]
   in GHC.Core.Opt.Simplify.Inline; (case e of b -> blah) is just
   like a strict `let`.

Conclusion: in interestingCallCtxt, a case-expression (i.e. Select continuation)
usually gives a CaseCtxt (cases 1,2); but when (cases 3,4):
  * It has a non-dead case-binder
  * It has one alternative
  * All the binders in the alternative are dead
then the `case` is just a strict let-binding, so use RhsCtxt NonRecursive.
This RhsCtxt gives a small incentive for small functions to inline.
That incentive is what is needed in case (4).

Wrinkle (SB1).  The 'small incentive' is implemented by `calc_some_benefit` in
GHC.Core.Opt.Simplify.Inline.tryUnfolding.  We restrict the incentive just to
funtions that have unfolding guidance of `UnfWhen`, which particularly includes
wrappers created by CPR, exactly case (4) above.  Without this limitation I
got too much fruitless inlining, which led to regressions (#22317 is an example).

A good example of a function where this 'small incentive' is important is
GHC.Num.Integer where we ended up with calls like this:
     case (integerSignum a b) of r -> ...
but were failing to inline integerSignum, even though it always returns
a single constructor, so it is very helpful to inline it. There is also an
issue of confluence-of-the-simplifier.  Suppose we have
    f x = case x of r -> ...
and the Simplifier sees
    f (integerSigNum a b)
Because `f` scrutines `x`, the unfolding guidance for f gives a discount
for `x`; and that discount makes interestingCallContext for the context
`f <>` return DiscArgCtxt, which again gives that incentive.  We don't want
the incentive to disappear when we inline `f`!
-}

lazyArgContext :: ArgInfo -> CallCtxt
-- Use this for lazy arguments
lazyArgContext (ArgInfo { ai_encl = encl_rules, ai_discs = discs })
  | encl_rules                = RuleArgCtxt
  | disc:_ <- discs, disc > 0 = DiscArgCtxt  -- Be keener here
  | otherwise                 = BoringCtxt   -- Nothing interesting

strictArgContext :: ArgInfo -> CallCtxt
strictArgContext (ArgInfo { ai_encl = encl_rules, ai_discs = discs })
-- Use this for strict arguments
  | encl_rules                = RuleArgCtxt
  | disc:_ <- discs, disc > 0 = DiscArgCtxt  -- Be keener here
  | otherwise                 = RhsCtxt NonRecursive
      -- Why RhsCtxt?  if we see f (g x), and f is strict, we
      -- want to be a bit more eager to inline g, because it may
      -- expose an eval (on x perhaps) that can be eliminated or
      -- shared. I saw this in nofib 'boyer2', RewriteFuns.onewayunify1
      -- It's worth an 18% improvement in allocation for this
      -- particular benchmark; 5% on 'mate' and 1.3% on 'multiplier'
      --
      -- Why NonRecursive?  Becuase it's a bit like
      --   let a = g x in f a

interestingCallContext :: SimplEnv -> SimplCont -> CallCtxt
-- See Note [Interesting call context]
interestingCallContext env cont
  = interesting cont
  where
    interesting (Select {sc_alts=alts, sc_bndr=case_bndr})
      | not (seCaseCase env)         = BoringCtxt -- See Note [No case of case is boring]
      | [Alt _ bs _] <- alts
      , all isDeadBinder bs
      , not (isDeadBinder case_bndr) = RhsCtxt NonRecursive -- See Note [Seq is boring]
      | otherwise                    = CaseCtxt


    interesting (ApplyToVal {}) = ValAppCtxt
        -- Can happen if we have (f Int |> co) y
        -- If f has an INLINE prag we need to give it some
        -- motivation to inline. See Note [Cast then apply]
        -- in GHC.Core.Unfold

    interesting (StrictArg { sc_fun = fun }) = strictArgContext fun
    interesting (StrictBind {})              = BoringCtxt
    interesting (Stop _ cci _)               = cci
    interesting (TickIt _ k)                 = interesting k
    interesting (ApplyToTy { sc_cont = k })  = interesting k
    interesting (CastIt { sc_cont = k })     = interesting k
        -- If this call is the arg of a strict function, the context
        -- is a bit interesting.  If we inline here, we may get useful
        -- evaluation information to avoid repeated evals: e.g.
        --      x + (y * z)
        -- Here the contIsInteresting makes the '*' keener to inline,
        -- which in turn exposes a constructor which makes the '+' inline.
        -- Assuming that +,* aren't small enough to inline regardless.
        --
        -- It's also very important to inline in a strict context for things
        -- like
        --              foldr k z (f x)
        -- Here, the context of (f x) is strict, and if f's unfolding is
        -- a build it's *great* to inline it here.  So we must ensure that
        -- the context for (f x) is not totally uninteresting.

contHasRules :: SimplCont -> Bool
-- If the argument has form (f x y), where x,y are boring,
-- and f is marked INLINE, then we don't want to inline f.
-- But if the context of the argument is
--      g (f x y)
-- where g has rules, then we *do* want to inline f, in case it
-- exposes a rule that might fire.  Similarly, if the context is
--      h (g (f x x))
-- where h has rules, then we do want to inline f.  So contHasRules
-- tries to see if the context of the f-call is a call to a function
-- with rules.
--
-- The ai_encl flag makes this happen; if it's
-- set, the inliner gets just enough keener to inline f
-- regardless of how boring f's arguments are, if it's marked INLINE
--
-- The alternative would be to *always* inline an INLINE function,
-- regardless of how boring its context is; but that seems overkill
-- For example, it'd mean that wrapper functions were always inlined
contHasRules cont
  = go cont
  where
    go (ApplyToVal { sc_cont = cont }) = go cont
    go (ApplyToTy  { sc_cont = cont }) = go cont
    go (CastIt { sc_cont = cont })     = go cont
    go (StrictArg { sc_fun = fun })    = ai_encl fun
    go (Stop _ RuleArgCtxt _)          = True
    go (TickIt _ c)                    = go c
    go (Select {})                     = False
    go (StrictBind {})                 = False      -- ??
    go (Stop _ _ _)                    = False

{- Note [Interesting arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An argument is interesting if it deserves a discount for unfoldings
with a discount in that argument position.  The idea is to avoid
unfolding a function that is applied only to variables that have no
unfolding (i.e. they are probably lambda bound): f x y z There is
little point in inlining f here.

Generally, *values* (like (C a b) and (\x.e)) deserve discounts.  But
we must look through lets, eg (let x = e in C a b), because the let will
float, exposing the value, if we inline.  That makes it different to
exprIsHNF.

Before 2009 we said it was interesting if the argument had *any* structure
at all; i.e. (hasSomeUnfolding v).  But does too much inlining; see #3016.

But we don't regard (f x y) as interesting, unless f is unsaturated.
If it's saturated and f hasn't inlined, then it's probably not going
to now!

Note [Conlike is interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        f d = ...((*) d x y)...
        ... f (df d')...
where df is con-like. Then we'd really like to inline 'f' so that the
rule for (*) (df d) can fire.  To do this
  a) we give a discount for being an argument of a class-op (eg (*) d)
  b) we say that a con-like argument (eg (df d)) is interesting
-}

interestingArg :: SimplEnv -> CoreExpr -> ArgSummary
-- See Note [Interesting arguments]
interestingArg env e = go env 0 e
  where
    -- n is # value args to which the expression is applied
    go env n (Var v)
       = case substId env v of
           DoneId v'            -> go_var n v'
           DoneEx e _           -> go (zapSubstEnv env)             n e
           ContEx tvs cvs ids e -> go (setSubstEnv env tvs cvs ids) n e

    go _   _ (Lit l)
       | isLitRubbish l        = TrivArg -- Leads to unproductive inlining in WWRec, #20035
       | otherwise             = ValueArg
    go _   _ (Type _)          = TrivArg
    go _   _ (Coercion _)      = TrivArg
    go env n (App fn (Type _)) = go env n fn
    go env n (App fn _)        = go env (n+1) fn
    go env n (Tick _ a)        = go env n a
    go env n (Cast e _)        = go env n e
    go env n (Lam v e)
       | isTyVar v             = go env n e
       | n>0                   = NonTrivArg     -- (\x.b) e   is NonTriv
       | otherwise             = ValueArg
    go _ _ (Case {})           = NonTrivArg
    go env n (Let b e)         = case go env' n e of
                                   ValueArg -> ValueArg
                                   _        -> NonTrivArg
                               where
                                 env' = env `addNewInScopeIds` bindersOf b

    go_var n v
       | isConLikeId v = ValueArg   -- Experimenting with 'conlike' rather that
                                    --    data constructors here
                                    -- DFuns are con-like; see Note [Conlike is interesting]
       | idArity v > n = ValueArg   -- Catches (eg) primops with arity but no unfolding
       | n > 0         = NonTrivArg -- Saturated or unknown call
       | otherwise  -- n==0, no value arguments; look for an interesting unfolding
       = case idUnfolding v of
           OtherCon [] -> NonTrivArg   -- It's evaluated, but that's all we know
           OtherCon _  -> ValueArg     -- Evaluated and we know it isn't these constructors
              -- See Note [OtherCon and interestingArg]
           DFunUnfolding {} -> ValueArg   -- We konw that idArity=0
           CoreUnfolding{ uf_cache = cache }
             | uf_is_conlike cache -> ValueArg    -- Includes constructor applications
             | uf_is_value cache   -> NonTrivArg  -- Things like partial applications
             | otherwise           -> TrivArg
           BootUnfolding           -> TrivArg
           NoUnfolding             -> TrivArg

{- Note [OtherCon and interestingArg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
interstingArg returns
   (a) NonTrivArg for an arg with an OtherCon [] unfolding
   (b) ValueArg for an arg with an OtherCon [c1,c2..] unfolding.

Reason for (a): I found (in the GHC.Num.Integer library) that I was
inlining a pretty big function when all we knew was that its arguments
were evaluated, nothing more.  That in turn make the enclosing function
too big to inline elsewhere.

Reason for (b): we want to inline integerCompare here
  integerLt# :: Integer -> Integer -> Bool#
  integerLt# (IS x) (IS y)                  = x <# y
  integerLt# x y | LT <- integerCompare x y = 1#
  integerLt# _ _                            = 0#

************************************************************************
*                                                                      *
                  SimplMode
*                                                                      *
************************************************************************
-}

updModeForStableUnfoldings :: Activation (GhcPass p) -> SimplMode -> SimplMode
-- See Note [The environments of the Simplify pass]
updModeForStableUnfoldings unf_act current_mode
  = current_mode { sm_phase      = phaseFromActivation unf_act
                 , sm_eta_expand = False
                 , sm_inline     = True }
       -- sm_eta_expand: see Note [Eta expansion in stable unfoldings and rules]
       -- sm_rules: just inherit; sm_rules might be "off"
       --           because of -fno-enable-rewrite-rules
  where
    phaseFromActivation (ActiveAfter _ n) = Phase n
    phaseFromActivation _                 = InitialPhase

updModeForRules :: SimplMode -> SimplMode
-- See Note [Simplifying rules]
-- See Note [The environments of the Simplify pass]
updModeForRules current_mode
  = current_mode { sm_phase        = InitialPhase
                 , sm_inline       = False
                      -- See Note [Do not expose strictness if sm_inline=False]
                 , sm_rules        = False
                 , sm_cast_swizzle = False
                      -- See Note [Cast swizzling on rule LHSs]
                 , sm_eta_expand   = False }

{- Note [Simplifying rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When simplifying a rule LHS, refrain from /any/ inlining or applying
of other RULES. Doing anything to the LHS is plain confusing, because
it means that what the rule matches is not what the user
wrote. c.f. #10595, and #10528.

* sm_inline, sm_rules: inlining (or applying rules) on rule LHSs risks
  introducing Ticks into the LHS, which makes matching
  trickier. #10665, #10745.

  Doing this to either side confounds tools like HERMIT, which seek to reason
  about and apply the RULES as originally written. See #10829.

  See also Note [Do not expose strictness if sm_inline=False]

* sm_eta_expand: the template (LHS) of a rule must only mention coercion
  /variables/ not arbitrary coercions.  See Note [Casts in the template] in
  GHC.Core.Rules.  Eta expansion can create new coercions; so we switch
  it off.

There is, however, one case where we are pretty much /forced/ to transform the
LHS of a rule: postInlineUnconditionally. For instance, in the case of

    let f = g @Int in f

We very much want to inline f into the body of the let. However, to do so (and
be able to safely drop f's binding) we must inline into all occurrences of f,
including those in the LHS of rules.

This can cause somewhat surprising results; for instance, in #18162 we found
that a rule template contained ticks in its arguments, because
postInlineUnconditionally substituted in a trivial expression that contains
ticks. See Note [Tick annotations in RULE matching] in GHC.Core.Rules for
details.

Note [Cast swizzling on rule LHSs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the LHS of a RULE we may have
       (\x. blah |> CoVar cv)
where `cv` is a coercion variable.  Critically, we really only want
coercion /variables/, not general coercions, on the LHS of a RULE.  So
we don't want to swizzle this to
      (\x. blah) |> (Refl xty `FunCo` CoVar cv)
So we switch off cast swizzling in updModeForRules.

Note [Eta expansion in stable unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SPJ Jul 22: whether or not eta-expansion is switched on in a stable
unfolding, or the RHS of a RULE, seems to be a bit moot. But switching
it on adds clutter, so I'm experimenting with switching off
eta-expansion in such places.

In the olden days, we really /wanted/ to switch it off.

    Old note: If we have a stable unfolding
      f :: Ord a => a -> IO ()
      -- Unfolding template
      --    = /\a \(d:Ord a) (x:a). bla
    we do not want to eta-expand to
      f :: Ord a => a -> IO ()
      -- Unfolding template
      --    = (/\a \(d:Ord a) (x:a) (eta:State#). bla eta) |> co
    because now specialisation of the overloading doesn't work properly
    (see Note [Specialisation shape] in GHC.Core.Opt.Specialise), #9509.
    So we disable eta-expansion in stable unfoldings.

But this old note is no longer relevant because the specialiser has
improved: see Note [Account for casts in binding] in
GHC.Core.Opt.Specialise.  So we seem to have a free choice.

Note [Inlining in gentle mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Something is inlined if
   (i)   the sm_inline flag is on, AND
   (ii)  the thing has an INLINE pragma, AND
   (iii) the thing is inlinable in the earliest phase.

Example of why (iii) is important:
  {-# INLINE [~1] g #-}
  g = ...

  {-# INLINE f #-}
  f x = g (g x)

If we were to inline g into f's inlining, then an importing module would
never be able to do
        f e --> g (g e) ---> RULE fires
because the stable unfolding for f has had g inlined into it.

On the other hand, it is bad not to do ANY inlining into an
stable unfolding, because then recursive knots in instance declarations
don't get unravelled.

However, *sometimes* SimplGently must do no call-site inlining at all
(hence sm_inline = False).  Before full laziness we must be careful
not to inline wrappers, because doing so inhibits floating
    e.g. ...(case f x of ...)...
    ==> ...(case (case x of I# x# -> fw x#) of ...)...
    ==> ...(case x of I# x# -> case fw x# of ...)...
and now the redex (f x) isn't floatable any more.

The no-inlining thing is also important for Template Haskell.  You might be
compiling in one-shot mode with -O2; but when TH compiles a splice before
running it, we don't want to use -O2.  Indeed, we don't want to inline
anything, because the byte-code interpreter might get confused about
unboxed tuples and suchlike.

Note [Simplifying inside stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must take care with simplification inside stable unfoldings (which come from
INLINE pragmas).

First, consider the following example
        let f = \pq -> BIG
        in
        let g = \y -> f y y
            {-# INLINE g #-}
        in ...g...g...g...g...g...
Now, if that's the ONLY occurrence of f, it might be inlined inside g,
and thence copied multiple times when g is inlined. HENCE we treat
any occurrence in a stable unfolding as a multiple occurrence, not a single
one; see OccurAnal.addRuleUsage.

Second, we do want *do* to some modest rules/inlining stuff in stable
unfoldings, partly to eliminate senseless crap, and partly to break
the recursive knots generated by instance declarations.

However, suppose we have
        {-# INLINE <act> f #-}
        f = <rhs>
meaning "inline f in phases p where activation <act>(p) holds".
Then what inlinings/rules can we apply to the copy of <rhs> captured in
f's stable unfolding?  Our model is that literally <rhs> is substituted for
f when it is inlined.  So our conservative plan (implemented by
updModeForStableUnfoldings) is this:

  -------------------------------------------------------------
  When simplifying the RHS of a stable unfolding, set the phase
  to the phase in which the stable unfolding first becomes active
  -------------------------------------------------------------

That ensures that

  a) Rules/inlinings that *cease* being active before p will
     not apply to the stable unfolding, consistent with it being
     inlined in its *original* form in phase p.

  b) Rules/inlinings that only become active *after* p will
     not apply to the stable unfolding, again to be consistent with
     inlining the *original* rhs in phase p.

For example,
        {-# INLINE f #-}
        f x = ...g...

        {-# NOINLINE [1] g #-}
        g y = ...

        {-# RULE h g = ... #-}
Here we must not inline g into f's RHS, even when we get to phase 0,
because when f is later inlined into some other module we want the
rule for h to fire.

Similarly, consider
        {-# INLINE f #-}
        f x = ...g...

        g y = ...
and suppose that there are auto-generated specialisations and a strictness
wrapper for g.  The specialisations get activation AlwaysActive, and the
strictness wrapper get activation (ActiveAfter 0).  So the strictness
wrepper fails the test and won't be inlined into f's stable unfolding. That
means f can inline, expose the specialised call to g, so the specialisation
rules can fire.

A note about wrappers
~~~~~~~~~~~~~~~~~~~~~
It's also important not to inline a worker back into a wrapper.
A wrapper looks like
        wraper = inline_me (\x -> ...worker... )
Normally, the inline_me prevents the worker getting inlined into
the wrapper (initially, the worker's only call site!).  But,
if the wrapper is sure to be called, the strictness analyser will
mark it 'demanded', so when the RHS is simplified, it'll get an ArgOf
continuation.
-}

getUnfoldingInRuleMatch :: SimplEnv -> InScopeEnv
-- When matching in RULE, we want to "look through" an unfolding
-- (to see a constructor) if *rules* are on, even if *inlinings*
-- are not.  A notable example is DFuns, which really we want to
-- match in rules like (op dfun) in gentle mode. Another example
-- is 'otherwise' which we want exprIsConApp_maybe to be able to
-- see very early on
getUnfoldingInRuleMatch env
  = ISE in_scope id_unf
  where
    in_scope = seInScope env
    phase    = sePhase env
    id_unf   = whenActiveUnfoldingFun (isActive phase)
     -- When sm_rules was off we used to test for a /stable/ unfolding,
     -- but that seems wrong (#20941)

----------------------
activeRule :: SimplMode -> Activation (GhcPass p) -> Bool
-- Nothing => No rules at all
activeRule mode
  | not (sm_rules mode) = \_ -> False     -- Rewriting is off
  | otherwise           = isActive (sm_phase mode)

{-
************************************************************************
*                                                                      *
                  preInlineUnconditionally
*                                                                      *
************************************************************************

preInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~
@preInlineUnconditionally@ examines a bndr to see if it is used just
once in a completely safe way, so that it is safe to discard the
binding inline its RHS at the (unique) usage site, REGARDLESS of how
big the RHS might be.  If this is the case we don't simplify the RHS
first, but just inline it un-simplified.

This is much better than first simplifying a perhaps-huge RHS and then
inlining and re-simplifying it.  Indeed, it can be at least quadratically
better.  Consider

        x1 = e1
        x2 = e2[x1]
        x3 = e3[x2]
        ...etc...
        xN = eN[xN-1]

We may end up simplifying e1 N times, e2 N-1 times, e3 N-3 times etc.
This can happen with cascades of functions too:

        f1 = \x1.e1
        f2 = \xs.e2[f1]
        f3 = \xs.e3[f3]
        ...etc...

THE MAIN INVARIANT is this:

        ----  preInlineUnconditionally invariant -----
   IF preInlineUnconditionally chooses to inline x = <rhs>
   THEN doing the inlining should not change the occurrence
        info for the free vars of <rhs>
        ----------------------------------------------

For example, it's tempting to look at trivial binding like
        x = y
and inline it unconditionally.  But suppose x is used many times,
but this is the unique occurrence of y.  Then inlining x would change
y's occurrence info, which breaks the invariant.  It matters: y
might have a BIG rhs, which will now be dup'd at every occurrence of x.


Even RHSs labelled InlineMe aren't caught here, because there might be
no benefit from inlining at the call site.

[Sept 01] Don't unconditionally inline a top-level thing, because that
can simply make a static thing into something built dynamically.  E.g.
        x = (a,b)
        main = \s -> h x

[Remember that we treat \s as a one-shot lambda.]  No point in
inlining x unless there is something interesting about the call site.

But watch out: if you aren't careful, some useful foldr/build fusion
can be lost (most notably in spectral/hartel/parstof) because the
foldr didn't see the build.  Doing the dynamic allocation isn't a big
deal, in fact, but losing the fusion can be.  But the right thing here
seems to be to do a callSiteInline based on the fact that there is
something interesting about the call site (it's strict).  Hmm.  That
seems a bit fragile.

Conclusion: inline top level things gaily until FinalPhase (the last
phase), at which point don't.

Note [pre/postInlineUnconditionally in gentle mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even in gentle mode we want to do preInlineUnconditionally.  The
reason is that too little clean-up happens if you don't inline
use-once things.  Also a bit of inlining is *good* for full laziness;
it can expose constant sub-expressions.  Example in
spectral/mandel/Mandel.hs, where the mandelset function gets a useful
let-float if you inline windowToViewport

However, as usual for Gentle mode, do not inline things that are
inactive in the initial stages.  See Note [Gentle mode].

Note [Stable unfoldings and preInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Surprisingly, do not pre-inline-unconditionally Ids with INLINE pragmas!
Example

   {-# INLINE f #-}
   f :: Eq a => a -> a
   f x = ...

   fInt :: Int -> Int
   fInt = f Int dEqInt

   ...fInt...fInt...fInt...

Here f occurs just once, in the RHS of fInt. But if we inline it there
it might make fInt look big, and we'll lose the opportunity to inline f
at each of fInt's call sites.  The INLINE pragma will only inline when
the application is saturated for exactly this reason; and we don't
want PreInlineUnconditionally to second-guess it. A live example is #3736.
    c.f. Note [Stable unfoldings and postInlineUnconditionally]

NB: this only applies for INLINE things. Do /not/ switch off
preInlineUnconditionally for

* INLINABLE. It just says to GHC "inline this if you like".  If there
  is a unique occurrence, we want to inline the stable unfolding, not
  the RHS.

* NONLINE[n] just switches off inlining until phase n.  We should
  respect that, but after phase n, just behave as usual.

* NoUserInlinePrag.  There is no pragma at all. This ends up on wrappers.
  (See #18815.)

Note [Top-level bottoming Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Don't inline top-level Ids that are bottoming, even if they are used just
once, because FloatOut has gone to some trouble to extract them out.
Inlining them won't make the program run faster!

Note [Do not inline CoVars unconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Coercion variables appear inside coercions, and the RHS of a let-binding
is a term (not a coercion) so we can't necessarily inline the latter in
the former.
-}

preInlineUnconditionally
    :: SimplEnv -> TopLevelFlag -> InId
    -> InExpr -> StaticEnv  -- These two go together
    -> Maybe SimplEnv       -- Returned env has extended substitution
-- Precondition: rhs satisfies the let-can-float invariant
-- See Note [Core let-can-float invariant] in GHC.Core
-- Reason: we don't want to inline single uses, or discard dead bindings,
--         for unlifted, side-effect-ful bindings
preInlineUnconditionally env top_lvl bndr rhs rhs_env
  | not pre_inline_unconditionally           = Nothing
  | not active                               = Nothing
  | isTopLevel top_lvl && isDeadEndId bndr   = Nothing -- Note [Top-level bottoming Ids]
  | isCoVar bndr                             = Nothing -- Note [Do not inline CoVars unconditionally]
  | isExitJoinId bndr                        = Nothing -- Note [Do not inline exit join points]
                                                       -- in module Exitify
  | not (one_occ (idOccInfo bndr))           = Nothing
  | not (isStableUnfolding unf)              = Just $! (extend_subst_with rhs)

  -- See Note [Stable unfoldings and preInlineUnconditionally]
  | not (isInlinePragma inline_prag)
  , Just inl <- maybeUnfoldingTemplate unf   = Just $! (extend_subst_with inl)
  | otherwise                                = Nothing
  where
    unf = idUnfolding bndr
    extend_subst_with inl_rhs = extendIdSubst env bndr $! (mkContEx rhs_env inl_rhs)

    one_occ IAmDead = True -- Happens in ((\x.1) v)
    one_occ OneOcc{ occ_n_br   = 1
                  , occ_in_lam = NotInsideLam }   = isNotTopLevel top_lvl || early_phase
    one_occ OneOcc{ occ_n_br   = 1
                  , occ_in_lam = IsInsideLam
                  , occ_int_cxt = IsInteresting } = canInlineInLam rhs
    one_occ _                                     = False

    pre_inline_unconditionally = sePreInline env
    active = isActive (sePhase env) (inl_act inline_prag)
             -- See Note [pre/postInlineUnconditionally in gentle mode]
    inline_prag = idInlinePragma bndr

-- Be very careful before inlining inside a lambda, because (a) we must not
-- invalidate occurrence information, and (b) we want to avoid pushing a
-- single allocation (here) into multiple allocations (inside lambda).
-- Inlining a *function* with a single *saturated* call would be ok, mind you.
--      || (if is_cheap && not (canInlineInLam rhs) then pprTrace "preinline" (ppr bndr <+> ppr rhs) ok else ok)
--      where
--              is_cheap = exprIsCheap rhs
--              ok = is_cheap && int_cxt

        --      int_cxt         The context isn't totally boring
        -- E.g. let f = \ab.BIG in \y. map f xs
        --      Don't want to substitute for f, because then we allocate
        --      its closure every time the \y is called
        -- But: let f = \ab.BIG in \y. map (f y) xs
        --      Now we do want to substitute for f, even though it's not
        --      saturated, because we're going to allocate a closure for
        --      (f y) every time round the loop anyhow.

        -- canInlineInLam => free vars of rhs are (Once in_lam) or Many,
        -- so substituting rhs inside a lambda doesn't change the occ info.
        -- Sadly, not quite the same as exprIsHNF.
    canInlineInLam (Lit _)    = True
    canInlineInLam (Lam b e)  = isRuntimeVar b || canInlineInLam e
    canInlineInLam (Tick t e) = not (tickishIsCode t) && canInlineInLam e
    canInlineInLam _          = False
      -- not ticks.  Counting ticks cannot be duplicated, and non-counting
      -- ticks around a Lam will disappear anyway.

    early_phase = sePhase env /= FinalPhase
    -- If we don't have this early_phase test, consider
    --      x = length [1,2,3]
    -- The full laziness pass carefully floats all the cons cells to
    -- top level, and preInlineUnconditionally floats them all back in.
    -- Result is (a) static allocation replaced by dynamic allocation
    --           (b) many simplifier iterations because this tickles
    --               a related problem; only one inlining per pass
    --
    -- On the other hand, I have seen cases where top-level fusion is
    -- lost if we don't inline top level thing (e.g. string constants)
    -- Hence the test for phase zero (which is the phase for all the final
    -- simplifications).  Until phase zero we take no special notice of
    -- top level things, but then we become more leery about inlining
    -- them.
    --
    -- What exactly to check in `early_phase` above is the subject of #17910.
    --
    -- !10088 introduced an additional Simplifier iteration in LargeRecord
    -- because we first FloatOut `case unsafeEqualityProof of ... -> I# 2#`
    -- (a non-trivial value) which we immediately inline back in.
    -- Ideally, we'd never have inlined it because the binding turns out to
    -- be expandable; unfortunately we need an iteration of the Simplifier to
    -- attach the proper unfolding and can't check isExpandableUnfolding right
    -- here.
    -- (Nor can we check for `exprIsExpandable rhs`, because that needs to look
    -- at the non-existent unfolding for the `I# 2#` which is also floated out.)

{-
************************************************************************
*                                                                      *
                  postInlineUnconditionally
*                                                                      *
************************************************************************

postInlineUnconditionally
~~~~~~~~~~~~~~~~~~~~~~~~~
@postInlineUnconditionally@ decides whether to unconditionally inline
a thing based on the form of its RHS; in particular if it has a
trivial RHS.  If so, we can inline and discard the binding altogether.

NB: a loop breaker has must_keep_binding = True and non-loop-breakers
only have *forward* references. Hence, it's safe to discard the binding

NOTE: This isn't our last opportunity to inline.  We're at the binding
site right now, and we'll get another opportunity when we get to the
occurrence(s)

Note that we do this unconditional inlining only for trivial RHSs.
Don't inline even WHNFs inside lambdas; doing so may simply increase
allocation when the function is called. This isn't the last chance; see
NOTE above.

NB: Even inline pragmas (e.g. IMustBeINLINEd) are ignored here Why?
Because we don't even want to inline them into the RHS of constructor
arguments. See NOTE above

NB: At one time even NOINLINE was ignored here: if the rhs is trivial
it's best to inline it anyway.  We often get a=E; b=a from desugaring,
with both a and b marked NOINLINE.  But that seems incompatible with
our new view that inlining is like a RULE, so I'm sticking to the 'active'
story for now.

NB: unconditional inlining of this sort can introduce ticks in places that
may seem surprising; for instance, the LHS of rules. See Note [Simplifying
rules] for details.
-}

postInlineUnconditionally
    :: SimplEnv -> BindContext
    -> InId -> OutId    -- The binder (*not* a CoVar), including its unfolding
    -> OutExpr
    -> Bool
-- Precondition: rhs satisfies the let-can-float invariant
-- See Note [Core let-can-float invariant] in GHC.Core
-- Reason: we don't want to inline single uses, or discard dead bindings,
--         for unlifted, side-effect-ful bindings
postInlineUnconditionally env bind_cxt old_bndr bndr rhs
  | not active                  = False
  | isWeakLoopBreaker occ_info  = False -- If it's a loop-breaker of any kind, don't inline
                                        -- because it might be referred to "earlier"
  | isStableUnfolding unfolding = False -- Note [Stable unfoldings and postInlineUnconditionally]
  | isTopLevel (bindContextLevel bind_cxt)
                                = False -- Note [Top level and postInlineUnconditionally]
  | exprIsTrivial rhs           = True
  | BC_Join {} <- bind_cxt      = False -- See point (1) of Note [Duplicating join points]
                                        --     in GHC.Core.Opt.Simplify.Iteration
  | otherwise
  = case occ_info of
      OneOcc { occ_in_lam = in_lam, occ_int_cxt = int_cxt, occ_n_br = n_br }
        -- See Note [Inline small things to avoid creating a thunk]

        | n_br >= 100 -> False  -- See #23627

        | n_br == 1, NotInsideLam <- in_lam  -- One syntactic occurrence
        -> True                              -- See Note [Post-inline for single-use things]

--        | is_unlifted                        -- Unlifted binding, hence ok-for-spec
--        -> True                              -- hence cheap to inline probably just a primop
--                                             -- Not a big deal either way
-- No, this is wrong.  {v = p +# q; x = K v}.
-- Don't inline v; it'll just get floated out again. Stupid.

        | is_demanded
        -> False                            -- No allocation (it'll be a case expression in the end)
                                            -- so inlining duplicates code but nothing more

        | otherwise
        -> work_ok in_lam int_cxt && smallEnoughToInline uf_opts unfolding
              -- Multiple syntactic occurences; but lazy, and small enough to dup
              -- ToDo: consider discount on smallEnoughToInline if int_cxt is true

      IAmDead -> True   -- This happens; for example, the case_bndr during case of
                        -- known constructor:  case (a,b) of x { (p,q) -> ... }
                        -- Here x isn't mentioned in the RHS, so we don't want to
                        -- create the (dead) let-binding  let x = (a,b) in ...

      _ -> False

  where
    work_ok NotInsideLam _              = True
    work_ok IsInsideLam  IsInteresting  = isCheapUnfolding unfolding
    work_ok IsInsideLam  NotInteresting = False
      -- NotInsideLam: outside a lambda, we want to be reasonably aggressive
      -- about inlining into multiple branches of case
      -- e.g. let x = <non-value>
      --      in case y of { C1 -> ..x..; C2 -> ..x..; C3 -> ... }
      -- Inlining can be a big win if C3 is the hot-spot, even if
      -- the uses in C1, C2 are not 'interesting'
      -- An example that gets worse if you add int_cxt here is 'clausify'

      -- InsideLam: check for acceptable work duplication, using isCheapUnfoldign
      -- int_cxt to prevent us inlining inside a lambda without some
      -- good reason.  See the notes on int_cxt in preInlineUnconditionally

--    is_unlifted = isUnliftedType (idType bndr)
    is_demanded = isStrUsedDmd (idDemandInfo bndr)
    occ_info    = idOccInfo old_bndr
    unfolding   = idUnfolding bndr
    uf_opts     = seUnfoldingOpts env
    phase       = sePhase env
    active      = isActive phase (idInlineActivation bndr)
        -- See Note [pre/postInlineUnconditionally in gentle mode]

{- Note [Inline small things to avoid creating a thunk]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The point of examining occ_info here is that for *non-values* that
occur outside a lambda, the call-site inliner won't have a chance
(because it doesn't know that the thing only occurs once).  The
pre-inliner won't have gotten it either, if the thing occurs in more
than one branch So the main target is things like

     let x = f y in
     case v of
        True  -> case x of ...
        False -> case x of ...

This is very important in practice; e.g. wheel-seive1 doubles
in allocation if you miss this out.  And bits of GHC itself start
to allocate more.  An egregious example is test perf/compiler/T14697,
where GHC.Driver.CmdLine.$wprocessArgs allocated hugely more.

Note [Post-inline for single-use things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have

   let x = rhs in ...x...

and `x` is used exactly once, and not inside a lambda, then we will usually
preInlineUnconditinally. But we can still get this situation in
postInlineUnconditionally:

  case K rhs of K x -> ...x....

Here we'll use `simplAuxBind` to bind `x` to (the already-simplified) `rhs`;
and `x` is used exactly once.  It's beneficial to inline right away; otherwise
we risk creating

   let x = rhs in ...x...

which will take another iteration of the Simplifier to eliminate.  We do this in
two places

1. In the full `postInlineUnconditionally` look for the special case
   of "one occurrence, not under a lambda", and inline unconditionally then.

   This is a bit risky: see Note [Avoiding simplifying repeatedly] in
   Simplify.Iteration.  But in practice it seems to be a small win.

2. `simplAuxBind` does a kind of poor-man's `postInlineUnconditionally`.  It
   does not need to account for many of the cases (e.g. top level) that the
   full `postInlineUnconditionally` does.  Moreover, we don't have an
   OutId, which `postInlineUnconditionally` needs.  I got a slight improvement
   in compiler performance when I added this test.

Here's an example that we don't currently handle well:
     let f = if b then Left (\x.BIG) else Right (\y.BIG)
     in \y. ....case f of {...} ....
Here f is used just once, and duplicating the case work is fine (exprIsCheap).
But
 - We can't preInlineUnconditionally because that would invalidate
   the occ info for b.
 - We can't postInlineUnconditionally because the RHS is big, and
   that risks exponential behaviour
 - We can't call-site inline, because the rhs is big
Alas!


Note [Top level and postInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't do postInlineUnconditionally for top-level things (even for
ones that are trivial):

  * Doing so will inline top-level error expressions that have been
    carefully floated out by FloatOut.  More generally, it might
    replace static allocation with dynamic.

  * Even for trivial expressions there's a problem.  Consider
      {-# RULE "foo" forall (xs::[T]). reverse xs = ruggle xs #-}
      blah xs = reverse xs
      ruggle = sort
    In one simplifier pass we might fire the rule, getting
      blah xs = ruggle xs
    but in *that* simplifier pass we must not do postInlineUnconditionally
    on 'ruggle' because then we'll have an unbound occurrence of 'ruggle'

    If the rhs is trivial it'll be inlined by callSiteInline, and then
    the binding will be dead and discarded by the next use of OccurAnal

  * There is less point, because the main goal is to get rid of local
    bindings used in multiple case branches.

  * The inliner should inline trivial things at call sites anyway.

  * The Id might be exported.  We could check for that separately,
    but since we aren't going to postInlineUnconditionally /any/
    top-level bindings, we don't need to test.

Note [Stable unfoldings and postInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do not do postInlineUnconditionally if the Id has a stable unfolding,
otherwise we lose the unfolding.  Example

     -- f has stable unfolding with rhs (e |> co)
     --   where 'e' is big
     f = e |> co

Then there's a danger we'll optimise to

     f' = e
     f = f' |> co

and now postInlineUnconditionally, losing the stable unfolding on f.  Now f'
won't inline because 'e' is too big.

    c.f. Note [Stable unfoldings and preInlineUnconditionally]


************************************************************************
*                                                                      *
        Rebuilding a lambda
*                                                                      *
************************************************************************
-}

rebuildLam :: SimplEnv
           -> [OutBndr] -> OutExpr
           -> SimplCont
           -> SimplM OutExpr
-- (rebuildLam env bndrs body cont)
-- returns expr which means the same as \bndrs. body
--
-- But it tries
--      a) eta reduction, if that gives a trivial expression
--      b) eta expansion [only if there are some value lambdas]
--
-- NB: the SimplEnv already includes the [OutBndr] in its in-scope set

rebuildLam _env [] body _cont
  = return body

rebuildLam env bndrs@(bndr:_) body cont
  = {-# SCC "rebuildLam" #-} try_eta bndrs body
  where
    rec_ids  = seRecIds env
    in_scope = getInScope env  -- Includes 'bndrs'
    mb_rhs   = contIsRhs cont

    -- See Note [Eta reduction based on evaluation context]
    eval_sd = contEvalContext cont
        -- NB: cont is never ApplyToVal, because beta-reduction would
        -- have happened.  So contEvalContext can panic on ApplyToVal.

    try_eta :: [OutBndr] -> OutExpr -> SimplM OutExpr
    try_eta bndrs body
      | -- Try eta reduction
        seDoEtaReduction env
      , Just etad_lam <- tryEtaReduce rec_ids bndrs body eval_sd
      = do { tick (EtaReduction bndr)
           ; return etad_lam }

      | -- Try eta expansion
        Nothing <- mb_rhs  -- See Note [Eta expanding lambdas]
      , seEtaExpand env
      , any isRuntimeVar bndrs  -- Only when there is at least one value lambda already
      , Just body_arity <- exprEtaExpandArity (seArityOpts env) body
      = do { tick (EtaExpansion bndr)
           ; let body' = etaExpandAT in_scope body_arity body
           ; traceSmpl "eta expand" (vcat [text "before" <+> ppr body
                                          , text "after" <+> ppr body'])
           -- NB: body' might have an outer Cast, but if so
           --     mk_lams will pull it further out, past 'bndrs' to the top
           ; return (mk_lams bndrs body') }

      | otherwise
      = return (mk_lams bndrs body)

    mk_lams :: [OutBndr] -> OutExpr -> OutExpr
    -- mk_lams pulls casts and ticks to the top
    mk_lams bndrs body@(Lam {})
      = mk_lams (bndrs ++ bndrs1) body1
      where
        (bndrs1, body1) = collectBinders body

    mk_lams bndrs (Tick t expr)
      | tickishFloatable t
      = mkTick t (mk_lams bndrs expr)

    mk_lams bndrs (Cast body co)
      | -- Note [Casts and lambdas]
        seCastSwizzle env
      , not (any bad bndrs)
      = mkCast (mk_lams bndrs body) (mkPiCos Representational bndrs co)
      where
        co_vars  = tyCoVarsOfCo co
        bad bndr = isCoVar bndr && bndr `elemVarSet` co_vars

    mk_lams bndrs body
      = mkLams bndrs body

{-
Note [Eta expanding lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we *do* want to eta-expand lambdas. Consider
   f (\x -> case x of (a,b) -> \s -> blah)
where 's' is a state token, and hence can be eta expanded.  This
showed up in the code for GHc.IO.Handle.Text.hPutChar, a rather
important function!

The eta-expansion will never happen unless we do it now.  (Well, it's
possible that CorePrep will do it, but CorePrep only has a half-baked
eta-expander that can't deal with casts.  So it's much better to do it
here.)

However, when the lambda is let-bound, as the RHS of a let, we have a
better eta-expander (in the form of tryEtaExpandRhs), so we don't
bother to try expansion in mkLam in that case; hence the contIsRhs
guard.

Note [Casts and lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        (\(x:tx). (\(y:ty). e) `cast` co)

We float the cast out, thus
        (\(x:tx) (y:ty). e) `cast` (tx -> co)

We do this for at least three reasons:

1. There is a danger here that the two lambdas look separated, and the
   full laziness pass might float an expression to between the two.

2. The occurrence analyser will mark x as InsideLam if the Lam nodes
   are separated (see the Lam case of occAnal).  By floating the cast
   out we put the two Lams together, so x can get a vanilla Once
   annotation.  If this lambda is the RHS of a let, which we inline,
   we can do preInlineUnconditionally on that x=arg binding.  With the
   InsideLam OccInfo, we can't do that, which results in an extra
   iteration of the Simplifier.

3. It may cancel with another cast.  E.g
      (\x. e |> co1) |> co2
   If we float out co1 it might cancel with co2.  Similarly
      let f = (\x. e |> co1) in ...
   If we float out co1, and then do cast worker/wrapper, we get
      let f1 = \x.e; f = f1 |> co1 in ...
   and now we can inline f, hoping that co1 may cancel at a call site.

TL;DR: put the lambdas together if at all possible.

In general, here's the transformation:
        \x. e `cast` co   ===>   (\x. e) `cast` (tx -> co)
        /\a. e `cast` co  ===>   (/\a. e) `cast` (/\a. co)
        /\g. e `cast` co  ===>   (/\g. e) `cast` (/\g. co)
                          (if not (g `in` co))

We call this "cast swizzling". It is controlled by sm_cast_swizzle.
See also Note [Cast swizzling on rule LHSs]

Wrinkles

* Notice that it works regardless of 'e'.  Originally it worked only
  if 'e' was itself a lambda, but in some cases that resulted in
  fruitless iteration in the simplifier.  A good example was when
  compiling Text.ParserCombinators.ReadPrec, where we had a definition
  like    (\x. Get `cast` g)
  where Get is a constructor with nonzero arity.  Then mkLam eta-expanded
  the Get, and the next iteration eta-reduced it, and then eta-expanded
  it again.

* Note also the side condition for the case of coercion binders, namely
  not (any bad bndrs).  It does not make sense to transform
          /\g. e `cast` g  ==>  (/\g.e) `cast` (/\g.g)
  because the latter is not well-kinded.


************************************************************************
*                                                                      *
              Eta expansion
*                                                                      *
************************************************************************
-}

tryEtaExpandRhs :: SimplEnv -> BindContext -> OutId -> OutExpr
                -> SimplM (ArityType, OutExpr)
-- See Note [Eta-expanding at let bindings]
tryEtaExpandRhs env bind_cxt bndr rhs
  | seEtaExpand env         -- If Eta-expansion is on
  , wantEtaExpansion rhs    -- and we'd like to eta-expand e
  , do_eta_expand           -- and e's manifest arity is lower than
                            --     what it could be
                            --     (never true for join points)
  =                         -- Do eta-expansion.
    assertPpr( not (isJoinBC bind_cxt) ) (ppr bndr) $
       -- assert: this never happens for join points; see GHC.Core.Opt.Arity
       --         Note [Do not eta-expand join points]
    do { tick (EtaExpansion bndr)
       ; return (arity_type, etaExpandAT in_scope arity_type rhs) }

  | otherwise
  = return (arity_type, rhs)

  where
    in_scope   = getInScope env
    arity_opts = seArityOpts env
    is_rec     = bindContextRec bind_cxt
    (do_eta_expand, arity_type) = findRhsArity arity_opts is_rec bndr rhs

wantEtaExpansion :: CoreExpr -> Bool
-- Mostly True; but False of PAPs which will immediately eta-reduce again
-- See Note [Which RHSs do we eta-expand?]
wantEtaExpansion (Cast e _)             = wantEtaExpansion e
wantEtaExpansion (Tick _ e)             = wantEtaExpansion e
wantEtaExpansion (Lam b e) | isTyVar b  = wantEtaExpansion e
wantEtaExpansion (App e _)              = wantEtaExpansion e
wantEtaExpansion (Var {})               = False
wantEtaExpansion (Lit {})               = False
wantEtaExpansion _                      = True

{-
Note [Eta-expanding at let bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We now eta expand at let-bindings, which is where the payoff comes.
The most significant thing is that we can do a simple arity analysis
(in GHC.Core.Opt.Arity.findRhsArity), which we can't do for free-floating lambdas

One useful consequence of not eta-expanding lambdas is this example:
   genMap :: C a => ...
   {-# INLINE genMap #-}
   genMap f xs = ...

   myMap :: D a => ...
   {-# INLINE myMap #-}
   myMap = genMap

Notice that 'genMap' should only inline if applied to two arguments.
In the stable unfolding for myMap we'll have the unfolding
    (\d -> genMap Int (..d..))
We do not want to eta-expand to
    (\d f xs -> genMap Int (..d..) f xs)
because then 'genMap' will inline, and it really shouldn't: at least
as far as the programmer is concerned, it's not applied to two
arguments!

Note [Which RHSs do we eta-expand?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't eta-expand:

* Trivial RHSs, e.g.     f = g
  If we eta expand do
    f = \x. g x
  we'll just eta-reduce again, and so on; so the
  simplifier never terminates.

* PAPs: see Note [Do not eta-expand PAPs]

What about things like this?
   f = case y of p -> \x -> blah

Here we do eta-expand.  This is a change (Jun 20), but if we have
really decided that f has arity 1, then putting that lambda at the top
seems like a Good idea.

Note [Do not eta-expand PAPs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to have old_arity = manifestArity rhs, which meant that we
would eta-expand even PAPs.  But this gives no particular advantage,
and can lead to a massive blow-up in code size, exhibited by #9020.
Suppose we have a PAP
    foo :: IO ()
    foo = returnIO ()
Then we can eta-expand to
    foo = (\eta. (returnIO () |> sym g) eta) |> g
where
    g :: IO () ~ State# RealWorld -> (# State# RealWorld, () #)

But there is really no point in doing this, and it generates masses of
coercions and whatnot that eventually disappear again. For T9020, GHC
allocated 6.6G before, and 0.8G afterwards; and residency dropped from
1.8G to 45M.

Moreover, if we eta expand
        f = g d  ==>  f = \x. g d x
that might in turn make g inline (if it has an inline pragma), which
we might not want.  After all, INLINE pragmas say "inline only when
saturated" so we don't want to be too gung-ho about saturating!

But note that this won't eta-expand, say
  f = \g -> map g
Does it matter not eta-expanding such functions?  I'm not sure.  Perhaps
strictness analysis will have less to bite on?


************************************************************************
*                                                                      *
\subsection{Floating lets out of big lambdas}
*                                                                      *
************************************************************************

Note [Floating and type abstraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
        x = /\a. C e1 e2
We'd like to float this to
        y1 = /\a. e1
        y2 = /\a. e2
        x  = /\a. C (y1 a) (y2 a)
for the usual reasons: we want to inline x rather vigorously.

You may think that this kind of thing is rare.  But in some programs it is
common.  For example, if you do closure conversion you might get:

        data a :-> b = forall e. (e -> a -> b) :$ e

        f_cc :: forall a. a :-> a
        f_cc = /\a. (\e. id a) :$ ()

Now we really want to inline that f_cc thing so that the
construction of the closure goes away.

So I have elaborated simplLazyBind to understand right-hand sides that look
like
        /\ a1..an. body

and treat them specially. The real work is done in
GHC.Core.Opt.Simplify.Utils.abstractFloats, but there is quite a bit of plumbing
in simplLazyBind as well.

The same transformation is good when there are lets in the body:

        /\abc -> let(rec) x = e in b
   ==>
        let(rec) x' = /\abc -> let x = x' a b c in e
        in
        /\abc -> let x = x' a b c in b

This is good because it can turn things like:

        let f = /\a -> letrec g = ... g ... in g
into
        letrec g' = /\a -> ... g' a ...
        in
        let f = /\ a -> g' a

which is better.  In effect, it means that big lambdas don't impede
let-floating.

This optimisation is CRUCIAL in eliminating the junk introduced by
desugaring mutually recursive definitions.  Don't eliminate it lightly!

[May 1999]  If we do this transformation *regardless* then we can
end up with some pretty silly stuff.  For example,

        let
            st = /\ s -> let { x1=r1 ; x2=r2 } in ...
        in ..
becomes
        let y1 = /\s -> r1
            y2 = /\s -> r2
            st = /\s -> ...[y1 s/x1, y2 s/x2]
        in ..

Unless the "..." is a WHNF there is really no point in doing this.
Indeed it can make things worse.  Suppose x1 is used strictly,
and is of the form

        x1* = case f y of { (a,b) -> e }

If we abstract this wrt the tyvar we then can't do the case inline
as we would normally do.

That's why the whole transformation is part of the same process that
floats let-bindings and constructor arguments out of RHSs.  In particular,
it is guarded by the doFloatFromRhs call in simplLazyBind.

Note [Which type variables to abstract over]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Abstract only over the type variables free in the rhs wrt which the
new binding is abstracted.  Several points worth noting

(AB1) The naive approach of abstracting wrt the
      tyvars free in the Id's /type/ fails. Consider:
          /\ a b -> let t :: (a,b) = (e1, e2)
                        x :: a     = fst t
                    in ...
      Here, b isn't free in x's type, but we must nevertheless
      abstract wrt b as well, because t's type mentions b.
      Since t is floated too, we'd end up with the bogus:
           poly_t = /\ a b -> (e1, e2)
           poly_x = /\ a   -> fst (poly_t a *b*)

(AB2) We must do closeOverKinds.  Example (#10934):
       f = /\k (f:k->*) (a:k). let t = AccFailure @ (f a) in ...
      Here we want to float 't', but we must remember to abstract over
      'k' as well, even though it is not explicitly mentioned in the RHS,
      otherwise we get
         t = /\ (f:k->*) (a:k). AccFailure @ (f a)
      which is obviously bogus.

(AB3) We get the variables to abstract over by filtering down the
      the main_tvs for the original function, picking only ones
      mentioned in the abstracted body. This means:
      - they are automatically in dependency order, because main_tvs is
      - there is no issue about non-determinism
      - we don't gratuitously change order, which may help (in a tiny
        way) with CSE and/or the compiler-debugging experience

(AB4) For a recursive group, it's a bit of a pain to work out the minimal
      set of tyvars over which to abstract:
           /\ a b c.  let x = ...a... in
                      letrec { p = ...x...q...
                               q = .....p...b... } in
                      ...
      Since 'x' is abstracted over 'a', the {p,q} group must be abstracted
      over 'a' (because x is replaced by (poly_x a)) as well as 'b'.
      Remember this bizarre case too:
           x::a = x
      Here, we must abstract 'x' over 'a'.

      Why is it worth doing this?  Partly tidiness; and partly #22459
      which showed that it's harder to do polymorphic specialisation well
      if there are dictionaries abstracted over unnecessary type variables.
      See Note [Weird special case for SpecDict] in GHC.Core.Opt.Specialise

(AB5) We do dependency analysis on recursive groups prior to determining
      which variables to abstract over.
      This is useful, because ANFisation in prepareBinding may float out
      values out of a complex recursive binding, e.g.,
          letrec { xs = g @a "blah"# ((:) 1 []) xs } in ...
        ==> { prepareBinding }
          letrec { foo = "blah"#
                   bar = [42]
                   xs = g @a foo bar xs } in
          ...
      and we don't want to abstract foo and bar over @a.

      (Why is it OK to float the unlifted `foo` there?
      See Note [Core top-level string literals] in GHC.Core;
      it is controlled by GHC.Core.Opt.Simplify.Env.unitLetFloat.)

      It is also necessary to do dependency analysis, because
      otherwise (in #24551) we might get `foo = \@_ -> "missing"#` at the
      top-level, and that triggers a CoreLint error because `foo` is *not*
      manifestly a literal string.
-}

abstractFloats :: UnfoldingOpts -> TopLevelFlag -> [OutTyVar] -> SimplFloats
              -> OutExpr -> SimplM ([OutBind], OutExpr)
abstractFloats uf_opts top_lvl main_tvs floats body
  = assert (notNull body_floats) $
    assert (isNilOL (sfJoinFloats floats)) $
    do  { let sccs = concatMap to_sccs body_floats
        ; (subst, float_binds) <- mapAccumLM abstract empty_subst sccs
        ; return (float_binds, GHC.Core.Subst.substExpr subst body) }
  where
    is_top_lvl  = isTopLevel top_lvl
    body_floats = letFloatBinds (sfLetFloats floats)
    empty_subst = GHC.Core.Subst.mkEmptySubst (sfInScope floats)

    -- See wrinkle (AB5) in Note [Which type variables to abstract over]
    -- for why we need to re-do dependency analysis
    to_sccs :: OutBind -> [SCC (Id, CoreExpr, VarSet)]
    to_sccs (NonRec id e) = [AcyclicSCC (id, e, emptyVarSet)] -- emptyVarSet: abstract doesn't need it
    to_sccs (Rec prs)     = sccs
      where
        (ids,rhss) = unzip prs
        sccs = depAnal (\(id,_rhs,_fvs) -> [getName id])
                       (\(_id,_rhs,fvs) -> nonDetStrictFoldVarSet ((:) . getName) [] fvs) -- Wrinkle (AB3)
                       (zip3 ids rhss (map exprFreeVars rhss))

    abstract :: GHC.Core.Subst.Subst -> SCC (Id, CoreExpr, VarSet) -> SimplM (GHC.Core.Subst.Subst, OutBind)
    abstract subst (AcyclicSCC (id, rhs, _empty_var_set))
      = do { (poly_id1, poly_app) <- mk_poly1 tvs_here id
           ; let (poly_id2, poly_rhs) = mk_poly2 poly_id1 tvs_here rhs'
                 !subst' = GHC.Core.Subst.extendIdSubst subst id poly_app
           ; return (subst', NonRec poly_id2 poly_rhs) }
      where
        rhs' = GHC.Core.Subst.substExpr subst rhs

        -- tvs_here: see Note [Which type variables to abstract over]
        tvs_here = choose_tvs (exprSomeFreeVars isTyVar rhs')

    abstract subst (CyclicSCC trpls)
      = do { (poly_ids, poly_apps) <- mapAndUnzipM (mk_poly1 tvs_here) ids
           ; let subst' = GHC.Core.Subst.extendSubstList subst (ids `zip` poly_apps)
                 poly_pairs = [ mk_poly2 poly_id tvs_here rhs'
                              | (poly_id, rhs) <- poly_ids `zip` rhss
                              , let rhs' = GHC.Core.Subst.substExpr subst' rhs ]
           ; return (subst', Rec poly_pairs) }
      where
        (ids,rhss,_fvss) = unzip3 trpls

        -- tvs_here: see Note [Which type variables to abstract over]
        tvs_here = choose_tvs (mapUnionVarSet get_bind_fvs trpls)

        -- See wrinkle (AB4) in Note [Which type variables to abstract over]
        get_bind_fvs (id,_rhs,rhs_fvs) = tyCoVarsOfType (idType id) `unionVarSet` get_rec_rhs_tvs rhs_fvs
        get_rec_rhs_tvs rhs_fvs        = nonDetStrictFoldVarSet get_tvs emptyVarSet rhs_fvs
                                  -- nonDet is safe because of wrinkle (AB3)

        get_tvs :: Var -> VarSet -> VarSet
        get_tvs var free_tvs
           | isTyVar var      -- CoVars have been substituted away
           = extendVarSet free_tvs var
           | isCoVar var  -- CoVars can be free in the RHS, but they are never let-bound;
           = free_tvs     -- Do not call lookupIdSubst_maybe, though (#23426)
                          --    because it has a non-CoVar precondition
           | Just poly_app <- GHC.Core.Subst.lookupIdSubst_maybe subst var
           = -- 'var' is like 'x' in (AB4)
             exprSomeFreeVars isTyVar poly_app `unionVarSet` free_tvs
           | otherwise
           = free_tvs

    choose_tvs free_tvs
       = filter (`elemVarSet` all_free_tvs) main_tvs  -- (AB3)
       where
         all_free_tvs = closeOverKinds free_tvs       -- (AB2)

    mk_poly1 :: [TyVar] -> Id -> SimplM (Id, CoreExpr)
    mk_poly1 tvs_here var
      = do { uniq <- getUniqueM
           ; let  poly_name = setNameUnique (idName var) uniq      -- Keep same name
                  poly_ty   = mkInfForAllTys tvs_here (idType var) -- But new type of course
                  poly_id   = transferPolyIdInfo var tvs_here $ -- Note [transferPolyIdInfo] in GHC.Types.Id
                              mkLocalId poly_name (idMult var) poly_ty
           ; return (poly_id, mkTyApps (Var poly_id) (mkTyVarTys tvs_here)) }
                -- In the olden days, it was crucial to copy the occInfo of the original var,
                -- because we were looking at occurrence-analysed but as yet unsimplified code!
                -- In particular, we mustn't lose the loop breakers.  BUT NOW we are looking
                -- at already simplified code, so it doesn't matter
                --
                -- It's even right to retain single-occurrence or dead-var info:
                -- Suppose we started with  /\a -> let x = E in B
                -- where x occurs once in B. Then we transform to:
                --      let x' = /\a -> E in /\a -> let x* = x' a in B
                -- where x* has an INLINE prag on it.  Now, once x* is inlined,
                -- the occurrences of x' will be just the occurrences originally
                -- pinned on x.

    mk_poly2 :: Id -> [TyVar] -> CoreExpr -> (Id, CoreExpr)
    mk_poly2 poly_id tvs_here rhs
      = (poly_id `setIdUnfolding` unf, poly_rhs)
      where
        poly_rhs = mkLams tvs_here rhs
        unf = mkUnfolding uf_opts VanillaSrc is_top_lvl False False poly_rhs Nothing

        -- We want the unfolding.  Consider
        --      let
        --            x = /\a. let y = ... in Just y
        --      in body
        -- Then we float the y-binding out (via abstractFloats and addPolyBind)
        -- but 'x' may well then be inlined in 'body' in which case we'd like the
        -- opportunity to inline 'y' too.

{-
Note [Abstract over coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a coercion variable (g :: a ~ Int) is free in the RHS, then so is the
type variable a.  Rather than sort this mess out, we simply bale out and abstract
wrt all the type variables if any of them are coercion variables.


Historical note: if you use let-bindings instead of a substitution, beware of this:

                -- Suppose we start with:
                --
                --      x = /\ a -> let g = G in E
                --
                -- Then we'll float to get
                --
                --      x = let poly_g = /\ a -> G
                --          in /\ a -> let g = poly_g a in E
                --
                -- But now the occurrence analyser will see just one occurrence
                -- of poly_g, not inside a lambda, so the simplifier will
                -- PreInlineUnconditionally poly_g back into g!  Badk to square 1!
                -- (I used to think that the "don't inline lone occurrences" stuff
                --  would stop this happening, but since it's the *only* occurrence,
                --  PreInlineUnconditionally kicks in first!)
                --
                -- Solution: put an INLINE note on g's RHS, so that poly_g seems
                --           to appear many times.  (NB: mkInlineMe eliminates
                --           such notes on trivial RHSs, so do it manually.)

************************************************************************
*                                                                      *
                prepareAlts
*                                                                      *
************************************************************************

prepareAlts tries these things:

1.  filterAlts: eliminate alternatives that cannot match, including
    the DEFAULT alternative.  Here "cannot match" includes knowledge
    from GADTs

2.  refineDefaultAlt: if the DEFAULT alternative can match only one
    possible constructor, then make that constructor explicit.
    e.g.
        case e of x { DEFAULT -> rhs }
     ===>
        case e of x { (a,b) -> rhs }
    where the type is a single constructor type.  This gives better code
    when rhs also scrutinises x or e.
    See GHC.Core.Utils Note [Refine DEFAULT case alternatives]

3. combineIdenticalAlts: combine identical alternatives into a DEFAULT.
   See CoreUtils Note [Combine identical alternatives], which also
   says why we do this on InAlts not on OutAlts

4. Returns a list of the constructors that cannot holds in the
   DEFAULT alternative (if there is one)

It's a good idea to do this stuff before simplifying the alternatives, to
avoid simplifying alternatives we know can't happen, and to come up with
the list of constructors that are handled, to put into the IdInfo of the
case binder, for use when simplifying the alternatives.

Eliminating the default alternative in (1) isn't so obvious, but it can
happen:

data Colour = Red | Green | Blue

f x = case x of
        Red -> ..
        Green -> ..
        DEFAULT -> h x

h y = case y of
        Blue -> ..
        DEFAULT -> [ case y of ... ]

If we inline h into f, the default case of the inlined h can't happen.
If we don't notice this, we may end up filtering out *all* the cases
of the inner case y, which give us nowhere to go!

Note [Shadowing in prepareAlts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that we pass case_bndr::InId to prepareAlts; an /InId/, not an
/OutId/.  This is vital, because `refineDefaultAlt` uses `tys` to build
a new /InAlt/.  If you pass an OutId, we'll end up applying the
substitution twice: disaster (#23012).

However this does mean that filling in the default alt might be
delayed by a simplifier cycle, because an InId has less info than an
OutId.  Test simplCore/should_compile/simpl013 apparently shows this
up, although I'm not sure exactly how..
-}

prepareAlts :: OutExpr -> InId -> [InAlt] -> SimplM ([AltCon], [InAlt])
-- The returned alternatives can be empty, none are possible
--
-- Note that case_bndr is an InId; see Note [Shadowing in prepareAlts]
prepareAlts scrut case_bndr alts
  | Just (tc, tys) <- splitTyConApp_maybe (idType case_bndr)
  = do { us <- getUniquesM
       ; let (idcs1, alts1) = filterAlts tc tys imposs_cons alts
             (yes2,  alts2) = refineDefaultAlt us (idMult case_bndr) tc tys idcs1 alts1
               -- The multiplicity on case_bndr's is the multiplicity of the
               -- case expression The newly introduced patterns in
               -- refineDefaultAlt must be scaled by this multiplicity
             (yes3, idcs3, alts3) = combineIdenticalAlts idcs1 alts2
             -- "idcs" stands for "impossible default data constructors"
             -- i.e. the constructors that can't match the default case
       ; when yes2 $ tick (FillInCaseDefault case_bndr)
       ; when yes3 $ tick (AltMerge case_bndr)
       ; return (idcs3, alts3) }

  | otherwise  -- Not a data type, so nothing interesting happens
  = return ([], alts)
  where
    imposs_cons = case scrut of
                    Var v -> otherCons (idUnfolding v)
                    _     -> []

{- Note [Merging nested cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic case-merge stuff is described in Note [Merge Nested Cases] in GHC.Core.Utils

We do it here in `prepareAlts` (on InAlts) rather than after (on OutAlts) for two reasons:

* It "belongs" here with `filterAlts`, `refineDefaultAlt` and `combineIdenticalAlts`.

* In test perf/compiler/T22428 I found that I was getting extra Simplifer iterations:
    1. Create a join point
    2. That join point gets inlined at all call sites, so it is now dead.
    3. Case-merge happened, but left behind some trivial bindings (see `mergeCaseAlts`)
    4. Get rid of the trivial bindings
  The first two seem reasonable.  It's imaginable that we could do better on
  (3), by making case-merge join-point-aware, but it's not trivial.  But the
  fourth is just stupid.  Rather than always do an extra iteration, it's better
  to do the transformation on the input-end of teh Simplifier.
-}

{-
************************************************************************
*                                                                      *
                mkCase
*                                                                      *
************************************************************************

mkCase tries these things

* Note [Eliminate Identity Case]
* Note [Scrutinee Constant Folding]

Note [Eliminate Identity Case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        case e of               ===> e
                True  -> True;
                False -> False

and similar friends.

Note [Scrutinee Constant Folding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     case x op# k# of _ {  ===> case x of _ {
        a1# -> e1                  (a1# inv_op# k#) -> e1
        a2# -> e2                  (a2# inv_op# k#) -> e2
        ...                        ...
        DEFAULT -> ed              DEFAULT -> ed

     where (x op# k#) inv_op# k# == x

And similarly for commuted arguments and for some unary operations.

The purpose of this transformation is not only to avoid an arithmetic
operation at runtime but to allow other transformations to apply in cascade.

Example with the "Merge Nested Cases" optimization (from #12877):

      main = case t of t0
         0##     -> ...
         DEFAULT -> case t0 `minusWord#` 1## of t1
            0##     -> ...
            DEFAULT -> case t1 `minusWord#` 1## of t2
               0##     -> ...
               DEFAULT -> case t2 `minusWord#` 1## of _
                  0##     -> ...
                  DEFAULT -> ...

  becomes:

      main = case t of _
      0##     -> ...
      1##     -> ...
      2##     -> ...
      3##     -> ...
      DEFAULT -> ...

There are some wrinkles.

Wrinkle 1:
  Do not apply caseRules if there is just a single DEFAULT alternative,
  unless the case-binder is dead. Example:
     case e +# 3# of b { DEFAULT -> rhs }
  If we applied the transformation here we would (stupidly) get
     case e of b' { DEFAULT -> let b = b' +# 3# in rhs }
  and now the process may repeat, because that let will really
  be a case. But if the original case binder b is dead, we instead get
     case e of b' { DEFAULT -> rhs }
  and there is no such problem.

  See Note [Example of case-merging and caseRules] for a compelling
  example of why this dead-binder business can be really important.


Wrinkle 2:
  The type of the scrutinee might change.  E.g.
        case tagToEnum (x :: Int#) of (b::Bool)
          False -> e1
          True -> e2
  ==>
        case x of (b'::Int#)
          DEFAULT -> e1
          1#      -> e2

Wrinkle 3:
  The case binder may be used in the right hand sides, so we need
  to make a local binding for it, if it is alive.  e.g.
         case e +# 10# of b
           DEFAULT -> blah...b...
           44#     -> blah2...b...
  ===>
         case e of b'
           DEFAULT -> let b = b' +# 10# in blah...b...
           34#     -> let b = 44# in blah2...b...

  Note that in the non-DEFAULT cases we know what to bind 'b' to,
  whereas in the DEFAULT case we must reconstruct the original value.
  But NB: we use b'; we do not duplicate 'e'.

Wrinkle 4:
  In dataToTag we might need to make up some fake binders;
  see Note [caseRules for dataToTag] in GHC.Core.Opt.ConstantFold


Note [Example of case-merging and caseRules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The case-transformation rules are quite powerful. Here's a
subtle example from #22375.  We start with

  data T = A | B | ...
    deriving Eq

  f :: T -> String
  f x = if | x==A -> "one"
           | x==B -> "two"
           | ...

In Core after a bit of simplification we get:

    f x = case dataToTagLarge# x of a# { _DEFAULT ->
          case a# of
            _DEFAULT -> case dataToTagLarge# x of b# { _DEFAULT ->
                        case b# of
                           _DEFAULT -> ...
                           1# -> "two"
                        }
            0# -> "one"
          }

Now consider what mkCase does to these case expressions.
The case-merge transformation Note [Merge Nested Cases]
does this (affecting both pairs of cases):

    f x = case dataToTagLarge# x of a# {
             _DEFAULT -> case dataToTagLarge# x of b# {
                          _DEFAULT -> ...
                          1# -> "two"
                         }
             0# -> "one"
          }

Now Note [caseRules for dataToTag] does its work, again
on both dataToTagLarge# cases:

    f x = case x of x1 {
             _DEFAULT -> case dataToTagLarge# x1 of a# { _DEFAULT ->
                         case x of x2 {
                           _DEFAULT -> case dataToTagLarge# x2 of b# { _DEFAULT -> ... }
                           B -> "two"
                         }}
             A -> "one"
          }


The new dataToTagLarge# calls come from the "reconstruct scrutinee" part of
caseRules (note that a# and b# were not dead in the original program
before all this merging).  However, since a# and b# /are/ in fact dead
in the resulting program, we are left with redundant dataToTagLarge# calls.
But they are easily eliminated by doing caseRules again, in
the next Simplifier iteration, this time noticing that a# and b# are
dead.  Hence the "dead-binder" sub-case of Wrinkle 1 of Note
[Scrutinee Constant Folding] above.  Once we do this we get

    f x = case x of x1 {
             _DEFAULT -> case x1 of x2 { _DEFAULT ->
                         case x1 of x2 {
                            _DEFAULT -> case x2 of x3 { _DEFAULT -> ... }
                            B -> "two"
                         }}
             A -> "one"
          }

and now we can do case-merge again, getting the desired

    f x = case x of
            A -> "one"
            B -> "two"
            ...

-}

mkCase, mkCase1, mkCase2, mkCase3
   :: SimplMode
   -> OutExpr -> OutId
   -> OutType -> [OutAlt]               -- Alternatives in standard (increasing) order
   -> SimplM OutExpr

--------------------------------------------------
--      1. Merge Nested Cases
--         See Note [Merge Nested Cases]
--             Note [Example of case-merging and caseRules]
--             Note [Cascading case merge]
--------------------------------------------------

mkCase mode scrut outer_bndr alts_ty alts
  | sm_case_merge mode
  , Just (joins, alts') <- mergeCaseAlts outer_bndr alts
  = do  { tick (CaseMerge outer_bndr)
        ; case_expr <- mkCase1 mode scrut outer_bndr alts_ty alts'
        ; return (mkLets joins case_expr) }
        -- mkCase1: don't call mkCase recursively!
        -- Firstly, there's no point, because inner alts have already had
        -- mkCase applied to them, so they won't have a case in their default
        -- Secondly, if you do, you get an infinite loop, because the bindCaseBndr
        -- in munge_rhs may put a case into the DEFAULT branch!
  | otherwise
  = mkCase1 mode scrut outer_bndr alts_ty alts

--------------------------------------------------
--      2. Eliminate Identity Case
--         See Note [Eliminate Identity Case]
--------------------------------------------------

mkCase1 _mode scrut case_bndr _ alts@(Alt _ _ rhs1 : alts')      -- Identity case
  | all identity_alt alts
  = do { tick (CaseIdentity case_bndr)
       ; return (mkTicks ticks $ re_cast scrut rhs1) }
  where
    ticks = concatMap (\(Alt _ _ rhs) -> stripTicksT tickishFloatable rhs) alts'
    identity_alt (Alt con args rhs) = check_eq rhs con args

    check_eq (Cast rhs co) con args        -- See Note [RHS casts]
      = not (any (`elemVarSet` tyCoVarsOfCo co) args) && check_eq rhs con args
    check_eq (Tick t e) alt args
      = tickishFloatable t && check_eq e alt args

    check_eq (Lit lit) (LitAlt lit') _     = lit == lit'
    check_eq (Var v) _ _  | v == case_bndr = True
    check_eq (Var v)   (DataAlt con) args
      | null arg_tys, null args            = v == dataConWorkId con
                                             -- Optimisation only
    check_eq rhs        (DataAlt con) args = cheapEqExpr' tickishFloatable rhs $
                                             mkConApp2 con arg_tys args
    check_eq _          _             _    = False

    arg_tys = tyConAppArgs (idType case_bndr)

        -- Note [RHS casts]
        -- ~~~~~~~~~~~~~~~~
        -- We've seen this:
        --      case e of x { _ -> x `cast` c }
        -- And we definitely want to eliminate this case, to give
        --      e `cast` c
        -- So we throw away the cast from the RHS, and reconstruct
        -- it at the other end.  All the RHS casts must be the same
        -- if (all identity_alt alts) holds.
        --
        -- Don't worry about nested casts, because the simplifier combines them

    re_cast scrut (Cast rhs co) = Cast (re_cast scrut rhs) co
    re_cast scrut _             = scrut

mkCase1 mode scrut bndr alts_ty alts = mkCase2 mode scrut bndr alts_ty alts


--------------------------------------------------
--      2. Scrutinee Constant Folding
--         See Note [Scrutinee Constant Folding]
--------------------------------------------------

mkCase2 mode scrut bndr alts_ty alts
  | -- See Note [Scrutinee Constant Folding]
    case alts of
      [Alt DEFAULT _ _] -> isDeadBinder bndr -- see wrinkle 1
      _                 -> True
  , sm_case_folding mode
  , Just (scrut', tx_con, mk_orig) <- caseRules (smPlatform mode) scrut
  = do { bndr' <- newId (fsLit "lwild") ManyTy (exprType scrut')

       ; alts' <- mapMaybeM (tx_alt tx_con mk_orig bndr') alts
                  -- mapMaybeM: discard unreachable alternatives
                  -- See Note [Unreachable caseRules alternatives]
                  -- in GHC.Core.Opt.ConstantFold

       ; mkCase3 mode scrut' bndr' alts_ty $
         add_default (re_sort alts')
       }

  | otherwise
  = mkCase3 mode scrut bndr alts_ty alts
  where
    -- We need to keep the correct association between the scrutinee and its
    -- binder if the latter isn't dead. Hence we wrap rhs of alternatives with
    -- "let bndr = ... in":
    --
    --     case v + 10 of y        =====> case v of y'
    --        20      -> e1                 10      -> let y = 20      in e1
    --        DEFAULT -> e2                 DEFAULT -> let y = y' + 10 in e2
    --
    -- This wrapping is done in tx_alt; we use mk_orig, returned by caseRules,
    -- to construct an expression equivalent to the original one, for use
    -- in the DEFAULT case

    tx_alt :: (AltCon -> Maybe AltCon) -> (Id -> CoreExpr) -> Id
           -> CoreAlt -> SimplM (Maybe CoreAlt)
    tx_alt tx_con mk_orig new_bndr (Alt con bs rhs)
      = case tx_con con of
          Nothing   -> return Nothing
          Just con' -> do { bs' <- mk_new_bndrs new_bndr con'
                          ; return (Just (Alt con' bs' rhs')) }
      where
        rhs' | isDeadBinder bndr = rhs
             | otherwise         = bindNonRec bndr orig_val rhs

        orig_val = case con of
                      DEFAULT    -> mk_orig new_bndr
                      LitAlt l   -> Lit l
                      DataAlt dc -> mkConApp2 dc (tyConAppArgs (idType bndr)) bs

    mk_new_bndrs new_bndr (DataAlt dc)
      | not (isNullaryRepDataCon dc)
      = -- For non-nullary data cons we must invent some fake binders
        -- See Note [caseRules for dataToTag] in GHC.Core.Opt.ConstantFold
        do { us <- getUniquesM
           ; let (ex_tvs, arg_ids) = dataConRepInstPat us (idMult new_bndr) dc
                                        (tyConAppArgs (idType new_bndr))
           ; return (ex_tvs ++ arg_ids) }
    mk_new_bndrs _ _ = return []

    re_sort :: [CoreAlt] -> [CoreAlt]
    -- Sort the alternatives to re-establish
    -- GHC.Core Note [Case expression invariants]
    re_sort alts = sortBy cmpAlt alts

    add_default :: [CoreAlt] -> [CoreAlt]
    -- See Note [Literal cases]
    add_default (Alt (LitAlt {}) bs rhs : alts) = Alt DEFAULT bs rhs : alts
    add_default alts                            = alts

{- Note [Literal cases]
~~~~~~~~~~~~~~~~~~~~~~~
If we have
  case tagToEnum (a ># b) of
     False -> e1
     True  -> e2

then caseRules for TagToEnum will turn it into
  case tagToEnum (a ># b) of
     0# -> e1
     1# -> e2

Since the case is exhaustive (all cases are) we can convert it to
  case tagToEnum (a ># b) of
     DEFAULT -> e1
     1#      -> e2

This may generate slightly better code (although it should not, since
all cases are exhaustive) and/or optimise better.  I'm not certain that
it's necessary, but currently we do make this change.  We do it here,
NOT in the TagToEnum rules (see "Beware" in Note [caseRules for tagToEnum]
in GHC.Core.Opt.ConstantFold)
-}

--------------------------------------------------
--      Catch-all
--------------------------------------------------
mkCase3 _mode scrut bndr alts_ty alts
  = return (Case scrut bndr alts_ty alts)

-- See Note [Exitification] and Note [Do not inline exit join points] in
-- GHC.Core.Opt.Exitify
-- This lives here (and not in Id) because occurrence info is only valid on
-- InIds, so it's crucial that isExitJoinId is only called on freshly
-- occ-analysed code. It's not a generic function you can call anywhere.
isExitJoinId :: Var -> Bool
isExitJoinId id
  = isJoinId id
  && case idOccInfo id of
        OneOcc { occ_in_lam = IsInsideLam } -> True
        _                                   -> False

{-
Note [Dead binders]
~~~~~~~~~~~~~~~~~~~~
Note that dead-ness is maintained by the simplifier, so that it is
accurate after simplification as well as before.

-}
