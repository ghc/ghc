{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[SimplUtils]{The simplifier utilities}
-}

{-# LANGUAGE CPP #-}

module SimplUtils (
        -- Rebuilding
        mkLam, mkCase, prepareAlts, tryEtaExpandRhs,

        -- Inlining,
        preInlineUnconditionally, postInlineUnconditionally,
        activeUnfolding, activeRule,
        getUnfoldingInRuleMatch,
        simplEnvForGHCi, updModeForStableUnfoldings, updModeForRules,

        -- The continuation type
        SimplCont(..), DupFlag(..),
        isSimplified, contIsStop,
        contIsDupable, contResultType, contHoleType,
        contIsTrivial, contArgs,
        countArgs,
        mkBoringStop, mkRhsStop, mkLazyArgStop, contIsRhsOrArg,
        interestingCallContext,

        -- ArgInfo
        ArgInfo(..), ArgSpec(..), mkArgInfo,
        addValArgTo, addCastTo, addTyArgTo,
        argInfoExpr, argInfoAppArgs, pushSimplifiedArgs,

        abstractFloats
    ) where

#include "HsVersions.h"

import SimplEnv
import CoreMonad        ( SimplMode(..), Tick(..) )
import DynFlags
import CoreSyn
import qualified CoreSubst
import PprCore
import CoreFVs
import CoreUtils
import CoreArity
import CoreUnfold
import Name
import Id
import IdInfo
import Var
import Demand
import SimplMonad
import Type     hiding( substTy )
import Coercion hiding( substCo )
import DataCon          ( dataConWorkId, isNullaryRepDataCon )
import VarSet
import BasicTypes
import Util
import OrdList          ( isNilOL )
import MonadUtils
import Outputable
import Pair
import PrelRules
import FastString       ( fsLit )

import Control.Monad    ( when )
import Data.List        ( sortBy )

{-
************************************************************************
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
  = Stop                -- Stop[e] = e
        OutType         -- Type of the <hole>
        CallCtxt        -- Tells if there is something interesting about
                        --          the context, and hence the inliner
                        --          should be a bit keener (see interestingCallContext)
                        -- Specifically:
                        --     This is an argument of a function that has RULES
                        --     Inlining the call might allow the rule to fire
                        -- Never ValAppCxt (use ApplyToVal instead)
                        -- or CaseCtxt (use Select instead)

  | CastIt              -- (CastIt co K)[e] = K[ e `cast` co ]
        OutCoercion             -- The coercion simplified
                                -- Invariant: never an identity coercion
        SimplCont

  | ApplyToVal         -- (ApplyToVal arg K)[e] = K[ e arg ]
      { sc_dup  :: DupFlag      -- See Note [DupFlag invariants]
      , sc_arg  :: InExpr       -- The argument,
      , sc_env  :: StaticEnv    --     and its static env
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
      , sc_env  :: StaticEnv      --   and their static environment
      , sc_cont :: SimplCont }

  -- The two strict forms have no DupFlag, because we never duplicate them
  | StrictBind          -- (StrictBind x xs b K)[e] = let x = e in K[\xs.b]
                        --       or, equivalently,  = K[ (\x xs.b) e ]
      { sc_dup   :: DupFlag        -- See Note [DupFlag invariants]
      , sc_bndr  :: InId
      , sc_bndrs :: [InBndr]
      , sc_body  :: InExpr
      , sc_env   :: StaticEnv
      , sc_cont  :: SimplCont }

  | StrictArg           -- (StrictArg (f e1 ..en) K)[e] = K[ f e1 .. en e ]
      { sc_dup  :: DupFlag     -- Always Simplified or OkToDup
      , sc_fun  :: ArgInfo     -- Specifies f, e1..en, Whether f has rules, etc
                               --     plus strictness flags for *further* args
      , sc_cci  :: CallCtxt    -- Whether *this* argument position is interesting
      , sc_cont :: SimplCont }

  | TickIt              -- (TickIt t K)[e] = K[ tick t e ]
        (Tickish Id)    -- Tick tickish <hole>
        SimplCont

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

{-
Note [DupFlag invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~
In both (ApplyToVal dup _ env k)
   and  (Select dup _ _ env k)
the following invariants hold

  (a) if dup = OkToDup, then continuation k is also ok-to-dup
  (b) if dup = OkToDup or Simplified, the subst-env is empty
      (and and hence no need to re-simplify)
-}

instance Outputable DupFlag where
  ppr OkToDup    = text "ok"
  ppr NoDup      = text "nodup"
  ppr Simplified = text "simpl"

instance Outputable SimplCont where
  ppr (Stop ty interesting) = text "Stop" <> brackets (ppr interesting) <+> ppr ty
  ppr (CastIt co cont  )    = (text "CastIt" <+> pprOptCo co) $$ ppr cont
  ppr (TickIt t cont)       = (text "TickIt" <+> ppr t) $$ ppr cont
  ppr (ApplyToTy  { sc_arg_ty = ty, sc_cont = cont })
    = (text "ApplyToTy" <+> pprParendType ty) $$ ppr cont
  ppr (ApplyToVal { sc_arg = arg, sc_dup = dup, sc_cont = cont })
    = (text "ApplyToVal" <+> ppr dup <+> pprParendExpr arg)
                                        $$ ppr cont
  ppr (StrictBind { sc_bndr = b, sc_cont = cont })
    = (text "StrictBind" <+> ppr b) $$ ppr cont
  ppr (StrictArg { sc_fun = ai, sc_cont = cont })
    = (text "StrictArg" <+> ppr (ai_fun ai)) $$ ppr cont
  ppr (Select { sc_dup = dup, sc_bndr = bndr, sc_alts = alts, sc_env = se, sc_cont = cont })
    = (text "Select" <+> ppr dup <+> ppr bndr) $$
       ifPprDebug (nest 2 $ vcat [ppr (seTvSubst se), ppr alts]) $$ ppr cont


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

        ai_type  :: OutType,    -- Type of (f a1 ... an)

        ai_rules :: FunRules,   -- Rules for this function

        ai_encl :: Bool,        -- Flag saying whether this function
                                -- or an enclosing one has rules (recursively)
                                --      True => be keener to inline in all args

        ai_strs :: [Bool],      -- Strictness of remaining arguments
                                --   Usually infinite, but if it is finite it guarantees
                                --   that the function diverges after being given
                                --   that number of args
        ai_discs :: [Int]       -- Discounts for remaining arguments; non-zero => be keener to inline
                                --   Always infinite
    }

data ArgSpec
  = ValArg OutExpr                    -- Apply to this (coercion or value); c.f. ApplyToVal
  | TyArg { as_arg_ty  :: OutType     -- Apply to this type; c.f. ApplyToTy
          , as_hole_ty :: OutType }   -- Type of the function (presumably forall a. blah)
  | CastBy OutCoercion                -- Cast by this; c.f. CastIt

instance Outputable ArgSpec where
  ppr (ValArg e)                 = text "ValArg" <+> ppr e
  ppr (TyArg { as_arg_ty = ty }) = text "TyArg" <+> ppr ty
  ppr (CastBy c)                 = text "CastBy" <+> ppr c

addValArgTo :: ArgInfo -> OutExpr -> ArgInfo
addValArgTo ai arg = ai { ai_args = ValArg arg : ai_args ai
                        , ai_type = applyTypeToArg (ai_type ai) arg
                        , ai_rules = decRules (ai_rules ai) }

addTyArgTo :: ArgInfo -> OutType -> ArgInfo
addTyArgTo ai arg_ty = ai { ai_args = arg_spec : ai_args ai
                          , ai_type = piResultTy poly_fun_ty arg_ty
                          , ai_rules = decRules (ai_rules ai) }
  where
    poly_fun_ty = ai_type ai
    arg_spec    = TyArg { as_arg_ty = arg_ty, as_hole_ty = poly_fun_ty }

addCastTo :: ArgInfo -> OutCoercion -> ArgInfo
addCastTo ai co = ai { ai_args = CastBy co : ai_args ai
                     , ai_type = pSnd (coercionKind co) }

argInfoAppArgs :: [ArgSpec] -> [OutExpr]
argInfoAppArgs []                              = []
argInfoAppArgs (CastBy {}                : _)  = []  -- Stop at a cast
argInfoAppArgs (ValArg e                 : as) = e       : argInfoAppArgs as
argInfoAppArgs (TyArg { as_arg_ty = ty } : as) = Type ty : argInfoAppArgs as

pushSimplifiedArgs :: SimplEnv -> [ArgSpec] -> SimplCont -> SimplCont
pushSimplifiedArgs _env []           k = k
pushSimplifiedArgs env  (arg : args) k
  = case arg of
      TyArg { as_arg_ty = arg_ty, as_hole_ty = hole_ty }
               -> ApplyToTy  { sc_arg_ty = arg_ty, sc_hole_ty = hole_ty, sc_cont = rest }
      ValArg e -> ApplyToVal { sc_arg = e, sc_env = env, sc_dup = Simplified, sc_cont = rest }
      CastBy c -> CastIt c rest
  where
    rest = pushSimplifiedArgs env args k
           -- The env has an empty SubstEnv

argInfoExpr :: OutId -> [ArgSpec] -> OutExpr
-- NB: the [ArgSpec] is reversed so that the first arg
-- in the list is the last one in the application
argInfoExpr fun rev_args
  = go rev_args
  where
    go []                              = Var fun
    go (ValArg a                 : as) = go as `App` a
    go (TyArg { as_arg_ty = ty } : as) = go as `App` Type ty
    go (CastBy co                : as) = mkCast (go as) co


type FunRules = Maybe (Int, [CoreRule]) -- Remaining rules for this function
     -- Nothing => No rules
     -- Just (n, rules) => some rules, requiring at least n more type/value args

decRules :: FunRules -> FunRules
decRules (Just (n, rules)) = Just (n-1, rules)
decRules Nothing           = Nothing

mkFunRules :: [CoreRule] -> FunRules
mkFunRules [] = Nothing
mkFunRules rs = Just (n_required, rs)
  where
    n_required = maximum (map ruleArity rs)

{-
************************************************************************
*                                                                      *
                Functions on SimplCont
*                                                                      *
************************************************************************
-}

mkBoringStop :: OutType -> SimplCont
mkBoringStop ty = Stop ty BoringCtxt

mkRhsStop :: OutType -> SimplCont       -- See Note [RHS of lets] in CoreUnfold
mkRhsStop ty = Stop ty RhsCtxt

mkLazyArgStop :: OutType -> CallCtxt -> SimplCont
mkLazyArgStop ty cci = Stop ty cci

-------------------
contIsRhsOrArg :: SimplCont -> Bool
contIsRhsOrArg (Stop {})       = True
contIsRhsOrArg (StrictBind {}) = True
contIsRhsOrArg (StrictArg {})  = True
contIsRhsOrArg _               = False

contIsRhs :: SimplCont -> Bool
contIsRhs (Stop _ RhsCtxt) = True
contIsRhs _                = False

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
contIsDupable (CastIt _ k)                      = contIsDupable k
contIsDupable _                                 = False

-------------------
contIsTrivial :: SimplCont -> Bool
contIsTrivial (Stop {})                                         = True
contIsTrivial (ApplyToTy { sc_cont = k })                       = contIsTrivial k
contIsTrivial (ApplyToVal { sc_arg = Coercion _, sc_cont = k }) = contIsTrivial k
contIsTrivial (CastIt _ k)                                      = contIsTrivial k
contIsTrivial _                                                 = False

-------------------
contResultType :: SimplCont -> OutType
contResultType (Stop ty _)                  = ty
contResultType (CastIt _ k)                 = contResultType k
contResultType (StrictBind { sc_cont = k }) = contResultType k
contResultType (StrictArg { sc_cont = k })  = contResultType k
contResultType (Select { sc_cont = k })     = contResultType k
contResultType (ApplyToTy  { sc_cont = k }) = contResultType k
contResultType (ApplyToVal { sc_cont = k }) = contResultType k
contResultType (TickIt _ k)                 = contResultType k

contHoleType :: SimplCont -> OutType
contHoleType (Stop ty _)                      = ty
contHoleType (TickIt _ k)                     = contHoleType k
contHoleType (CastIt co _)                    = pFst (coercionKind co)
contHoleType (StrictBind { sc_bndr = b, sc_dup = dup, sc_env = se })
  = perhapsSubstTy dup se (idType b)
contHoleType (StrictArg { sc_fun = ai })      = funArgTy (ai_type ai)
contHoleType (ApplyToTy  { sc_hole_ty = ty }) = ty  -- See Note [The hole type in ApplyToTy]
contHoleType (ApplyToVal { sc_arg = e, sc_env = se, sc_dup = dup, sc_cont = k })
  = mkFunTy (perhapsSubstTy dup se (exprType e))
            (contHoleType k)
contHoleType (Select { sc_dup = d, sc_bndr =  b, sc_env = se })
  = perhapsSubstTy d se (idType b)

-------------------
countArgs :: SimplCont -> Int
-- Count all arguments, including types, coercions, and other values
countArgs (ApplyToTy  { sc_cont = cont }) = 1 + countArgs cont
countArgs (ApplyToVal { sc_cont = cont }) = 1 + countArgs cont
countArgs _                               = 0

contArgs :: SimplCont -> (Bool, [ArgSummary], SimplCont)
-- Summarises value args, discards type args and coercions
-- The returned continuation of the call is only used to
-- answer questions like "are you interesting?"
contArgs cont
  | lone cont = (True, [], cont)
  | otherwise = go [] cont
  where
    lone (ApplyToTy  {}) = False  -- See Note [Lone variables] in CoreUnfold
    lone (ApplyToVal {}) = False
    lone (CastIt {})     = False
    lone _               = True

    go args (ApplyToVal { sc_arg = arg, sc_env = se, sc_cont = k })
                                        = go (is_interesting arg se : args) k
    go args (ApplyToTy { sc_cont = k }) = go args k
    go args (CastIt _ k)                = go args k
    go args k                           = (False, reverse args, k)

    is_interesting arg se = interestingArg se arg
                   -- Do *not* use short-cutting substitution here
                   -- because we want to get as much IdInfo as possible


-------------------
mkArgInfo :: Id
          -> [CoreRule] -- Rules for function
          -> Int        -- Number of value args
          -> SimplCont  -- Context of the call
          -> ArgInfo

mkArgInfo fun rules n_val_args call_cont
  | n_val_args < idArity fun            -- Note [Unsaturated functions]
  = ArgInfo { ai_fun = fun, ai_args = [], ai_type = fun_ty
            , ai_rules = fun_rules, ai_encl = False
            , ai_strs = vanilla_stricts
            , ai_discs = vanilla_discounts }
  | otherwise
  = ArgInfo { ai_fun = fun, ai_args = [], ai_type = fun_ty
            , ai_rules = fun_rules
            , ai_encl = interestingArgContext rules call_cont
            , ai_strs  = add_type_str fun_ty arg_stricts
            , ai_discs = arg_discounts }
  where
    fun_ty = idType fun

    fun_rules = mkFunRules rules

    vanilla_discounts, arg_discounts :: [Int]
    vanilla_discounts = repeat 0
    arg_discounts = case idUnfolding fun of
                        CoreUnfolding {uf_guidance = UnfIfGoodArgs {ug_args = discounts}}
                              -> discounts ++ vanilla_discounts
                        _     -> vanilla_discounts

    vanilla_stricts, arg_stricts :: [Bool]
    vanilla_stricts  = repeat False

    arg_stricts
      = case splitStrictSig (idStrictness fun) of
          (demands, result_info)
                | not (demands `lengthExceeds` n_val_args)
                ->      -- Enough args, use the strictness given.
                        -- For bottoming functions we used to pretend that the arg
                        -- is lazy, so that we don't treat the arg as an
                        -- interesting context.  This avoids substituting
                        -- top-level bindings for (say) strings into
                        -- calls to error.  But now we are more careful about
                        -- inlining lone variables, so its ok (see SimplUtils.analyseCont)
                   if isBotRes result_info then
                        map isStrictDmd demands         -- Finite => result is bottom
                   else
                        map isStrictDmd demands ++ vanilla_stricts
               | otherwise
               -> WARN( True, text "More demands than arity" <+> ppr fun <+> ppr (idArity fun)
                                <+> ppr n_val_args <+> ppr demands )
                   vanilla_stricts      -- Not enough args, or no strictness

    add_type_str :: Type -> [Bool] -> [Bool]
    -- If the function arg types are strict, record that in the 'strictness bits'
    -- No need to instantiate because unboxed types (which dominate the strict
    -- types) can't instantiate type variables.
    -- add_type_str is done repeatedly (for each call); might be better
    -- once-for-all in the function
    -- But beware primops/datacons with no strictness

    add_type_str
      = go
      where
        go _ [] = []
        go fun_ty strs            -- Look through foralls
            | Just (_, fun_ty') <- splitForAllTy_maybe fun_ty       -- Includes coercions
            = go fun_ty' strs
        go fun_ty (str:strs)      -- Add strict-type info
            | Just (arg_ty, fun_ty') <- splitFunTy_maybe fun_ty
            = (str || Just False == isLiftedType_maybe arg_ty) : go fun_ty' strs
               -- If the type is levity-polymorphic, we can't know whether it's
               -- strict. isLiftedType_maybe will return Just False only when
               -- we're sure the type is unlifted.
        go _ strs
            = strs

{- Note [Unsaturated functions]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (test eyeball/inline4)
        x = a:as
        y = f x
where f has arity 2.  Then we do not want to inline 'x', because
it'll just be floated out again.  Even if f has lots of discounts
on its first argument -- it must be saturated for these to kick in
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
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.

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
-}

interestingCallContext :: SimplEnv -> SimplCont -> CallCtxt
-- See Note [Interesting call context]
interestingCallContext env cont
  = interesting cont
  where
    interesting (Select {})
       | sm_case_case (getMode env) = CaseCtxt
       | otherwise                  = BoringCtxt
       -- See Note [No case of case is boring]

    interesting (ApplyToVal {}) = ValAppCtxt
        -- Can happen if we have (f Int |> co) y
        -- If f has an INLINE prag we need to give it some
        -- motivation to inline. See Note [Cast then apply]
        -- in CoreUnfold

    interesting (StrictArg { sc_cci = cci }) = cci
    interesting (StrictBind {})              = BoringCtxt
    interesting (Stop _ cci)                 = cci
    interesting (TickIt _ k)                 = interesting k
    interesting (ApplyToTy { sc_cont = k })  = interesting k
    interesting (CastIt _ k)                 = interesting k
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

interestingArgContext :: [CoreRule] -> SimplCont -> Bool
-- If the argument has form (f x y), where x,y are boring,
-- and f is marked INLINE, then we don't want to inline f.
-- But if the context of the argument is
--      g (f x y)
-- where g has rules, then we *do* want to inline f, in case it
-- exposes a rule that might fire.  Similarly, if the context is
--      h (g (f x x))
-- where h has rules, then we do want to inline f; hence the
-- call_cont argument to interestingArgContext
--
-- The ai-rules flag makes this happen; if it's
-- set, the inliner gets just enough keener to inline f
-- regardless of how boring f's arguments are, if it's marked INLINE
--
-- The alternative would be to *always* inline an INLINE function,
-- regardless of how boring its context is; but that seems overkill
-- For example, it'd mean that wrapper functions were always inlined
--
-- The call_cont passed to interestingArgContext is the context of
-- the call itself, e.g. g <hole> in the example above
interestingArgContext rules call_cont
  = notNull rules || enclosing_fn_has_rules
  where
    enclosing_fn_has_rules = go call_cont

    go (Select {})                  = False
    go (ApplyToVal {})              = False  -- Shouldn't really happen
    go (ApplyToTy  {})              = False  -- Ditto
    go (StrictArg { sc_cci = cci }) = interesting cci
    go (StrictBind {})              = False      -- ??
    go (CastIt _ c)                 = go c
    go (Stop _ cci)                 = interesting cci
    go (TickIt _ c)                 = go c

    interesting RuleArgCtxt = True
    interesting _           = False


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
at all; i.e. (hasSomeUnfolding v).  But does too much inlining; see Trac #3016.

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

    go _   _ (Lit {})          = ValueArg
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
       | isConLikeId v     = ValueArg   -- Experimenting with 'conlike' rather that
                                        --    data constructors here
       | idArity v > n     = ValueArg   -- Catches (eg) primops with arity but no unfolding
       | n > 0             = NonTrivArg -- Saturated or unknown call
       | conlike_unfolding = ValueArg   -- n==0; look for an interesting unfolding
                                        -- See Note [Conlike is interesting]
       | otherwise         = TrivArg    -- n==0, no useful unfolding
       where
         conlike_unfolding = isConLikeUnfolding (idUnfolding v)

{-
************************************************************************
*                                                                      *
                  SimplMode
*                                                                      *
************************************************************************

The SimplMode controls several switches; see its definition in
CoreMonad
        sm_rules      :: Bool     -- Whether RULES are enabled
        sm_inline     :: Bool     -- Whether inlining is enabled
        sm_case_case  :: Bool     -- Whether case-of-case is enabled
        sm_eta_expand :: Bool     -- Whether eta-expansion is enabled
-}

simplEnvForGHCi :: DynFlags -> SimplEnv
simplEnvForGHCi dflags
  = mkSimplEnv $ SimplMode { sm_names  = ["GHCi"]
                           , sm_phase  = InitialPhase
                           , sm_dflags = dflags
                           , sm_rules  = rules_on
                           , sm_inline = False
                           , sm_eta_expand = eta_expand_on
                           , sm_case_case  = True }
  where
    rules_on      = gopt Opt_EnableRewriteRules   dflags
    eta_expand_on = gopt Opt_DoLambdaEtaExpansion dflags
   -- Do not do any inlining, in case we expose some unboxed
   -- tuple stuff that confuses the bytecode interpreter

updModeForStableUnfoldings :: Activation -> SimplMode -> SimplMode
-- See Note [Simplifying inside stable unfoldings]
updModeForStableUnfoldings inline_rule_act current_mode
  = current_mode { sm_phase      = phaseFromActivation inline_rule_act
                 , sm_inline     = True
                 , sm_eta_expand = False }
                     -- sm_eta_expand: see Note [No eta expansion in stable unfoldings]
       -- For sm_rules, just inherit; sm_rules might be "off"
       -- because of -fno-enable-rewrite-rules
  where
    phaseFromActivation (ActiveAfter _ n) = Phase n
    phaseFromActivation _                 = InitialPhase

updModeForRules :: SimplMode -> SimplMode
-- See Note [Simplifying rules]
updModeForRules current_mode
  = current_mode { sm_phase  = InitialPhase
                 , sm_inline = False
                 , sm_rules  = False
                 , sm_eta_expand = False }

{- Note [Simplifying rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When simplifying a rule LHS, refrain from /any/ inlining or applying
of other RULES.

Doing anything to the LHS is plain confusing, because it means that what the
rule matches is not what the user wrote. c.f. Trac #10595, and #10528.
Moreover, inlining (or applying rules) on rule LHSs risks introducing
Ticks into the LHS, which makes matching trickier. Trac #10665, #10745.

Doing this to either side confounds tools like HERMIT, which seek to reason
about and apply the RULES as originally written. See Trac #10829.

Note [No eta expansion in stable unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a stable unfolding

  f :: Ord a => a -> IO ()
  -- Unfolding template
  --    = /\a \(d:Ord a) (x:a). bla

we do not want to eta-expand to

  f :: Ord a => a -> IO ()
  -- Unfolding template
  --    = (/\a \(d:Ord a) (x:a) (eta:State#). bla eta) |> co

because not specialisation of the overloading doesn't work properly
(see Note [Specialisation shape] in Specialise), Trac #9509.

So we disable eta-expansion in stable unfoldings.

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
  When simplifying the RHS of an stable unfolding, set the phase
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

activeUnfolding :: SimplEnv -> Id -> Bool
activeUnfolding env id
  | isCompulsoryUnfolding (realIdUnfolding id)
  = True   -- Even sm_inline can't override compulsory unfoldings
  | otherwise
  = isActive (sm_phase mode) (idInlineActivation id)
  && sm_inline mode
      -- `or` isStableUnfolding (realIdUnfolding id)
      -- Inline things when
      --  (a) they are active
      --  (b) sm_inline says so, except that for stable unfoldings
      --                         (ie pragmas) we inline anyway
  where
    mode = getMode env

getUnfoldingInRuleMatch :: SimplEnv -> InScopeEnv
-- When matching in RULE, we want to "look through" an unfolding
-- (to see a constructor) if *rules* are on, even if *inlinings*
-- are not.  A notable example is DFuns, which really we want to
-- match in rules like (op dfun) in gentle mode. Another example
-- is 'otherwise' which we want exprIsConApp_maybe to be able to
-- see very early on
getUnfoldingInRuleMatch env
  = (in_scope, id_unf)
  where
    in_scope = seInScope env
    mode = getMode env
    id_unf id | unf_is_active id = idUnfolding id
              | otherwise        = NoUnfolding
    unf_is_active id
     | not (sm_rules mode) = -- active_unfolding_minimal id
                             isStableUnfolding (realIdUnfolding id)
        -- Do we even need to test this?  I think this InScopeEnv
        -- is only consulted if activeRule returns True, which
        -- never happens if sm_rules is False
     | otherwise           = isActive (sm_phase mode) (idInlineActivation id)

----------------------
activeRule :: SimplEnv -> Activation -> Bool
-- Nothing => No rules at all
activeRule env
  | not (sm_rules mode) = \_ -> False     -- Rewriting is off
  | otherwise           = isActive (sm_phase mode)
  where
    mode = getMode env

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
might have a BIG rhs, which will now be dup'd at every occurrenc of x.


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

Conclusion: inline top level things gaily until Phase 0 (the last
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
inactive in the intial stages.  See Note [Gentle mode].

Note [Stable unfoldings and preInlineUnconditionally]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
want PreInlineUnconditionally to second-guess it.  A live example is
Trac #3736.
    c.f. Note [Stable unfoldings and postInlineUnconditionally]

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

preInlineUnconditionally :: SimplEnv -> TopLevelFlag -> InId -> InExpr -> Bool
-- Precondition: rhs satisfies the let/app invariant
-- See Note [CoreSyn let/app invariant] in CoreSyn
-- Reason: we don't want to inline single uses, or discard dead bindings,
--         for unlifted, side-effect-ful bindings
preInlineUnconditionally env top_lvl bndr rhs
  | not pre_inline_unconditionally           = False
  | not active                               = False
  | isStableUnfolding (idUnfolding bndr)     = False -- Note [Stable unfoldings and preInlineUnconditionally]
  | isTopLevel top_lvl && isBottomingId bndr = False -- Note [Top-level bottoming Ids]
  | isCoVar bndr                             = False -- Note [Do not inline CoVars unconditionally]
  | otherwise = case idOccInfo bndr of
                  IAmDead                    -> True -- Happens in ((\x.1) v)
                  occ@OneOcc { occ_one_br = True }
                                             -> try_once (occ_in_lam occ)
                                                         (occ_int_cxt occ)
                  _                          -> False
  where
    pre_inline_unconditionally = gopt Opt_SimplPreInlining (seDynFlags env)
    mode   = getMode env
    active = isActive (sm_phase mode) act
             -- See Note [pre/postInlineUnconditionally in gentle mode]
    act = idInlineActivation bndr
    try_once in_lam int_cxt     -- There's one textual occurrence
        | not in_lam = isNotTopLevel top_lvl || early_phase
        | otherwise  = int_cxt && canInlineInLam rhs

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

    early_phase = case sm_phase mode of
                    Phase 0 -> False
                    _       -> True
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

Note that we do this unconditional inlining only for trival RHSs.
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
-}

postInlineUnconditionally
    :: SimplEnv -> TopLevelFlag
    -> OutId            -- The binder (*not* a CoVar), including its unfolding
    -> OccInfo          -- From the InId
    -> OutExpr
    -> Bool
-- Precondition: rhs satisfies the let/app invariant
-- See Note [CoreSyn let/app invariant] in CoreSyn
-- Reason: we don't want to inline single uses, or discard dead bindings,
--         for unlifted, side-effect-ful bindings
postInlineUnconditionally env top_lvl bndr occ_info rhs
  | not active                  = False
  | isWeakLoopBreaker occ_info  = False -- If it's a loop-breaker of any kind, don't inline
                                        -- because it might be referred to "earlier"
  | isStableUnfolding unfolding = False -- Note [Stable unfoldings and postInlineUnconditionally]
  | isTopLevel top_lvl          = False -- Note [Top level and postInlineUnconditionally]
  | exprIsTrivial rhs           = True
  | otherwise
  = case occ_info of
        -- The point of examining occ_info here is that for *non-values*
        -- that occur outside a lambda, the call-site inliner won't have
        -- a chance (because it doesn't know that the thing
        -- only occurs once).   The pre-inliner won't have gotten
        -- it either, if the thing occurs in more than one branch
        -- So the main target is things like
        --      let x = f y in
        --      case v of
        --         True  -> case x of ...
        --         False -> case x of ...
        -- This is very important in practice; e.g. wheel-seive1 doubles
        -- in allocation if you miss this out
      OneOcc { occ_in_lam = in_lam, occ_int_cxt = int_cxt }
               -- OneOcc => no code-duplication issue
        ->     smallEnoughToInline dflags unfolding     -- Small enough to dup
                        -- ToDo: consider discount on smallEnoughToInline if int_cxt is true
                        --
                        -- NB: Do NOT inline arbitrarily big things, even if one_br is True
                        -- Reason: doing so risks exponential behaviour.  We simplify a big
                        --         expression, inline it, and simplify it again.  But if the
                        --         very same thing happens in the big expression, we get
                        --         exponential cost!
                        -- PRINCIPLE: when we've already simplified an expression once,
                        -- make sure that we only inline it if it's reasonably small.

           && (not in_lam ||
                        -- Outside a lambda, we want to be reasonably aggressive
                        -- about inlining into multiple branches of case
                        -- e.g. let x = <non-value>
                        --      in case y of { C1 -> ..x..; C2 -> ..x..; C3 -> ... }
                        -- Inlining can be a big win if C3 is the hot-spot, even if
                        -- the uses in C1, C2 are not 'interesting'
                        -- An example that gets worse if you add int_cxt here is 'clausify'

                (isCheapUnfolding unfolding && int_cxt))
                        -- isCheap => acceptable work duplication; in_lam may be true
                        -- int_cxt to prevent us inlining inside a lambda without some
                        -- good reason.  See the notes on int_cxt in preInlineUnconditionally

      IAmDead -> True   -- This happens; for example, the case_bndr during case of
                        -- known constructor:  case (a,b) of x { (p,q) -> ... }
                        -- Here x isn't mentioned in the RHS, so we don't want to
                        -- create the (dead) let-binding  let x = (a,b) in ...

      _ -> False

-- Here's an example that we don't handle well:
--      let f = if b then Left (\x.BIG) else Right (\y.BIG)
--      in \y. ....case f of {...} ....
-- Here f is used just once, and duplicating the case work is fine (exprIsCheap).
-- But
--  - We can't preInlineUnconditionally because that woud invalidate
--    the occ info for b.
--  - We can't postInlineUnconditionally because the RHS is big, and
--    that risks exponential behaviour
--  - We can't call-site inline, because the rhs is big
-- Alas!

  where
    unfolding = idUnfolding bndr
    dflags    = seDynFlags env
    active    = isActive (sm_phase (getMode env)) (idInlineActivation bndr)
        -- See Note [pre/postInlineUnconditionally in gentle mode]

{-
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
Do not do postInlineUnconditionally if the Id has an stable unfolding,
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

mkLam :: SimplEnv -> [OutBndr] -> OutExpr -> SimplCont -> SimplM OutExpr
-- mkLam tries three things
--      a) eta reduction, if that gives a trivial expression
--      b) eta expansion [only if there are some value lambdas]

mkLam _env [] body _cont
  = return body
mkLam env bndrs body cont
  = do { dflags <- getDynFlags
       ; mkLam' dflags bndrs body }
  where
    mkLam' :: DynFlags -> [OutBndr] -> OutExpr -> SimplM OutExpr
    mkLam' dflags bndrs (Cast body co)
      | not (any bad bndrs)
        -- Note [Casts and lambdas]
      = do { lam <- mkLam' dflags bndrs body
           ; return (mkCast lam (mkPiCos Representational bndrs co)) }
      where
        co_vars  = tyCoVarsOfCo co
        bad bndr = isCoVar bndr && bndr `elemVarSet` co_vars

    mkLam' dflags bndrs body@(Lam {})
      = mkLam' dflags (bndrs ++ bndrs1) body1
      where
        (bndrs1, body1) = collectBinders body

    mkLam' dflags bndrs (Tick t expr)
      | tickishFloatable t
      = mkTick t <$> mkLam' dflags bndrs expr

    mkLam' dflags bndrs body
      | gopt Opt_DoEtaReduction dflags
      , Just etad_lam <- tryEtaReduce bndrs body
      = do { tick (EtaReduction (head bndrs))
           ; return etad_lam }

      | not (contIsRhs cont)   -- See Note [Eta-expanding lambdas]
      , sm_eta_expand (getMode env)
      , any isRuntimeVar bndrs
      , let body_arity = exprEtaExpandArity dflags body
      , body_arity > 0
      = do { tick (EtaExpansion (head bndrs))
           ; let res = mkLams bndrs (etaExpand body_arity body)
           ; traceSmpl "eta expand" (vcat [text "before" <+> ppr (mkLams bndrs body)
                                          , text "after" <+> ppr res])
           ; return res }

      | otherwise
      = return (mkLams bndrs body)

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

NB: We check the SimplEnv (sm_eta_expand), not DynFlags.
    See Note [No eta expansion in stable unfoldings]

Note [Casts and lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        (\x. (\y. e) `cast` g1) `cast` g2
There is a danger here that the two lambdas look separated, and the
full laziness pass might float an expression to between the two.

So this equation in mkLam' floats the g1 out, thus:
        (\x. e `cast` g1)  -->  (\x.e) `cast` (tx -> g1)
where x:tx.

In general, this floats casts outside lambdas, where (I hope) they
might meet and cancel with some other cast:
        \x. e `cast` co   ===>   (\x. e) `cast` (tx -> co)
        /\a. e `cast` co  ===>   (/\a. e) `cast` (/\a. co)
        /\g. e `cast` co  ===>   (/\g. e) `cast` (/\g. co)
                          (if not (g `in` co))

Notice that it works regardless of 'e'.  Originally it worked only
if 'e' was itself a lambda, but in some cases that resulted in
fruitless iteration in the simplifier.  A good example was when
compiling Text.ParserCombinators.ReadPrec, where we had a definition
like    (\x. Get `cast` g)
where Get is a constructor with nonzero arity.  Then mkLam eta-expanded
the Get, and the next iteration eta-reduced it, and then eta-expanded
it again.

Note also the side condition for the case of coercion binders.
It does not make sense to transform
        /\g. e `cast` g  ==>  (/\g.e) `cast` (/\g.g)
because the latter is not well-kinded.

************************************************************************
*                                                                      *
              Eta expansion
*                                                                      *
************************************************************************
-}

tryEtaExpandRhs :: SimplMode -> OutId -> OutExpr
                -> SimplM (Arity, Bool, OutExpr)
-- See Note [Eta-expanding at let bindings]
-- If tryEtaExpandRhs rhs = (n, is_bot, rhs') then
--   (a) rhs' has manifest arity
--   (b) if is_bot is True then rhs' applied to n args is guaranteed bottom
tryEtaExpandRhs mode bndr rhs
  | isJoinId bndr
  = return (manifestArity rhs, False, rhs)
    -- Note [Do not eta-expand join points]

  | otherwise
  = do { (new_arity, is_bot, new_rhs) <- try_expand

       ; WARN( new_arity < old_id_arity,
               (text "Arity decrease:" <+> (ppr bndr <+> ppr old_id_arity
                <+> ppr old_arity <+> ppr new_arity) $$ ppr new_rhs) )
                        -- Note [Arity decrease] in Simplify
         return (new_arity, is_bot, new_rhs) }
  where
    try_expand
      | exprIsTrivial rhs
      = return (exprArity rhs, False, rhs)

      | sm_eta_expand mode      -- Provided eta-expansion is on
      , new_arity > old_arity   -- And the current manifest arity isn't enough
      = do { tick (EtaExpansion bndr)
           ; return (new_arity, is_bot, etaExpand new_arity rhs) }

      | otherwise
      = return (old_arity, is_bot && new_arity == old_arity, rhs)

    dflags       = sm_dflags mode
    old_arity    = exprArity rhs -- See Note [Do not expand eta-expand PAPs]
    old_id_arity = idArity bndr

    (new_arity1, is_bot) = findRhsArity dflags bndr rhs old_arity
    new_arity2 = idCallArity bndr
    new_arity  = max new_arity1 new_arity2

{-
Note [Eta-expanding at let bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We now eta expand at let-bindings, which is where the payoff comes.
The most significant thing is that we can do a simple arity analysis
(in CoreArity.findRhsArity), which we can't do for free-floating lambdas

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

Note [Do not eta-expand join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Similarly to CPR (see Note [Don't CPR join points] in WorkWrap), a join point
stands well to gain from its outer binding's eta-expansion, and eta-expanding a
join point is fraught with issues like how to deal with a cast:

    let join $j1 :: IO ()
             $j1 = ...
             $j2 :: Int -> IO ()
             $j2 n = if n > 0 then $j1
                              else ...

    =>

    let join $j1 :: IO ()
             $j1 = (\eta -> ...)
                     `cast` N:IO :: State# RealWorld -> (# State# RealWorld, ())
                                 ~  IO ()
             $j2 :: Int -> IO ()
             $j2 n = (\eta -> if n > 0 then $j1
                                       else ...)
                     `cast` N:IO :: State# RealWorld -> (# State# RealWorld, ())
                                 ~  IO ()

The cast here can't be pushed inside the lambda (since it's not casting to a
function type), so the lambda has to stay, but it can't because it contains a
reference to a join point. In fact, $j2 can't be eta-expanded at all. Rather
than try and detect this situation (and whatever other situations crop up!), we
don't bother; again, any surrounding eta-expansion will improve these join
points anyway, since an outer cast can *always* be pushed inside. By the time
CorePrep comes around, the code is very likely to look more like this:

    let join $j1 :: State# RealWorld -> (# State# RealWorld, ())
             $j1 = (...) eta
             $j2 :: Int -> State# RealWorld -> (# State# RealWorld, ())
             $j2 = if n > 0 then $j1
                            else (...) eta

Note [Do not eta-expand PAPs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to have old_arity = manifestArity rhs, which meant that we
would eta-expand even PAPs.  But this gives no particular advantage,
and can lead to a massive blow-up in code size, exhibited by Trac #9020.
Suppose we have a PAP
    foo :: IO ()
    foo = returnIO ()
Then we can eta-expand do
    foo = (\eta. (returnIO () |> sym g) eta) |> g
where
    g :: IO () ~ State# RealWorld -> (# State# RealWorld, () #)

But there is really no point in doing this, and it generates masses of
coercions and whatnot that eventually disappear again. For T9020, GHC
allocated 6.6G beore, and 0.8G afterwards; and residency dropped from
1.8G to 45M.

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

and treat them specially. The real work is done in SimplUtils.abstractFloats,
but there is quite a bit of plumbing in simplLazyBind as well.

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
new binding is abstracted.  Note that

  * The naive approach of abstracting wrt the
    tyvars free in the Id's /type/ fails. Consider:
        /\ a b -> let t :: (a,b) = (e1, e2)
                      x :: a     = fst t
                  in ...
    Here, b isn't free in x's type, but we must nevertheless
    abstract wrt b as well, because t's type mentions b.
    Since t is floated too, we'd end up with the bogus:
         poly_t = /\ a b -> (e1, e2)
         poly_x = /\ a   -> fst (poly_t a *b*)

  * We must do closeOverKinds.  Example (Trac #10934):
       f = /\k (f:k->*) (a:k). let t = AccFailure @ (f a) in ...
    Here we want to float 't', but we must remember to abstract over
    'k' as well, even though it is not explicitly mentioned in the RHS,
    otherwise we get
       t = /\ (f:k->*) (a:k). AccFailure @ (f a)
    which is obviously bogus.
-}

abstractFloats :: DynFlags -> TopLevelFlag -> [OutTyVar] -> SimplFloats
              -> OutExpr -> SimplM ([OutBind], OutExpr)
abstractFloats dflags top_lvl main_tvs floats body
  = ASSERT( notNull body_floats )
    ASSERT( isNilOL (sfJoinFloats floats) )
    do  { (subst, float_binds) <- mapAccumLM abstract empty_subst body_floats
        ; return (float_binds, CoreSubst.substExpr (text "abstract_floats1") subst body) }
  where
    is_top_lvl  = isTopLevel top_lvl
    main_tv_set = mkVarSet main_tvs
    body_floats = letFloatBinds (sfLetFloats floats)
    empty_subst = CoreSubst.mkEmptySubst (sfInScope floats)

    abstract :: CoreSubst.Subst -> OutBind -> SimplM (CoreSubst.Subst, OutBind)
    abstract subst (NonRec id rhs)
      = do { (poly_id1, poly_app) <- mk_poly1 tvs_here id
           ; let (poly_id2, poly_rhs) = mk_poly2 poly_id1 tvs_here rhs'
                 subst' = CoreSubst.extendIdSubst subst id poly_app
           ; return (subst', NonRec poly_id2 poly_rhs) }
      where
        rhs' = CoreSubst.substExpr (text "abstract_floats2") subst rhs

        -- tvs_here: see Note [Which type variables to abstract over]
        tvs_here = toposortTyVars $
                   filter (`elemVarSet` main_tv_set) $
                   closeOverKindsList $
                   exprSomeFreeVarsList isTyVar rhs'

    abstract subst (Rec prs)
       = do { (poly_ids, poly_apps) <- mapAndUnzipM (mk_poly1 tvs_here) ids
            ; let subst' = CoreSubst.extendSubstList subst (ids `zip` poly_apps)
                  poly_pairs = [ mk_poly2 poly_id tvs_here rhs'
                               | (poly_id, rhs) <- poly_ids `zip` rhss
                               , let rhs' = CoreSubst.substExpr (text "abstract_floats")
                                                                subst' rhs ]
            ; return (subst', Rec poly_pairs) }
       where
         (ids,rhss) = unzip prs
                -- For a recursive group, it's a bit of a pain to work out the minimal
                -- set of tyvars over which to abstract:
                --      /\ a b c.  let x = ...a... in
                --                 letrec { p = ...x...q...
                --                          q = .....p...b... } in
                --                 ...
                -- Since 'x' is abstracted over 'a', the {p,q} group must be abstracted
                -- over 'a' (because x is replaced by (poly_x a)) as well as 'b'.
                -- Since it's a pain, we just use the whole set, which is always safe
                --
                -- If you ever want to be more selective, remember this bizarre case too:
                --      x::a = x
                -- Here, we must abstract 'x' over 'a'.
         tvs_here = toposortTyVars main_tvs

    mk_poly1 :: [TyVar] -> Id -> SimplM (Id, CoreExpr)
    mk_poly1 tvs_here var
      = do { uniq <- getUniqueM
           ; let  poly_name = setNameUnique (idName var) uniq           -- Keep same name
                  poly_ty   = mkInvForAllTys tvs_here (idType var) -- But new type of course
                  poly_id   = transferPolyIdInfo var tvs_here $ -- Note [transferPolyIdInfo] in Id.hs
                              mkLocalIdOrCoVar poly_name poly_ty
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
        unf = mkUnfolding dflags InlineRhs is_top_lvl False poly_rhs

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

1.  Eliminate alternatives that cannot match, including the
    DEFAULT alternative.

2.  If the DEFAULT alternative can match only one possible constructor,
    then make that constructor explicit.
    e.g.
        case e of x { DEFAULT -> rhs }
     ===>
        case e of x { (a,b) -> rhs }
    where the type is a single constructor type.  This gives better code
    when rhs also scrutinises x or e.

3. Returns a list of the constructors that cannot holds in the
   DEFAULT alternative (if there is one)

Here "cannot match" includes knowledge from GADTs

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
-}

prepareAlts :: OutExpr -> OutId -> [InAlt] -> SimplM ([AltCon], [InAlt])
-- The returned alternatives can be empty, none are possible
prepareAlts scrut case_bndr' alts
  | Just (tc, tys) <- splitTyConApp_maybe (varType case_bndr')
           -- Case binder is needed just for its type. Note that as an
           --   OutId, it has maximum information; this is important.
           --   Test simpl013 is an example
  = do { us <- getUniquesM
       ; let (idcs1, alts1)       = filterAlts tc tys imposs_cons alts
             (yes2,  alts2)       = refineDefaultAlt us tc tys idcs1 alts1
             (yes3, idcs3, alts3) = combineIdenticalAlts idcs1 alts2
             -- "idcs" stands for "impossible default data constructors"
             -- i.e. the constructors that can't match the default case
       ; when yes2 $ tick (FillInCaseDefault case_bndr')
       ; when yes3 $ tick (AltMerge case_bndr')
       ; return (idcs3, alts3) }

  | otherwise  -- Not a data type, so nothing interesting happens
  = return ([], alts)
  where
    imposs_cons = case scrut of
                    Var v -> otherCons (idUnfolding v)
                    _     -> []


{-
************************************************************************
*                                                                      *
                mkCase
*                                                                      *
************************************************************************

mkCase tries these things

* Note [Nerge nested cases]
* Note [Eliminate identity case]
* Note [Scrutinee constant folding]

Note [Merge Nested Cases]
~~~~~~~~~~~~~~~~~~~~~~~~~
       case e of b {             ==>   case e of b {
         p1 -> rhs1                      p1 -> rhs1
         ...                             ...
         pm -> rhsm                      pm -> rhsm
         _  -> case b of b' {            pn -> let b'=b in rhsn
                     pn -> rhsn          ...
                     ...                 po -> let b'=b in rhso
                     po -> rhso          _  -> let b'=b in rhsd
                     _  -> rhsd
       }

which merges two cases in one case when -- the default alternative of
the outer case scrutises the same variable as the outer case. This
transformation is called Case Merging.  It avoids that the same
variable is scrutinised multiple times.

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
            0##    -> ...
            DEFAUT -> case t1 `minusWord#` 1## of t2
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

There are some wrinkles

* Do not apply caseRules if there is just a single DEFAULT alternative
     case e +# 3# of b { DEFAULT -> rhs }
  If we applied the transformation here we would (stupidly) get
     case a of b' { DEFAULT -> let b = e +# 3# in rhs }
  and now the process may repeat, because that let will really
  be a case.

* The type of the scrutinee might change.  E.g.
        case tagToEnum (x :: Int#) of (b::Bool)
          False -> e1
          True -> e2
  ==>
        case x of (b'::Int#)
          DEFAULT -> e1
          1#      -> e2

* The case binder may be used in the right hand sides, so we need
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

* In dataToTag we might need to make up some fake binders;
  see Note [caseRules for dataToTag] in PrelRules
-}

mkCase, mkCase1, mkCase2, mkCase3
   :: DynFlags
   -> OutExpr -> OutId
   -> OutType -> [OutAlt]               -- Alternatives in standard (increasing) order
   -> SimplM OutExpr

--------------------------------------------------
--      1. Merge Nested Cases
--------------------------------------------------

mkCase dflags scrut outer_bndr alts_ty ((DEFAULT, _, deflt_rhs) : outer_alts)
  | gopt Opt_CaseMerge dflags
  , (ticks, Case (Var inner_scrut_var) inner_bndr _ inner_alts)
       <- stripTicksTop tickishFloatable deflt_rhs
  , inner_scrut_var == outer_bndr
  = do  { tick (CaseMerge outer_bndr)

        ; let wrap_alt (con, args, rhs) = ASSERT( outer_bndr `notElem` args )
                                          (con, args, wrap_rhs rhs)
                -- Simplifier's no-shadowing invariant should ensure
                -- that outer_bndr is not shadowed by the inner patterns
              wrap_rhs rhs = Let (NonRec inner_bndr (Var outer_bndr)) rhs
                -- The let is OK even for unboxed binders,

              wrapped_alts | isDeadBinder inner_bndr = inner_alts
                           | otherwise               = map wrap_alt inner_alts

              merged_alts = mergeAlts outer_alts wrapped_alts
                -- NB: mergeAlts gives priority to the left
                --      case x of
                --        A -> e1
                --        DEFAULT -> case x of
                --                      A -> e2
                --                      B -> e3
                -- When we merge, we must ensure that e1 takes
                -- precedence over e2 as the value for A!

        ; fmap (mkTicks ticks) $
          mkCase1 dflags scrut outer_bndr alts_ty merged_alts
        }
        -- Warning: don't call mkCase recursively!
        -- Firstly, there's no point, because inner alts have already had
        -- mkCase applied to them, so they won't have a case in their default
        -- Secondly, if you do, you get an infinite loop, because the bindCaseBndr
        -- in munge_rhs may put a case into the DEFAULT branch!

mkCase dflags scrut bndr alts_ty alts = mkCase1 dflags scrut bndr alts_ty alts

--------------------------------------------------
--      2. Eliminate Identity Case
--------------------------------------------------

mkCase1 _dflags scrut case_bndr _ alts@((_,_,rhs1) : _)      -- Identity case
  | all identity_alt alts
  = do { tick (CaseIdentity case_bndr)
       ; return (mkTicks ticks $ re_cast scrut rhs1) }
  where
    ticks = concatMap (stripTicksT tickishFloatable . thdOf3) (tail alts)
    identity_alt (con, args, rhs) = check_eq rhs con args

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

mkCase1 dflags scrut bndr alts_ty alts = mkCase2 dflags scrut bndr alts_ty alts

--------------------------------------------------
--      2. Scrutinee Constant Folding
--------------------------------------------------

mkCase2 dflags scrut bndr alts_ty alts
  | -- See Note [Scrutinee Constant Folding]
    case alts of  -- Not if there is just a DEFAULT alterantive
      [(DEFAULT,_,_)] -> False
      _               -> True
  , gopt Opt_CaseFolding dflags
  , Just (scrut', tx_con, mk_orig) <- caseRules dflags scrut
  = do { bndr' <- newId (fsLit "lwild") (exprType scrut')
       ; alts' <- mapM  (tx_alt tx_con mk_orig bndr') alts
       ; mkCase3 dflags scrut' bndr' alts_ty $
         add_default (re_sort alts')
       }

  | otherwise
  = mkCase3 dflags scrut bndr alts_ty alts
  where
    -- We need to keep the correct association between the scrutinee and its
    -- binder if the latter isn't dead. Hence we wrap rhs of alternatives with
    -- "let bndr = ... in":
    --
    --     case v + 10 of y        =====> case v of y
    --        20      -> e1                 10      -> let y = 20     in e1
    --        DEFAULT -> e2                 DEFAULT -> let y = v + 10 in e2
    --
    -- Other transformations give: =====> case v of y'
    --                                      10      -> let y = 20      in e1
    --                                      DEFAULT -> let y = y' + 10 in e2
    --
    -- This wrapping is done in tx_alt; we use mk_orig, returned by caseRules,
    -- to construct an expression equivalent to the original one, for use
    -- in the DEFAULT case

    tx_alt tx_con mk_orig new_bndr (con, bs, rhs)
      | DataAlt dc <- con', not (isNullaryRepDataCon dc)
      = -- For non-nullary data cons we must invent some fake binders
        -- See Note [caseRules for dataToTag] in PrelRules
        do { us <- getUniquesM
           ; let (ex_tvs, arg_ids) = dataConRepInstPat us dc
                                         (tyConAppArgs (idType new_bndr))
           ; return (con', ex_tvs ++ arg_ids, rhs') }
      | otherwise
      = return (con', [], rhs')
      where
        con' = tx_con con

        rhs' | isDeadBinder bndr = rhs
             | otherwise         = bindNonRec bndr orig_val rhs

        orig_val = case con of
                      DEFAULT    -> mk_orig new_bndr
                      LitAlt l   -> Lit l
                      DataAlt dc -> mkConApp2 dc (tyConAppArgs (idType bndr)) bs


    re_sort :: [CoreAlt] -> [CoreAlt]  -- Re-sort the alternatives to
    re_sort alts = sortBy cmpAlt alts  -- preserve the #case_invariants#

    add_default :: [CoreAlt] -> [CoreAlt]
    -- TagToEnum may change a boolean True/False set of alternatives
    -- to LitAlt 0#/1# alterantives.  But literal alternatives always
    -- have a DEFAULT (I think).  So add it.
    add_default ((LitAlt {}, bs, rhs) : alts) = (DEFAULT, bs, rhs) : alts
    add_default alts                          = alts

--------------------------------------------------
--      Catch-all
--------------------------------------------------
mkCase3 _dflags scrut bndr alts_ty alts
  = return (Case scrut bndr alts_ty alts)

{-
Note [Dead binders]
~~~~~~~~~~~~~~~~~~~~
Note that dead-ness is maintained by the simplifier, so that it is
accurate after simplification as well as before.


Note [Cascading case merge]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case merging should cascade in one sweep, because it
happens bottom-up

      case e of a {
        DEFAULT -> case a of b
                      DEFAULT -> case b of c {
                                     DEFAULT -> e
                                     A -> ea
                      B -> eb
        C -> ec
==>
      case e of a {
        DEFAULT -> case a of b
                      DEFAULT -> let c = b in e
                      A -> let c = b in ea
                      B -> eb
        C -> ec
==>
      case e of a {
        DEFAULT -> let b = a in let c = b in e
        A -> let b = a in let c = b in ea
        B -> let b = a in eb
        C -> ec


However here's a tricky case that we still don't catch, and I don't
see how to catch it in one pass:

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

After occurrence analysis (and its binder-swap) we get this

  case x of c1 { I# a1 ->
  let x = c1 in         -- Binder-swap addition
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case x of c3 { I# a2 ->
               case a2 of ...

When we simplify the inner case x, we'll see that
x=c1=I# a1.  So we'll bind a2 to a1, and get

  case x of c1 { I# a1 ->
  case a1 of c2 ->
    0 -> ...
    DEFAULT -> case a1 of ...

This is corect, but we can't do a case merge in this sweep
because c2 /= a1.  Reason: the binding c1=I# a1 went inwards
without getting changed to c1=I# c2.

I don't think this is worth fixing, even if I knew how. It'll
all come out in the next pass anyway.
-}
