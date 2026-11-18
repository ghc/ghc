{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Gen.App
       ( tcApp
       , tcExprPrag
       , tcRetainedInferableArg ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcPolyExpr, tcInferRho )

import GHC.Hs

import GHC.Rename.Env ( addUsedGRE, DeprecationWarnings(NoDeprecationWarnings) )
import GHC.Rename.Utils ( mkExpandedLExpr )
import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Match ( tcMatches, tcBody )
import GHC.Tc.Gen.SourceTable ( tcLookupSourceTableId, recordSourceMethodUse, recordSourceLiteralUse )
import GHC.Tc.Gen.Sig ( isCompleteHsSig )
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr
import GHC.Tc.Utils.Monad
import GHC.Tc.Solver ( tcRewriteFamilyAxioms )
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Env ( tcLookupIdMaybe, tcLookupConLike, tcLookupGlobal, tcLookupLcl_maybe )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst_maybe )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Concrete  ( unifyConcrete, idConcreteTvs )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.TcRef ( TcRef )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint (WantedConstraints(wc_simple), ctPred, ctEvLoc, ctEvEvId,
                                mkNonCanonical, DelayedError(DE_SourceMethod))
import GHC.Tc.Types.ErrCtxt ( FunAppCtxtFunArg(..) )
import GHC.Tc.Types.CtLoc ( CtLoc )
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Zonk.TcType

import GHC.Core.Class ( Class )
import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon ( dataConConcreteTyVars, dataConFullSig, dataConName, isNewDataCon, dataConOrigArgTys, dataConRepArgBinders, dataConTyCon, dataConWorkId )
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.Reduction ( reductionReducedType )
import GHC.Core.Utils ( applyTypeToArgs, exprType )
import GHC.Core.FVs ( exprFreeVarsList )
import GHC.Core.InstEnv ( lookupUniqueInstEnv, instanceDFunId )
import GHC.Core.Predicate ( classMethodInstTy )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Unify ( tcMatchTyX_BM, tcMatchRetainedTyX_BM, BindFlag(..), BindTvFun )
import GHC.Core.Type
import GHC.Core.UsageEnv ( UsageEnv, singleUsageUE, zeroUE )
import GHC.Core.Coercion

import GHC.Builtin.WiredIn.Types ( multiplicityTy, runtimeRepTy, nilDataCon, consDataCon, mkListTy )
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.KnownKeys

import GHC.Driver.DynFlags ( DynFlags(reductionDepth), HasDynFlags(getDynFlags) )
import GHC.Types.TyThing ( TyThing(AnId, AConLike) )
import GHC.Types.Basic ( TypeOrKind(TypeLevel), intGtLimit )
import GHC.Types.TypeEnv ( lookupTypeEnv )
import GHC.Types.Var
import GHC.Types.Id ( mkLocalId, isClassOpId_maybe, sourceMethodTarget_maybe, sourceLiteralCons_maybe )
import GHC.Types.Var.Set ( elemVarSet, emptyVarSet, extendVarSet )
import GHC.Types.Var.FV
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Var.Env  ( emptyTidyEnv, mkInScopeSet )

import GHC.Data.Maybe
import GHC.Data.Bag ( unitBag )
import GHC.Data.FastString

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Data ( Data, cast, gmapQ )
import Data.Function
import Data.List ( mapAccumL )
import qualified Data.List.NonEmpty as NE
import Data.Semigroup

import GHC.Prelude

{- *********************************************************************
*                                                                      *
                 Quick Look overview
*                                                                      *
********************************************************************* -}

{- Note [Quick Look overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The implementation of Quick Look closely follows the QL paper
   A quick look at impredicativity, Serrano et al, ICFP 2020
   https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/

All the moving parts are in this module, GHC.Tc.Gen.App, so named
because it deal with n-ary application.  The main workhorse is tcApp.

Some notes relative to the paper

(QL1) The "instantiation variables" of the paper are ordinary unification
  variables.  We keep track of which variables are instantiation variables
  by giving them a TcLevel of QLInstVar, which is like "infinity".

  See Note [QuickLook instantiation variables] in GHC.Tc.Types.TcType.

(QL2) When we learn what an instantiation variable must be, we simply unify
  it with that type; this is done in qlUnify, which is the function mgu_ql(t1,t2)
  of the paper.  This may fill in a (mutable) instantiation variable with
  a polytype.

(QL3) When QL is done, we turn the instantiation variables into ordinary unification
  variables, using qlZonkTcType.  This function fully zonks the type (thereby
  revealing all the polytypes), and updates any instantiation variables with
  ordinary unification variables. See Note [Instantiation variables are short lived].

(QL4) We cleverly avoid the quadratic cost of QL, alluded to in the paper.
  See Note [Quick Look at value arguments]

Note [Instantiation variables are short lived]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* An instantation variable is a mutable meta-type-variable, whose level number
  is QLInstVar.  See Note [QuickLook instantiation variables] in GHC.Tc.Utils.TcType.

* Ordinary unification variables always stand for monotypes; only instantiation
  variables can be unified with a polytype (by `qlUnify`).

* Before we start typechecking the arguments of the call, in tcValArgs, we will
  (a) Monomorphise any un-filled-in instantiation variables
      (see Note [Monomorphise instantiation variables]),
      This happens in Step 4, at the start of `finishApp`.
  (b) Zonk the argument type to reveal any polytypes before typechecking that
      argument (see calls to `zonkTcType` and "Crucial step" in tcValArg).
  See Section 4.3 "Applications and instantiation" of the paper.

  TL;DR: instantiation variables are short-lived. So it is fine for them
         to have an infinite level (=QLInstVar) because they are monomorphised
         before we do anything like skolem-escape checks.

* The constraint solver never sees an instantiation variable
  [not quite true; see below]

  However, the constraint solver can see a meta-type-variable filled
  in with a polytype (#18987). Suppose
    f :: forall a. Dict a => [a] -> [a]
    xs :: [forall b. b->b]
  and consider the call (f xs).  QL will
  * Instantiate f, with a := kappa, where kappa is an instantiation variable
  * Emit a constraint (Dict kappa), via instantiateSigma, called from tcInstFun
  * Do QL on the argument, to discover kappa := forall b. b->b

  But by the time the third step has happened, the constraint has been emitted
  into the monad.  The constraint solver will later find it, and rewrite it to
  (Dict (forall b. b->b)). That's fine -- the constraint solver does no implicit
  instantiation (which is what makes it so tricky to have foralls hiding inside
  unification variables), so there is no difficulty with allowing those
  filled-in kappa's to persist.  (We could find them and zonk them away, but
  that would cost code and execution time, for no purpose.)

  Since the constraint solver does not do implicit instantiation (as the
  constraint generator does), the fact that a unification variable might stand
  for a polytype does not matter.

* Actually, sadly the constraint solver /can/ see an instantiation variable.
  Consider this from test VisFlag1_ql:
     f :: forall {k} {a :: k} (hk :: forall j. j -> Type). hk a -> ()

     bad_wild :: ()
     bad_wild = f @_ MkV
  In tcInstFun instantiate f with [k:=k0, a:=a0], and then encounter the `@_`,
  expecting it to have kind (forall j. j->Type).  We make a fresh variable (it'll
  be an instantiation variable since we are in tcInstFun) for the `_`, thus
  (_ : k0) and do `checkExpectedKind` to match up `k0` with `forall j. j->Type`.
  The unifier doesn't solve it (it does not unify instantiation variables) so
  it leaves it for the constraint solver.  Yuk.   It's hard to see what to do
  about this, but it seems to do no harm for the constraint solver to see the
  occasional instantiation variable.
-}


{- *********************************************************************
*                                                                      *
              Typechecking n-ary applications
*                                                                      *
********************************************************************* -}

{- Note [Application chains and heads]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quick Look treats application chains specially.  What is an
"application chain"?  See Fig 2, of the QL paper: "A quick look at
impredicativity" (ICFP'20). Here's the syntax:

app ::= head
     |  app expr            -- HsApp: ordinary application
     |  app @type           -- HsTypeApp: VTA
     |  expr `head` expr    -- OpApp: infix applications
     |  ( app )             -- HsPar: parens
     |  {-# PRAGMA #-} app  -- HsPragE: pragmas

head ::= f                -- HsVar:    variables
      |  fld              -- HsRecSel: record field selectors
      |  (expr :: ty)     -- ExprWithTySig: expr with user type sig
      |  lit              -- HsOverLit: overloaded literals

* When tcExpr sees something that starts an application chain (namely,
  any of the constructors in 'head' or in 'app'), it invokes tcApp to
  typecheck it: see Note [tcApp: typechecking applications].  However,
  for HsPar and HsPragE, there is no tcWrapResult (which would
  instantiate types, bypassing Quick Look), so nothing is gained by
  using the application chain route, and we can just recurse to tcExpr.

* If tcExpr sees a constructor belonging to app (except HsPar and HsPargE),
  it does not directly call tcApp, it first splits the expression
  into a maximal application chain using splitHsApps (see `GHC.Tc.Gen.Expr.tcCollectApp`), and obtain
  the head of the application chain and a list of arguments.

* A "head" has three special cases (for which we can infer a polytype
  using tcInferAppHead_maybe); otherwise is just any old expression (for
  which we can infer a rho-type (via runInferExpr).

* There is no special treatment for HsHole (HsVar ...), HsOverLit, etc, because
  we can't get a polytype from them.

* Left and right sections (e.g. (x +) and (+ x)) are not yet supported.
  Probably left sections (x +) would be easy to add, since x is the
  first arg of (+); but right sections are not so easy.  For symmetry
  reasons I've left both unchanged, in GHC.Tc.Gen.Expr.

* It may not be immediately obvious why ExprWithTySig (e::ty) should be
  dealt with by tcApp, even when it is not applied to anything. Consider

      f :: [forall a. a->a] -> Int
      ...(f (undefined :: forall b. b))...

  Clearly this should work!  But it will /only/ work because if we
  instantiate that (forall b. b) impredicatively!  And that only happens
  in tcApp.

Note [tcApp: typechecking applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcApp implements the APP-Downarrow/Uparrow rule of
Fig 3, plus the modification in Fig 5, of the QL paper:
"A quick look at impredicativity" (ICFP'20).

It treats application chains (f e1 @ty e2) specially:

* So we can report errors like "in the third argument of a call of f"

* So we can do Visible Type Application (VTA), for which we must not
  eagerly instantiate the function part of the application.

* So that we can do Quick Look impredicativity.

tcApp accepts 4 arguments:
  1. The original expression
  2. The expression at the head of the application
  3. the argument list (types and terms)
  4. The expected result type

PRECONDITION : the head (2) and the list of arguments (3) will
               are the de-constructred version of the expression (1)
POSTCONDITION: The return expression is the typechecked version of (1)

tcApp works on the application chain by:

1. Using `tcInferAppHead` to infer the type of the function,
     as an (uninstantiated) TcSigmaType
   There are special cases for
     HsVar, HsRecSel, and ExprWithTySig
   Otherwise, delegate back to tcExpr, which
     infers an (instantiated) TcRhoType

   This isn't perfect. Consider this (which uses visible type application):
    (let { f :: forall a. a -> a; f x = x } in f) @Int
   Since 'let' is not among the special cases for tcInferAppHead,
   we'll delegate back to tcExpr, which will instantiate f's type
   and the type application to @Int will fail.  Too bad!

2. Using tcInstFun to instantiate the function, Quick-Looking as we go.  This
   implements the |-inst judgement in Fig 4, plus the modification in Fig 5, of
   the QL paper: "A quick look at impredicativity" (ICFP'20).

   In tcInstFun we take a quick look at value arguments, using quickLookArg.
   See Note [Quick Look at value arguments].

   Crucially, `tcInstFun` ensures that all the unification variables
   it allocates, notably by instantiating the function at the head of the
   application, have level QLInstVar, and hence will be "instantiation
   variables", written using \kappa in the paper.
   See Note [Instantiating type variables in QuickLook]

   Note that we take a quick look at the arguments before quick-looking
   the result type; see Note [QuickLook: arguments before result]

3. In case DoQL, when we are checking, use quickLookResultType to take a
   quick look at the result type.

The next steps are in `finishApp`:

4. In case DoQL:
   4.1  Monomorphise any as-yet-ununified instantiation variables (`qlMonoInstArgs`)
        See Note [Monomorphise instantiation variables]
   4.2  Use `zonkTcType` to expose the polymorphism hidden under instantiation
        variables in `app_res_rho`, and the monomorphic versions of any
        un-unified instantiation variables.

5. Use `checkResultTy` to connect the result type.
   Do this /before/ checking the arguments; see
   Note [Unify with expected type before typechecking arguments]

6.  Check the arguments with `tcValArgs`. Importantly, we must remember to zonk in
    the EValArg case, to expose polymorphism

7. The function `finishApp` mainly calls `rebuildHsApps` to rebuild the
   application; but it also has a horrible special case for `tagToEnum`.

Note [Quick Look for particular Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We switch on Quick Look (regardless of -XImpredicativeTypes) for certain
particular Ids:

* ($): For a long time GHC has had a special typing rule for ($), that
  allows it to type (runST $ foo), which requires impredicative instantiation
  of ($), without language flags.  It's a bit ad-hoc, but it's been that
  way for ages.  Using quickLookKeys is the only special treatment ($) needs
  now, which is a lot better.

* leftSection, rightSection: these are introduced by the expansion step in
  the renamer (Note [Handling overloaded and rebindable constructs] in
  GHC.Rename.Expr), and we want them to be instantiated impredicatively
  so that (f `op`), say, will work OK even if `f` is higher rank.
  See Note [Left and right sections] in GHC.Rename.Expr.

Note [QuickLook: arguments before result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally we try to push the expected result type into the call;
see Note [Unify with expected type before typechecking arguments].
But surprisingly we can't do that for QuickLook.  (#26599 was a failed
attempt to suggest this.)  Consider
   h :: (Int->Int) -> Int    ids :: [forall a. a->a]
and the call
   h (head ids)
Let's say we instantiate `head :: [a] -> a` with `a:=kappa`.
We QuickLook at the argument with expected type (Int->Int). We must NOT
unify kappa:=(Int->Int), because kappa:=forall a. a->a is the only
solution, and one that we will find if we look at the argument first.

TL;DR: we must do `quickLookArg` on all arguments before doing `quickLookResultType`
on the result type.

Remember: `quickLookResultType` is really anticipating a /subsumption/ test, not
an equality test.  Doing `qlUnify` on the result type (always a rho-type) is
in general wrong if there is an un-guarded kappa in the result type.  It's wrong
UNLESS we have already gotten all the info about `kappa` (from the
arguments) that we are going to get.  That's why we QuickLook the arguments first.

This is a bit sad, as #26599 shows.  Consider

  woo :: (x, [forall a. a->a])
  bar1 = snd woo
  bar2 = snd (id woo)

For `bar1` we instantiate `snd` at [a:=k1,b:=k2].  Then we QL on the argument `woo`
(instantiating with `x:=k3`).  Since the result type is guarded we qlUnify
    (k3,forall a.a->a) ~ (k1,k2)
and thus succeed in discovering k2:=forall a.a->a, as desired.

But for `bar2` we fail. When quick-looking (id woo) we instantiate `id` with [a:=k],
and quick-look the argument `woo` with expected type `k`.  That's not guarded,
so we get no information.  Adding the `id` call makes it un-typeable. Boo.

This is a bit tricky.
* We QuickLook the arguments (tcInstFun)
     THEN QuickLook the result type (quickLookResultType).
  See this Note
* and then...
* We typecheck the result type (checkResultTy)
     THEN typecheck the arguments (tcValArgs)
  see Note [Unify with expected type before typechecking arguments]

Note [Unify with expected type before typechecking arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#19364)
  data Pair a b = Pair a b
  baz :: MkPair Int Bool
  baz = MkPair "yes" "no"

We instantiate MkPair with `alpha`, `beta`, and push its argument
types (`alpha` and `beta`) into the arguments ("yes" and "no").
But if we first unify the result type (Pair alpha beta) with the expected
type (Pair Int Bool) we will push the much more informative types
`Int` and `Bool` into the arguments.   This makes a difference:

Unify result type /after/ typechecking the args
    • Couldn't match type ‘[Char]’ with ‘Bool’
      Expected type: Pair Foo Bar
        Actual type: Pair [Char] [Char]
    • In the expression: Pair "yes" "no"

Unify result type /before/ typechecking the args
    • Couldn't match type ‘[Char]’ with ‘Bool’
      Expected: Foo
        Actual: String
    • In the first argument of ‘Pair’, namely ‘"yes"’

The latter is much better. That is why we call `checkResultTy` before `tcValArgs`.
-}

--------------------
tcApp :: HsExpr GhcRn        -- ^ The whole application (For error messages)
      -> HsExpr GhcRn        -- ^ The Function head
      -> [HsExprArg 'TcpRn]  -- ^ The list of arguments
      -> ExpRhoType          -- ^ When checking, -XDeepSubsumption <=> deeply skolemised
      -> TcM (HsExpr GhcTc)
-- See Note [tcApp: typechecking applications]
-- See Note [splitHsApps] in GHC.Tc.Head
tcApp rn_expr rn_fun rn_args exp_res_ty
  = do { wanted_ql <- wantQuickLook rn_fun
       ; fun_lspan <- getFunSrcSpan rn_args
       ; traceTc "tcApp {" $
           vcat [ text "rn_fun:" <+> ppr rn_fun
                , text "fun_lspan:" <+> ppr fun_lspan
                , text "rn_args:" <+> ppr rn_args ]


       -- Step 1: Infer the type of `fun`, the head of the application
       ; let rn_head = (rn_fun, fun_lspan)
       ; (tc_fun0, fun_sigma0) <- tcInferAppHead rn_head
       -- See Note [Imported native application contracts].
       ; native_app <- tcRetainedAppCapability fun_sigma0 exp_res_ty rn_args
       -- See Note [Known native variables with ImpredicativeTypes].
       ; head_ql <- if any isExplicitRetainedArg rn_args
                    then return NoQL
                    else tcRetainedKnownHeadQL native_app wanted_ql rn_args exp_res_ty tc_fun0 fun_sigma0
       ; tcRetainedAppEvidence native_app $
         do { (prefix, prepared_ql, prepared_sigma, remaining_args, prepared_plan) <-
                tcRetainedGroundApp native_app head_ql exp_res_ty rn_head tc_fun0 fun_sigma0 rn_args
            ; let infer_args = null prefix && isNothing prepared_plan
            ; (tc_fun1, fun_sigma1) <-
                if infer_args
                then tcRetainedExpectedInst prepared_ql exp_res_ty remaining_args
                                            tc_fun0 prepared_sigma
                else return (tc_fun0, prepared_sigma)
            ; (tc_fun2, fun_sigma2, initial_cache, inst_plan) <-
                if infer_args
                then tcRetainedArgInst native_app prepared_ql exp_res_ty rn_head remaining_args
                                      tc_fun1 fun_sigma1
                else return (tc_fun1, fun_sigma1, [], prepared_plan)
            ; (dollar_inst, dollar_cache) <-
                if infer_args
                then tcRetainedDollarInst native_app exp_res_ty rn_head remaining_args tc_fun2 fun_sigma2
                else return (Nothing, [])
            ; let (tc_fun, fun_sigma) = fromMaybe (tc_fun2, fun_sigma2) dollar_inst
                  arg_cache = initial_cache ++ dollar_cache
                  do_ql | null dollar_cache = prepared_ql
                        | otherwise = NoQL
                  expected_rho = case exp_res_ty of
                                   Check rho -> Just rho
                                   Infer {}  -> Nothing
                  -- Top-instantiate the result unless inferring a sigma type.
                  inst_final = case exp_res_ty of
                                 Check {} -> True
                                 Infer (IR {ir_inst=iif}) ->
                                   case iif of
                                     IIF_ShallowRho -> True
                                     IIF_DeepRho    -> True
                                     IIF_Sigma      -> False
            ; traceTc "tcApp:inferAppHead" $
                vcat [ text "tc_fun:" <+> ppr tc_fun
                     , text "fun_sigma:" <+> ppr fun_sigma
                     , text "do_ql:" <+> ppr do_ql ]
            ; -- Step 2: Instantiate the function type, then take a quick look
              -- at value arguments. Preserve the checked leading prefix.
              -- See Note [Quick Look for particular Ids].
              vta_cache <- case do_ql of
                NoQL | native_app, not (null [() | ETypeArg{} <- rn_args])
                  -> Just <$> newTcRef Nothing
                _ -> return Nothing
            ; (inst_args, app_res_rho) <-
                tcInstFunWithPrefix False prefix vta_cache expected_rho inst_plan
                  do_ql inst_final rn_head tc_fun fun_sigma remaining_args
            ; extra_cache <- case vta_cache of
                Nothing -> return []
                Just ref -> fromMaybe [] <$> readTcRef ref
            ; -- Step 3: Take a quick look at the result type.
              -- See Note [QuickLook: arguments before result].
              case (do_ql, exp_res_ty) of
                (DoQL, Check exp_rho) -> quickLookResultType app_res_rho exp_rho
                _                    -> return ()
            ; finishAppWithCache (arg_cache ++ extra_cache) do_ql rn_expr fun_lspan
                rn_fun tc_fun inst_args app_res_rho exp_res_ty } }

-- | Select native application checking from the flag or existing type contracts.
-- See Note [Imported native application contracts].
tcRetainedAppCapability :: TcSigmaType -> ExpRhoType -> [HsExprArg 'TcpRn]
                        -> TcM Bool
tcRetainedAppCapability sigma expected args
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; if enabled then return True else
    do { sigma <- liftZonkM (zonkTcType sigma)
       ; expected <- readExpType_maybe expected
       ; expected <- mapM (liftZonkM . zonkTcType) expected
       ; if any (hasNativeDependentExpr . Core.Type) (sigma : maybeToList expected)
         then return True else
    do { shapes <- mapM tcRetainedInferableArg [arg | EValArg { ea_arg = arg } <- args]
       ; return (any snd shapes) } } }

{- Note [Imported native application contracts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An imported type can contain a retained index when the client does not enable
ExperimentalDependentTypes. For example, toList consumes Vec a n through an
erased n. An argument with the declared type Vec Int n supplies that choice.
The client flag must not remove the inference needed to use this contract.

Compute one capability for the application. Enable it when the source flag is
set, when the zonked head or an available expected type contains native syntax,
or when tcRetainedInferableArg finds a declared native value anchor. This
selection only reads existing types. It does not infer arguments, instantiate
a signature, record occurrences or usage, or expand source syntax. Read an
available expected type without allocating an inference cell.

Pass the capability to the existing application helpers and evidence capture.
Keep their Quick Look modes, complete immutable choices, scope checks, typed
argument caches and final subsumption. A false capability keeps the original
ordinary path. It does not disable checks triggered by an actual DepFunTy.

A checked callback type can establish the same contract at tcValArg. The
guarded paths in tcRetainedCallbackArgs also capture evidence at each callback
instantiation or result check. This change does not enable source type syntax,
container inference or dependent case refinement. It does not assign a native
type to a mutable metavariable. See Note [Retained application evidence].
-}

-- | Prepare a checked native application without inspecting value arguments.
-- See Note [Ground retained applications].
tcRetainedGroundApp :: Bool -> QLFlag -> ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                    -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn]
                    -> TcM ([HsExprArg 'TcpInst], QLFlag, TcSigmaType,
                            [HsExprArg 'TcpRn], RetainedInstPlan)
tcRetainedGroundApp True DoQL exp_ty rn_head fun sigma args
  | known_head fun
  = do { let (leading, rest) = span prefix_arg args
       ; (prefix, residual, remaining) <-
           if any type_arg leading
           then do { (checked, ty) <- tcInstFunWithPrefix True [] Nothing Nothing
                                       Nothing DoQL False rn_head fun sigma leading
                   ; return (checked, ty, rest) }
           else return ([], sigma, args)
       ; expected <- readExpType_maybe exp_ty
       ; expected <- mapM (liftZonkM . zonkTcType) expected
       ; residual <- liftZonkM (zonkTcType residual)
       ; let keep_ql = return (prefix, DoQL, residual, remaining, Nothing)
             declared_plan =
               do { plan <- tcRetainedKnownArgPlan expected fun residual remaining
                  ; case plan of
                      Just _ -> return (prefix, NoQL, residual, remaining, plan)
                      Nothing -> keep_ql }
       ; prefix_metadata <- retainedPrefixTypes prefix
       ; case prefix_metadata of
           Nothing -> keep_ql
           Just prefix_tys ->
            do { prefix_tys <- liftZonkM (zonkTcTypes prefix_tys)
               ; let tys = residual : prefix_tys ++ maybeToList expected
               ; valid_scope <- tcRetainedInstScope tys
               ; if not (valid_scope && all no_ql_vars tys
                         && maybe True isRetainedValueType expected)
                 then keep_ql else
                 if not (any (hasNativeDependentExpr . Core.Type) tys)
                 then declared_plan else
            do { let (tvs, _, tau) = tcRetainedInstTelescope residual
               ; if null tvs && isRetainedValueType tau
                 then return (prefix, NoQL, residual, remaining, Nothing)
                 else do
                   { matched <- tcRetainedResultInst NoQL expected remaining fun residual
                   ; case matched of
                       Just (choices, body)
                         | let (remaining_tvs, _, body_tau) = tcRetainedInstTelescope body
                         , null remaining_tvs, isRetainedValueType body_tau
                         -> do { choices <- liftZonkM (zonkTcTypes choices)
                               ; body <- liftZonkM (zonkTcType body)
                               ; valid_choices <- tcRetainedInstScope (body : choices)
                               ; if valid_choices && all no_ql_vars (body : choices)
                                 then return (prefix ++ [EWrap (EHsWrap (mkWpTyApps choices))],
                                              NoQL, body, remaining, Nothing)
                                 else keep_ql }
                       _ -> declared_plan } } } }
  where
    known_head HsVar{} = True
    known_head (XExpr (ConLikeTc (RealDataCon _))) = True
    known_head _ = False
    prefix_arg ETypeArg{} = True
    prefix_arg EPrag{} = True
    prefix_arg EWrap{} = True
    prefix_arg _ = False
    type_arg ETypeArg{} = True
    type_arg _ = False
    no_ql_vars = not . any is_ql . tyCoVarsOfTypeList
    is_ql v = isTcTyVar v && isQLInstTyVar v
tcRetainedGroundApp _ ql _ _ _ sigma args = return ([], ql, sigma, args, Nothing)

-- | Collect the types that can retain Quick Look choices in a checked prefix.
-- Unrecognised evidence and wrappers keep the original Quick Look path.
retainedPrefixTypes :: [HsExprArg 'TcpInst] -> TcM (Maybe [TcType])
retainedPrefixTypes args
  = fmap (fmap concat . sequence) (mapM arg_types args)
  where
    value_type ty
      = do { ty <- liftZonkM (zonkTcType ty)
           ; return $ if hasNativeDependentExpr (Core.Type ty)
                         && not (isRetainedValueType ty)
                      then Nothing else Just [ty] }
    arg_types (ETypeArg { ea_ty_arg = ty }) = value_type ty
    arg_types (EWrap (EHsWrap wrap)) = wrap_types wrap
    arg_types (EWrap (EPar _)) = return (Just [])
    arg_types (EWrap (EExpand _ _)) = return (Just [])
    arg_types EPrag{} = return (Just [])
    arg_types _ = return Nothing
    wrap_types WpHole = return (Just [])
    wrap_types (WpCompose first second)
      = do { first_tys <- wrap_types first
           ; second_tys <- wrap_types second
           ; return ((++) <$> first_tys <*> second_tys) }
    wrap_types (WpTyApp ty) = value_type ty
    wrap_types (WpEvApp (EvExpr expr))
      = return (Just (concatMap var_types (exprFreeVarsList expr)))
    wrap_types _ = return Nothing
    var_types v | isTyVar v = [mkTyVarTy v, varType v]
                | otherwise = [varType v]

-- | Select erased arguments from declared monomorphic values without usage.
-- See Note [Declared native argument choices].
tcRetainedKnownArgPlan :: Maybe TcRhoType -> HsExpr GhcTc -> TcSigmaType
                      -> [HsExprArg 'TcpRn] -> TcM RetainedInstPlan
tcRetainedKnownArgPlan expected fun sigma args
  | hasNoConcreteTyVars fun
  , not (any is_type_arg args)
  , (tvs, _, tau) <- tcRetainedInstTelescope sigma
  , not (null tvs), isRetainedValueType tau
  , let values = [arg | EValArg { ea_arg = arg } <- args]
  , not (null values)
  = case coreFullView tau of
      DepFunTy DepFunInvisible binder body ->
        fmap (fmap snd) (tcInferInvisibleRetained tvs binder body values)
      _ -> choose_erased tvs values (retainedDeclaredDomains (length values) tau)
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    choose_erased tvs values (domains, result)
      = do { pairs <- fmap catMaybes $ mapM (declared_arg tvs) (zip domains values)
         ; let actuals = map snd pairs
               seed | length domains == length values
                    , Just result_ty <- result
                    , Just rho <- expected
                    = fromMaybe Subst.emptySubst $
                        matchRetainedAppType tvs Subst.emptySubst result_ty rho
                    | otherwise = Subst.emptySubst
               match subst = foldM (\s (formal, actual) ->
                                      matchRetainedAppType tvs s formal actual)
                                   subst pairs
               matched = case match seed of
                           Nothing -> match Subst.emptySubst
                           other -> other
         ; case matched of
             Just subst
               | not (null pairs)
               , any (hasNativeDependentExpr . Core.Type)
                     (sigma : actuals ++ maybeToList expected)
               , Just choices <- traverse (Subst.lookupTyVar subst) tvs
               , not (any (`elemVarSet` tyCoVarsOfTypes choices) tvs)
               , and (zipWith (\tv ty ->
                        eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                      tvs choices)
               , all (\(formal, actual) -> eqType (Subst.substTy subst formal) actual) pairs
               , all noQuickLookVars (sigma : actuals ++ choices ++ maybeToList expected)
               -> do { valid <- tcRetainedInstScope (sigma : actuals ++ choices ++ maybeToList expected)
                     ; return (if valid then Just choices else Nothing) }
             _ -> return Nothing }

    declared_arg tvs (Just (retained, formal), arg)
      | any (`elemVarSet` tyCoVarsOfType formal) tvs
      = do { actual <- if retained then tcRetainedConstructorArgType arg
                         else tcRetainedDeclaredArgType tcRetainedDeclaredTypeView arg
           ; return ((formal,) <$> actual) }
      | otherwise = return Nothing
    declared_arg _ _ = return Nothing

tcRetainedKnownArgPlan _ _ _ _ = return Nothing

-- | Read a monomorphic declaration for immutable retained argument matching.
-- See Note [Declared retained-domain choices].
tcRetainedDeclaredArgType :: (Type -> TcM Type) -> LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedDeclaredArgType view (L _ (HsPar _ arg)) = tcRetainedDeclaredArgType view arg
tcRetainedDeclaredArgType view (L _ (ExplicitList _ fields))
  = fmap (fmap mkListTy) (tcRetainedListElementType (tcRetainedDeclaredArgType view) fields)
tcRetainedDeclaredArgType view_type (L loc (HsVar _ lname))
  | not (isDataOcc (nameOccName (unLocWithUserRdr lname)))
  = setSrcSpan (locA loc) $
    do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
       ; case mb_id of
           Just var -> do
             { ty <- liftZonkM (zonkTcType (varType var))
             ; valid <- tcRetainedInstScope [ty]
             ; let (tvs, theta, _) = tcRetainedInstTelescope ty
             ; if valid && null tvs && null theta && isRetainedValueType ty
                        && noQuickLookVars ty
               then do { view <- view_type ty
                       ; return (if isRetainedValueType view && noQuickLookVars view
                                 then Just view else Nothing) }
               else return Nothing }
           Nothing -> return Nothing }
tcRetainedDeclaredArgType _ _ = return Nothing

-- | Read a visible retained argument's type from its constructor fields.
-- See Note [Erased parameters of a retained constructor argument].
tcRetainedConstructorArgType :: LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedConstructorArgType = collect []
  where
    collect args (L _ (HsPar _ fun)) = collect args fun
    collect args (L _ (HsApp _ fun arg)) = collect (arg : args) fun
    collect [] (L _ (OpApp _ lhs op rhs)) = collect [lhs, rhs] op
    collect [] (L _ (ExplicitList _ fields))
      = fmap (fmap mkListTy) (firstJustsM (map tcRetainedConstructorArgType fields))
    collect args (L loc (HsVar _ lname))
      | let name = unLocWithUserRdr lname
      , isDataOcc (nameOccName name)
      = setSrcSpanA loc $ do
          { thing <- tcLookupGlobal name
          ; case thing of
              AConLike (RealDataCon dc)
                | (tvs, [], [], [], fields, result) <- dataConFullSig dc
                , fields `equalLength` args
                , isJust (nativeIndexDataConTyArgs result dc)
                -> do { subst <- foldM (match_field tvs) Subst.emptySubst (zip fields args)
                      ; return $ do
                          { choices <- traverse (Subst.lookupTyVar subst) tvs
                          ; let ty = substTyWith tvs choices result
                          ; guard (isNativeIndexType ty)
                          ; return ty } }
              _ -> return Nothing }
    collect [] arg = tcRetainedDeclaredArgType tcRetainedDeclaredTypeView arg
    collect _ _ = return Nothing

    match_field tvs subst (Scaled _ formal, arg)
      | any (\tv -> isNothing (Subst.lookupTyVar subst tv)
                  && tv `elemVarSet` tyCoVarsOfType formal) tvs
      = do { actual <- tcRetainedConstructorArgType arg
           ; return (fromMaybe subst (actual >>= matchRetainedAppType tvs subst formal)) }
      | otherwise = return subst

{- Note [Erased parameters of a retained constructor argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In sourceVector (x : xs), the type of x supplies the list element type.
This can be Vec Bool n for a runtime binder n. Select the erased argument
before IALL creates a metavariable that cannot mention n.

Read the declared types of fields of saturated, eligible index constructors.
Resolve the constructor by its renamed Name. Match its original field types
in source order. A field contributes only while it can determine a missing
parameter. For Twin x y z with fields a, a, b, the declaration of x can fix
a. The ordinary argument checker then reports a mismatch at y. A nullary
constructor such as False determines Bool. A polymorphic nullary constructor
cannot supply its missing parameter. Numeric literals require a known domain.

This pass reads declarations without checking occurrences. The enclosing
selector validates the scope and kinds of the complete choice. tcRetainedArg
checks the actual constructor tree at that domain. Value checking accounts
for field usage. See Note [Declared retained-domain choices].
-}

-- | Test whether a type is independent of Quick Look instantiation variables.
noQuickLookVars :: Type -> Bool
noQuickLookVars = not . any (\v -> isTcTyVar v && isQLInstTyVar v) . tyCoVarsOfTypeList

-- | Select one invisible value operand and any outer erased parameters.
-- See Note [Inferring an invisible retained argument].
tcInferInvisibleRetained :: [TyVar] -> Id -> Type -> [LHsExpr GhcRn]
                         -> TcM (Maybe (InvisibleOperand, [Type]))
tcInferInvisibleRetained tvs binder body args
  = do { name <- newSysName (mkTyVarOcc "retained_argument")
       ; let template = mkTyVar name (varType binder)
       ; (indices, suffix) <- collect [(TermVarTy binder, template)] body
       ; let (domains, _) = retainedDeclaredDomains (length args) suffix
             choose view = do
               { pairs <- catMaybes <$> mapM (declared_pair view indices) (zip domains args)
               ; let determines (formal, _) = template `elemVarSet` injectiveVarsOfType True formal
                     anchors | null tvs  = filter determines pairs
                             | otherwise = pairs  -- Erased argument prepass
               ; select template (map snd indices ++ tvs) anchors }
       ; givens <- tcl_native_givens . tcl_lcl_ctxt <$> getLclEnv
       ; selected <- choose return
       ; case selected of
           Nothing | not (null givens) -> choose tcRetainedDeclaredTypeView
           _ -> return selected }
  where
    collect indices ty
      | DepFunTy DepFunInvisible next rest <- coreFullView ty
      , Just kind <- abstractNativeIndices indices (varType next)
      = do { name <- newSysName (mkTyVarOcc "retained_argument")
           ; collect ((TermVarTy next, mkTyVar name kind) : indices) rest }
      | otherwise = return (indices, ty)

    select template variables pairs
      = case foldM match Subst.emptySubst pairs of
          Just subst
            | Just index <- Subst.lookupTyVar subst template
            , Just choices <- traverse (Subst.lookupTyVar subst) tvs
            , all noQuickLookVars (index : choices)
            , not (any (`elemVarSet` tyCoVarsOfTypes (index : choices)) variables)
            , typeKind index `eqType` Subst.substTy subst (varType binder)
            , and (zipWith (\tv ty ->
                     eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                   tvs choices)
            , all (\(formal, actual) -> eqType (Subst.substTy subst formal) actual) pairs
            -> do { valid <- tcRetainedInstScope (index : choices)
                  ; if valid then fmap (fmap (, choices)) (tcInvisibleOperand index)
                             else return Nothing }
          _ -> return Nothing
      where
        match subst (formal, actual) = matchRetainedAppType variables subst formal actual

    declared_pair view indices (Just (_, formal), arg)
      | any (`elemVarSet` tyCoVarsOfType formal) (binder : tvs)
      , Just abstract <- abstractNativeIndices indices formal
      = do { actual <- tcRetainedDeclaredArgType view arg
           ; return ((abstract,) <$> actual) }
    declared_pair _ _ _ = return Nothing

-- | The inferred value and the usage to record when it is inserted.
data InvisibleOperand = InvisibleOperand Core.CoreExpr (TcM ())

-- | Recover a runtime operand and its term and constructor uses.
-- See Note [Inferring an invisible retained argument].
tcInvisibleOperand :: Type -> TcM (Maybe InvisibleOperand)
tcInvisibleOperand index
  = do { rdr_env <- getGlobalRdrEnv
       ; candidate <- recover rdr_env index
       ; return $ do { (operand, uses) <- candidate
                     ; recovered <- Core.exprToNativeIndex operand
                     ; guard (eqType recovered index)
                     ; return (InvisibleOperand operand uses) } }
  where
    recover _ (TermVarTy occurrence)
      = do { thing <- tcLookupLcl_maybe (getName occurrence)
           ; case thing of
               Just (ATcId { tct_id = actual }) ->
                 do { actual <- liftZonkM (zonkId actual)
                    ; return $ do { guard (occurrence == actual && isNativeOpaqueValueId actual
                                          && varType occurrence `eqType` varType actual)
                                  ; return (Core.Var actual, tcEmitBindingUsage (singleUsageUE actual)) } }
               _ -> return Nothing }
    recover env ty@(TyConApp tc args)
      | Just con <- isPromotedDataCon_maybe tc
      , not (isTypeDataTyCon (dataConTyCon con))
      , Just gre <- lookupGRE_Name env (dataConName con)
      , Just tys <- nativeIndexDataConTyArgs (typeKind ty) con
      = do { fields <- mapM (recover env) (drop (length tys) args)
           ; return $ do { (values, uses) <- unzip <$> sequence fields
                         ; let worker = Core.mkTyApps (Core.Var (dataConWorkId con)) tys
                         ; return (Core.mkApps worker values,
                                   addUsedGRE NoDeprecationWarnings gre >> sequence_ uses) } }
    recover _ _ = return Nothing

{- Note [Inferring an invisible retained argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a signature such as

  f :: forall a. foreach (n :: Nat). Vec a n -> Int

the declaration of xs in f xs can determine a := Int and n := the local
runtime Id used in Vec Int n. Replace the formal index with an immutable
template. Match the declared argument domains together. Require an exact
match and a complete choice for the erased parameters. Recheck the runtime
Id in the actual lexical environment. Treat an unavailable declaration as
no advice. The normal argument checker still checks the written argument.

Use this match before IALL can allocate native metavariable candidates.
When the invisible binder is reached, match its instantiated type and
insert the same runtime operand. Keep the written argument and its position
for the usual cache and diagnostics. Emit usage once at Many and let the
remaining application instantiate constraints with the chosen local Id.
See Note [Retained application evidence].

For appendHidden, the two consecutive invisible Nat binders precede
Vec a n and Vec a m. Abstract each binder with a fresh template. Abstract
earlier indices in the kind of each later template. Read the domains after
the group. Select only the current operand and outer erased parameters.
The next application step sees the substituted type and checks the next
operand in its own scope. A written operand also enters through that step.

When inserting a runtime operand, match only domains with an injective
occurrence of its template. For example, Vec Int (Forget n) cannot select n
when Forget maps every Nat to Z. A later Vec Int n can supply it. Check the
Forget domain at its written argument after substitution. The erased
argument prepass may use all domain matches to select its type arguments.
It discards the speculative runtime operand. This distinction also lets
an explicit operand select the runtime value.

Prefer the original declaration for runtime identity. A constructor Given
can refine the index n in Vec a n to Z. The caller can still pass its local
n. Retry with the existing refined declaration view when raw matching fails
in a context with Givens. A generated branch capture can already have the
refined declaration Vec a (S pred). Rebuild the constructor application
from its promoted form. Require term visibility for each constructor and
resolve each field variable in its lexical environment. The Core index
judgment must recover the selected index from the complete operand.
Only term fields contribute occurrences to runtime usage. Record the
constructor imports when inserting the selected operand, after the erased
argument prepass. See wrinkle (DTW2) of Note [DataToTag overview] in
GHC.Tc.Instance.Class.

Agda inserts hidden argument terms from the expected telescope. Here the
immutable match supplies a checked runtime occurrence. It leaves stuck
families and opaque calls alone. See the pinned Agda sources in
docs/dependent-haskell/REFERENCES.md#invisible-retained-binders-next-candidate.
-}

-- | Find a declared native element type without checking the literal.
-- See Note [Declared retained-domain choices] and Note [Native list anchors].
tcRetainedListElementType :: (LHsExpr GhcRn -> TcM (Maybe Type))
                          -> [LHsExpr GhcRn] -> TcM (Maybe Type)
tcRetainedListElementType _ [] = return Nothing
tcRetainedListElementType declared (L _ (HsPar _ field) : fields)
  = tcRetainedListElementType declared (field : fields)
tcRetainedListElementType declared (field@(L _ HsVar{}) : fields)
  = do { actual <- declared field
       ; case actual of
           Just ty | hasNativeDependentExpr (Core.Type ty)
                   , isTauTy ty
                   , typeKind ty `eqType` liftedTypeKind
             -> return (Just ty)
           _ -> tcRetainedListElementType declared fields }
tcRetainedListElementType declared (_ : fields)
  = tcRetainedListElementType declared fields

-- | Refine a declared argument type with actual lexical constructor Givens.
-- Keep the argument Id and its stored type unchanged.
-- See Note [Declared retained argument refinement].
tcRetainedDeclaredTypeView :: Type -> TcM Type
tcRetainedDeclaredTypeView ty
  = do { givens <- tcl_native_givens . tcl_lcl_ctxt <$> getLclEnv
       ; givens <- mapM (liftZonkM . zonkId) givens
       ; foldM refine ty (reverse givens) }
  where
    refine ty given
      | isCoVar given
      , let co = mkCoVarCo given
      , coercionRole co == Nominal
      , TermVarTy index <- coercionLKind co
      , isNativeIndexId index
      , TyConApp tc _ <- coreFullView (coercionRKind co)
      , Just con <- isPromotedDataCon_maybe tc
      , Just _ <- nativeIndexDataConTyArgs (varType index) con
      , typeKind (coercionRKind co) `eqType` varType index
      , anyFreeVarsOfType (== index) ty
      = do { name <- newSysName (mkTyVarOcc "retained_view")
           ; let template = mkTyVar name (varType index)
           ; case abstractNativeIndices [(TermVarTy index, template)] ty of
               Nothing -> return ty
               Just _ -> coercionRKind <$> tcRetainedCoercion index co ty }
      | otherwise = return ty

{- Note [Declared retained argument refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor branch can have retained ~# S k while a declared argument keeps
its original type [Vec Int retained]. Select the erased element parameter of
genericVector from the proof-backed view [Vec Int (S k)]. Otherwise Count at
the original and refined element types can block result checking before the
ordinary solver checks the argument.

Use only the actual lexical evidence parameters in tcl_native_givens. This
list is newest first. Apply checked nominal constructor proofs from outer to
inner scope. The existing tcRetainedCoercion supplies the type view. Check the
original declaration's runtime scope first. Keep its Id and stored type.
First probe the existing abstractNativeIndices grammar with a fresh immutable
template. Unsupported input casts or coercion types keep the current type.
This optional selection must not turn an unavailable view into a diagnostic.
After a successful probe, tcRetainedCoercion still reports any real error.
The ordinary argument checker must still prove the domain conversion and cast
the actual occurrence. The view is a type-argument choice, not a replacement
argument or equality evidence. Final result subsumption still runs.

A generated retained binder can be absent from the source name environment
until desugaring installs its AsPat. Its exact prototype occurs free in the
predicate of an actual lexical Given. tcRetainedInstScope can use that
prototype only when no normal binding exists. It must not obtain scope from
the candidate type itself. A present normal binding remains authoritative,
even when its metadata or multiplicity fails the check. CoVars are evidence,
not runtime values; ordinary coercion scope checking remains responsible for
them. See Note [Native index bindings during typechecking].

Keep free-variable prototypes separately for each Given predicate. Require a
nonempty set of matching prototypes and check all their metadata. The standard
FV list deduplicates within one predicate. Therefore also use the existing
deep anyFreeVarsOfType traversal to reject every incompatible occurrence,
including occurrences in variable types. That traversal does not deduplicate
occurrences. This check assumes canonical finite typechecked predicates and
Core's NoTypeShadowing invariant. It does not validate arbitrary cyclic or
malformed type graphs. See Note [Computing deep free variables] in
GHC.Core.TyCo.FVs.
-}

-- | Collect declared domains for the written value-argument prefix.
-- Mark visible retained domains with True. Keep dependent positions with Nothing.
-- See Note [Declared retained-domain choices].
retainedDeclaredDomains :: Int -> Type -> ([Maybe (Bool, Type)], Maybe Type)
retainedDeclaredDomains supplied = go supplied emptyVarSet
  where
    go 0 bound result = result_view bound result
    go remaining bound ty
      | Just ty' <- coreView ty = go remaining bound ty'
    go _ _ (DepFunTy DepFunInvisible _ _) = ([], Nothing)
    go remaining bound (DepFunTy DepFunVisible binder body)
      = let domain = independent bound True (varType binder)
            (rest, result) = go (remaining - 1) (extendVarSet bound binder) body
        in (domain : rest, result)
    go remaining bound (FunTy af _ _ arg res)
      | isVisibleFunArg af
      = let (rest, result) = go (remaining - 1) bound res
        in (independent bound False arg : rest, result)
      | otherwise = go remaining bound res
    go _ bound result = result_view bound result

    result_view bound result
      = ([], if mentions bound result then Nothing else Just result)

    independent bound retained domain
      | mentions bound domain = Nothing
      | otherwise = Just (retained, domain)
    mentions bound = anyFreeVarsOfType (`elemVarSet` bound)

{- Note [Declared retained-domain choices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For genericVector :: forall a. foreach (xs :: [a]) -> Vec a (Count xs),
a declared argument xs0 :: [Vec Int n] determines a := Vec Int n. Select it
before ordinary instantiation creates a meta that cannot be filled with n
inside the retained application implication. Read the declaration without
checking the occurrence or consuming its usage. The complete immutable IALL
plan and its existing kind, exact-domain, Quick Look and lexical-scope checks
remain authoritative.

A built-in list literal such as [vector] can also supply [Vec Int n] from a
field's declared type. Read the first bare or parenthesized variable field
whose declared view is native, monomorphic and lifted. Form its list type
without inferring the literal or checking any occurrence. Skip empty lists,
fields without such a declaration, polymorphic fields and checking-only forms.
Applications, splices and nested literals cannot supply this advice. The
ordinary argument checker must still check the literal and every element at
the selected type and record their usage. This advice only selects complete
outer erased arguments; it adds no type equality or mutable meta assignment.

Include a retained binder's own domain, which is outside that binder's scope.
Do not match a later domain or result that mentions a crossed runtime binder.
Such a match would need the actual runtime argument, which this declaration
scan does not select. Keep a skipped domain's position for the source argument
alignment. Original argument checking still resolves every actual index and
checks the substituted domain. No runtime Id or metadata-only index is invented.

Stop after the written value-argument prefix. A remaining retained telescope
can be the result value. For example, withScoped Refl f can return ScopedQ m
while f has type ScopedQ n. The expected result selects index := m, and the
declared f selects value := ScopedQ n. Keep that complete candidate. The
ordinary Refl and result checks must diagnose the different scoped indices.
Do not count the returned ScopedQ telescope as more supplied arguments.

Run the declaration selector at the NoQL prepass and after VTA too. Reading
types and zonking filled assignments allocates no native meta. Failed or
incomplete selection uses the existing cached inference path. The original
signature, evidence order, and final result subsumption remain unchanged.
-}

-- | Match kinds and types with the same outer erased binder substitution.
-- See Note [Declared native argument choices] and
-- Note [Native worker argument inference].
matchRetainedAppType :: [TyVar] -> Subst.Subst -> Type -> Type -> Maybe Subst.Subst
matchRetainedAppType tvs subst formal actual
  = do { subst <- tcMatchRetainedTyX_BM bind_outer subst (typeKind formal) (typeKind actual)
       ; tcMatchRetainedTyX_BM bind_outer subst formal actual }
  where
    bind_outer tv _ | tv `elem` tvs = BindMe
                   | otherwise = DontBindMe

{- Note [Declared native argument choices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In withQ Refl f, the declared monomorphic type of f can determine value := QPi.
Reading this declaration does not check the occurrence or consume its usage.
Do not call tcInferId: it emits usage. Global lookup may load an interface.
Visible retained domains can also use constructor fields. See
Note [Erased parameters of a retained constructor argument]. Look through
parentheses without expanding source syntax. A built-in list literal can
use a declared native variable field as element advice. See Note [Declared retained-domain choices].

Match known types against declared independent visible argument domains.
Include retained domains without choosing their runtime arguments. See
Note [Declared retained-domain choices]. Match kinds before types and bind
only the outer erased variables. Require every choice,
correct substituted kinds, exact matched domains and valid runtime scope.
Reject free Quick Look variables, including those in kinds and coercions.
Use an expected seed only after collecting all written values, with an
independent remaining result. Retry a failed seeded match
independently from the empty substitution. Conflicts retain the guarded path.

Return the choices as an IALL plan. Keep the original signature and checked
VTA prefix. IALL consumes each forall/context group in its original order.
Do not infer arguments or create a cache before installing this plan. Refl
then checks at its actual expected proof type. All arguments retain ordinary
subsumption and scaled usage; all constraints and final result checks remain.
A linear value may supply a type. Only runtime indices inside that type must
meet the unrestricted native-index scope rule. No native meta is assigned.

Quick Look's h-var reads a declared type and its argument advice precedes full
checking. Agda checks each domain under the substituted telescope and retains
the checked prefix. This route uses complete immutable choices, not their
mutable instantiation rules. See docs/dependent-haskell/REFERENCES.md.
-}

{- Note [Ground retained applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ImpredicativeTypes enables Quick Look even when a checked application does
not need impredicative inference. A known worker can have all its native
parameters fixed by explicit type arguments. A proof constructor can instead
have all its parameters fixed by a complete native expected result.

Check only the leading type, pragma and wrapper prefix with the ordinary
IALL and ITYARG rules. Keep their original Quick Look allocations. Stop before
IRESULT and before any value argument. Resume with the original head and the
checked prefix in telescope order. Do not check a type argument twice. Keep
this preparation inside the application's evidence scope.

Select ordinary checking only when no erased choices remain, the native
value types are supported, and deep zonking finds no free Quick Look variables
in the residual type, expected type or saved prefix metadata. Ground theta
remains an obligation for IALL and instCall. Outer rigid variables and runtime
indices are permitted. Ordinary metavariables keep their existing rules.

Alternatively, use the existing immutable expected-result selection, including
its algebraic candidate fallback. Keep the actual substituted signature. Append
its type applications after the explicit prefix. This lets a nested Refl use
its own complete expected proof type. It does not assume that the proof holds;
argument checking, nominal entailment and final subsumption still run.

A failed selection keeps the checked prefix and resumes Quick Look. It never
infers a value argument or creates an argument cache. Public tcInstFun callers
retain their native Quick Look guards. See Note [Retained types and Quick Look]
and Note [Retained application evidence]. Declared arguments can supply a
complete IALL plan; see Note [Declared native argument choices].
The IALL/ITYARG rules follow
Serrano et al., A quick look at impredicativity, ICFP 2020; see
Note [Quick Look overview]. This checked route does not extend those rules
with mutable native instantiation. Agda v2.8.0 retains a checked prefix in
Rules/Application.hs:619-641 and substitutes arguments in telescope order at
774-808. Its contextual metas do not justify native GHC meta assignment.
See docs/dependent-haskell/REFERENCES.md#applications-with-fixed-native-type-choices
for the pinned source links and the limits of this adaptation.
-}

-- | Use an existing monomorphic native variable without Quick Look.
-- See Note [Known native variables with ImpredicativeTypes].
tcRetainedKnownHeadQL :: Bool -> QLFlag -> [HsExprArg 'TcpRn] -> ExpRhoType
                     -> HsExpr GhcTc -> TcSigmaType -> TcM QLFlag
tcRetainedKnownHeadQL True DoQL [] exp_res_ty HsVar{} actual
  = do { actual <- liftZonkM (zonkTcType actual)
       ; expected <- readExpType_maybe exp_res_ty
       ; expected <- mapM (liftZonkM . zonkTcType) expected
       ; let tys = actual : maybeToList expected
             has_ql_var v = isTcTyVar v && isQLInstTyVar v
       ; return $ if not (isSigmaTy actual || isForAllTy actual)
                     && any (hasNativeDependentExpr . Core.Type) tys
                     && not (any has_ql_var (tyCoVarsOfTypesList tys))
                  then NoQL else DoQL }
tcRetainedKnownHeadQL _ do_ql _ _ _ _ = return do_ql

{- Note [Known native variables with ImpredicativeTypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ImpredicativeTypes permits a qualified retained Pi in a checked type argument.
It also enables Quick Look for every application, including a bare variable.
For fromGiven f = f under a Given value ~ (foreach (n :: Nat) ->
Eq Int => Vec Int n), the RHS has no argument or invisible binder to
instantiate. The checking context supplies its expected type. This check
needs conversion, not Quick Look instantiation.

Use NoQL for an actual variable with no arguments and no leading forall or
context. Require native syntax in the actual or an available expected type.
Zonk these types and reject this route if either has a free QL instantiation
variable. Use the native-aware deep free-variable traversal; foldQLInstVars
does not support retained binders or indices. Keep final result subsumption
and native evidence capture. Do not eta-expand the variable.

A case scrutinee uses inference mode even when its variable has a complete
type. Return that existing type through the ordinary InferResult path. Read
an available expected type without allocating an inference cell. Preserve
InferResult promotion, level, runtime-representation and branch checks.

All other applications retain their Quick Look guards. In particular, this
route does not instantiate a polymorphic head or monomorphise QL variables.
See docs/dependent-haskell/REFERENCES.md#qualified-retained-function-types.
-}

-- | Bind native dictionary evidence at the application that needs it.
tcRetainedAppEvidence :: Bool -> TcM (HsExpr GhcTc) -> TcM (HsExpr GhcTc)
tcRetainedAppEvidence enabled thing_inside
  | not enabled = thing_inside
  | otherwise
  = do { (expr, wanted) <- captureConstraints thing_inside
       ; if any (hasNativeDependentExpr . Core.Type . ctPred) (wc_simple wanted)
         then do { tclvl <- getTcLevel
                 ; (implic, ev_binds) <- buildImplicationFor tclvl RetainedAppSkol [] [] wanted
                 ; emitImplications implic
                 ; return (mkHsWrap (mkWpLet ev_binds) expr) }
         else do { emitConstraints wanted
                 ; return expr } }

{- Note [Retained application evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Immutable instantiation can produce Show (Vec Int n), where n is a runtime Id.
The dictionary expression can contain n in its type arguments. Its binding must
therefore remain below the runtime lambda that binds n. Checking the type
arguments at the call site does not establish this evidence scope.

Capture constraint generation for the instantiated application. Inspect the
actual simple Wanteds after instantiation, not the initial theta. A visible type
argument or an earlier value argument can expose a later forall whose immutable
instantiation introduces a native predicate. Nested implications already own
their evidence and do not require another wrapper.

For a native Wanted, build an
implication with its own evidence bindings and put WpLet around the application.
Force this implication even with no type-variable skolems or Givens. The normal
checkConstraints fast path would omit it. The solver inherits enclosing Givens
and uses the ordinary instance and equality rules. Do not use StaticFormSkol,
which deliberately removes enclosing Givens. RetainedAppSkol records the actual
origin for diagnostics. Residual native predicates must also remain inside this
implication; see Note [Retained residual constraints] in GHC.Tc.Types.Constraint.
Keep the current TcLevel. There are no new type-variable binders. Raising the
level would force an inferred native result through promoteTcType and a mutable
meta-variable. Applications without native Wanteds emit their original constraints.

This is a context boundary for evidence, not a new equality rule or a way to fill
native meta-variables. Keep the original theta and the final subsumption checks.
Agda inserts instance arguments in the current context and postpones their search
during application checking. The GHC implementation uses delayed FC evidence and
an expression-local implication. See
 docs/dependent-haskell/REFERENCES.md#retained-class-consumers.
-}

finishApp :: QLFlag
          -> HsExpr GhcRn           -- The whole application
          -> SrcSpan                -- Span for the /head/ of the application
          -> HsExpr GhcRn           -- The head of the application (renamed)
          -> HsExpr GhcTc           -- The head of the application (typechecked)
          -> [HsExprArg 'TcpInst]   -- Arguments
          -> TcRhoType -> ExpRhoType
          -> TcM (HsExpr GhcTc)
-- Instantiation and Quick Look are done.  Now do typechecking,
-- of both the result type and the arguments, before rebuilding.
finishApp = finishAppWithCache []

-- | Finish an application with arguments inferred for immutable index matching.
finishAppWithCache :: RetainedArgCache -> QLFlag
                   -> HsExpr GhcRn -> SrcSpan -> HsExpr GhcRn -> HsExpr GhcTc
                   -> [HsExprArg 'TcpInst] -> TcRhoType -> ExpRhoType
                   -> TcM (HsExpr GhcTc)
finishAppWithCache cache do_ql rn_expr fun_loc rn_fun tc_fun inst_args
                   app_res_rho exp_res_ty
  = do { -- Step 4.1: monomorphise un-unified QuickLook instantiation variables
         -- Step 4.2: zonk app_res_rho to expose the polymorphism hidden under
         --           instantiation variables in `app_res_rho`
         app_res_rho <- case do_ql of
           NoQL -> return app_res_rho
           DoQL -> do { rejectNativeQuickLook app_res_rho
                      ; setSrcSpan fun_loc $ rejectNativeQLInstArgs inst_args
                      ; liftZonkM $
                        do { qlMonoInstArgs inst_args
                           ; zonkTcType app_res_rho } }

         -- Step 5: subsumption check against the expected type
         -- See Note [Unify with expected type before typechecking arguments]
       ; res_wrap <- checkResultTy rn_expr (tc_fun, fun_loc) inst_args
                                   app_res_rho exp_res_ty

         -- Step 6: Typecheck the arguments
       ; tc_args <- tcValArgsWithCache cache do_ql (rn_fun, fun_loc) inst_args

         -- Step 7: Horrible special case for tagToEnum#.
       ; res_expr <- if isTagToEnum tc_fun
                     then tcTagToEnum tc_fun tc_args app_res_rho
                     else return (rebuildHsApps tc_fun tc_args)

       ; traceTc "End tcApp }" (ppr tc_fun)
       ; return (mkHsWrap res_wrap res_expr) }

quickLookResultType :: TcRhoType -> TcRhoType -> TcM ()
-- This function implements the shaded bit of rule APP-Downarrow in
-- Fig 5 of the QL paper: "A quick look at impredicativity" (ICFP'20).
-- See Note [QuickLook: arguments before result]
quickLookResultType app_res_rho exp_res_rho
  = qlUnify app_res_rho exp_res_rho

-- | Connect up the inferred type of an application with the expected type.
-- This is usually just a unification, but with deep subsumption there is more to do.
checkResultTy :: HsExpr GhcRn
              -> (HsExpr GhcTc, SrcSpan)  -- Head
              -> [HsExprArg p]            -- Arguments, just for error messages
              -> TcRhoType  -- Inferred type of the application; zonked to
                            --   expose foralls, but maybe not /deeply/ instantiated
              -> ExpRhoType -- Expected type; this is deeply skolemised
              -> TcM HsWrapper
checkResultTy rn_expr (tc_fun, fun_loc) inst_args app_res_rho res_ty
 = perhaps_add_res_ty_ctxt $
   tcSubTypeApp rn_expr tc_fun app_res_rho res_ty
  where
    -- perhaps_add_res_ty_ctxt: Inside an expansion, the addFunResCtxt stuff is
    -- more confusing than helpful because the function at the head isn't in
    -- the source program; it was added by the renamer.  See
    -- Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr
    perhaps_add_res_ty_ctxt thing_inside
      | isGeneratedSrcSpan fun_loc
      = thing_inside
      | otherwise
      = addFunResCtxt tc_fun inst_args app_res_rho res_ty $
        thing_inside

----------------
-- | Check cached arguments by subsumption and emit each saved usage once.
tcValArgsWithCache :: RetainedArgCache -> QLFlag
                   -> (HsExpr GhcRn, SrcSpan) -> [HsExprArg 'TcpInst]
                   -> TcM [HsExprArg 'TcpTc]
tcValArgsWithCache cache do_ql rn_head args = go do_ql 0 0 args
  where
    go :: QLFlag -> Int -> Int -> [HsExprArg 'TcpInst] -> TcM [HsExprArg 'TcpTc]
    go _ _ _ [] = return []
    go do_ql pos value_pos (arg : args)
      = do { arg'  <- case (arg, lookup value_pos' cache) of
               (EValArg { ea_loc_span = lspan, ea_arg = larg
                        , ea_arg_ty = Scaled mult expected },
                Just (typed_arg, actual, usage))
                 -> addArgCtxt pos' rn_head larg $ tcScalingUsage mult $
                    do { tcEmitBindingUsage usage
                       ; wrap <- tcSubTypeSigma (exprCtOrigin (unLoc larg))
                                   GenSigCtxt actual expected
                       ; return (EValArg { ea_loc_span = lspan
                                         , ea_arg_ty = noExtField
                                         , ea_arg = fmap (mkHsWrap wrap) typed_arg }) }
               _ -> tcValArg do_ql pos' rn_head arg
           ; args' <- go do_ql pos' value_pos' args
           ; return (arg' : args') }
      where
        -- See Note [Native inference after visible type applications].
        !value_pos' | EValArg{} <- arg = value_pos + 1
                    | EValArgQL{} <- arg = value_pos + 1
                    | otherwise = value_pos
        -- Increment position if the argument is user-written type or value argument
        !pos' | user_visible arg = pos + 1
              | otherwise        = pos

    user_visible (EValArg{})                   = True
    user_visible (EValArgQL{})                 = True
    user_visible (ERetainedArg{})              = True
    user_visible (ETypeArg{ ea_loc_span = l }) = not (isGeneratedSrcSpan (locA l))
    user_visible _                             = False

tcValArg :: QLFlag                  -- ^ Are we typechecking with Quick Look turned on?
         -> Int                     -- ^ Argument position (used only for error message generation)
         -> (HsExpr GhcRn, SrcSpan) -- ^ Head of the application chain (used only for error message generation)
         -> HsExprArg 'TcpInst      -- ^ Actual argument
         -> TcM (HsExprArg 'TcpTc)  -- ^ Resulting argument
tcValArg _ _ _ (EPrag l p)         = return (EPrag l (tcExprPrag p))
tcValArg _ _ _ (ETypeArg l hty ty) = return (ETypeArg l hty ty)
tcValArg _ _ _ (EWrap (EHsWrap w)) = return (EWrap (EHsWrap w))
tcValArg _ _ _ (EWrap ew)          = return (EWrap ew)

tcValArg _ pos rn_head (ERetainedArg loc (Scaled mult ty) arg res_ty)
  = addArgCtxt pos rn_head arg $
    do { checked <- tcScalingUsage mult $
                     traverse (\e -> tcPolyExpr e (mkCheckExpType ty)) arg
       ; return (ERetainedArg loc noExtField checked res_ty) }

tcValArg do_ql pos rn_fun_head
         (EValArg { ea_loc_span  = lspan
                  , ea_arg    = larg@(L arg_loc arg)
                  , ea_arg_ty = sc_arg_ty })
  = addArgCtxt pos rn_fun_head larg $
    do { -- Crucial step: expose QL results before checking exp_arg_ty
         -- So far as the paper is concerned, this step applies
         -- the poly-substitution Theta, learned by QL, so that we
         -- "see" the polymorphism in that argument type. E.g.
         --    (:) e ids, where ids :: [forall a. a->a]
         --                     (:) :: forall p. p->[p]->[p]
         -- Then Theta = [p :-> forall a. a->a], and we want
         -- to check 'e' with expected type (forall a. a->a)
         -- See Note [Instantiation variables are short lived]
         Scaled mult exp_arg_ty <- case do_ql of
              DoQL -> liftZonkM $ zonkScaledTcType sc_arg_ty
              NoQL -> return sc_arg_ty
       ; traceTc "tcValArg {" $
         vcat [ text "arg lspan:" <+> ppr lspan
              , text "rn_head" <+> ppr rn_fun_head
              , text "sigma_type" <+> ppr (mkCheckExpType exp_arg_ty)
              , text "arg:" <+> ppr larg
              , text "arg_loc:" <+> ppr arg_loc
              ]


         -- Now check the argument. Fully explicit outer type applications
         -- can already have supplied a known native callback input.
         -- See Note [Named callbacks at known native inputs].
       ; enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; callback_enabled <- if enabled then return True else
           do { callback_ty <- liftZonkM (zonkTcType exp_arg_ty)
              ; return (hasNativeDependentExpr (Core.Type callback_ty)) }
       ; (_, callback_cache) <- case do_ql of
           NoQL | callback_enabled -> tcRetainedCallbackArgs [] [Scaled mult exp_arg_ty]
                               [EValArg { ea_loc_span = lspan, ea_arg = larg
                                        , ea_arg_ty = noExtField }] Subst.emptySubst []
           _ -> return (Nothing, [])
       ; arg' <- tcScalingUsage mult $
           case callback_cache of
             [(1, (typed, actual, usage))] ->
               do { tcEmitBindingUsage usage
                  ; wrap <- tcSubTypeSigma (exprCtOrigin arg) GenSigCtxt actual exp_arg_ty
                  ; return (mkHsWrap wrap (unLoc typed)) }
             _ -> tcPolyExpr arg (mkCheckExpType exp_arg_ty)
       ; traceTc "tcValArg" $ vcat [ ppr arg'
                                   , text "}" ]
       ; return (EValArg { ea_loc_span = lspan
                         , ea_arg = L arg_loc arg'
                         , ea_arg_ty = noExtField }) }

tcValArg _ pos rn_fun_head
         (EValArgQL { eaql_wanted   = wanted
                    , eaql_loc_span = lspan
                    , eaql_arg_ty   = sc_arg_ty
                    , eaql_larg     = larg@(L arg_loc rn_expr)
                    , eaql_rn_fun   = rn_fun
                    , eaql_tc_fun   = tc_arg_head@(tc_fun,arg_head_loc)
                    , eaql_fun_ue   = head_ue
                    , eaql_args     = inst_args
                    , eaql_encl     = arg_influences_enclosing_call
                    , eaql_res_rho  = app_res_rho })
  = addArgCtxt pos rn_fun_head larg $
    do { -- Expose QL results to tcSkolemise, as in EValArg case
         Scaled mult exp_arg_ty <- liftZonkM $ zonkScaledTcType sc_arg_ty

       ; traceTc "tcEValArgQL {" (vcat [ text "app_res_rho:" <+> ppr app_res_rho
                                       , text "exp_arg_ty:" <+> ppr exp_arg_ty
                                       , text "args:" <+> ppr inst_args
                                       , text "mult:" <+> ppr mult
                                       , text "app_lspan" <+> ppr lspan
                                       , text "fun_head" <+> ppr rn_fun_head
                                       , text "tc_arg_head" <+> ppr tc_arg_head])
       ; ds_flag <- getDeepSubsumptionFlag
         -- NB: whether to do deep /skolemisation/ is independent of data constructors
       ; (wrap, arg')
            <- tcScalingUsage mult  $
               tcSkolemise ds_flag GenSigCtxt exp_arg_ty $ \ exp_arg_rho ->
               do { -- Emit saved-up constraints, /under/ the tcSkolemise
                    -- See (QLA4) in Note [Quick Look at value arguments]
                    emitConstraints wanted
                    -- Emit saved-up usages /under/ the tcScalingUsage.
                    -- See (QLA5) in Note [Quick Look at value arguments]
                  ; tcEmitBindingUsage head_ue

                    -- Unify with context if we have not already done so
                    -- See (QLA4) in Note [Quick Look at value arguments]
                  ; unless arg_influences_enclosing_call $   -- Don't repeat
                    qlUnify app_res_rho exp_arg_rho          -- the qlUnify

                  ; finishApp DoQL rn_expr arg_head_loc rn_fun tc_fun inst_args app_res_rho
                              (mkCheckExpType exp_arg_rho) }

       ; traceTc "tcEValArgQL }" $
           vcat [ text "app_res_rho:" <+> ppr app_res_rho ]

       ; return (EValArg { ea_loc_span   = lspan
                         , ea_arg    = L arg_loc (mkHsWrap wrap arg')
                         , ea_arg_ty = noExtField }) }


--------------------
wantQuickLook :: HsExpr GhcRn -> TcM QLFlag
wantQuickLook (HsVar _ (L _ (WithUserRdr _ f)))
  | getUnique f `elem` quickLookKeys = return DoQL
wantQuickLook _                      = do { impred <- xoptM LangExt.ImpredicativeTypes
                                          ; if impred then return DoQL else return NoQL }

skipQuickLook :: HsExprLoc -> LHsExpr GhcRn -> Scaled TcRhoType
              -> TcM (HsExprArg 'TcpInst)
skipQuickLook loc larg arg_ty
  = return (EValArg { ea_loc_span = loc
                    , ea_arg      = larg
                    , ea_arg_ty   = arg_ty })

quickLookKeys :: [Unique]
-- See Note [Quick Look for particular Ids]
quickLookKeys = [dollarIdKey, leftSectionKey, rightSectionKey]

{- *********************************************************************
*                                                                      *
              Instantiating the call
*                                                                      *
********************************************************************* -}

tcInstFun :: QLFlag
          -> Bool   -- False <=> Instantiate only /top-level, inferred/ variables;
                    --           so may return a sigma-type
                    -- True  <=> Instantiate /top-level, invisible/ type variables;
                    --           always return a rho-type (but not a deep-rho type)
                    -- Generally speaking we pass in True; in Fig 5 of the paper
                    --    |-inst returns a rho-type
          -> (HsExpr GhcRn, SrcSpan)
          -> HsExpr GhcTc
          -> TcSigmaType -> [HsExprArg 'TcpRn]
          -> TcM ( [HsExprArg 'TcpInst]
                 , TcSigmaType )   -- Does not instantiate trailing invisible foralls
-- This crucial function implements the |-inst judgement in Fig 4,
-- plus the modification in Fig 5, of the QL paper:
-- "A quick look at impredicativity" (ICFP'20).
tcInstFun = tcInstFunWithExpected Nothing Nothing

-- | Instantiate arguments in telescope order, with optional immutable choices
-- and a retained expected result after ordinary VTA.
-- See Note [Native inference after visible type applications].
tcInstFunWithExpected :: Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                      -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                      -> TcSigmaType -> [HsExprArg 'TcpRn]
                      -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithExpected = tcInstFunWithExpectedCache Nothing

{- Note [Native inference after visible type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ITYARG checks each written type argument once. After leading type arguments,
use the substituted signature to infer native arguments before IALL
allocates the remaining type variables. Keep the original head and checked
type arguments. Pass immutable choices to IALL so that type applications and
dictionaries retain their telescope order. Require NoQL, no consumed value
arguments, no pending type arguments, and the existing inference guards.
Both routes can use a nonempty declared argument prefix on eligible heads.
The native-index-kind route still requires complete immutable choices.
Calls without type arguments keep the existing prepass.

Only tcApp enables this step. It retains the inferred argument cache even when
matching fails. The public tcInstFun caller cannot consume a cache and leaves
this step disabled. The optional reference records whether inference was
attempted, so further IALL steps cannot infer an argument again. The mode
argument gives the diagnostic offset of the consumed written type arguments.
Calls without type arguments do not allocate the reference.

Cache keys count value arguments. Diagnostic positions also count written type
arguments. Keep separate counters in tcValArgsWithCache; otherwise a written
@Bool shifts the cache lookup and repeats argument checking or uses another
argument's cached expression. Generated type arguments do not change diagnostic
positions. Use the written-type-argument offset for Quick Look contexts too.
Emit each saved usage once at the argument's actual multiplicity.
See Note [Generic retained argument inference] for prefix matching and final
subsumption. These rules do not assign native syntax to a type meta-variable.
-}

-- | Retain post-VTA argument inference for callers that consume its cache.
-- See Note [Native inference after visible type applications].
tcInstFunWithExpectedCache :: Maybe (TcRef (Maybe RetainedArgCache))
                          -> Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                          -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                          -> TcSigmaType -> [HsExprArg 'TcpRn]
                          -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithExpectedCache = tcInstFunWithPrefix False []

-- | Resume instantiation after a checked prefix, or check that prefix only.
-- Prefix-only calls cannot enter value inference or IRESULT.
-- See Note [Ground retained applications].
tcInstFunWithPrefix :: Bool -> [HsExprArg 'TcpInst]
                    -> Maybe (TcRef (Maybe RetainedArgCache))
                    -> Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                    -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                    -> TcSigmaType -> [HsExprArg 'TcpRn]
                    -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithPrefix prefix_only prefix vta_cache expected_rho inst_plan do_ql inst_final
                    rn_head@(_, fun_lspan) tc_fun fun_sigma rn_args
  = do { lvl <- getTcLevel
       ; traceTc "tcInstFun" (vcat [ text "tc_fun" <+> ppr tc_fun
                                   , text "rn_fun" <+> ppr rn_head
                                   , text "fun_sigma" <+> ppr fun_sigma
                                   , text "args:" <+> ppr rn_args
                                   , text "do_ql" <+> ppr do_ql
                                   , text "lvl:" <+> ppr lvl ])
       ; case do_ql of
           NoQL -> return ()
           DoQL | not prefix_only -> setSrcSpan fun_lspan $ rejectNativeQuickLook fun_sigma
                | otherwise -> return ()
       ; fun_origin <- mk_origin rn_head
       ; res@(_, fun_ty) <- go inst_plan fun_origin 1 (reverse prefix) fun_sigma rn_args
       ; traceTc "tcInstFun:ret" (ppr fun_ty)
       ; return res
       }
  where
    -- What should be the origin for this function call?
    -- If the head of the function is user written
    -- then it can be used in the error message
    -- If it is generated code location span, blame it on the
    -- origin that can be retrived from the top of the error ctxt stack.
    -- See Note [Error contexts in generated code]
    mk_origin :: (HsExpr GhcRn, SrcSpan)  -- The head of the application chain and its location
              -> TcM CtOrigin
    mk_origin (rn_fun, fun_lspan)
     | not (isGeneratedSrcSpan fun_lspan)
     = return $ exprCtOrigin rn_fun

     | otherwise -- If the location is generated, the best we can do is to
                 -- approximate by looking on top of the error message stack
     = do { err_ctxt_stack <- getErrCtxt
          ; let hs_ctxt = case err_ctxt_stack of
                             (c:_) -> c
                             [] -> pprPanic "mk_origin" (ppr rn_fun)
          ; traceTc "mk_origin" (pprHsCtxt hs_ctxt)
          ; return $ hsCtxtCtOrigin hs_ctxt
          }

    -- These are the type variables which must be instantiated to concrete
    -- types. See Note [Representation-polymorphic Ids with no binding]
    -- in GHC.Tc.Utils.Concrete
    fun_conc_tvs
      | HsVar _ (L _ fun_id) <- tc_fun
      = idConcreteTvs fun_id
      -- Recall that DataCons are represented using ConLikeTc at GhcTc stage,
      -- see Note [Typechecking data constructors] in GHC.Tc.Gen.Head.
      | XExpr (ConLikeTc (RealDataCon dc)) <- tc_fun
      = dataConConcreteTyVars dc
      | otherwise
      = noConcreteTyVars

    -- Count value args only when complaining about a function
    -- applied to too many value args
    -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify.
    n_val_args = count isHsValArg rn_args

    fun_is_out_of_scope  -- See Note [VTA for out-of-scope functions]
      = case tc_fun of
          HsHole _        -> True
          _               -> False

    inst_fun :: [HsExprArg 'TcpRn] -> ForAllTyFlag -> Bool
    -- True <=> instantiate a tyvar that has this ForAllTyFlag
    inst_fun [] | inst_final  = isInvisibleForAllTyFlag
                | otherwise   = const False
    inst_fun (EValArg {} : _) = isInvisibleForAllTyFlag
    inst_fun (ERetainedArg {} : _) = isInvisibleForAllTyFlag
    inst_fun _                = isInferredForAllTyFlag

    -----------
    go, go1 :: RetainedInstPlan          -- Immutable choices in binder order
            -> CtOrigin                 -- Of the function
            -> Int                      -- Value-argument position of next arg
            -> [HsExprArg 'TcpInst]     -- Accumulator, reversed
            -> TcSigmaType -> [HsExprArg 'TcpRn]
            -> TcM ([HsExprArg 'TcpInst], TcSigmaType)

    -- A prefix is not a whole application. Do not run final IALL or IRESULT.
    go _ _ _ acc fun_ty [] | prefix_only = return (reverse acc, fun_ty)

    -- go: If fun_ty=kappa, look it up in Theta
    go plan fun_orig pos acc fun_ty args
      | Just kappa <- getTyVar_maybe fun_ty
      , isQLInstTyVar kappa
      = do { cts <- readMetaTyVar kappa
           ; case cts of
                Indirect fun_ty' -> go plan fun_orig pos acc fun_ty' args
                Flexi            -> go1 plan fun_orig pos acc fun_ty  args }
     | isJust plan
     = go1 plan fun_orig pos acc fun_ty args
     | otherwise
     = do { matched <- tcRetainedResultInst do_ql expected_rho args tc_fun fun_ty
          ; case matched of
              Just (inst_tys, body)
                -> go plan fun_orig pos (addArgWrap (mkWpTyApps inst_tys) acc) body args
              Nothing -> do
                { inferred <- case vta_cache of
                    Just ref
                      | pos == 1
                      , any is_type_arg acc
                      , not (any is_type_arg args)
                      , hasNoConcreteTyVars tc_fun
                      -> do { attempted <- readTcRef ref
                            ; case attempted of
                                Just _ -> return Nothing
                                Nothing -> do
                                  { (_, _, cache, choices) <-
                                      tcRetainedArgInstEnabled (Just (count written_type_arg acc))
                                        do_ql expected_rho
                                        rn_head args tc_fun fun_ty
                                  ; writeTcRef ref (Just cache)
                                  ; return choices } }
                    _ -> return Nothing
                ; go1 inferred fun_orig pos acc fun_ty args } }

    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    written_type_arg (ETypeArg { ea_loc_span = loc })
      = not (isGeneratedSrcSpan (locA loc))
    written_type_arg _ = False

    written_prefix_arg arg = written_type_arg arg || isExplicitRetainedArg arg

    -- go1: fun_ty is not filled-in instantiation variable
    --      ('go' dealt with that case)

    -- Handle out-of-scope functions gracefully
    go1 plan fun_orig pos acc fun_ty (arg : rest_args)
      | fun_is_out_of_scope
      , looks_like_type_arg arg || isExplicitRetainedArg arg
      -- See Note [VTA for out-of-scope functions]
      = go plan fun_orig pos acc fun_ty rest_args

    -- Rule IALL from Fig 4 of the QL paper; applies even if args = []
    -- Instantiate invisible foralls and dictionaries.
    -- c.f. GHC.Tc.Utils.Instantiate.topInstantiate
    go1 plan fun_orig pos acc fun_ty args
      | (tvs,   body1) <- tcSplitSomeForAllTyVars (inst_fun args) fun_ty
      , (theta, body2) <- if inst_fun args Inferred
                          then tcSplitPhiTy body1
                          else ([], body1)
        -- inst_fun args Inferred: dictionary parameters are like Inferred foralls
        -- E.g. #22908: f :: Foo => blah
        -- No foralls!  But if inst_final=False, don't instantiate
      , let no_tvs   = null tvs
            no_theta = null theta
      , not (no_tvs && no_theta)
      = do { (wrap, fun_rho, rest_plan) <-
                -- setSrcSpan of the function: important for the class constraints
                -- that may be emitted from instantiating fun_sigma
                setSrcSpan fun_lspan $
                case plan of
                  Nothing -> do { (wrap, rho) <- instantiateSigmaQL do_ql fun_orig
                                                  fun_conc_tvs tvs theta body2
                                ; return (wrap, rho, Nothing) }
                  Just choices -> do
                    { let (inst_tys, rest) = splitAt (length tvs) choices
                          subst = Subst.zipTvSubst (map snd tvs) inst_tys
                    ; unless (tvs `equalLength` inst_tys
                              && and (zipWith (\tv ty ->
                                   eqType (Subst.substTy subst (tyVarKind tv))
                                          (typeKind ty)) (map snd tvs) inst_tys)) $
                        pprPanic "tcInstFun: invalid retained instantiations"
                                 (ppr tvs $$ ppr choices)
                    ; wrap <- instCall fun_orig inst_tys (substTheta subst theta)
                    ; return (wrap, substTy subst body2, Just rest) }

                  -- See Note [Representation-polymorphism checking built-ins]
                  -- in GHC.Tc.Utils.Concrete.
                  -- NB: we are doing this even when "acc" is not empty,
                  -- to handle e.g.
                  --
                  --   badTup :: forall r (a :: TYPE r). a -> (# Int, a #)
                  --   badTup = (# , #) @LiftedRep
                  --
                  -- in which we already have instantiated the first RuntimeRep
                  -- argument of (#,#) to @LiftedRep, but want to rule out the
                  -- second instantiation @r.

           ; go rest_plan fun_orig pos (addArgWrap wrap acc) fun_rho args }
                -- Going around again means we deal easily with
                -- nested  forall a. Eq a => forall b. Show b => blah

    -- See Note [Explicit invisible retained arguments].
    go1 plan fun_orig pos acc fun_ty (ERetainedArg loc _ arg _ : rest_args)
      = do { (binder, (operand, runtime_arg)) <-
               addArgCtxt (pos + count written_prefix_arg acc) rn_head arg $
               case coreFullView fun_ty of
                 DepFunTy DepFunInvisible binder _ ->
                   (binder,) <$> tcRetainedOperand binder arg
                 _ -> failWithTc
                        (TcRnRetainedQuantificationError (RetainedExplicitArgument fun_ty))
           ; let res_ty = applyTypeToArgs fun_ty [operand]
                 checked = ERetainedArg loc (Scaled ManyTy (varType binder)) runtime_arg res_ty
           ; go plan fun_orig pos (checked : acc) res_ty rest_args }

    -- See Note [Inferring an invisible retained argument].
    go1 plan fun_orig pos acc fun_ty args@(EValArg { ea_arg = first } : _)
      | DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      = do { InvisibleOperand operand record_usage <- addArgCtxt (pos + count written_prefix_arg acc) rn_head first $
                 do { selected <- tcInferInvisibleRetained [] binder body
                                    [arg | EValArg { ea_arg = arg } <- args]
                    ; case selected of
                        Just (actual, _) -> return actual
                        Nothing -> failWithTc (TcRnRetainedQuantificationError
                                                (RetainedInvisibleArgument binder)) }
           ; tcScalingUsage ManyTy record_usage
           ; let wrap = mkWpEvApps [EvExpr operand]
           ; go plan fun_orig pos (addArgWrap wrap acc) (applyTypeToArgs fun_ty [operand]) args }

    -- Rule IRESULT from Fig 4 of the QL paper; no more arguments
    go1 plan _fun_orig _pos acc fun_ty []
       | Just (_ : _) <- plan
       = pprPanic "tcInstFun: unused retained instantiations" (ppr plan)
       | XExpr (ConLikeTc (RealDataCon dc)) <- tc_fun
       , isNewDataCon dc
       , [Scaled _ orig_arg_ty] <- dataConOrigArgTys dc
       , n_val_args == 0
       -- If we're dealing with an unsaturated representation-polymorphic
       -- UnliftedNewype, then perform a representation-polymorphism check.
       -- See Note [Representation-polymorphism checks for unsaturated unlifted newtypes]
       -- in GHC.Tc.Utils.Concrete.
       , not $ typeHasFixedRuntimeRep orig_arg_ty
       = do { (wrap_co, ma, arg_ty, res_ty) <-
                  matchActualFunTy (FRRRepPolyUnliftedNewtype dc)
                    (Just $ HsExprTcThing tc_fun)
                    (n_val_args, fun_sigma) fun_ty
             ; let acc' = addArgWrap (mkWpCastN wrap_co) acc
             ; return (reverse acc', tcMkScaledFunTy ma arg_ty res_ty) }
      | otherwise
      = return (reverse acc, fun_ty)

    -- See Note [Retained argument application].
    go1 plan fun_orig pos acc fun_ty
        (EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      | DepFunTy DepFunVisible binder _ <- coreFullView fun_ty
      = do { (core_arg, runtime_arg) <- tcRetainedOperand binder arg
           ; arg' <- skipQuickLook ctxt runtime_arg (Scaled ManyTy (varType binder))
           ; let body_ty = applyTypeToArgs fun_ty [core_arg]
           ; go plan fun_orig (pos + 1) (arg' : acc) body_ty rest_args }

    -- Rule ITVDQ from the GHC Proposal #281
    go1 plan fun_orig pos acc fun_ty ((EValArg { ea_arg = arg }) : rest_args)
      | Just (ma, tvb, body) <- tcSplitForAllTyVarBinder_maybe fun_ty
      = assertPpr (isVisibleForAllTyFlag (binderFlag tvb)) (ppr fun_ty $$ ppr arg) $
        -- Any invisible binders have been instantiated by IALL above,
        -- so this forall must be visible (i.e. Required)
        do { (ty_arg, inst_body) <- tcVDQ fun_conc_tvs (ma, tvb, body) arg
           ; let wrap = mkWpTyApps [ty_arg]
           ; go plan fun_orig (pos+1) (addArgWrap wrap acc) inst_body rest_args }

    go1 plan fun_orig pos acc fun_ty (EWrap w : args)
      = go1 plan fun_orig pos (EWrap w : acc) fun_ty args

    go1 plan fun_orig pos acc fun_ty (EPrag sp prag : args)
      = go1 plan fun_orig pos (EPrag sp prag : acc) fun_ty args

    -- Rule ITYARG from Fig 4 of the QL paper
    go1 plan fun_orig pos acc fun_ty ( ETypeArg { ea_loc_span = ctxt, ea_hs_ty = hs_ty }
                                  : rest_args )
      = do { (ty_arg, inst_ty) <- tcVTA fun_conc_tvs fun_ty hs_ty
           ; let arg' = ETypeArg { ea_loc_span = ctxt, ea_hs_ty = hs_ty, ea_ty_arg = ty_arg }
           ; go plan fun_orig pos (arg' : acc) inst_ty rest_args }

    -- Rule IVAR from Fig 4 of the QL paper:
    go1 plan fun_orig pos acc fun_ty args@(EValArg {} : _)
      | Just kappa <- getTyVar_maybe fun_ty
      , isQLInstTyVar kappa
      = -- Function type was of form   f :: forall a b. t1 -> t2 -> b
        -- with 'b', one of the quantified type variables, in the corner
        -- but the call applies it to three or more value args.
        -- Suppose b is instantiated by kappa.  Then we want to make fresh
        -- instantiation variables nu1, nu2, and set kappa := nu1 -> nu2
        --
        -- In principle what is happening here is not unlike matchActualFunTys
        -- but there are many small differences:
        --   - We know that the function type in unfilled meta-tyvar
        --     matchActualFunTys is much more general, has a loop, etc.
        --   - We must be sure to actually update the variable right now,
        --     not defer in any way, because this is a QL instantiation variable.
        -- It's easier just to do the job directly here.
        do { arg_tys <- zipWithM (new_arg_ty fun_orig) (leadingValArgs args) [pos..]
           ; res_ty  <- newOpenFlexiTyVarTyQL do_ql TauTv
           ; let fun_ty' = mkScaledFunTysU arg_tys res_ty

           -- Fill in kappa := nu_1 -> .. -> nu_n -> res_nu
           -- NB: kappa is uninstantiated ('go' already checked that)
           ; kind_co <- unifyKind Nothing liftedTypeKind (tyVarKind kappa)
                 -- unifyKind: see (UQL3) in Note [QuickLook unification]
           ; liftZonkM (writeMetaTyVar kappa (mkCastTy fun_ty' kind_co))

           ; let co_wrap = mkWpCastN (mkGReflLeftCo Nominal fun_ty' kind_co)
                 acc'    = addArgWrap co_wrap acc
                 -- Suppose kappa :: kk
                 -- Then fun_ty :: kk, fun_ty' :: Type, kind_co :: Type ~ kk
                 --      co_wrap :: (fun_ty' |> kind_co) ~ fun_ty'

           ; go plan fun_orig pos acc' fun_ty' args }

    -- Rule IARG from Fig 4 of the QL paper:
    go1 plan fun_orig pos acc fun_ty
        (EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      = do { let herald = mk_herald fun_orig tc_fun (unLoc arg)
           ; (fun_co, _ma, arg_ty, res_ty) <-
                -- NB: matchActualFunTy does the rep-poly check.
                -- For example, suppose we have f :: forall r (a::TYPE r). a -> Int
                -- In an application (f x), we need 'x' to have a fixed runtime
                -- representation; matchActualFunTy checks that when
                -- taking apart the arrow type (a -> Int).
                --
                -- TODO: example from T26072
                matchActualFunTy herald
                  (Just $ HsExprTcThing tc_fun)
                  (n_val_args, fun_sigma) fun_ty
           ; arg' <- quickLookArg do_ql (pos + count written_prefix_arg acc)
                                  ctxt rn_head arg arg_ty
           ; let acc' = arg' : addArgWrap (mkWpCastN fun_co) acc
           ; go plan fun_orig (pos+1) acc' res_ty rest_args }

    new_arg_ty :: CtOrigin -> LHsExpr GhcRn -> Int -> TcM (Scaled TcType)
    -- Make a fresh nus for each argument in rule IVAR
    new_arg_ty fun_orig (L _ arg) i
      = do { arg_nu <- newArgTyVarTyQL do_ql $
                       FRRExpectedFunTy (mk_herald fun_orig tc_fun arg) i
               -- Following matchActualFunTy, we create nu_i :: TYPE kappa_i[conc],
               -- thereby ensuring that the arguments have concrete runtime representations

            ; mult_ty <- newFlexiTyVarTyQL do_ql (mkTyVarOccFS (fsLit "m")) TauTv multiplicityTy
               -- mult_ty: e need variables for argument multiplicities (#18731)
               -- Otherwise, 'undefined x' wouldn't be linear in x

           ; return (mkScaled mult_ty arg_nu) }

    mk_herald :: CtOrigin -> HsExpr GhcTc -> HsExpr GhcRn -> ExpectedFunTyCtxt
    mk_herald fun_orig tc_fun arg
      = case fun_orig of
           DoStmtOrigin -> ExpectedFunTySyntaxOp DoStmtOrigin tc_fun
           _ -> ExpectedFunTyArg (HsExprTcThing tc_fun) arg


-- | Resolve the index at the binder's checked scalar or function domain.
tcRetainedOperand :: Id -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedOperand binder arg
  | isNativeIndexFunctionId binder = (,arg) <$> tcRetainedFunctionArg domain arg
  | otherwise = tcRetainedValueOperand domain arg
  where
    domain = varType binder

-- | Check a value operand and specialise a class selector to its checked method.
-- See Note [Dictionary selection in retained arguments].
tcRetainedValueOperand :: Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedValueOperand domain arg@(L loc original)
  | (op, args@(_:_)) <- split_call arg []
  , let (L head_loc head_expr, type_arg) = case op of
          L _ (HsAppType _ f ty) -> (stripParensLHsExpr f, Just ty)
          _ -> (op, Nothing)
  , HsVar _ lname <- head_expr
  , let name = unLocWithUserRdr lname
  , isExternalName name, isVarOcc (nameOccName name)
  = setSrcSpanA head_loc $ do
      mb_sel <- tcLookupIdMaybe name
      case mb_sel of
        Just sel | Just cls <- isClassOpId_maybe sel -> do
          let unchecked :: TcM a
              unchecked = failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall name))
          case tcSplitSigmaTy (varType sel) of
            ([(_, tv)], [theta], rho)
              | let (_, _, result) = splitFunTys rho
              , result `tcEqType` mkTyVarTy tv
              , theta `tcEqType` mkClassPred cls [mkTyVarTy tv]
              , noFreeVarsOfType domain, typeKind domain `tcEqType` tyVarKind tv -> return ()
            _ -> unchecked
          forM_ type_arg $ \ty -> do
            (supplied, _) <- tcVTA (idConcreteTvs sel) (varType sel) ty
            unless (supplied `tcEqType` domain) unchecked
          dfun <- tcRetainedDFun name cls domain >>= maybe unchecked return
          target_name <- maybe unchecked return (sourceMethodTarget_maybe dfun sel)
          target <- tcLookupIdMaybe target_name >>= maybe unchecked return
          unless (varType target `tcEqType` classMethodInstTy sel [domain]) unchecked

          let target_head = L head_loc (HsVar noExtField (fmap (const (noUserRdr target_name)) lname))
              runtime_arg = L loc (mkExpandedLExpr original (mkHsApps target_head args))
          checked <- tcRetainedArg domain runtime_arg
          recordSourceMethodUse dfun sel target
          wantRetainedMethod sel dfun
          return checked
        _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = tcRetainedArg domain arg
    split_call :: LHsExpr GhcRn -> [LHsExpr GhcRn] -> (LHsExpr GhcRn, [LHsExpr GhcRn])
    split_call (L _ (HsPar _ e)) args = split_call e args
    split_call (L _ (HsApp _ f a)) args = split_call f (a:args)
    split_call (L _ (OpApp _ a op b)) args = split_call op (a:b:args)
    split_call e args = (e, args)

-- | Look up a completed, ground instance with no context.
tcRetainedDFun :: Name -> Class -> Type -> TcM (Maybe Id)
tcRetainedDFun method cls domain = do
  insts <- tcGetInstEnvs
  case lookupUniqueInstEnv insts cls [domain] of
    Right (inst, []) -> do
      env <- getGblEnv
      let dfun_name = getName (instanceDFunId inst)
      when (nameIsLocalOrFrom (tcg_semantic_mod env) dfun_name) $
        failWithTc (TcRnRetainedQuantificationError (RetainedLocalSourceMethod method))
      tcLookupIdMaybe dfun_name
    _ -> return Nothing

-- | Require the ordinary solver to select this dictionary at the use site.
wantRetainedMethod :: Id -> Id -> TcM ()
wantRetainedMethod sel dfun = do
  let name = getName sel
  ev <- newWanted (OccurrenceOf name) Nothing (varType dfun)
  emitSimple (mkNonCanonical ev)
  emitDelayedErrors (unitBag (DE_SourceMethod (ctEvEvId ev) dfun name (ctEvLoc ev)))

-- | Elaborate a nonnegative literal at a known retained domain.
-- See Note [Retained integral literals].
tcRetainedNumeral :: Type -> Name -> Integer -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedNumeral domain from n = do
  let unverified :: TcM a
      unverified = failWithTc (TcRnRetainedQuantificationError (RetainedLiteralConversion from domain))
  sel <- tcLookupIdMaybe from >>= maybe unverified return
  cls <- case isClassOpId_maybe sel of
    Just cls | cls `hasKey` numClassKey, noFreeVarsOfType domain -> return cls
    _ -> unverified
  dfun <- tcRetainedDFun from cls domain >>= maybe unverified return
  (zero, succ_con) <- maybe unverified return (sourceLiteralCons_maybe dfun sel)
  dflags <- getDynFlags
  when (n > toInteger (maxBound :: Int) || intGtLimit (fromInteger n) (reductionDepth dflags)) $
    failWithTc (TcRnRetainedQuantificationError (RetainedLiteralDepth n))
  recordSourceLiteralUse dfun sel
  wantRetainedMethod sel dfun
  let build :: Int -> (Core.CoreExpr, LHsExpr GhcRn)
      build 0 = (Core.Var (dataConWorkId zero), nlHsVar (dataConName zero))
      build k = let (val, src) = build (k - 1)
                in (Core.App (Core.Var (dataConWorkId succ_con)) val,
                    nlHsApp (nlHsVar (dataConName succ_con)) src)
  return (build (fromInteger n))

{- Note [Retained integral literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In replicateNative 3 x, the renamer supplies the literal conversion Name.
The completed Num dictionary must certify its zero and successor equations.
Require the ordinary solver to select this dictionary, just as for (+).
Build both the runtime argument and its index from those exact constructors.
The constructors may be private to the module that defines the instance.

A result signature such as Vec a 3 resolves its numeral independently through
FromNatural. Ordinary type equality checks that the two meanings agree.
Each successor adds one constructor. Check the reduction-depth budget before
allocating the tree, including for a literal too large to fit in an Int.
-}

{- Note [Dictionary selection in retained arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For replicateNative (n + m), the result type depends on which addition
method the caller uses. Look up a candidate concrete dictionary from the
known index domain. Require the method's result to be its class parameter:
the result of an ambiguous method such as C a => Bool -> Bool cannot
determine the dictionary type. The interface identifies an exact alias to a checked
source function. The ordinary constraint solver must select this same
dictionary for the method occurrence in its lexical scope. Emit an
ordinary Wanted and attach the check to that Wanted's evidence variable.
A Given for Num Nat can select a different dictionary.

Check the target call and its rewrite rules before emitting the method
obligation. An invalid operand or rule can already explain the error.
Use the target for both the index and runtime. The expanded expression keeps the user's
operator and its source span. The later argument check must not resolve
a second dictionary. M81's source-call check verifies saturation and
operand kinds and preserves the lazy call through erasure.
-}

-- | Resolve one already-bound local function at its exact retained domain.
-- Read existing assignments without instantiating or checking the value again.
-- See Note [Function-valued retained binders] in GHC.Core.Type.
tcRetainedFunctionArg :: Type -> LHsExpr GhcRn -> TcM Core.CoreExpr
tcRetainedFunctionArg domain arg
  = setSrcSpanA (getLoc arg) $
    case unLoc (stripParensLHsExpr arg) of
      HsVar _ lname
        -> do { thing <- tcLookupLcl_maybe (unLocWithUserRdr lname)
              ; case thing of
                  Just (ATcId { tct_id = actual }) ->
                    do { actual <- liftZonkM (zonkId actual)
                       ; if not (isExternalName (getName actual))
                            && isNativeIndexFunctionId actual
                            && varType actual `eqType` domain
                         then return (Core.Var actual)
                         else unsupported }
                  _ -> unsupported }
      _ -> unsupported
  where
    unsupported = failWithTc $
      TcRnRetainedQuantificationError RetainedFunctionArgument

-- | Resolve a retained argument at its known domain without checking its value.
-- See Note [Retained argument application].
tcRetainedArg :: Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedArg domain arg
  = setSrcSpanA (getLoc arg) $
    do { result@(core_arg, _) <- resolve emptyNameEnv domain arg
       ; case Core.retainedArgIndex core_arg of
           Just index | typeKind index `tcEqType` domain -> return result
           _ -> unsupported }
  where
    resolve :: NameEnv Id -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
    resolve env expected original@(L loc expr)
      = setSrcSpanA loc $ case expr of
          HsPar x arg -> do
            (core, arg') <- resolve env expected arg
            return (core, L loc (HsPar x arg'))
          XExpr (ExpandedThingRn (HSE ctxt arg)) -> do
            (core, arg') <- resolve env expected arg
            return (core, L loc (XExpr (ExpandedThingRn (HSE ctxt arg'))))
          HsOverLit _ (OverLit OverLitRn{ol_from_fun = L _ from} (HsIntegral lit))
            | let n = il_value lit, n >= 0
            -> do { (core, generated) <- tcRetainedNumeral expected from n
                  ; return (core, L loc (mkExpandedLExpr expr generated)) }
          HsApp x fun arg -> do
            (core, fun', arg') <- resolve_app env expected fun arg
            return (core, L loc (HsApp x fun' arg'))
          OpApp x left op right -> do
            (partial, op', left') <- resolve_app env expected op left
            (core, right') <- apply env partial right
            return (core, L loc (OpApp x left' op' right'))
          ExplicitList x elements
            | Just [element_ty] <- nativeIndexDataConTyArgs expected nilDataCon
            -> do { nil <- constructor expected nilDataCon
                  ; cons <- constructor expected consDataCon
                  ; (values, elements') <- unzip <$> mapM (resolve env element_ty) elements
                  ; return (foldr (\value rest -> Core.mkApps cons [value, rest]) nil values,
                            L loc (ExplicitList x elements')) }
          HsVar _ lname
            | Just arg_id <- lookupNameEnv env (unLocWithUserRdr lname)
            -> (,original) <$> tcRetainedArgVar expected arg_id
            | isDataOcc (nameOccName (unLocWithUserRdr lname))
            -> do { con <- tcLookupConLike (unLoc lname)
                  ; case con of
                      RealDataCon dc -> (,original) <$> constructor expected dc
                      _ -> unsupported }
            | otherwise
            -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
                  ; case mb_id of
                      Just arg_id
                        -> do { arg_id <- liftZonkM (zonkId arg_id)
                              ; if isNativeOpaqueValueId (setIdMult arg_id ManyTy)
                                then do
                                  { let mult = idMult arg_id
                                        flexible = case coreFullView mult of
                                          TyVarTy tv -> isMetaTyVar tv
                                          _ -> False
                                  ; unless (isManyTy mult || flexible) $ failWithTc $
                                      TcRnRetainedQuantificationError (RetainedIndexMultiplicity mult)
                                  ; tcSubMult (OccurrenceOf (getName arg_id)) ManyTy mult
                                  ; arg_id <- liftZonkM (zonkId arg_id)
                                  ; if isNativeOpaqueValueId arg_id
                                    then (,original) <$> tcRetainedArgVar expected arg_id
                                    else failWithTc $ TcRnRetainedQuantificationError
                                           (RetainedIndexMultiplicity (idMult arg_id)) }
                                else unsupported }
                      _ -> unsupported }
          _ -> unsupported

    -- Instantiate the erased parameters from the complete expected domain.
    constructor expected dc = case nativeIndexDataConTyArgs expected dc of
      Just tys -> return (Core.mkTyApps (Core.Var (dataConWorkId dc)) tys)
      Nothing -> unsupported

    resolve_app :: NameEnv Id -> Type -> LHsExpr GhcRn -> LHsExpr GhcRn
                -> TcM (Core.CoreExpr, LHsExpr GhcRn, LHsExpr GhcRn)
    resolve_app env expected (L loc (HsPar x fun)) arg = do
        (core, fun', arg') <- resolve_app env expected fun arg
        return (core, L loc (HsPar x fun'), arg')
    resolve_app env expected (L loc (XExpr (ExpandedThingRn (HSE ctxt fun)))) arg = do
        (core, fun', arg') <- resolve_app env expected fun arg
        return (core, L loc (XExpr (ExpandedThingRn (HSE ctxt fun'))), arg')
    resolve_app env expected (L loc (HsLam x LamSingle matches)) arg
      | L alts_loc [L match_loc match@Match
          { m_pats = L _ [L _ (VarPat _ lname)]
          , m_grhss = rhss@GRHSs { grhssGRHSs = grhss
                                , grhssLocalBinds = L _ (EmptyLocalBinds _) } }]
          <- mg_alts matches
      , [L grhs_loc (GRHS grhs_ext [] body)] <- NE.toList grhss
      = setSrcSpanA loc $
        do { strict <- xoptM LangExt.Strict
           ; when strict $ failWithTc
               (TcRnRetainedQuantificationError RetainedIndexLambdaStrict)
           ; let name = unLoc lname
                 binder = mkLocalId name ManyTy expected
           ; (core_arg, arg') <- resolve env expected arg
           ; (core_body, body') <- resolve (extendNameEnv env name binder) expected body
           ; let rhss' = rhss { grhssGRHSs = NE.singleton (L grhs_loc (GRHS grhs_ext [] body')) }
                 match' = match { m_grhss = rhss' }
                 matches' = matches { mg_alts = L alts_loc [L match_loc match'] }
           ; return (Core.App (Core.Lam binder core_body) core_arg,
                     L loc (HsLam x LamSingle matches'), arg') }
    resolve_app env expected fun arg
      = do { (core_fun, fun') <- case unLoc fun of
               HsVar _ name | let n = unLocWithUserRdr name
                            , not (isDataOcc (nameOccName n)) -> do
                     certified <- if isExternalName n
                       then tcLookupIdMaybe n >>= maybe (pure Nothing) tcLookupSourceTableId
                       else pure Nothing
                     case certified of
                       Just v -> pure (Core.Var v, fun)
                       _ -> setSrcSpanA (getLoc fun) $
                              failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall n))
               _ -> resolve env expected fun
           ; (app, arg') <- apply env core_fun arg
           ; return (app, fun', arg') }

    apply env fun arg = case splitFunTy_maybe (exprType fun) of
      Just (_, _, _, arg_ty, _) -> do
        (core_arg, arg') <- resolve env arg_ty arg
        return (Core.App fun core_arg, arg')
      _ -> unsupported

    unsupported :: TcM a
    unsupported = failWithTc (TcRnRetainedQuantificationError RetainedIndexExpression)

-- | Resolve an index variable at its expected domain. Function values keep
-- their complete fixed type and lexical identity.
-- See Note [Retained argument application].
tcRetainedArgVar :: TcType -> Id -> TcM Core.CoreExpr
tcRetainedArgVar expected arg_id
  | isNativeIndexFunctionId arg_id
  = if not (isExternalName (getName arg_id))
       && varType arg_id `tcEqType` expected
    then return (Core.Var arg_id)
    else failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument
  | varType arg_id `tcEqType` expected = return (Core.Var arg_id)
  | otherwise
  = do { co <- unifyType Nothing (varType arg_id) expected
       ; return (Core.Cast (Core.Var arg_id) (SubCo co)) }

{- Note [Retained argument application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A plain source value argument consumes DepFunVisible. At DepFunInvisible it
can determine an omitted operand. See Note [Inferring an invisible retained
argument] and Note [Explicit invisible retained arguments].

A retained argument is a value. Resolve a local unrestricted Id or a saturated
constructor tree before instantiating the codomain. Also resolve an immediate
single-variable lambda application in the checked native beta grammar.
applyTypeToArgs uses the same index substitution as Core type recovery.
Do not use tcVDQ: it translates an expression into an erased type argument.

The retained binder supplies the checked domain. A lambda's provisional Many
Id uses its actual renamed Name. Keep these Ids in a separate NameEnv; do not
install them in the Tc environment or collect usage. Resolve the argument in
the outer environment and the body in the extended environment. Constructor
worker arrows supply their actual field domains. Do not use a constructor's
result domain for all its fields. The shared Core bridge validates domains,
saturation, and atomic beta arguments. See Note [Checked native index beta
exposure] in GHC.Core. No provisional lambda binder survives normalization.

A closed algebraic argument can contain eligible function leaves. Resolve
each leaf to its actual lexical Id at its complete fixed function type.
Do not cast a function to another domain or admit a global function here.
The promoted constructor index stores the same Id as TermVarTy. A saturated
family can expose that value; canonical application then forms TermAppTy.

Keep the lambda grammar narrow: one VarPat, one unguarded RHS, and no local
binds or signatures. Strict can make this variable pattern force its argument,
so reject this new form with Strict. Under ordinary inference each admitted
lambda starts with a fresh multiplicity. One usage does not equate it to One;
Many usage requires Many. Remaining multiplicity metas default to Many during
final zonking. No admitted signature, linear function head, or expected linear
arrow can impose One. Check actual zonked lambda metadata when this grammar
changes. The source pattern checker preserves the renamed Name/Unique.

A local NoGen let can keep a fresh multiplicity metavariable. For example,
let bools = [flag, False] can capture a do-bound flag before genericVector bools
uses bools as a retained index. First check its local, non-CoVar, non-join,
lifted shape with a temporary Many view. Do not store or return that view.
Require Many on the actual Id with tcSubMult at retained resolution, then
re-zonk the Id before its exact Core metadata check. This
constrains an ordinary multiplicity. It does not default an unknown value or
accept One or a rigid multiplicity. tcSubMult requires reflexive evidence.
Use the same rule as Note [Checking retained package patterns] in
GHC.Tc.Gen.Pat. Keep optional declaration lookup read-only. Resolution emits
no usage; the later ordinary argument check records the occurrence once.
The separate fixed-domain function argument resolver keeps its existing rule.
Reject an already known One or rigid multiplicity before requesting that
constraint. Report the retained argument's Many requirement and its actual
multiplicity. This is an unsupported retained argument, not a request to use
GADT evidence to equate multiplicities.

A scalar variable can have a nominally equal domain with a different family
application. Keep its nominal domain cast in the resolved Core argument.
The index substitution then receives an argument of the expected kind.
The ordinary value checker still checks the argument and its usage. A failed
domain equality remains a located type error instead of an invalid substitution.
See Note [Nominal native index casts] in GHC.Core.

Keep a normal EValArg with the binder's domain as its expected type. tcValArg
checks the value once and records its runtime usage. Resolution does not inspect
unfoldings or evaluate values. Later argument types use the same runtime Id,
including a lazy binding whose RHS is bottom. No singleton is added.
The provisional Core expression does not replace this original source check.
See docs/dependent-haskell/REFERENCES.md#agda-provenance and
docs/dependent-haskell/CORE.md#laziness-and-eta.
-}

{- Note [Explicit invisible retained arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In f @Int @{n} xs, @Int is an erased type application and @{n} is an
expression in the ordinary term namespace. Agda uses braces to supply an
implicit term. The prefix @ distinguishes the argument from GHC's record
syntax. See docs/dependent-haskell/REFERENCES.md#invisible-retained-binders-next-candidate.

Instantiate preceding erased foralls and dictionaries before consuming the
invisible retained binder. Declaration advice can choose erased arguments.
Substitute the written operand, resolved by the same rules as a visible
retained argument. An inferred operand cannot replace it. Check the original
expression at the binder's domain and multiplicity Many exactly once. Store
the substituted result type in HsAppRetained. Desugar its expression to a
runtime value application.

Keep ERetainedArg separate from EValArg through application checking. It
counts in source diagnostic positions. It does not consume a position in
the ordinary argument cache. Skip Quick Look for this application so that
the retained result is checked without Quick Look instantiation variables.
-}

-- | Is this a written runtime argument for an invisible retained binder?
isExplicitRetainedArg :: HsExprArg p -> Bool
isExplicitRetainedArg ERetainedArg{} = True
isExplicitRetainedArg _ = False

{- Note [Immutable retained result instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary polymorphic constructor or helper can return an indexed type.
Match its saturated result against the expected type before creating mutable
instantiation variables. Restrict matching to its outer invisible binders.
Free skolems must remain rigid. Each binder must have a substitution of the
correct kind, and the substituted result must equal the expected result.

Apply the substitution with explicit type applications on the head. Every
runtime Id in these applications must still name a local unrestricted binding
with the same Unique and domain. This prevents stale identifiers from entering
the application. No mutable type variable is filled with retained syntax.

This route requires NoQL and a saturated ordinary function type without
retained binders. A leading constraint context remains in the substituted
body. See Note [Constrained immutable instantiation]. The early match requires
no explicit type arguments. tcInstFunWithExpected also attempts the match
before IALL allocates
fresh meta-variables. It waits until all explicit type arguments have been
consumed by ordinary VTA checking. For example, bottomPolymorphicVec @Bool
can instantiate its remaining index from the expected vector type. Explicit
type arguments are checked and emitted once. Other applications use the
ordinary path and its existing Quick Look checks.
-}

-- | Instantiate outer invisible binders for retained result checking.
-- The substitution is immutable; retained syntax never enters a meta-variable.
tcRetainedExpectedInst :: QLFlag -> ExpRhoType -> [HsExprArg 'TcpRn]
                       -> HsExpr GhcTc -> TcSigmaType
                       -> TcM (HsExpr GhcTc, TcSigmaType)
tcRetainedExpectedInst do_ql expected args fun sigma
  = do { matched <- tcRetainedResultInst do_ql expected_rho args fun sigma
       ; case matched of
           Just (inst_tys, body) -> return (mkHsWrap (mkWpTyApps inst_tys) fun, body)
           Nothing -> return (fun, sigma) }
  where
    expected_rho = case expected of
                     Check rho -> Just rho
                     Infer {}  -> Nothing

{- Note [Retained instantiation candidates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Refl repeats its index in its result. An expected result can contain two
indices that are equal only under a local Given. For example, the recursive
step of a right-zero proof expects Add (S k) Z :~: S k. An exact immutable
match fails before the solver can use the recursive proof Add k Z ~ k.
Ordinary instantiation would instead create an index meta-variable. The
native meta-variable fill check correctly rejects that assignment.

For a known ordinary Id or data constructor, match corresponding result
arguments independently. Use the first candidate for each outer invisible
binder. Require a complete, well-kinded substitution and check the local scope
of every runtime Id.
This substitution chooses type arguments. It does not prove that the result
has the expected type. Repeated occurrences can still require an equality.

For example, leftVector :: forall n. Vec Int n can be checked against
Vec alpha bottomLength while the ordinary payload meta alpha is unresolved.
The whole-result match cannot bind alpha. The independent index argument
provides n := bottomLength. Keep Vec Int bottomLength as the actual result;
final subsumption can fill the ordinary alpha with Int. It must still reject
an incompatible expected index. Candidate selection is not equality evidence.

Emit WpTyApps and keep the actual substituted function type. Both the early
application route and tcInstFunWithExpected continue to finishAppWithCache.
Its checkResultTy call checks subsumption against the original expected type
and emits the required equality evidence. For example, a non-reflexive Refl
still fails without a suitable Given. Never replace the actual result with
the expected result, or use a reflexive coercion for this check.

The expected algebraic result can contain a supported retained function type.
For example, Refl can be checked at value :~: QPi under value ~ QPi. Choose
its repeated endpoint immutably. Keep the actual reflexive result and let
final nominal entailment use the Given. Representational evidence is not
sufficient. See Note [Ground retained applications].

Keep this fallback under NoQL. Require a known head without concreteness
requirements, a saturated ordinary algebraic result, no pending type arguments,
and no constraints or native template. Pattern synonyms do not use this
fallback. No mutable type variable receives retained syntax. The exact
result match remains the first choice. Before either match, zonk the native
expected type. The enclosing application can have filled ordinary payload
metas during its result check. For example, PackVec n bottomPayload checks
bottomPayload :: forall m. Vec Int m against Vec alpha n after alpha := Int.
The pure matcher must see Int instead of the filled alpha node. Otherwise it
fails and ordinary index instantiation creates a meta that cannot hold n.
This zonking reads existing assignments. It does not fill a native meta or
infer the payload before its expected type is available. See
Note [Immutable retained result instantiation].
-}

-- | Expose filled payload metas before exact or candidate immutable matching.
tcRetainedResultInst :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                     -> HsExpr GhcTc -> TcSigmaType
                     -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedResultInst do_ql expected args fun sigma
  = do { expected <- case (do_ql, expected) of
           (NoQL, Just rho)
             | hasNativeDependentExpr (Core.Type rho)
             -> Just <$> liftZonkM (zonkTcType rho)
           _ -> return expected
       ; matched <- tcRetainedResultMatch do_ql expected args fun sigma
       ; case matched of
           Just _ -> return matched
           Nothing -> tcRetainedResultCandidate do_ql expected args fun sigma }

-- | Choose immutable type arguments from independent algebraic result arguments.
-- Keep the actual result type for the subsequent subsumption check.
tcRetainedResultCandidate :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                         -> HsExpr GhcTc -> TcSigmaType
                         -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedResultCandidate NoQL (Just expected) args fun sigma
  | hasNoConcreteTyVars fun
  , hasNativeDependentExpr (Core.Type expected)
  , not (any is_type_arg args)
  , (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , not (null tvs)
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) body
  , isRetainedValueType expected, isTauTy body
  , not (hasNativeDependentExpr (Core.Type body))
  , Just (tc, templates) <- splitTyConApp_maybe result
  , isAlgTyCon tc
  , case fun of
      HsVar{} -> True
      XExpr (ConLikeTc (RealDataCon dc)) -> tc == dataConTyCon dc
      _ -> False
  , length templates == tyConArity tc
  , Just (expected_tc, actuals) <- splitTyConApp_maybe expected
  , tc == expected_tc, templates `equalLength` actuals
  , let candidates = [ subst | (template, actual) <- zip templates actuals
                            , Just subst <- [tcMatchTyX_BM (bind_outer tvs)
                                             Subst.emptySubst template actual] ]
  , Just inst_tys <- traverse (candidate candidates) tvs
  , not (any (`elemVarSet` tyCoVarsOfTypes inst_tys) tvs)
  , let subst = Subst.zipTvSubst tvs inst_tys
  , and (zipWith (kind_agrees subst) tvs inst_tys)
  = do { valid <- tcRetainedInstScope inst_tys
       ; return (if valid then Just (inst_tys, Subst.substTy subst body) else Nothing) }
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _
      | tv `elem` tvs = BindMe
      | otherwise = DontBindMe

    candidate substs tv = listToMaybe [ ty | subst <- substs
                                         , Just ty <- [Subst.lookupTyVar subst tv] ]

    kind_agrees subst tv ty = eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty)
tcRetainedResultCandidate _ _ _ _ _ = return Nothing

{- Note [Constrained immutable instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary consumer can require a dictionary for a type with native syntax.
For example, show (rightZero n) requires Show (Add n Z :~: n). Split the
leading constraint context from the ordinary tau type when matching results
or cached arguments. Match only the tau type. Return the substituted original
body, including its constraints. Do not return just the substituted tau type.

Apply only the immutable type arguments here. tcInstFunWithExpected then
finds the instantiated context in IALL. With no outer type variables left,
instantiateSigmaQL calls instCall to emit the dictionary or equality evidence.
Keep this ordinary evidence pipeline and the final subsumption checks.
The early expected-result route uses the same rule even without an argument
anchor. Every outer binder must still be determined before instantiation.

All newly constrained routes require a known head with no concreteness
requirements. This includes the exact expected-result route and the existing
native-index-kind argument route. Otherwise, immutable type applications
could bypass the representation checks in ordinary instantiation. Keep the
unconstrained routes unchanged. Constructor candidates remain unconstrained.
See Note [Immutable retained result instantiation] and
Note [Generic retained argument inference].
-}

-- | Check that a known application head has no concreteness requirements.
hasNoConcreteTyVars :: HsExpr GhcTc -> Bool
hasNoConcreteTyVars (HsVar _ (L _ var)) = isEmptyNameEnv (idConcreteTvs var)
hasNoConcreteTyVars (XExpr (ConLikeTc (RealDataCon dc)))
  = isEmptyNameEnv (dataConConcreteTyVars dc)
hasNoConcreteTyVars _ = False

-- | Match a retained expected result and preserve its instantiated constraints.
tcRetainedResultMatch :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                      -> HsExpr GhcTc -> TcSigmaType
                      -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedResultMatch NoQL (Just expected) args fun sigma
  | hasNativeDependentExpr (Core.Type expected)
  , not (any is_type_arg args)
  , (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , not (null tvs)
  , let (theta, tau) = tcSplitPhiTy body
  , null theta || hasNoConcreteTyVars fun
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) tau
  , isTauTy expected || (hasNoConcreteTyVars fun && isRetainedValueType expected)
  , isTauTy result
  , not (hasNativeDependentExpr (Core.Type body))
  , Just kind_subst <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst
                                   (typeKind result) (typeKind expected)
  , Just subst <- tcMatchTyX_BM (bind_outer tvs) kind_subst result expected
  , Just inst_tys <- traverse (Subst.lookupTyVar subst) tvs
  , and (zipWith (kind_agrees subst) tvs inst_tys)
  , eqType (Subst.substTy subst result) expected
  = do { valid <- tcRetainedInstScope inst_tys
       ; return (if valid then Just (inst_tys, Subst.substTy subst body) else Nothing) }
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _
      | tv `elem` tvs = BindMe
      | otherwise = DontBindMe

    kind_agrees subst tv ty = eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty)
tcRetainedResultMatch do_ql expected args fun sigma
  = tcRetainedWorkerResultMatch do_ql expected args fun sigma

-- | Specialize a known retained worker from its complete expected result.
-- See Note [Expected retained worker instantiation].
tcRetainedWorkerResultMatch :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                           -> HsExpr GhcTc -> TcSigmaType
                           -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedWorkerResultMatch NoQL (Just expected) args fun sigma
  | hasNoConcreteTyVars fun
  , hasNativeDependentExpr (Core.Type expected)
  , not (any is_type_arg args)
  , (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , not (null tvs)
  , let (_, tau) = tcSplitPhiTy body
  , hasNativeDependentExpr (Core.Type tau)
  , isRetainedValueType expected, isRetainedValueType tau
  , Just result <- worker_result (count isHsValArg args) tau
  , isRetainedValueType result
  , Just subst <- tcMatchRetainedTyX_BM (bind_outer tvs) Subst.emptySubst result expected
  , Just inst_tys <- traverse (Subst.lookupTyVar subst) tvs
  , not (any (`elemVarSet` tyCoVarsOfTypes inst_tys) tvs)
  , and (zipWith (kind_agrees subst) tvs inst_tys)
  , eqType (Subst.substTy subst result) expected
  = do { valid <- tcRetainedInstScope inst_tys
       ; return (if valid then Just (inst_tys, Subst.substTy subst body) else Nothing) }
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _
      | tv `elem` tvs = BindMe
      | otherwise = DontBindMe

    kind_agrees subst tv ty = eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty)
    worker_result n ty
      | XExpr (ConLikeTc (RealDataCon dc)) <- fun
      , let fields = dataConRepArgBinders dc
      , not (null fields)
      , n == length fields
      = constructor_result fields ty
    worker_result 0 ty = Just ty
    worker_result n ty
      | Just (_, res) <- splitVisibleFunTy_maybe ty
      = worker_result (n-1) res
      | otherwise = Nothing

    constructor_result [] ty = Just ty
    constructor_result (Just _ : fields) (DepFunTy DepFunVisible _ body)
      = constructor_result fields body
    constructor_result (Just index : fields) ty
      | FunTy flag _ma mult domain body <- coreFullView ty
      , isVisibleFunArg flag
      , isManyTy mult
      , eqType domain (varType index)
      = constructor_result fields body
    constructor_result (Nothing : fields) ty
      | Just (_, body) <- splitVisibleFunTy_maybe ty
      = constructor_result fields body
    constructor_result _ _ = Nothing
tcRetainedWorkerResultMatch _ _ _ _ _ = return Nothing

-- | Admit known scalar or function-valued retained binders without erased polymorphism.
-- Constraint arrows are supported only below a retained binder.
-- See Note [Known retained function instantiation].
isRetainedValueType :: Type -> Bool
isRetainedValueType = go False
  where
    go _ ty | isTauTy ty = True
    go under_pi ty = case coreFullView ty of
      DepFunTy _ index body -> isNativeOpaqueValueId index
                         && typeKind body `eqType` liftedTypeKind
                         && go True body
      FunTy flag ma mult arg res -> isTauTy ma && isTauTy mult
                              && (isVisibleFunArg flag || under_pi)
                              && go under_pi arg && go under_pi res
      TyConApp tc args
        | isAlgTyCon tc, not (isClassTyCon tc)
        , length args == tyConArity tc
        , typeKind ty `eqType` liftedTypeKind
        -> all (go under_pi) args
      _ -> False

{- Note [Expected retained worker instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A known worker can have a type such as forall a. foreach n -> Vec a n -> Vec a n.
When checking it against a complete retained function type, infer a from that
expected type before ordinary instantiation creates metavariables. For example,
applyAt @Int n lambdaId xs should not require lambdaId @Int.

Keep this route separate from ordinary immutable result instantiation. Require
a known head without concreteness variables, NoQL, and no pending explicit type
arguments. Consume only the written ordinary value arguments. An unapplied
ordinary arrow can remain part of the expected function value. Do not consume
or guess a retained runtime argument here.

A fully applied real constructor is a separate case. Its validated retained
field metadata guarantees that field indices do not escape into its result.
Traverse its current signature in metadata order: a retained field consumes
DepFunTy; an ordinary field consumes a visible arrow. This can expose Sum n m
in Sum :: forall n m. foreach p -> (p :~: Add n m) -> Sum n m. Choose only
the erased n and m from the expected result. Field checking still resolves
the supplied runtime p and checks its equality proof. Do not infer p here.
Use the current body because earlier explicit type applications may already
have substituted declaration variables. Require every field to be supplied.
Partial constructors and ordinary retained functions keep the existing path.
mkDepFunTy represents an unused retained binder as an ordinary Many arrow.
For a retained metadata entry, also accept that arrow with the same closed
index domain. This consumes the field without choosing its runtime value.
See docs/dependent-haskell/REFERENCES.md#computed-length-append-next-source-probe.

tcMatchRetainedTyX_BM compares fixed-domain retained bodies under paired runtime
binders. Its existing escape check prevents an outer erased parameter from
capturing an inner index. Default matching and general unification retain their
native gates. See Note [Matching retained function types] in GHC.Core.Unify.

Require every outer parameter, agreement of its substituted kind, valid runtime
scope, and exact equality of the instantiated result with the expected type.
Preserve the original context and emit the actual type applications. Ordinary
argument checking and final subsumption still run. Do not fill a native meta,
inspect an unfolding, evaluate an index, or eta-expand the worker. A bottom
worker must remain bottom when its function WHNF is demanded.
The eligibility check also applies to algebraic containers. See
Note [Known retained function instantiation].

Agda inserts hidden arguments from the expected result and compares Pi bodies
under one shared context. This route uses an immutable substitution instead of
contextual mutable metas. See docs/dependent-haskell/REFERENCES.md under
Expected-type instantiation of retained workers.
-}

{- Note [Known retained function instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary polymorphic value can be checked at a known retained function type.
For example, bottomFunction :: forall a. a can instantiate a to the entire Pi.
Match its ordinary result template against that expected type before allocating
instantiation metavariables. The pure matcher can bind an outer template variable
to a whole Pi without structurally unifying the Pi's body.

Keep the ordinary tau expected-result path. The additional path accepts runtime
function types through ordinary arrows and supported retained binders. It also
accepts constraint arrows below a retained binder. An exactly saturated
algebraic container of kind Type can hold these values. A Refl witness for a
retained function type needs the complete expected equality type to select
its erased arguments.
Recurse through each container argument with the incoming under_pi flag.
A Pi in one argument must not admit a qualified type in a sibling argument.
The ordinary tau fast path remains unchanged. Reject newly non-tau class,
family, unsaturated and nonlifted heads, and reject erased foralls. This changes
only eligibility for the existing exact matcher. Require a known head without
concreteness variables.
The template remains ordinary and its result remains tau. The existing NoQL,
type-argument, complete-substitution, kind, runtime-scope and exact-result checks
still apply. Preserve the original body and its constraints in the substitution.
The ordinary argument and result subsumption checks still run afterward.

Use isNativeOpaqueValueId for each known Core Pi binder. Source foreach domain
formation keeps its separate structural check. An eligible opaque function
head still has a closed unrestricted arrow between scalar domains.
The retained matcher pairs function binders only when their full domains agree.
It does not construct a function cast or infer an index by decomposing a call.
Actual function arguments still pass through tcRetainedFunctionArg. The value
classifier does not admit new runtime operands or change their scope checks.
See Note [Function-valued retained binders] in GHC.Core.Type.

Emit the actual type applications. Do not fill a mutable metavariable with the
Pi, introduce an equality axiom, or eta-expand the value. In particular, a bottom
function remains bottom when its function WHNF is demanded. This route also
allows id applied to a checked lambda when the expected result supplies its Pi.
-}

{- Note [Remaining ordinary arrows in retained result matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An expected result can determine all erased arguments after only some source
value arguments. For example, expectMarker label marker can be checked at
IO nativeProof -> IO (). Its remaining result is an ordinary tau arrow.
The exact immutable matcher can use that whole result without eta expansion.
Require tau syntax throughout the remaining arrow: no erased forall or
constraint below it. Emit the selected type arguments and preserve the actual
remaining function type for final subsumption.

Cached argument inference still requires all ordinary value arguments and
explicitly rejects a remaining function result. The algebraic result candidate
route still requires a saturated TyConApp, so it cannot use this arrow case.
-}

-- | Find the result after the written ordinary value arguments. A remaining
-- visible arrow is allowed, but erased foralls and constraint arrows are not.
tcRetainedOrdinaryResult :: Int -> Type -> Maybe Type
tcRetainedOrdinaryResult 0 ty
  | isTauTy ty = Just ty
tcRetainedOrdinaryResult n ty
  | n > 0, Just (_, res') <- splitVisibleFunTy_maybe ty
  = tcRetainedOrdinaryResult (n-1) res'
  | otherwise = Nothing

{- Note [Cached retained argument inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A consumer can quantify over an erased index without returning that index.
For example, observeVec :: forall n. Vec Int n -> IO String needs the index
from its argument. With native application checking and NoQL, infer arguments
whose formal types mention outer invisible index binders. Match their actual
types against the formal types with an immutable substitution. A retained
expected result can supply additional bindings. Native index binders must be
determined. An anchored generic consumer can use fresh ordinary parameters as
described in Note [Ordinary parameters in native applications]. Each final
choice must have the correct kind and local runtime scope.

Save the typed arguments and their usage environments. tcValArgsWithCache
checks each saved type by subsumption and emits its usage under the final
argument multiplicity. It does not typecheck the source expression again.
Other arguments retain ordinary expected-type checking. If matching fails or
leaves index binders undetermined, ordinary instantiation checks the same cached
arguments. The ordinary unifier still rejects retained meta-variable fills.

First match index-bearing domains. On heads without concreteness requirements,
infer eligible independent domains only for parameters that remain unknown.
For filterBounded (const False) xs, the vector fixes the predicate's element
type before the predicate is checked. Inferring the predicate first could
leave a rigid placeholder in the immutable candidate. Classify independent
domains with tcRetainedInferableArg and skip already cached positions.
Parameters fixed by the result or index arguments retain expected checking.
Concrete heads keep exact, index-only inference.
Require every index-route choice before instantiation; do not allocate native
meta-variables. For leading type arguments,
see Note [Native inference after visible type applications].
See Note [Generic retained argument inference] for extra arguments on the
generic route. Preserve any leading context as described in
Note [Constrained immutable instantiation]. Inference uses a fresh infer cell
at the current TcLevel. It can return an exact retained type without assigning a type meta-variable.
-}

{- Note [Generic retained argument inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A generic consumer can accept a type that contains a runtime index. For
example, force :: forall a. a -> IO String can accept rightZero n. The kind
of a is Type, so the native-index-kind test does not select this argument.
An ordinary instantiation meta-variable cannot hold the proof's native type.

Keep the native-index-kind route. Add a separate route for named argument
spines when the call has a native anchor. An anchor is a known Id whose type
contains native syntax. Look through named applications, type applications,
parentheses, pragmas, and already expanded expressions to find anchors.
This permits nested uses such as force (id (rightZero n)). Use read-only
lookups. Do not call splitHsApps or tcExpand during classification. They can
expand a Template Haskell splice. The classifier does not execute a splice.

Once a call has an anchor, infer named argument spines whose formal domains
mention undetermined outer binders. Ordinary variables and constructors can
supply other bindings, as in const (rightZero n) Nothing. Any fresh variables
from Nothing remain ordinary meta-variables. At exact saturation, seed the
immutable substitution from the expected result first. Do not infer a domain
whose outer binders are already determined by that result.

A generic result can itself be a function. For fst pair 42, infer pair from
the one declared domain of fst, then check 42 against the chosen function
result. Admit a nonempty declared argument prefix on eligible heads in both
routes. The index route requires complete immutable choices; see
Note [Cached retained argument inference]. Classify and cache only prefix
arguments. The generic route requires an anchor in that prefix. Start matching from
the empty substitution: the final expected result is after the extra
arguments, not the intermediate result used to choose fst's parameter.
Keep the full original argument spine for tcInstFunWithExpected and
finishAppWithCache. They check the extra arguments, evidence order, usage and
final result. Do not infer or cache an extra argument here.

A top-level lambda, literal, case, let, or do expression keeps its ordinary
expected-type check. A named application can contain these expressions as
arguments. Its own application checker supplies their expected types.
Calls without an anchor keep their ordinary checking behavior. Constructors
and Ids with concreteness requirements also keep ordinary instantiation.

A complete expression signature supplies its own checking type. Infer signed
arguments such as (42 :: Int) through the same cache. In nested inference,
const (42 :: Int) (rightZero n) has no expected result to determine const's
first parameter. Skipping the signed argument leaves the substitution partial
and forces ordinary instantiation of the native proof type. Reuse
isCompleteHsSig and inspect the renamed type syntax without elaboration.
Exclude written quantifiers, contexts and type splices, including those in
kinds and multiplicities. Stop at Names and embedded Core types. Type synonyms
can hide polymorphism; normal signature checking and final subsumption still
check their actual types. This syntactic gate does not prove monomorphism.
See Note [Application chains and heads] and tcExprSig in GHC.Tc.Gen.Head.

Use the existing argument cache and subsumption checks. Keep cached entries
even if matching fails. Emit each saved usage once at the final argument
multiplicity. Require every outer binder to have a candidate of the correct
kind and local runtime scope. Use the generic substitution only if it contains
native syntax. The actual and formal cached types must still match exactly.
The final result and argument subsumption checks remain in place. No mutable
type variable receives native syntax. See Note [Cached retained argument inference].
-}

{- Note [Immutable instantiation of nested contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dictionary selectors have nested invisible telescopes. For example, (>>) has
  forall m. Monad m => forall a b. m a -> m b -> m b
See Note [Dictionary selectors] in GHC.Types.Id.Make. In a retained callback,
  evaluate proof >> pure ()
can need a := LE n bound, where n is the actual runtime index. Splitting only
one forall/context group misses a and creates an ordinary meta-variable that
cannot hold this native type.

For matching, collect consecutive invisible forall/context groups before the
first visible arrow. Keep the existing saturation, native anchor, inference,
concreteness, complete substitution, kind, runtime scope and exact domain
checks. A nested plan must contain native syntax. Do not collect binders under
visible arrows or enable higher-rank argument inference.

Keep the original function type. Pass the immutable choices to IALL instead
of applying all type arguments at once. IALL consumes each original group and
uses instCall for that group's substituted constraints. Thus type applications
and evidence applications retain their original order. Constraint generation
stays inside tcRetainedAppEvidence. Argument cache usage and final subsumption
checks stay unchanged.

Store choices in binder order, not by the original TyVar Unique. Substitution
of an earlier group can freshen later binders. Positional consumption survives
this freshening. Recheck the instantiated kinds at each group and require that
all choices are consumed. Nothing retains ordinary instantiation; Just []
retains the completed immutable mode and cannot allocate another type meta.
The existing flat routes still apply their type arguments directly.
-}

-- | Classify arguments without elaboration.
-- See Note [Inference of native tuple fields] in GHC.Tc.Gen.Expr for tuples.
-- See Note [Native list anchors] for lists.
tcRetainedInferableArg :: LHsExpr GhcRn -> TcM (Bool, Bool)
tcRetainedInferableArg (L _ expr)
  = case expr of
      HsVar _ lname
        | isDataOcc (nameOccName (unLocWithUserRdr lname))
        -> do { con <- tcLookupConLike (unLoc lname)
              ; return (case con of RealDataCon _ -> (True, False)
                                    _ -> (False, False)) }
        | otherwise
        -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
              ; return (case mb_id of
                          Just var -> (True, hasNativeDependentExpr (Core.Type (varType var)))
                          Nothing -> (False, False)) }
      HsApp _ fun arg
        -> do { (inferable, fun_anchor) <- tcRetainedInferableArg fun
              ; if not inferable then return (False, False)
                else do { (_, arg_anchor) <- tcRetainedInferableArg arg
                        ; return (True, fun_anchor || arg_anchor) } }
      HsAppType _ fun _ -> tcRetainedInferableArg fun
      ExplicitList _ fields
        -> do { overloaded <- xoptM LangExt.OverloadedLists
              ; anchor <- if overloaded then return Nothing
                          else tcRetainedListElementType declared_field fields
              ; return (False, isJust anchor) }
      ExplicitTuple _ args Boxed
        -> do { shapes <- mapM tcRetainedInferableArg [arg | Present _ arg <- args]
              ; return (True, any snd shapes) }
      ExprWithTySig _ arg sig@(HsWC { hswc_body = L _ (HsSig { sig_bndrs = bndrs
                                                            , sig_body = body }) })
        | isCompleteHsSig sig, null (hsOuterTyVarNames bndrs), simple_type body
        -> do { (_, anchor) <- tcRetainedInferableArg arg
              ; return (True, anchor) }
      HsPar _ arg -> tcRetainedInferableArg arg
      HsPragE _ _ arg -> tcRetainedInferableArg arg
      XExpr (ExpandedThingRn (HSE { hse_exp = arg })) -> tcRetainedInferableArg arg
      _ -> return (False, False)
  where
    declared_field (L _ (HsVar _ lname))
      | not (isDataOcc (nameOccName name))
      = do { mb_id <- tcLookupIdMaybe name
           ; traverse (liftZonkM . zonkTcType . varType) mb_id }
      where name = unLocWithUserRdr lname
    declared_field _ = return Nothing

    -- | Exclude checking-only syntax without traversing compiler type graphs.
    simple_type :: Data a => a -> Bool
    simple_type thing
      | Just ty <- (cast thing :: Maybe (HsType GhcRn))
      , not (simple_shape ty) = False
      | Just ty <- (cast thing :: Maybe Type) = isTauTy ty
      | Just _ <- (cast thing :: Maybe Name) = True
      | otherwise = and (gmapQ simple_type thing)

    -- | Keep explicit quantifiers, contexts and type splices on the usual path.
    simple_shape :: HsType GhcRn -> Bool
    simple_shape HsForAllTy{} = False
    simple_shape HsForEachTy{} = False
    simple_shape HsQualTy{} = False
    simple_shape HsSpliceTy{} = False
    simple_shape _ = True

{- Note [Native list anchors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In fmap toList (lookupGeneric [vector] 0), the declared type of vector can
contain a runtime index. Discover it through the built-in list literal so
the enclosing lookupGeneric call can supply an argument type to fmap's
cache. Otherwise checking toList can choose a fresh index meta before the
lookup argument supplies the actual runtime index. The existing callback
path can choose the erased index from the exact cached argument type.

Report an anchor without marking the literal itself inferable. Checking it
at a known list type works even without ExperimentalDependentTypes. The
standalone list-inference rule has its own extension check. The anchor probe
reads declarations only and excludes overloaded lists. The inner call still
selects its type arguments and checks each element, including its usage.
Keep the original type, evidence and usage of the inferred call in the
existing cache. See Note [Named callbacks at known native inputs].
-}

-- | Specialize the wired-in dollar operator from a checked result and one
-- cached native-anchored RHS. Keep its original type and evidence application.
-- See Note [Immutable native dollar applications].
tcRetainedDollarInst :: Bool -> ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                    -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                    -> TcM (Maybe (HsExpr GhcTc, TcSigmaType), RetainedArgCache)
tcRetainedDollarInst enabled (Check expected) rn_head@(HsVar _ (L _ (WithUserRdr _ name)), _)
                    args fun@(HsVar _ (L _ dollar)) sigma
  | enabled
  , name `hasKey` dollarIdKey, dollar `hasKey` dollarIdKey
  , not (any is_type_arg args)
  , [lhs, rhs] <- [arg | EValArg { ea_arg = arg } <- args]
  , (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , not (null tvs)
  , (_, [_, domain], result) <- splitFunTys body
  , isTauTy body, not (hasNativeDependentExpr (Core.Type body))
  = do { result_ty <-
           do { expected <- liftZonkM (zonkTcType expected)
              ; if complete_result expected then return (Just expected)
                else declared_result 1 lhs }
       ; case result_ty of
           Nothing -> return (Nothing, [])
           Just expected -> do
           { (inferable, anchor) <- tcRetainedInferableArg rhs
           ; tau_domains <- if inferable && anchor then rhs_tau_domains rhs
                            else return False
           ; if not tau_domains then return (Nothing, []) else
        do { (usage, (typed_rhs, rhs_ty)) <- addArgCtxt 2 rn_head rhs $
               tcCollectingUsage (tcInferRho rhs)
           ; rhs_ty <- liftZonkM (zonkTcType rhs_ty)
           ; let cache = [(2, (typed_rhs, rhs_ty, usage))]
                 concrete_tvs = idConcreteTvs dollar
                 matched = do
                   subst <- match_type tvs Subst.emptySubst result expected
                   match_type tvs subst (scaledThing domain) rhs_ty
           ; case matched of
               Just subst
                 | Just inst_tys <- traverse (Subst.lookupTyVar subst) tvs
                 , not (any (`elemVarSet` tyCoVarsOfTypes inst_tys) tvs)
                 , and (zipWith (kind_agrees subst) tvs inst_tys)
                 , eqType (Subst.substTy subst result) expected
                 , eqType (Subst.substTy subst (scaledThing domain)) rhs_ty
                 , isEmptyNameEnv (delListFromNameEnv concrete_tvs (map getName tvs))
                 , and [ isConcreteType ty
                       | (tv, ty) <- zip tvs inst_tys
                       , isJust (lookupNameEnv concrete_tvs (getName tv)) ]
                 -> do { valid <- tcRetainedInstScope inst_tys
                       ; return (if valid
                                 then Just (mkHsWrap (mkWpTyApps inst_tys) fun,
                                            Subst.substTy subst body)
                                 else Nothing, cache) }
               _ -> return (Nothing, cache) } } }
  where
    -- Preserve expected checking for a head with higher-rank callback domains.
    -- Outer retained binders are function arguments, not callback domains.
    rhs_tau_domains (L _ expr)
      = case expr of
          HsApp _ fun _ -> rhs_tau_domains fun
          HsAppType _ fun _ -> rhs_tau_domains fun
          HsPar _ fun -> rhs_tau_domains fun
          HsPragE _ _ fun -> rhs_tau_domains fun
          XExpr (ExpandedThingRn (HSE { hse_exp = fun })) -> rhs_tau_domains fun
          HsVar _ lname
            | isDataOcc (nameOccName (unLocWithUserRdr lname))
            -> do { con <- tcLookupConLike (unLoc lname)
                  ; case con of
                      RealDataCon dc -> check_domains (varType (dataConWorkId dc))
                      _ -> return False }
            | otherwise
            -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
                  ; maybe (return False) (check_domains . varType) mb_id }
          _ -> return False

    check_domains ty = tau_domains <$> liftZonkM (zonkTcType ty)

    tau_domains ty
      | Just ty' <- coreView ty = tau_domains ty'
    tau_domains (ForAllTy _ _ body) = tau_domains body
    tau_domains (CastTy ty _) = tau_domains ty
    tau_domains (DepFunTy DepFunVisible binder body) = isTauTy (varType binder) && tau_domains body
    tau_domains (DepFunTy DepFunInvisible _ _) = False
    tau_domains (FunTy { ft_arg = domain, ft_res = result })
      = isTauTy domain && tau_domains result
    tau_domains _ = True

    complete_result ty
      = isTauTy ty && not (anyFreeVarsOfType (\v -> isTyVar v && isMetaTyVar v) ty)

    -- Read the declared result after the written arguments and dollar's RHS.
    -- Do not instantiate the first operand or inspect its argument expressions.
    declared_result n (L _ expr)
      = case expr of
          HsApp _ fun _ -> declared_result (n+1) fun
          HsPar _ fun -> declared_result n fun
          HsVar _ lname
            | not (isDataOcc (nameOccName (unLocWithUserRdr lname)))
            -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
                  ; case mb_id of
                      Just var | isEmptyNameEnv (idConcreteTvs var)
                        -> do { sigma <- liftZonkM (zonkTcType (varType var))
                              ; let (ma_tvs, body) = tcSplitForAllInvisTyVars sigma
                                    tvs = map snd ma_tvs
                              ; return $ do
                                  guard (isTauTy body
                                         && not (hasNativeDependentExpr (Core.Type body)))
                                  result <- tcRetainedOrdinaryResult n body
                                  guard (complete_result result
                                         && not (any (`elemVarSet` tyCoVarsOfType result) tvs))
                                  return result }
                      _ -> return Nothing }
          _ -> return Nothing

    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _
      | tv `elem` tvs = BindMe
      | otherwise = DontBindMe

    match_type tvs subst template actual
      = do { subst <- tcMatchTyX_BM (bind_outer tvs) subst (typeKind template) (typeKind actual)
           ; tcMatchTyX_BM (bind_outer tvs) subst template actual }

    kind_agrees subst tv ty = eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty)
tcRetainedDollarInst _ _ _ _ _ _ = return (Nothing, [])

{- Note [Immutable native dollar applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wired-in ($) uses Quick Look. quickLookArg1 can instantiate evaluate's
argument before its native RHS is inferred. For example,
  expectMarker label marker $ evaluate (sameNat n Z)
then tries to fill an ordinary meta-variable with Maybe (n :~: Z).
Do not enable native syntax in mutable instantiation variables.

For this operator only, accept exactly two value arguments without VTA and a
complete known tau result. Prefer the expected result. A discarded do statement
can instead supply IO alpha for an unresolved ordinary alpha. In that case,
read the first operand's named declared signature without instantiation. Peel
its written value arguments and the dollar RHS. Require the resulting type to
be complete and independent of every outer forall binder. For example,
expectMarker label marker has the fixed result IO () after the RHS argument.
Reject VTA, constrained or native templates, and heads with concreteness
requirements on this fallback. Do not expand or typecheck the first operand.
Keep the original expected type for final checkResultTy; it can fill the
ordinary alpha with (). This fallback does not assign a native type to a meta.

Require a named, native-anchored RHS and an enabled application capability.
See Note [Imported native application contracts].
Before inference, inspect its head's declared argument domains. Reject a
higher-rank or retained callback domain. For example, withBounded's callback
needs an expected IO result before checking evaluate proof >> pure (). Keep
that call on its existing Quick Look path. Do this before inferring anything;
a fallback after inference would already have lost the expected shape.
Outer retained arguments such as sameNat's Nat binders and ordinary Vec
domains containing indices remain eligible. Infer an eligible RHS once and
save its usage in argument cache position two. Match the original operator's result and second domain
immutably. Match their kinds before their types with the same substitution:
type matching alone does not determine the representation binders of ($).
Require every forall binder, its substituted kind, both exact types and every
runtime binding to agree. Check all original idConcreteTvs choices with
isConcreteType; this checks representation choices, not the native payload.

Apply WpTyApps to the original operator. Do not rewrite ($) to a beta redex or
infer its first operand. Check that operand against the complete ordinary
arrow, including the native RHS type. Use NoQL for instantiation, result
checking and finishAppWithCache. The existing argument cache emits RHS usage
once, and ordinary final subsumption still runs. No evaluation order changes.

If immutable matching fails after inference, retain that cache and use NoQL
for this attempted call's ordinary fallback too. Re-entering Quick Look could
partially typecheck the RHS again, including nested splices and usage. Calls
that fail the eligibility checks before inference keep their existing path.
No general Quick Look rule or native meta-fill restriction changes. See
Note [Quick Look for particular Ids] and
docs/dependent-haskell/REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe.
-}

-- | Immutable choices for successive invisible binder groups. Nothing uses
-- ordinary instantiation. Just [] records a fully consumed immutable plan.
type RetainedInstPlan = Maybe [Type]

-- | Collect invisible foralls and contexts before the first visible argument.
-- Keep their original order in the application; this split is for matching only.
tcRetainedInstTelescope :: Type -> ([TyVar], ThetaType, Type)
tcRetainedInstTelescope sigma
  | (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , (theta, tau) <- tcSplitPhiTy body
  , not (null tvs && null theta)
  = let (rest_tvs, rest_theta, result) = tcRetainedInstTelescope tau
    in (tvs ++ rest_tvs, theta ++ rest_theta, result)
  | otherwise = ([], [], sigma)

-- | Arguments inferred once before immutable index instantiation.
-- The argument position counts source value arguments from one.
type RetainedArgCache = [(Int, (LHsExpr GhcTc, TcRhoType, UsageEnv))]

-- | Infer index-bearing arguments once and match only outer invisible binders.
tcRetainedArgInst :: Bool -> QLFlag -> ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                  -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                  -> TcM (HsExpr GhcTc, TcSigmaType, RetainedArgCache, RetainedInstPlan)
tcRetainedArgInst enabled do_ql expected rn_head args fun sigma
  = if enabled
    then do { expected <- readExpType_maybe expected
            ; tcRetainedArgInstEnabled Nothing do_ql expected rn_head args fun sigma }
    else return (fun, sigma, [], Nothing)

{- Note [Native worker argument inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A native worker can need an erased parameter from a later argument:
  withQ :: forall value. (value :~: QPi) -> value -> QPi
  withQ Refl f  -- f :: QPi
The result does not determine value. Infer f once and match its type against
the ordinary formal domain value. Do not infer Refl against the native
witness template. Check that argument after choosing value := QPi.

Use the existing cache-producing NoQL route for a known head without
concreteness requirements. The original body, including its leading theta,
selects this mode. Classify its tau with isRetainedValueType. Infer only
eligible original tau domains without native syntax. Require a native anchor
among these arguments or an immutable expected-result choice. An overapplied
call starts without a result choice: its final expected type is not the
declared result used for prefix inference.

Match kinds before types with the same outer-binder predicate and immutable
substitution. Retained-aware matching also compares a repeated selected Pi
type in another argument. Require every invisible parameter, its substituted
kind, exact cached domains and current runtime scope. Do not let an original
template variable escape. Do not infer callback results or freshen missing
ordinary parameters in this mode. Other argument-inference modes retain
their existing rules.

Keep every checked expression and usage if matching fails. The ordinary
fallback must consume the same cache. The ordered instantiation plan retains
forall and theta groups after nested contexts and leading VTA. IALL and
instCall still check the original evidence. Final argument subsumption and
result checking still run. No native type is assigned to a mutable meta.
No runtime argument order or function eta rule changes. See
Note [Cached retained argument inference] and
docs/dependent-haskell/REFERENCES.md#native-worker-argument-inference.
-}

-- | Match cached index arguments after a caller selects native checking.
-- See Note [Imported native application contracts].
-- See Note [Available expected types in native applications].
-- See Note [Generic retained argument inference] for overapplication.
-- See Note [Native inference after visible type applications] for the mode argument.
-- See Note [Native worker argument inference] for native templates.
-- See Note [Family views in native argument inference] for cached type matching.
tcRetainedArgInstEnabled :: Maybe Int -> QLFlag -> Maybe TcRhoType -> (HsExpr GhcRn, SrcSpan)
                         -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                         -> TcM (HsExpr GhcTc, TcSigmaType, RetainedArgCache, RetainedInstPlan)
tcRetainedArgInstEnabled after_vta do_ql expected rn_head args fun sigma
  = do { declared <- case do_ql of
           NoQL ->
             do { sigma' <- liftZonkM (zonkTcType sigma)
                ; expected' <- mapM (liftZonkM . zonkTcType) expected
                ; tcRetainedKnownArgPlan expected' fun sigma' args }
           _ -> return Nothing
       ; case declared of
           Just choices -> return (fun, sigma, [], Just choices)
           Nothing -> tcRetainedArgInstCached after_vta do_ql expected rn_head args fun sigma }

-- | Run the existing usage-cached inference after declaration selection fails.
-- See Note [Declared retained-domain choices].
tcRetainedArgInstCached :: Maybe Int -> QLFlag -> Maybe TcRhoType -> (HsExpr GhcRn, SrcSpan)
                       -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                       -> TcM (HsExpr GhcTc, TcSigmaType, RetainedArgCache, RetainedInstPlan)
tcRetainedArgInstCached after_vta NoQL expected rn_head args fun sigma
  | not (any is_type_arg args)
  , (outer_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let (tvs, theta, tau) = tcRetainedInstTelescope sigma
        nested = length tvs /= length outer_tvs
  , let index_tvs = filter (isNativeIndexType . tyVarKind) tvs
        native_worker = hasNativeDependentExpr (Core.Type body)
  , not (null tvs)
  , null theta || hasNoConcreteTyVars fun
  , let (_mas, domains, result) = splitFunTys tau
        arity = length domains
        value_args = [arg | EValArg { ea_arg = arg } <- args]
        overapplied = arity < length value_args
  , arity == length value_args
    || (overapplied && arity > 0 && hasNoConcreteTyVars fun)
  , if native_worker
    then hasNoConcreteTyVars fun && isRetainedValueType tau
    else isTauTy tau && not (isFunTy result)
  = do { expected_ty <- mapM (liftZonkM . zonkTcType) expected
       ; let index_route = not native_worker && not (null index_tvs)
             initial_subst | overapplied = Subst.emptySubst
                           | otherwise = result_subst expected_ty index_route native_worker tvs result
             result_anchor = any (hasNativeDependentExpr . Core.Type)
                               [ty | tv <- tvs, Just ty <- [Subst.lookupTyVar initial_subst tv]]
             needed_tvs | index_route = index_tvs
                        | otherwise = filter (isNothing . Subst.lookupTyVar initial_subst) tvs
       ; eligible <- if native_worker
                     then do { shapes <- mapM tcRetainedInferableArg (take arity value_args)
                             ; let eligible domain (inferable, anchor)
                                     = (inferable && isTauTy (scaledThing domain)
                                        && not (hasNativeDependentExpr (Core.Type (scaledThing domain))),
                                        anchor)
                                   selected = zipWith eligible domains shapes
                             ; return (if result_anchor || any (uncurry (&&)) selected
                                       then Just (map fst selected)
                                       else Nothing) }
                     else if index_route
                     then if hasNoConcreteTyVars fun
                          then do { shapes <- mapM tcRetainedInferableArg
                                                (take arity value_args)
                                  ; return (Just (zipWith
                                      (\domain (inferable, _) -> inferable ||
                                        any (`elemVarSet` tyCoVarsOfType (scaledThing domain)) index_tvs)
                                      domains shapes)) }
                          else return (Just (repeat True))
                     else if hasNoConcreteTyVars fun
                             && isJust (tcRetainedOrdinaryResult arity tau)
                          then do { shapes <- mapM tcRetainedInferableArg
                                                (take arity value_args)
                                  ; deferred <- zipWithM defer_named domains (take arity value_args)
                                  ; return (if result_anchor || any snd shapes
                                            then Just (zipWith (\shape defer -> fst shape && not defer)
                                                               shapes deferred)
                                            else Nothing) }
                          else return Nothing
       ; case eligible of
           Nothing -> return (fun, sigma, [], Nothing)
           Just inferable -> do
             { let domain_at pos = scaledThing (domains !! (pos-1))
                   matching_arg (pos, entry) =
                     let arg = value_args !! (pos-1)
                     in addArgCtxt (pos + fromMaybe 0 after_vta) rn_head arg $
                        do { ty <- liftZonkM (zonkTcType (actual_type entry))
                           ; loc <- getCtLocM (lexprCtOrigin arg) (Just TypeLevel)
                           ; view <- reductionReducedType <$> tcRewriteFamilyAxioms loc ty
                           ; return (loc, (domain_at pos, view)) }
                   match_indices = tcMatchRetainedIndexArgs (bind_outer tvs)
             ; initial_cache <- infer_args needed_tvs inferable domains 1 args
             ; initial_args <- mapM matching_arg initial_cache
             ; let initial_pairs = map snd initial_args
                   match_worker subst = foldM (\s (formal, actual) ->
                       matchRetainedAppType tvs s formal actual) subst initial_pairs
             ; (matched, cache, templates) <- if index_route
                 then do
                   { first <- match_indices initial_subst initial_args
                   ; case first of
                       Nothing -> return (Nothing, initial_cache, [])
                       Just (subst, views)
                         | hasNoConcreteTyVars fun ->
                           do { let missing = filter (isNothing . Subst.lookupTyVar subst) tvs
                                    remaining = zipWith (\pos eligible -> eligible && isNothing (lookup pos initial_cache))
                                                        [1..] inferable
                              ; extra_cache <- infer_args missing remaining domains 1 args
                              ; extra_args <- mapM matching_arg extra_cache
                              ; extra <- match_indices subst extra_args
                              ; return (fst <$> extra, initial_cache ++ extra_cache,
                                        views ++ maybe [] snd extra) }
                         | otherwise -> return (Just subst, initial_cache, views) }
                 else do
                   { initial_match <- if native_worker
                       then return (case match_worker initial_subst of
                                      Nothing -> match_worker Subst.emptySubst
                                      other -> other)
                       else do { m <- tcRetainedMatchArgs tvs initial_subst initial_pairs
                               ; case m of
                                   Nothing -> tcRetainedMatchArgs tvs Subst.emptySubst initial_pairs
                                   other -> return other }
                   ; (m, cache) <- case initial_match of
                       Just subst | not native_worker ->
                         tcRetainedCallbackArgs tvs domains args subst initial_cache
                       _ -> return (initial_match, initial_cache)
                   ; return (m, cache, map (domain_at . fst) cache) }
             ; actual_tys <- mapM (fmap (snd . snd) . matching_arg) cache
             ; choices <- case matched of
                            Just subst | native_worker
                              -> return $ do { inst_tys <- traverse (Subst.lookupTyVar subst) tvs
                                             ; return (subst, inst_tys) }
                            Just subst -> tcRetainedArgChoices index_route tvs subst
                            Nothing -> return Nothing
             ; case choices of
                 Just (subst, inst_tys)
                   | index_route || any (hasNativeDependentExpr . Core.Type) inst_tys
                   , not nested || any (hasNativeDependentExpr . Core.Type) inst_tys
                   , not native_worker || not (any (`elemVarSet` tyCoVarsOfTypes inst_tys) tvs)
                   , and (zipWith (kind_agrees subst) tvs inst_tys)
                   , and (zipWith (\formal actual -> eqType (Subst.substTy subst formal) actual)
                                  templates actual_tys)
                   -> do { valid <- tcRetainedInstScope inst_tys
                         ; if valid
                           then if nested || isJust after_vta
                                then return (fun, sigma, cache, Just inst_tys)
                                else return (mkHsWrap (mkWpTyApps inst_tys) fun,
                                             Subst.substTy subst body, cache, Nothing)
                           else return (fun, sigma, cache, Nothing) }
                 _ -> return (fun, sigma, cache, Nothing) } }
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _
      | tv `elem` tvs = BindMe
      | otherwise = DontBindMe

    kind_agrees subst tv ty = eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty)

    result_subst expected index_route native_worker tvs result
      | Just expected_ty <- expected
      , native_worker
      , isRetainedValueType expected_ty
      , Just subst <- matchRetainedAppType tvs Subst.emptySubst result expected_ty
      = subst
      | Just expected_ty <- expected
      , not native_worker
      , isTauTy expected_ty
      , not index_route || hasNativeDependentExpr (Core.Type expected_ty)
      , Just subst <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst result expected_ty
      = subst
      | otherwise = Subst.emptySubst

    actual_type (_, ty, _) = ty

    -- See Note [Named callbacks at known native inputs].
    defer_named domain arg
      | FunTy { ft_af = af, ft_mult = mult } <- coreFullView (scaledThing domain)
      , isVisibleFunArg af, isManyTy mult
      = isJust <$> tcRetainedNamedCallbackType arg
      | otherwise = return False

    infer_args :: [TyVar] -> [Bool] -> [Scaled Type] -> Int -> [HsExprArg 'TcpRn]
               -> TcM RetainedArgCache
    infer_args _ _ _ _ [] = return []
    infer_args tvs (inferable : rest) (domain : domains) pos
               (EValArg { ea_arg = arg } : args)
      = do { entry <- if inferable && any (`elemVarSet` tyCoVarsOfType (scaledThing domain)) tvs
                      then addArgCtxt (pos + fromMaybe 0 after_vta) rn_head arg $
                           do { (usage, (typed, actual)) <- tcCollectingUsage (tcInferRho arg)
                              ; return [(pos, (typed, actual, usage))] }
                      else return []
           ; entries <- infer_args tvs rest domains (pos+1) args
           ; return (entry ++ entries) }
    infer_args tvs inferable domains pos (_ : args)
      = infer_args tvs inferable domains pos args
tcRetainedArgInstCached _ _ _ _ _ fun sigma = return (fun, sigma, [], Nothing)

-- | Match cached index arguments and keep the formal views used for selection.
-- See Note [Matching a computed nonempty length].
tcMatchRetainedIndexArgs :: BindTvFun -> Subst.Subst -> [(CtLoc, (Type, Type))]
                         -> TcM (Maybe (Subst.Subst, [Type]))
tcMatchRetainedIndexArgs bind seed entries = runMaybeT (match_group seed entries)
  where
    match_group subst pairs = mapAccumLM finish selected pending
      where
        (selected, pending) = mapAccumL select subst pairs

    -- A later argument (or field) can select an input to an earlier family.
    select subst entry@(_, (formal, actual))
      | Just subst' <- tcMatchTyX_BM bind subst formal actual = (subst', Left formal)
      | otherwise = (subst, Right entry)

    finish subst (Left formal) = return (subst, formal)
    finish subst (Right (loc, (formal, actual)))
      | Just subst' <- tcMatchTyX_BM bind subst instantiated actual = return (subst', instantiated)
      | not (hasNativeDependentExpr (Core.Type actual)) = mzero
      | TyConApp tc _ <- instantiated, isTypeFamilyTyCon tc
      = do { view <- liftMaybeT (reductionReducedType <$> tcRewriteFamilyAxioms loc instantiated)
           ; subst' <- MaybeT (pure (tcMatchTyX_BM bind subst view actual))
           ; return (subst', view) }
      | TyConApp tc fields <- instantiated
      , TyConApp tc' args <- coreFullView actual
      , tc == tc', isGenerativeTyCon tc Nominal, fields `equalLength` args
      = do { (subst', views) <- match_group subst [(loc, pair) | pair <- zip fields args]
           ; return (subst', mkTyConApp tc views) }
      | otherwise = mzero
      where
        instantiated = coreFullView (Subst.substTy subst formal)

{- Note [Matching a computed nonempty length]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider safeHead :: Vec a (1 + k) -> a applied to replicateNative (1 + n) x.
The inner application has an inferred length S n. The formal length 1 + k
reduces to S k by the same nominal family equations. Matching those constructors
selects k := n while the runtime binder n is in scope.

First collect the structural matches from the cached arguments. For example,
a later Proxy flag can fix flag for an earlier Vec a (Select flag k). Check
every remaining pair against these same immutable choices. Descend through
equal generative type constructors, using the same rule for their fields.
Reduce a mismatched family application at the source argument's location. Keep the successful formal views
for the final exact-domain check. The original argument cache supplies the
expression and type to subsumption, which constructs its own coercion.
A stuck sum n + 1 supplies no successor equation for an unknown n.
See Note [Family views in native argument inference] and the Agda conversion
reference in docs/dependent-haskell/REFERENCES.md.
-}

{- Note [Family views in native argument inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A cached argument can have type OptionalPayload (Just n), with a nominal
family equation reducing it to Vec Int n. Structural matching against the
formal Vec a index cannot select index from the unreduced family head.
Normalize cached actual types at Nominal role for immutable selection.
Use the solver's bounded family-only rewrite. Restore the original argument
context before creating its location. Cache positions use the same value
argument list and VTA offset as infer_args. The depth diagnostic must identify
that argument. See Note [Bounded frontend family views] in GHC.Tc.Solver.
Use the same normalized views for initial matching, extra matching and the
final exact-domain check. This does not unwrap newtypes or invert families.

Keep the original expression, inferred type and usage in each cache entry.
tcValArgsWithCache checks that original type against the selected domain with
tcSubTypeSigma. That check constructs the family coercion and emits the saved
usage once. This follows mkInferredPolyId in GHC.Tc.Gen.Bind, which normalizes
an inferred type and reconstructs its coercion through subsumption. A stuck
family still uses the existing fallback. No native meta assignment is added.
-}

-- | Match complete generic argument candidates after ordinary head inference.
-- See Note [Ordinary action heads in native inference].
tcRetainedMatchArgs :: [TyVar] -> Subst.Subst -> [(Type, Type)]
                    -> TcM (Maybe Subst.Subst)
tcRetainedMatchArgs tvs = go
  where
    go subst [] = return (Just subst)
    go subst ((formal, actual) : rest)
      = do { formal <- liftZonkM (zonkTcType (Subst.substTy subst formal))
           ; actual <- liftZonkM (zonkTcType actual)
           ; case match subst formal actual of
               Just subst' -> go subst' rest
               Nothing
                 | Just (formal_head, _) <- splitAppTy_maybe formal
                 , Just (actual_head, _) <- splitAppTy_maybe actual
                 , ordinary_head formal_head, ordinary_head actual_head
                 , meta_head formal_head || meta_head actual_head
                 -> do { _ <- unifyType Nothing formal_head actual_head
                       ; formal <- liftZonkM (zonkTcType formal)
                       ; actual <- liftZonkM (zonkTcType actual)
                       ; case match subst formal actual of
                           Just subst' -> go subst' rest
                           Nothing -> return Nothing }
                 | otherwise -> return Nothing }

    match = tcMatchTyX_BM bind_outer
    bind_outer tv _ | tv `elem` tvs = BindMe
                    | otherwise = DontBindMe
    ordinary_head ty = not (hasNativeDependentExpr (Core.Type ty))
                    && not (any (`elemVarSet` tyCoVarsOfType ty) tvs)
    meta_head ty | TyVarTy tv <- coreFullView ty = isMetaTyVar tv
                 | otherwise = False

{- Note [Ordinary action heads in native inference]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In effect >> pure vector, effect fixes the ordinary monad head to IO.
The second argument can infer f (Vec Int n), with an ordinary head meta f.
Immutable matching cannot equate IO and f. A fresh complete candidate has
the same conflict. Do not discard the second argument or use a prefix of a
failed candidate.

The generic route can use ordinary unification on those head types only.
Require a head meta, no native syntax in either head or its metadata, and no
outer template variables. Then repeat exact matching of the whole argument.
The payload and runtime index remain immutable choices. Keep the separate
native-index route, all cached arguments, usage and final subsumption checks.
If an ordinary head cannot unify, normal type errors still apply.
-}

-- | Infer a callback result after earlier arguments determine its input type.
-- Keep each checked callback and its usage in the ordinary argument cache.
-- See Note [Scoped native callback results].
tcRetainedCallbackArgs :: [TyVar] -> [Scaled Type] -> [HsExprArg 'TcpRn]
                       -> Subst.Subst -> RetainedArgCache
                       -> TcM (Maybe Subst.Subst, RetainedArgCache)
tcRetainedCallbackArgs tvs domains args subst cache
  = go subst cache (zip3 [1..] domains [arg | EValArg { ea_arg = arg } <- args])
  where
    go subst cache []
      = do { subst <- foldM expose_choice subst tvs
           ; return (Just subst, cache) }
    go subst cache ((pos, domain, arg) : rest)
      | isNothing (lookup pos cache)
      , FunTy { ft_af = af, ft_mult = mult, ft_arg = input }
          <- coreFullView (Subst.substTy subst (scaledThing domain))
      , isVisibleFunArg af, isManyTy mult
      , not (mentions_templates input)
      , isTauTy input, typeKind input `eqType` liftedTypeKind
      , hasNativeDependentExpr (Core.Type input)
      , Just (build, head_arg) <- retainedNamedCallback arg
      = setSrcSpanA (getLoc arg) $ do
          { candidate <- tcRetainedNamedCallbackType arg
          ; case candidate of
              Just sigma
                | (callback_tvs, _, tau) <- tcRetainedInstTelescope sigma
                , FunTy { ft_arg = formal_input } <- coreFullView tau
                , Just selected <- matchRetainedAppType callback_tvs Subst.emptySubst formal_input input
                , Just choices <- traverse (Subst.lookupTyVar selected) callback_tvs
                , not (any (`elemVarSet` tyCoVarsOfTypes choices) callback_tvs)
                , and (zipWith (\tv ty ->
                         eqType (Subst.substTy selected (tyVarKind tv)) (typeKind ty))
                       callback_tvs choices)
                , let actual = Subst.substTy selected tau
                , Subst.substTy selected formal_input `eqType` input
                , all no_ql_vars (sigma : actual : input : choices)
                -> do
                  { valid <- tcRetainedInstScope (sigma : actual : input : choices)
                  ; if not valid then go subst cache rest else
                do { (usage, typed) <- tcCollectingUsage $ tcRetainedAppEvidence True $
                       do { let rn_head = (unLoc head_arg, locA (getLoc head_arg))
                          ; (fun, sigma') <- tcInferAppHead rn_head
                          ; (inst_args, rho) <- tcInstFunWithExpected Nothing (Just choices)
                                                NoQL True rn_head fun sigma' []
                          ; ensure_expected <- tcSubTypeSigma (lexprCtOrigin arg)
                                                 GenSigCtxt rho actual
                          ; checked_args <- tcValArgsWithCache [] NoQL rn_head inst_args
                          ; return (mkHsWrap ensure_expected (unLoc (build (rebuildHsApps fun checked_args)))) }
                   ; let entry = (pos, (fmap (const typed) arg, actual, usage))
                         matched = match_type subst (scaledThing domain) actual
                   ; case matched of
                       Just subst' -> go subst' (cache ++ [entry]) rest
                       Nothing -> return (Nothing, cache ++ [entry]) } }
              _ -> go subst cache rest }
      | isNothing (lookup pos cache)
      , FunTy { ft_af = af, ft_mult = mult, ft_arg = input, ft_res = output }
          <- coreFullView (Subst.substTy subst (scaledThing domain))
      , isVisibleFunArg af, isManyTy mult
      , not (mentions_templates input)
      , mentions_templates output
      , isTauTy input, typeKind input `eqType` liftedTypeKind
      , Just (build, annotation, matches, body_span) <- callback arg
      = setSrcSpanA (getLoc arg) $ do
          { valid <- tcRetainedInstScope [input]
          ; if not valid then return (Nothing, cache) else
        do { (usage, (matches', result)) <- tcCollectingUsage $
               runInferRho $ \result_ty ->
                 tcMatches (LamAlt LamSingle) tcBody
                   [mkCheckExpFunPatTy (Scaled ManyTy input)] result_ty matches
           ; result <- liftZonkM (zonkTcType result)
           -- tcMatches has restored the application environment here.
           ; valid_result <- tcRetainedInstScope [result]
           ; unless valid_result $ setSrcSpanA body_span $
               failWithTc (TcRnRetainedQuantificationError (RetainedCallbackResult result))
           ; unless (isTauTy result) $ setSrcSpanA body_span $
               failWithTc (TcRnRetainedQuantificationError (RetainedInference result))
           ; let actual = mkVisFunTyManyU input result
                 typed = build (HsLam annotation LamSingle matches')
                 cache' = cache ++ [(pos, (typed, actual, usage))]
                 candidate = case match_type subst output result of
                   Just subst' -> Just subst'
                   Nothing -> do
                     (_, formal_payload) <- splitAppTy_maybe output
                     (_, actual_payload) <- splitAppTy_maybe result
                     match_type subst formal_payload actual_payload
           ; case candidate of
               Just subst'
                 | let expected = Subst.substTy subst' (scaledThing domain)
                 , not (mentions_templates expected)
                 -> do { typed <- tcRetainedAppEvidence True $
                            do { wrap <- tcSubTypeSigma (exprCtOrigin (unLoc arg))
                                           GenSigCtxt actual expected
                               ; return (mkHsWrap wrap (unLoc typed)) }
                       ; let entry = (pos, (fmap (const typed) arg, expected, usage))
                       ; go subst' (cache ++ [entry]) rest }
               _ -> return (Nothing, cache') } }
      | otherwise = go subst cache rest

    mentions_templates ty = any (`elemVarSet` tyCoVarsOfType ty) tvs
    no_ql_vars = not . any (\v -> isTcTyVar v && isQLInstTyVar v) . tyCoVarsOfTypeList

    expose_choice subst tv
      | Just ty <- Subst.lookupTyVar subst tv
      = do { ty <- liftZonkM (zonkTcType ty)
           ; let subst' = Subst.extendSubstInScopeSet subst (tyCoVarsOfType ty)
           ; return (Subst.extendTvSubst subst' tv ty) }
      | otherwise = return subst

    match_type subst formal actual
      = do { subst <- tcMatchTyX_BM bind_outer subst (typeKind formal) (typeKind actual)
           ; tcMatchTyX_BM bind_outer subst formal actual }

    bind_outer tv _ | tv `elem` tvs = BindMe
                    | otherwise = DontBindMe

    -- Keep parentheses and source locations on the cached lambda.
    callback :: LHsExpr GhcRn
             -> Maybe ( HsExpr GhcTc -> LHsExpr GhcTc, XLam GhcTc
                      , MatchGroup GhcRn (LHsExpr GhcRn), SrcSpanAnnA )
    callback (L loc (HsPar annotation arg))
      = do { (build, lam_annotation, matches, body_span) <- callback arg
           ; return (L loc . HsPar annotation . build, lam_annotation, matches, body_span) }
    callback (L loc (HsLam annotation LamSingle matches))
      | [L _ (Match { m_pats = L _ [L _ VarPat{}]
                    , m_grhss = GRHSs { grhssGRHSs = grhss } })] <- unLoc (mg_alts matches)
      , [L _ (GRHS _ [] body)] <- NE.toList grhss
      = Just (L loc, annotation, matches, getLoc body)
    callback _ = Nothing

-- | Recognize a named callback without checking it or collecting usage.
-- See Note [Named callbacks at known native inputs].
retainedNamedCallback :: LHsExpr GhcRn
                      -> Maybe (HsExpr GhcTc -> LHsExpr GhcTc, LHsExpr GhcRn)
retainedNamedCallback (L loc (HsPar annotation arg))
  = do { (build, head_arg) <- retainedNamedCallback arg
       ; return (L loc . HsPar annotation . build, head_arg) }
retainedNamedCallback arg@(L loc (HsVar _ lname))
  | not (isDataOcc (nameOccName (unLocWithUserRdr lname))) = Just (L loc, arg)
retainedNamedCallback _ = Nothing

-- | Read a polymorphic callback declaration without instantiation or usage.
-- See Note [Named callbacks at known native inputs].
tcRetainedNamedCallbackType :: LHsExpr GhcRn -> TcM (Maybe TcSigmaType)
tcRetainedNamedCallbackType arg
  | Just (_, L _ (HsVar _ lname)) <- retainedNamedCallback arg
  = do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
       ; case mb_id of
           Just var | isEmptyNameEnv (idConcreteTvs var) ->
             do { sigma <- liftZonkM (zonkTcType (varType var))
                ; let (tvs, _, tau) = tcRetainedInstTelescope sigma
                ; return $ case coreFullView tau of
                    FunTy { ft_af = af, ft_mult = mult }
                      | not (null tvs), isTauTy tau, isVisibleFunArg af, isManyTy mult
                      -> Just sigma
                    _ -> Nothing }
           _ -> return Nothing }
  | otherwise = return Nothing

{- Note [Named callbacks at known native inputs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In map toList values, values :: [Vec Int n] fixes map's input to Vec Int n.
Inferring toList first would allocate an index meta n0. The later argument
cannot assign that meta to the actual runtime n. Defer a named polymorphic
callback in the ordinary generic native-anchored argument route. Infer the
other arguments first. Do not change the index-binder or native-worker routes.

Once the input is known, match the callback declaration's input immutably.
Require every callback binder, substituted kinds, an exact input match and
valid runtime scope. Instantiate the original declaration with existing IALL.
This preserves forall/context evidence order and concreteness checks. Cache
the typed callback and its usage once. Preserve all earlier argument and result
choices when matching this callback. Final argument subsumption and result
checking still run. The callback value is not eta-expanded or evaluated.

Keep this route to bare or parenthesized named callbacks with a tau Many
function body and no concrete-variable requirements. Other expressions keep
their existing inference path. Partial explicit type applications on the
callback keep that path too. Outer higher-order function VTA still uses its
existing ordered immutable plan. When that plan has consumed all binders,
tcValArg uses the same callback check at its known native input. It emits the
saved usage under the original argument scaling. A failed callback match keeps
the checked cache and its usage. Candidate types do not provide equality evidence.
-}

{- Note [Scoped native callback results]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A known action fixes the input of its ordinary continuation. For example,
IO (Vec Int n) fixes xs in (>>=) action (\xs -> pure xs). Do not allocate the
result beta before checking this callback. A mutable beta cannot hold its
native result type.

Check one bare variable pattern at the known Many input type. Infer the body
through an InferResult and keep the typed lambda and its usage exactly once.
The match checker retains its normal level, promotion and branch checks.
After it returns, validate the result in the outer application environment.
An input or inner local Id cannot escape into the ordinary function result.
Do not infer a retained arrow or an existential package.

Match the inferred result immutably. An ordinary monad head can still be a
meta-variable: pure xs can infer f (Vec Int n). In that case, match the payload
and keep ordinary callback subsumption to check the whole type, including f.
It can assign f := IO without assigning a native type to a meta-variable.
Cache the wrapped callback at its checked type. Final argument and result
subsumption still run. Preserve IALL evidence order and all native scope,
kind, concreteness and Quick Look guards.

Agda expands a do bind to ordinary application and a lambda. Its local
context stores the checked term and type. This implementation keeps GHC's
runtime Ids opaque and keeps Haskell evaluation unchanged. See
docs/dependent-haskell/REFERENCES.md#scoped-native-source-inference.
-}

-- | Preserve matched native choices and freshen undetermined ordinary binders.
-- See Note [Ordinary parameters in native applications].
tcRetainedArgChoices :: Bool -> [TyVar] -> Subst.Subst
                    -> TcM (Maybe (Subst.Subst, [Type]))
tcRetainedArgChoices index_route tvs subst
  | Just choices <- traverse (Subst.lookupTyVar subst) tvs
  = return (Just (subst, choices))
  | not index_route
  , any (hasNativeDependentExpr . Core.Type)
        [ty | tv <- tvs, Just ty <- [Subst.lookupTyVar subst tv]]
  = go subst [] tvs
  | otherwise = return Nothing
  where
    go subst choices [] = return (Just (subst, reverse choices))
    go subst choices (tv : tvs)
      | Just ty <- Subst.lookupTyVar subst tv = go subst (ty : choices) tvs
      | isNativeIndexType (Subst.substTy subst (tyVarKind tv)) = return Nothing
      | otherwise = do { (subst, tv) <- newMetaTyVarX subst tv
                       ; go subst (mkTyVarTy tv : choices) tvs }

{- Note [Ordinary parameters in native applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A known action IO (Vec Int n) determines m and a for (>>=), but its generated
continuation may still need to infer an ordinary result b. An exact argument
match can therefore leave b undetermined. Preserve the matched native input
and use newMetaTyVarX for missing ordinary binders. It creates ordinary TauTvs
at the current level in telescope order, with kinds substituted by the choices
already made. Check the continuation against Vec Int n -> IO beta. Its body
can then determine beta := [Int] without assigning a native type to a meta.

Complete only a successful partial match containing a native choice. Keep
index binders on the existing complete immutable route. Check each missing
binder's substituted kind before allocation; it must not be a native domain.
A failed joint argument match still fails. Preserve kind, exact cached-domain,
runtime scope and concreteness checks, the ordinary evidence order and
once-only cached usage. A native result must be determined before allocating
beta; see Note [Scoped native callback results]. Final subsumption remains
authoritative. See Note [Immutable instantiation of nested contexts].
-}

{- Note [Ordinary heads in native argument matching]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Inline pure (replicateNative n x) can infer f (Vec Int n) with an ordinary
head meta f. An expected IO result seeds m := IO for the enclosing bind, but
immutable matching cannot assign the actual f to IO. The native payload is
already known; ordinary result checking should determine this ordinary head.

If result-seeded generic matching fails, independently match all cached formal
and actual argument types from the empty substitution. This can choose m := f
and a := Vec Int n exactly. Complete ordinary parameters as described in
Note [Ordinary parameters in native applications]. Final result subsumption
can then assign f := IO and beta := [Int] without a native meta-variable fill.

This is a separate joint candidate, not a successful prefix of a failed match.
Conflicting cached arguments still reject. Preserve exact domain, kind, scope
and concreteness checks, expected checking of continuations and all final
subsumption checks. Native index matching keeps its existing result seed.
-}

{- Note [Available expected types in native applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary enclosing application can supply an expected type through filled
meta-variables. For example, a do bind can check against m beta after m and
beta have already been assigned IO and [Int]. Immutable matching must read
those assignments before matching the known action IO (Vec Int n). Otherwise
it treats the expected metas as rigid, misses the exact native input, and
falls back to ordinary instantiation that cannot hold that input type.

Read an available expected type with readExpType_maybe, then use the within-TC
zonker. Do not force or allocate an empty inference cell. This only exposes
existing assignments. Keep the final result subsumption, argument cache and
scope checks. No native type is assigned to a meta-variable.

A matched native expected-result choice also anchors generic instantiation.
For pure bottomPayload at IO (Vec Int n), it determines the argument domain
before the polymorphic payload is checked. For (>>) it can leave an ordinary
input parameter to infer from the first action. Do not require a complete
result seed or infer an argument whose parameters are already determined.

The exact expected-result matcher checks kinds before types, with the same
outer-binder predicate. For error :: forall r (a :: TYPE r). HasCallStack =>
String -> a, this determines r := LiftedRep before a := IO (Vec Int n).
Keep the original theta, IALL evidence order and concreteness checks. This is
the same two-step match used for native dollar applications, not a special
rewrite of error. See Note [tcMatchTy vs tcMatchTyKi] in GHC.Core.Unify.
-}

-- | Check runtime identifiers against their current lexical or module bindings.
-- Normalize type and multiplicity metadata without changing identifier identity.
-- See Note [Native index bindings during typechecking].
tcRetainedInstScope :: [Type] -> TcM Bool
tcRetainedInstScope tys
  = do { local_env <- getLclTypeEnv
       ; global_env <- tcg_type_env <$> getGblEnv
       ; givens <- tcl_native_givens . tcl_lcl_ctxt <$> getLclEnv
       ; givens <- mapM (liftZonkM . zonkId) givens
       ; let -- The outer Maybe distinguishes an absent binding from a
             -- present binding of the wrong sort. Only absence permits
             -- the lexical Given fallback.
             current_binding v
               = case lookupNameEnv local_env (getName v) of
                   Just (ATcId { tct_id = local_id }) -> Just (Just (True, local_id))
                   Just _ -> Just Nothing
                   Nothing -> case lookupTypeEnv global_env (getName v) of
                     Just (AnId module_id) -> Just (Just (False, module_id))
                     Just _ -> Just Nothing
                     Nothing -> Nothing
             given_predicates = map varType givens
             given_prototypes = filter isNonCoVarId $
                                  concatMap tyCoVarsOfTypeList given_predicates
             valid_binding lexical occurrence binding
               = occurrence == binding
                 && ((isNativeIndexId occurrence && isNativeIndexId binding)
                     || (lexical && not (isExternalName (getName occurrence))
                         && isNativeOpaqueValueId occurrence
                         && isNativeOpaqueValueId binding))
                 && eqType (varType occurrence) (varType binding)
                 && isManyTy (idMult occurrence) && isManyTy (idMult binding)
             valid_runtime_id v
               | not (isNonCoVarId v) = return True
               | otherwise
               = do { occurrence <- liftZonkM (zonkId v)
                    ; case current_binding v of
                        Just (Just (lexical, binding)) ->
                          do { binding <- liftZonkM (zonkId binding)
                             ; return (valid_binding lexical occurrence binding) }
                        Just Nothing -> return False
                        Nothing -> do
                          { prototypes <- mapM (liftZonkM . zonkId)
                                            (filter (== v) given_prototypes)
                          ; return (not (null prototypes)
                                    && all (valid_binding True occurrence) prototypes
                                    && not (any (anyFreeVarsOfType (bad_prototype occurrence))
                                                given_predicates)) } }
             bad_prototype occurrence prototype
               = isNonCoVarId prototype && prototype == occurrence
                 && not (valid_binding True occurrence prototype)
       ; and <$> mapM valid_runtime_id (tyCoVarsOfTypesList tys) }

{- Note [Native index bindings during typechecking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source IO binding such as n <- readIORef cell can still have type or
multiplicity metadata that contains filled meta-variables. Before testing its
native index domain and Many multiplicity, use the within-TC zonkId to read
those assignments. This updates only the Id's type and multiplicity. It does
not clone the Name or Unique, allocate a new index, or fill a native meta.
Do not use the final zonker's zonkIdOcc here.

Immutable instantiation must also validate each free runtime index against
its current binding. Lexical bindings are ATcIds in the local environment.
Same-module top-level LocalIds can instead be AnIds in tcg_type_env. For
example, evaluate (sameNat bottomLength Z) and PackVec bottomLength leftVector
need immutable instantiation at the actual bottomLength index. Checking only
the local environment would reject that binding and fall back to a forbidden
native meta-variable fill.

A hidden retained binder can instead have its exact prototype certified by
an actual lexical Given predicate. Use that fallback only when no ordinary
binding exists. See Note [Declared retained argument refinement]. CoVars do
not enter this runtime-value judgment.

Structural indices can also contain opaque local values. Check their exact
lifted types and lexical identities. An eligible ordinary function head keeps
its separate closed U/Many call policy. Keep the existing module lookup for
structural indices. See Note [Source opaque call indices] in GHC.Tc.Gen.HsType.

Look in the local environment first, then the existing module type environment.
Do not load interfaces for a missing or stale name. Normalize both the index
occurrence and its authoritative binding before checking their identity,
exact domain and Many multiplicity. Imported GlobalIds, linear indices and
missing bindings still fail. The existing
Core Tidy alias step keeps accepted module indices local when Tidy globalises
their top-level bindings. Keep the Core index grammar unchanged.

See docs/dependent-haskell/PLAN.md and
docs/dependent-haskell/REFERENCES.md#runtime-equality-and-aligned-zip-next-source-probe.
-}

-- Is the argument supposed to instantiate a forall?
--
-- In other words, given a function application `fn arg`,
-- can we look at the `arg` and conclude that `fn :: forall x. t`
-- or `fn :: forall x -> t`?
--
-- This is a conservative heuristic that returns `False` for "don't know".
-- Used to improve error messages only.
-- See Note [VTA for out-of-scope functions].
looks_like_type_arg :: HsExprArg 'TcpRn -> Bool
looks_like_type_arg ETypeArg{} =
  -- The argument is clearly supposed to instantiate an invisible forall,
  -- i.e. when we see `f @a`, we expect `f :: forall x. t`.
  True
looks_like_type_arg EValArg{ ea_arg = L _ e } =
  -- Check if the argument is supposed to instantiate a visible forall,
  -- i.e. when we see `f (type Int)`, we expect `f :: forall x -> t`,
  --      but not if we see `f True`.
  -- We can't say for sure though. Part 2 of GHC Proposal #281 allows
  -- type arguments without the `type` qualifier, so `f True` could
  -- instantiate `forall (b :: Bool) -> t`.
  case stripParensHsExpr e of
    HsEmbTy _ _ -> True
    _           -> False
looks_like_type_arg _ = False

addArgCtxt :: Int -> (HsExpr GhcRn, SrcSpan) -> LHsExpr GhcRn
           -> TcM a -> TcM a
-- There are 2 cases:
-- 1. In the normal case, we add an informative context
--     (<=> location span of f or head of application chain is user located)
--     "In the third argument of f, namely blah"
-- 2. If head of the application chain is generated
--    "In the expression: arg"

--  See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
--  See Note [Expanding HsDo with XXExprGhcRn] in GHC.Tc.Gen.Do
addArgCtxt arg_no (app_head, app_head_lspan) (L arg_loc arg) thing_inside
  | not (isGeneratedSrcSpan app_head_lspan)
  = do { traceTc "addArgCtxt" (vcat [text "not generated Head"
                                    , ppr app_head
                                    , ppr app_head_lspan
                                    , ppr arg_loc
                                    , ppr arg
                                    , ppr arg_no])
       ; setSrcSpanA arg_loc $
         addErrCtxt (FunAppCtxt (FunAppCtxtExpr app_head arg) arg_no) $
         thing_inside
       }
  | otherwise
  = do { traceTc "addArgCtxt" (vcat [text "generated Head"
                                    , ppr app_head
                                    , ppr app_head_lspan
                                    , ppr arg_loc
                                    , ppr arg])
       ; setSrcSpanA arg_loc $
         addExprCtxt arg $
         thing_inside
       }


{- *********************************************************************
*                                                                      *
              Instantiating fresh type variables

      Functions in here use getTcLevelQL to decide what level
      to put on fresh unification variables.  If do_ql = DoQL, we
      ignore the level in the monad, and use QLInstVar instead,
      thereby giving birth to a Quick Look instantiation varaible
*                                                                      *
********************************************************************* -}

{- Note [Instantiating type variables in QuickLook]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During QuickLook, when we instantiate a function's type (specifically, in
`tcInstFun`), we must instantiate it with so-called "instantiation variables".
See Note [QuickLook instantiation variables] in GHC.Tc.Utils.TcType.

So `tcInstFun` uses a family of specialised functions, defined below, like
   instantiateSigmaQL
   newFlexiTyVarTyQL
   etc
that create fresh instantiation variables rather than regular unification
variables.   But only if QuickLook is on!  So they all take a `QLFlag` to
tell them what to do; that flag is ultimately used in `getTcLevelQL`.

There is some code duplication between these functions and their friends
in GHC.Tc.Utils.TcMType, but that's just too bad.

Note that `tcInstFun` calls `quickLookArg` which calls `tcInstFun` recursively.
Consider (f (g (h x))).`tcApp` instantiates the call to `f`, and in doing so
quick-looks at the argument(s), in this case (g (h x)).  But `quickLookArg` on (g
(h x)) in turn instantiates `g` and quick-looks at /its/ argument(s), in this
case (h x).  And so on recursively.  Key point: all these instantiations make
instantiation variables.
-}

getTcLevelQL :: QLFlag -> TcM TcLevel
-- If Quick Look is on, instantiate all fresh unification variables
-- at level QLInstVar; they are instantiation variables
-- See Note [Instantiating type variables in QuickLook]
getTcLevelQL DoQL = return QLInstVar
getTcLevelQL NoQL = getTcLevel

newFlexiTyVarQL :: QLFlag -> OccName -> MetaInfo -> TcKind -> TcM TcTyVar
newFlexiTyVarQL do_ql occ info kind
  = do { lvl  <- getTcLevelQL do_ql
       ; ref  <- newMutVar Flexi
       ; name <- newSysName occ -- See Note [Name of a unification variable]
                                -- in GHC.Tc.Utils.TcMType
       ; let details = MetaTv { mtv_info  = info
                              , mtv_ref   = ref
                              , mtv_tclvl = lvl }
       ; return (mkTcTyVar name kind details) }

newFlexiTyVarTyQL :: QLFlag -> OccName -> MetaInfo -> TcKind -> TcM TcType
newFlexiTyVarTyQL do_ql occ info kind
  = mkTyVarTy <$> newFlexiTyVarQL do_ql occ info kind

newOpenFlexiTyVarTyQL :: QLFlag -> MetaInfo -> TcM TcType
newOpenFlexiTyVarTyQL do_ql rr_info
  = do { let rr_occ = mkTyVarOccFS (fsLit "cx")
             tv_occ = mkTyVarOccFS (fsLit "q")
        ; rr_ty  <- newFlexiTyVarTyQL do_ql rr_occ rr_info runtimeRepTy
        ; arg_nu <- newFlexiTyVarTyQL do_ql tv_occ TauTv   (mkTYPEapp rr_ty)
        ; return arg_nu }

newArgTyVarTyQL :: QLFlag -> FixedRuntimeRepContext -> TcM TcType
newArgTyVarTyQL do_ql frr_ctxt
  = mdo { let conc_orig = ConcreteFRR $
                          FixedRuntimeRepOrigin
                            { frr_context = frr_ctxt
                            , frr_type    = arg_nu }
        ; rr_info <- mkConcreteInfo conc_orig
        ; arg_nu  <- newOpenFlexiTyVarTyQL do_ql rr_info
        ; return arg_nu }

instantiateSigmaQL :: QLFlag
                   -> CtOrigin
                   -> ConcreteTyVars -- ^ concreteness information
                   -> [(Matchability, TyVar)]
                   -> TcThetaType -> TcSigmaType
                   -> TcM (HsWrapper, TcSigmaType)
-- (instantiateSigmaQL orig tvs theta ty)
--     instantiates the type variables tvs, emits the (instantiated)
--     constraints theta, and returns the (instantiated) type ty
-- See Note [Instantiating type variables in QuickLook]
instantiateSigmaQL do_ql orig concs tvs theta body_ty
  = do { rec (subst, inst_tvs) <- mapAccumLM (new_meta subst) empty_subst (map snd tvs)
       ; let inst_theta  = substTheta subst theta
             inst_body   = substTy subst body_ty

       ; wrap <- instCall orig (mkTyVarTys inst_tvs) inst_theta
       ; traceTc "Instantiating"
                 (vcat [ text "origin" <+> pprCtOrigin orig
                       , text "tvs"   <+> ppr tvs
                       , text "theta" <+> ppr theta
                       , text "type" <+> debugPprType body_ty
                       , text "with" <+> ppr inst_tvs
                       , text "theta:" <+> ppr inst_theta ])

      ; return (wrap, inst_body) }
  where
    in_scope = mkInScopeSet (tyCoVarsOfType (mkSpecSigmaTy tvs theta body_ty))
               -- mkSpecSigmaTy: Inferred vs Specified is not important here;
               --                We just want an accurate free-var set
    empty_subst = mkEmptySubst in_scope

    new_meta :: Subst -> Subst -> TyVar -> TcM (Subst, TcTyVar)
    new_meta final_subst subst tv
      = do { let occ = getOccName tv
                 substd_kind = substTy subst (tyVarKind tv)
           ; info   <- get_info final_subst tv
           ; new_tv <- newFlexiTyVarQL do_ql occ info substd_kind
           ; let new_subst   = extendTvSubstWithClone subst tv new_tv
           ; return (new_subst, new_tv) }

    get_info :: Subst -> TyVar -> TcM MetaInfo
    get_info final_subst tv
      -- Is this a type variable that must be instantiated to a concrete type?
      -- If so, create a ConcreteTv metavariable instead of a plain TauTv.
      -- See Note [Representation-polymorphism checking built-ins]
      --     in GHC.Tc.Utils.Concrete.

      | Just conc_orig0 <- lookupNameEnv concs (tyVarName tv)
      , let conc_orig = substConcreteTvOrigin final_subst body_ty conc_orig0
                        -- See Note [substConcreteTvOrigin].
      = mkConcreteInfo conc_orig

      -- The vastly common case
      | otherwise
      = return TauTv

{- *********************************************************************
*                                                                      *
              Visible type application
*                                                                      *
********************************************************************* -}

-- See Note [Visible type application and abstraction]
tcVTA :: ConcreteTyVars
         -- ^ Type variables that must be instantiated to concrete types.
         --
         -- See Note [Representation-polymorphism checking built-ins]
         -- in GHC.Tc.Utils.Concrete.
      -> TcType            -- ^ Function type
      -> LHsWcType GhcRn   -- ^ Argument type
      -> TcM (TcType, TcType)
-- Deal with a visible type application
-- The function type has already had its Inferred binders instantiated
tcVTA conc_tvs fun_ty hs_ty
  | Just (ma, tvb, inner_ty) <- tcSplitForAllTyVarBinder_maybe fun_ty
  , isSpecifiedForAllTyFlag (binderFlag tvb)
  = do { tc_inst_forall_arg conc_tvs (ma, tvb, inner_ty) hs_ty }

  | otherwise
  = do { (_, fun_ty) <- liftZonkM $ zonkTidyTcType emptyTidyEnv fun_ty
       ; failWith $ TcRnInvalidTypeApplication fun_ty hs_ty }

-- See Note [Visible type application and abstraction]
tcVDQ :: ConcreteTyVars              -- See Note [Representation-polymorphism checking built-ins]
      -> (Matchability, ForAllTyBinder, TcType)    -- Function type
      -> LHsExpr GhcRn               -- Argument type
      -> TcM (TcType, TcType)
tcVDQ conc_tvs (ma, tvb, inner_ty) arg
  = do { hs_wc_ty <- expr_to_type arg
       ; tc_inst_forall_arg conc_tvs (ma, tvb, inner_ty) hs_wc_ty }

-- Convert a HsExpr into the equivalent HsType.
-- See [RequiredTypeArguments and the T2T mapping]
expr_to_type :: LHsExpr GhcRn -> TcM (LHsWcType GhcRn)
expr_to_type earg =
  case stripParensLHsExpr earg of
    L _ (HsEmbTy _ hs_ty) ->
      -- The entire type argument is guarded with the `type` herald,
      -- e.g. `vfun (type (Maybe Int))`. This special case supports
      -- named wildcards. See Note [Wildcards in the T2T translation]
      return hs_ty
    e ->
      -- The type argument is not guarded with the `type` herald, or perhaps
      -- only parts of it are, e.g. `vfun (Maybe Int)` or `vfun (Maybe (type Int))`.
      -- Apply a recursive T2T transformation.
      HsWC [] <$> go e
        <* failIfErrsM   -- Suppress unhelpful errors that arise after a failed T2T
  where
    go :: LHsExpr GhcRn -> TcM (LHsType GhcRn)
    go (L _ (HsEmbTy _ t)) =
      -- HsEmbTy means there is an explicit `type` herald, e.g. vfun :: forall a -> blah
      -- and the call   vfun (type Int)
      --           or   vfun (Int -> type Int)
      -- The T2T transformation can simply discard the herald and use the embedded type.
      unwrap_wc t
    go (L l (HsFunArr _ mult arg res)) =
      do { arg' <- go arg
         ; mult' <- go_arrow
         ; res' <- go res
         ; return (L l (HsFunTy noExtField mult' arg' res'))}
         where
          go_arrow = do
            let HsModifiedFunArr _ mods arr = mult
            mods' <- mapM go_modifier mods
            pure $ HsModifiedFunArr noExtField mods' arr
          go_modifier (L l (HsModifier x ty)) = L l <$> HsModifier x <$> go ty
    go (L l (HsForAll _ tele expr)) =
      do { ty <- go expr
         ; return (L l (HsForAllTy noExtField tele ty))}
    go (L l (HsQual _ (L lc (HsContext an ctxt)) expr)) =
      do { ctxt' <- mapM go ctxt
         ; ty <- go expr
         ; return (L l (HsQualTy noExtField (L lc (HsContext an ctxt')) ty)) }
    go (L l (HsVar _ lname)) =
      -- GHC Proposal #281, section 7.5 "T2T-Mapping":
      --   variables and constructors (regardless of their namespace)
      --   are mapped directly, without modification.
      do { detect_puns lname
         ; return (L l (HsTyVar noAnn NotPromoted lname)) }
    go (L l (HsApp _ lhs rhs)) =
      do { lhs' <- go lhs
         ; rhs' <- go rhs
         ; return (L l (HsAppTy noExtField lhs' rhs')) }
    go (L l (HsAppType _ lhs rhs)) =
      do { lhs' <- go lhs
         ; rhs' <- unwrap_wc rhs
         ; return (L l (HsAppKindTy noExtField lhs' rhs')) }
    go (L l (OpApp _ lhs op rhs)) =
      do { lhs' <- go lhs
         ; op'  <- go op
         ; rhs' <- go rhs
         ; return (L l (HsOpTy noExtField lhs' op' rhs')) }
    go (L l (HsOverLit _ ol))
      = do { let lit = tyLitFromOverloadedLit (ol_val ol)
           ; return (L l (HsTyLit noExtField lit)) }
    go (L l (HsLit _ lit))
      = return (L l (HsTyLit noExtField lit))
    go (L l (ExplicitTuple _ tup_args boxity))
      -- Neither unboxed tuples (#e1,e2#) nor tuple sections (e1,,e2,) can be promoted
      | isBoxed boxity
      , Just es <- tupArgsPresent_maybe tup_args
      = do { ts <- traverse go es
           ; return (L l (HsExplicitTupleTy noExtField NotPromoted ts)) }
    go (L l (ExplicitList _ es)) =
      do { ts <- traverse go es
         ; return (L l (HsExplicitListTy noExtField NotPromoted ts)) }
    go (L l (ExprWithTySig _ e sig_ty)) =
      do { t <- go e
         ; sig_ki <- (unwrap_sig <=< unwrap_wc) sig_ty
         ; return (L l (HsKindSig noAnn t sig_ki)) }
      where
        unwrap_sig :: LHsSigType GhcRn -> TcM (LHsType GhcRn)
        unwrap_sig (L _ (HsSig _ HsOuterImplicit{hso_ximplicit=bndrs} body))
          | null bndrs = return body
          | otherwise  = illegal_implicit_tvs bndrs
        unwrap_sig (L l (HsSig _ HsOuterExplicit{hso_matchability=ma,hso_bndrs=bndrs} body)) =
          return $ L l (HsForAllTy noExtField (HsForAllInvis noAnn ma bndrs) body)
    go (L l (HsPar _ e)) =
      do { t <- go e
         ; return (L l (HsParTy noAnn t)) }
    go (L l (HsUntypedSplice splice_result splice))
      | HsUntypedSpliceTop finalizers e <- splice_result
      = do { t <- go (L l e)
           ; let splice_result' = HsUntypedSpliceTop finalizers t
           ; return (L l (HsSpliceTy splice_result' splice)) }
    go (L l (HsStar x))
      = return (L l (HsStarTy x))
    go (L l (HsHole h@(HoleVar (L _ rdr))))
      | isUnderscore occ = return (L l (HsWildCardTy h))
      | startsWithUnderscore occ =
          -- See Note [Wildcards in the T2T translation]
          do { wildcards_enabled <- xoptM LangExt.NamedWildCards
             ; if wildcards_enabled
               then illegal_wc rdr
               else not_in_scope }
      | otherwise = not_in_scope
      where occ = occName rdr
            not_in_scope = failWith $ TcRnNotInScope NotInScope rdr
    go (L l (XExpr (ExpandedThingRn (HSE (ExprCtxt orig) _)))) =
      -- Use the original, user-written expression (before expansion).
      -- Example. Say we have   vfun :: forall a -> blah
      --          and the call  vfun (Maybe [1,2,3])
      --          expanded to   vfun (Maybe (fromListN 3 [1,2,3]))
      -- (This happens when OverloadedLists is enabled).
      -- The expanded expression can't be promoted, as there is no type-level
      -- equivalent of fromListN, so we must use the original.
      go (L l orig)
    go e = failWith $ TcRnIllformedTypeArgument e

    detect_puns :: LocatedN (WithUserRdr Name) -> TcM ()
      -- GHC Proposal #281, section 7.5 "T2T-Mapping":
      --   there should be no variable of the same name but from
      --   a different namespace, or else raise an ambiguity error
      --   (does not apply to constructors)
    detect_puns (L l (WithUserRdr rdr _))
      | isTermVarOrFieldNameSpace (rdrNameSpace rdr)
      , Just promoted_rdr <- promoteRdrName rdr
      = do { envs <- getRdrEnvs
           ; whenIsJust (lookup_rdr envs rdr) $ \unpromoted ->
             whenIsJust (lookup_rdr envs promoted_rdr) $ \promoted ->
               addErrAt (locA l) $
               TcRnIllegalPunnedVarOccInTypeArgument { illegalPunTermName = unpromoted
                                                     , illegalPunTypeName = promoted } }
      | otherwise = return ()

    lookup_rdr :: (GlobalRdrEnv, LocalRdrEnv) -> RdrName -> Maybe ResolvedNameInfo
    lookup_rdr (gbl_env, lcl_env) rdr
      | Just name <- lookupLocalRdrEnv lcl_env rdr
      = Just (ResolvedNameInfo [] rdr name)
      | gres@(gre:_) <- lookupGRE gbl_env (LookupRdrName rdr (RelevantGREsFOS WantNormal))
      = Just (ResolvedNameInfo gres rdr (gre_name gre))
      | otherwise = Nothing

    unwrap_wc :: HsWildCardBndrs GhcRn t -> TcM t
    unwrap_wc (HsWC wcs t)
      = do { mapM_ (illegal_wc . nameRdrName) wcs
           ; return t }

    illegal_wc :: RdrName -> TcM t
    illegal_wc rdr = failWith $ TcRnIllegalNamedWildcardInTypeArgument rdr

    illegal_implicit_tvs :: [Name] -> TcM t
    illegal_implicit_tvs tvs
      = do { mapM_ (addErr . TcRnIllegalImplicitTyVarInTypeArgument . nameRdrName) tvs
           ; failM }

{- Note [RequiredTypeArguments and the T2T mapping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The "T2T-Mapping" section of GHC Proposal #281 introduces a term-to-type transformation
that comes into play when we typecheck function applications to required type arguments.
Say we have a function that expects a required type argument, vfun :: forall a -> ...
then it is possible to call it as follows:

  vfun (Maybe Int)

The Maybe Int argument is parsed and renamed as a term. There is no syntactic marker
to tell GHC that it is actually a type argument.  We only discover this by the time
we get to type checking, where we know that f's type has a visible forall at the front,
so we are expecting a type argument. More precisely, this happens in tcVDQ in GHC/Tc/Gen/App.hs:

  tcVDQ :: ConcreteTyVars              -- See Note [Representation-polymorphism checking built-ins]
        -> (Matchability, ForAllTyBinder, TcType)    -- Function type
        -> LHsExpr GhcRn               -- Argument type
        -> TcM (TcType, TcType)

What we want is a type to instantiate the forall-bound variable. But what we have is an HsExpr,
and we need to convert it to an HsType in order to reuse the same code paths as we use for
checking f @ty (see tc_inst_forall_arg).

  f (Maybe Int)
  -- ^^^^^^^^^
  -- parsed and renamed as:   HsApp   (HsVar   "Maybe") (HsVar   "Int")  ::  HsExpr GhcRn
  -- must be converted to:    HsTyApp (HsTyVar "Maybe") (HsTyVar "Int")  ::  HsType GhcRn

We do this using a helper function:

  expr_to_type :: LHsExpr GhcRn -> TcM (LHsWcType GhcRn)

This conversion is in the TcM monad because
* It can fail, if the expression is not convertible to a type.
      vfun [x | x <- xs]     Can't convert list comprehension to a type
      vfun (\x -> x)         Can't convert a lambda to a type
* It needs to check for LangExt.NamedWildCards to generate an appropriate
  error message for HsHole (HsVar ...).
     vfun _a    Not in scope: ‘_a’
                   (NamedWildCards disabled)
     vfun _a    Illegal named wildcard in a required type argument: ‘_a’
                   (NamedWildCards enabled)

Note [Wildcards in the T2T translation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose f1 :: forall a b. blah
        f2 :: forall a b -> blah

Consider the terms
  f1 @_ @(Either _ _)
  f2 (type _) (type (Either _ _))
Those `_` wildcards are type wildcards, each standing for a monotype.
All good.

Now consider this, with -XNamedWildCards:
  f1 @_a @(Either _a _a)
  f2 (type _a) (type (Either _a _a))
Those `_a` are "named wildcards", specified by the user manual like this: "All
occurrences of the same named wildcard within one type signature will unify to
the same type".  Note "within one signature".  So each type argument is considered
separately, and the examples mean the same as:
  f1 @_a1 @(Either _a2 _a2)
  f2 (type _a1) (type (Either _a2 _a2))
The repeated `_a2` ensures that the two arguments of `Either` are the same type;
but there is no connection with `_a1`.  (NB: `_a1` and `_a2` only scope within
their respective type, no further.)

Now, consider the T2T translation for
   f2 _ (Either _ _)
This is fine: the term wildcard `_` is translated to a type wildcard, so we get
the same as if we had written
   f2 (type _) (type (Either _ _))

But what about /named/ wildcards?
   f2 _a (Either _a _a)
Now we are in difficulties.  The renamer looks for a /term/ variable `_a` in scope,
and won't find one.  Even if it did, the three `_a`'s would not be renamed separately
as above.

Conclusion: we treat a named wildcard in the T2T translation as an error.  If you
want that, use a `(type ty)` argument instead.
-}

tc_inst_forall_arg :: ConcreteTyVars            -- See Note [Representation-polymorphism checking built-ins]
                   -> (Matchability, ForAllTyBinder, TcType)  -- Function type
                   -> LHsWcType GhcRn           -- Argument type
                   -> TcM (TcType, TcType)
tc_inst_forall_arg conc_tvs (ma, tvb, inner_ty) hs_ty
  = do { let tv   = binderVar tvb
             kind = tyVarKind tv
             tv_nm   = tyVarName tv
             mb_conc = lookupNameEnv conc_tvs tv_nm
       ; ty_arg0 <- tcHsTypeApp hs_ty kind

       -- Is this type variable required to be instantiated to a concrete type?
       -- If so, ensure that that is the case.
       --
       -- See [Wrinkle: VTA] in Note [Representation-polymorphism checking built-ins]
       -- in GHC.Tc.Utils.Concrete.
       ; th_lvl <- getThLevel
       ; ty_arg <- case mb_conc of
           Nothing   -> return ty_arg0
           Just conc
             -- See [Wrinkle: Typed Template Haskell]
             -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
             | TypedBrack {} <- th_lvl
             -> return ty_arg0
             | otherwise
             ->
             -- Example: user wrote e.g. (#,#) @(F Bool) for a type family F.
             -- Emit [W] F Bool ~ kappa[conc] and pretend the user wrote (#,#) @kappa.
               coercionRKind <$> unifyConcrete (occNameFS $ getOccName $ tv_nm) conc ty_arg0

       ; let fun_ty    = mkForAllTy ma tvb inner_ty
             insted_ty = piResultTy fun_ty ty_arg
               -- This substitution is well-kinded even when inner_ty
               -- is not fully zonked, because ty_arg is fully zonked.
               -- See Note [Type application substitution].

       ; lvl <- getTcLevel
       ; traceTc "tc_inst_forall_arg (VTA/VDQ)" (
                  vcat [ text "fun_ty" <+> ppr fun_ty
                       , text "tv" <+> ppr tv <+> dcolon <+> debugPprType kind
                       , text "ty_arg" <+> debugPprType ty_arg <+> dcolon
                                       <+> debugPprType (typeKind ty_arg)
                       , text "inner_ty" <+> debugPprType inner_ty
                       , text "insted_ty" <+> debugPprType insted_ty
                       , text "lvl:" <+> ppr lvl ])
       ; return (ty_arg, insted_ty) }

{- Note [Visible type application and abstraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC supports the types
    forall {a}.  a -> t     -- ForAllTyFlag is Inferred
    forall  a.   a -> t     -- ForAllTyFlag is Specified
    forall  a -> a -> t     -- ForAllTyFlag is Required

The design of type abstraction and type application for those types has gradually
evolved over time, and is based on the following papers and proposals:
  - "Visible Type Application"
    https://richarde.dev/papers/2016/type-app/visible-type-app.pdf
  - "Type Variables in Patterns"
    https://richarde.dev/papers/2018/pat-tyvars/pat-tyvars.pdf
  - "Modern Scoped Type Variables"
    https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0448-type-variable-scoping.rst
  - "Visible forall in types of terms"
    https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst

Here we offer an overview of the design mixed with commentary on the
implementation status. The proposals have not been fully implemented at the
time of writing this Note (see "not implemented" in the rest of this Note).

Now consider functions
    fi :: forall {a}. a -> t     -- Inferred:  type argument cannot be supplied
    fs :: forall a. a -> t       -- Specified: type argument may    be supplied
    fr :: forall a -> a -> t     -- Required:  type argument must   be supplied

At a call site we may have calls looking like this
    fi             True  -- Inferred: no visible type argument
    fs             True  -- Specified: type argument omitted
    fs      @Bool  True  -- Specified: type argument supplied
    fr (type Bool) True  -- Required: type argument is compulsory, `type` qualifier used
    fr       Bool  True  -- Required: type argument is compulsory, `type` qualifier omitted

At definition sites we may have type /patterns/ to abstract over type variables
   fi           x       = rhs   -- Inferred: no type pattern
   fs           x       = rhs   -- Specified: type pattern omitted
   fs @a       (x :: a) = rhs   -- Specified: type pattern supplied
   fr (type a) (x :: a) = rhs   -- Required: type pattern is compulsory, `type` qualifier used
   fr a        (x :: a) = rhs   -- Required: type pattern is compulsory, `type` qualifier omitted

Type patterns in lambdas mostly work the same way as they do in a function LHS,
except for @-binders
   OK:  fs = \           x       -> rhs   -- Specified: type pattern omitted
   Bad: fs = \ @a       (x :: a) -> rhs   -- Specified: type pattern supplied
   OK:  fr = \ (type a) (x :: a) -> rhs   -- Required: type pattern is compulsory, `type` qualifier used
   OK:  fr = \ a        (x :: a) -> rhs   -- Required: type pattern is compulsory, `type` qualifier omitted

When it comes to @-binders in lambdas, they do work, but only in a limited set
of circumstances:
  * the lambda occurs as an argument to a higher-rank function or constructor
      higher-rank function:  h :: (forall a. blah) -> ...
      call site:             x = h (\ @a -> ... )
  * the lambda is annotated with an inline type signature:
      (\ @a -> ... ) :: forall a. blah
  * the lambda is a field in a data structure, whose type is impredicative
      [ \ @a -> ... ] :: [forall a. blah]
  * the @-binder is not the first binder in the lambda:
      \ x @a -> ...

Type patterns may also occur in a constructor pattern. Consider the following data declaration
   data T where
     MkTI :: forall {a}. Show a => a -> T   -- Inferred
     MkTS :: forall a.   Show a => a -> T   -- Specified
     MkTR :: forall a -> Show a => a -> T   -- Required  (NB: not implemented)

Matching on its constructors may look like this
   f (MkTI           x)       = rhs  -- Inferred: no type pattern
   f (MkTS           x)       = rhs  -- Specified: type pattern omitted
   f (MkTS @a       (x :: a)) = rhs  -- Specified: type pattern supplied
   f (MkTR (type a) (x :: a)) = rhs  -- Required: type pattern is compulsory, `type` qualifier used    (NB: not implemented)
   f (MkTR a        (x :: a)) = rhs  -- Required: type pattern is compulsory, `type` qualifier omitted (NB: not implemented)

The moving parts are as follows:
  (abbreviations used: "c.o." = "constructor of")

Syntax of types
---------------
* The types are all initially represented with HsForAllTy (c.o. HsType).
  The binders are in the (hst_tele :: HsForAllTelescope pass) field of the HsForAllTy
  At this stage, we have
      forall {a}. t    -- HsForAllInvis (c.o. HsForAllTelescope) and InferredSpec  (c.o. Specificity)
      forall a. t      -- HsForAllInvis (c.o. HsForAllTelescope) and SpecifiedSpec (c.o. Specificity)
      forall a -> t    -- HsForAllVis (c.o. HsForAllTelescope)

* By the time we get to checking applications/abstractions (e.g. GHC.Tc.Gen.App)
  the types have been kind-checked (e.g. by tcLHsType) into ForAllTy (c.o. Type).
  At this stage, we have:
      forall {a}. t    -- ForAllTy (c.o. Type) and Inferred  (c.o. ForAllTyFlag)
      forall a. t      -- ForAllTy (c.o. Type) and Specified (c.o. ForAllTyFlag)
      forall a -> t    -- ForAllTy (c.o. Type) and Required  (c.o. ForAllTyFlag)

Syntax of applications in HsExpr
--------------------------------
* We represent type applications in HsExpr like this (ignoring parameterisation)
    data HsExpr = HsApp HsExpr HsExpr      -- (f True)    (plain function application)
                | HsAppType HsExpr HsType  -- (f @True)   (function application with `@`)
                | HsEmbTy HsType           -- (type Int)  (embed a type into an expression with `type`)
                | ...

* So (f @ty) is represented, just as you might expect:
    HsAppType f ty

* But (f (type ty)) is represented by:
    HsApp f (HsEmbTy ty)

  Why the difference?  Because we /also/ need to express these /nested/ uses of `type`:

      g (Maybe (type Int))               -- valid for g :: forall (a :: Type) -> t
      g (Either (type Int) (type Bool))  -- valid for g :: forall (a :: Type) -> t

  This nesting makes `type` rather different from `@`. Remember, the HsEmbTy mainly just
  switches namespace, and is subject to the term-to-type transformation.

Syntax of abstractions in Pat
-----------------------------
* Type patterns are represented in Pat roughly like this
     data Pat = ConPat   ConLike [Pat]  -- (Con @tp1 @tp2 p1 p2)  (constructor pattern)
              | EmbTyPat HsTyPat        -- (type tp)              (embed a type into a pattern with `type`)
              | InvisPat HsTyPat        -- (@tp)                  (invisible type pattern)
              | ...
     data HsTyPat = HsTP LHsType
  (In ConPat, the type and term arguments are actually inside HsConPatDetails.)

  * Similar to HsAppType in HsExpr, InvisPat in ConPat is used for @ty arguments
  * Similar to HsEmbTy   in HsExpr, EmbTyPat lets you embed a type in a pattern

* Examples:
      \ (MkT @a  (x :: a)) -> rhs    -- ConPat (c.o. Pat) and InvisPat (c.o. Pat)
      \ (type a) (x :: a)  -> rhs    -- EmbTyPat (c.o. Pat)
      \ a        (x :: a)  -> rhs    -- VarPat (c.o. Pat)
      \ @a       (x :: a)  -> rhs    -- InvisPat (c.o. Pat)

* A HsTyPat is not necessarily a plain variable. At the very least,
  we support kind signatures and wildcards:
      \ (type _)           -> rhs
      \ (type (b :: Bool)) -> rhs
      \ (type (_ :: Bool)) -> rhs
  But in constructor patterns we also support full-on types
      \ (P @(a -> Either b c)) -> rhs
  All these forms are represented with HsTP (c.o. HsTyPat).

Renaming type applications
--------------------------
rnExpr delegates renaming of type arguments to rnHsWcType if possible:
    f @t        -- HsAppType,         t is renamed with rnHsWcType
    f (type t)  -- HsApp and HsEmbTy, t is renamed with rnHsWcType

But what about:
    f t         -- HsApp, no HsEmbTy
We simply rename `t` as a term using a recursive call to rnExpr; in particular,
the type of `f` does not affect name resolution (Lexical Scoping Principle).
We will later convert `t` from a `HsExpr` to a `Type`, see "Typechecking type
applications" later in this Note. The details are spelled out in the "Resolved
Syntax Tree" and "T2T-Mapping" sections of GHC Proposal #281.

Renaming type abstractions
--------------------------
rnPat delegates renaming of type arguments to rnHsTyPat if possible:
  f (P @t)   = rhs  -- ConPat,   t is renamed with rnHsTyPat
  f (type t) = rhs  -- EmbTyPat, t is renamed with rnHsTyPat

But what about:
  f t = rhs   -- VarPat
The solution is as before (see previous section), mutatis mutandis.
Rename `t` as a pattern using a recursive call to `rnPat`, convert it
to a type pattern later.

One particularly prickly issue is that of implicit quantification. Consider:

  f :: forall a -> ...
  f t = ...   -- binding site of `t`
    where
      g :: t -> t   -- use site of `t` or a fresh variable?
      g = ...

Does the signature of `g` refer to `t` bound in `f`, or is it a fresh,
implicitly quantified variable? This is normally controlled by
ScopedTypeVariables, but in this example the renamer can't tell `t` from a term
variable.  Only later (in the type checker) will we find out that it stands for
the forall-bound type variable `a`.  So when RequiredTypeArguments is in effect,
we change implicit quantification to take term variables into account; that is,
we do not implicitly quantify the signature of `g` to `g :: forall t. t->t`
because of the term-level `t` that is in scope.
See Note [Term variable capture and implicit quantification].

Typechecking type applications
------------------------------
Type applications are checked alongside ordinary function applications
in tcInstFun.

First of all, we assume that the function type is known (i.e. not a metavariable)
and contains a `forall`. Consider:
  f :: forall a. a -> a
  f x = const x (f @Int 5)
If the type signature is removed, the definition results in an error:
  Cannot apply expression of type ‘t1’
  to a visible type argument ‘Int’

The same principle applies to required type arguments:
  f :: forall a -> a -> a
  f (type a) x = const x (f (type Int) 5)
If the type signature is removed, the error is:
  Illegal type pattern.
  A type pattern must be checked against a visible forall.

When the type of the function is known and contains a `forall`, all we need to
do is instantiate the forall-bound variable with the supplied type argument.
This is done by tcVTA (if Specified) and tcVDQ (if Required).

tcVDQ unwraps the HsEmbTy and uses the type contained within it.  Crucially, in
tcVDQ we know that we are expecting a type argument.  This means that we can
support
    f (Maybe Int)   -- HsApp, no HsEmbTy
The type argument (Maybe Int) is represented as an HsExpr, but tcVDQ can easily
convert it to HsType.  This conversion is called the "T2T-Mapping" in GHC
Proposal #281.

Typechecking type abstractions
------------------------------
Type abstractions are checked alongside ordinary patterns in GHC.Tc.Gen.Pat.tcMatchPats.
One of its inputs is a list of ExpPatType that has two constructors
  * ExpFunPatTy    ...   -- the type A of a function A -> B
  * ExpForAllPatTy ...   -- the binder (a::A) of forall (a::A) -> B
so when we are checking
  f :: forall a b -> a -> b -> ...
  f (type a) (type b) (x :: a) (y :: b) = ...
our expected pattern types are
  [ ExpForAllPatTy ...      -- forall a ->
  , ExpForAllPatTy ...      -- forall b ->
  , ExpFunPatTy    ...      -- a ->
  , ExpFunPatTy    ...      -- b ->
  ]

The [ExpPatType] is initially constructed by GHC.Tc.Utils.Unify.matchExpectedFunTys,
by decomposing the type signature for `f` in our example.  If we are given a
definition
   g (type a) = ...
we never /infer/ a type g :: forall a -> blah.  We can only /check/
explicit type abstractions in terms.

The [ExpPatType] allows us to use different code paths for type abstractions
and ordinary patterns:
  * tc_pat :: Scaled ExpSigmaTypeFRR -> Checker (Pat GhcRn) (Pat GhcTc)
  * tc_forall_pat :: Checker (Pat GhcRn, TcTyVar) (Pat GhcTc)

tc_forall_pat unwraps the EmbTyPat and uses the type pattern contained
within it. This is another spot where the "T2T-Mapping" can take place,
allowing us to support
  f a (x :: a) = rhs    -- no EmbTyPat

Type patterns in constructor patterns are handled in with tcConTyArg.
Both tc_forall_pat and tcConTyArg delegate most of the work to tcHsTyPat.

Note [VTA for out-of-scope functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose 'wurble' is not in scope, and we have
   (wurble @Int @Bool True 'x')

Then the renamer will make (HsHole (HsVar "wurble")) for 'wurble',
and the typechecker will typecheck it with tcUnboundId, giving it
a type 'alpha', and emitting a deferred Hole constraint, to
be reported later.

But then comes the visible type application. If we do nothing, we'll
generate an immediate failure (in tc_app_err), saying that a function
of type 'alpha' can't be applied to Bool.  That's insane!  And indeed
users complain bitterly (#13834, #17150.)

The right error is the Hole, which has /already/ been emitted by
tcUnboundId.  It later reports 'wurble' as out of scope, and tries to
give its type.

Fortunately in tcInstFun we still have access to the function, so we
can check if it is a HsHole.  We use this info to simply skip
over any visible type arguments.  We'll /already/ have emitted a
Hole constraint; failing preserves that constraint.

We do /not/ want to fail altogether in this case (via failM) because
that may abandon an entire instance decl, which (in the presence of
-fdefer-type-errors) leads to leading to #17792.

What about required type arguments?  Suppose we see
    f (type Int)
where `f` is out of scope.  Then again we don't want to crash because f's
type (which will be just a fresh unification variable) isn't a visible forall.
Instead we just skip the `(type Int)` argument, as before.

Downside: the typechecked term has lost its visible type arguments; we
don't even kind-check them.  But let's jump that bridge if we come to
it.  Meanwhile, let's not crash!

Note [Type application substitution]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `tc_inst_forall_arg`, suppose we are checking a visible type
application `f @hs_ty`, where `f :: forall (a :: k). body`.  We will:
  * Compute `ty <- tcHsTypeApp hs_ty k`
  * Then substitute `a :-> ty` in `body`.
Now, you might worry that `a` might not have the same kind as `ty`, so that the
substitution isn't kind-preserving.  How can that happen?  The kinds will
definitely be the same after zonking, and `ty` will be zonked (as this is
a postcondition of `tcHsTypeApp`). But the function type `forall a. body`
might not be fully zonked (hence the worry).

But it's OK!  During type checking, we don't require types to be well-kinded (without
zonking); we only require them to satsisfy the Purely Kinded Type Invariant (PKTI).
See Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType.

In the case of a type application:
  * `forall a. body` satisfies the PKTI
  * `ty` is zonked
  * If we substitute a fully-zonked thing into an un-zonked Type that
    satisfies the PKTI, the result still satisfies the PKTI.

This last statement isn't obvious, but read
Note [The Purely Kinded Type Invariant (PKTI)] in GHC.Tc.Gen.HsType.
The tricky case is when `body` contains an application of the form `a b1 ... bn`,
and we substitute `a :-> ty` where `ty` has fewer arrows in its kind than `a` does.
That can't happen: the call `tcHsTypeApp hs_ty k` would have rejected the
type application as ill-kinded.

Historical remark: we used to require a stronger invariant than the PKTI,
namely that all types are well-kinded prior to zonking. In that context, we did
need to zonk `body` before performing the substitution above. See test case
#14158, as well as the discussion in #23661.
-}

{- *********************************************************************
*                                                                      *
              Quick Look
*                                                                      *
********************************************************************* -}

{- Note [Quick Look at value arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The function quickLookArg implements the "QL argument" judgement of
the QL paper, in Fig 5 of "A quick look at impredicativity" (ICFP 2020),
rather directly.  The key rule, implemented by `quickLookArg` is

   G |-h h:sg                         -- Find the type of the head
   G |-inst sg;pis ~> phis;rho_r      -- tcInstFun on the args
   (A) rho = T sgs  OR  (B) fiv(phis) = emptyset  -- can_do_ql
   -------------------------------------- APP-QL
   G |-ql h pis : rho ~> qlUnify( rho, rho_r )

(The paper uses a lightning-bolt where we use "ql".)  The most straightforward
way to implement this rule for a call (f e1 ... en) would be:

   1. Take a quick look at the argumets e1..en to guide instantiation
      of the function f.
   2. Then typecheck e1..en from scratch.

That's wasteful, because in Step 1, the quick look at each argument, say (g
h1..hm), involves instantiating `h` and taking a quick look at /its/
arguments.  Then in Step 2 we typecheck (g h1..hm) and again take a quick look
at its arguments.  This is quadratic in the nesting depth of the arguments.

Instead, after the quick look, we /save/ the work we have done in an EValArgQL
record, and /resume/ it later.  The way to think of it is this:

  * `tcApp` typechecks an application.  It uses `tcInstFun`, which in turn
    calls `quickLookArg` on each value argument.

  * `quickLookArg` (which takes a quick look at the argument)

      - Does the "initial" part of `tcApp`, especially `tcInstFun`

      - Captures the result in an EValArgQL record

      - Later, `tcValArg` starts from the EValArgQL record, and
        completes the job of typechecking the application

This turned out to be more subtle than I expected.  Wrinkles:

(QLA1) `quickLookArg` decides whether or not premises (A) and (B) of the
  quick-look-arg judgement APP-QL are satisfied; this is captured in
  `arg_influences_enclosing_call`.

(QLA2) We avoid zonking, so the `arg_influences_enclosing_call` sees the
  argument type /before/ the QL substitution Theta is applied to it. So we
  achieve argument-order independence for free (see 5.7 in the paper).  See the
  `isGuardedTy orig_arg_rho` test in `quickLookArg`.

(QLA3) Deciding whether the premises are satisfied involves calling `tcInstFun`
  (which takes quite some work becuase it calls quickLookArg on nested calls).
  That's why we want to capture the work done, in EValArgQL.

  Do we really have to call `tcInstFun` before deciding (B) of
  `arg_influences_enclosing_call`? Yes (#24686).
  Suppose ids :: [forall a. a->a], and consider
     (:) (reverse ids) blah
  `tcApp` on the outer call will instantiate (:) with `kappa`, and take a
  quick look at (reverse ids). Only after instantiating `reverse` with kappa2,
  quick-looking at `ids` can we discover that (kappa2:=forall a. a->a), which
  satisfies premise (B) of `arg_influence_enclosing_call`.

(QLA4) When we resume typechecking an argument, in `tcValArg` on `EValArgQL`

  - Calling `tcInstFun` on the argument may have emitted some constraints, which
    we carefully captured in `quickLookArg` and stored in the EValArgQL.  We must
    now emit them with `emitConstraints`.  This must be done /under/ the skolemisation
    of the argument's type (see `tcSkolemise` in `tcValArg` for EValArgQL { ...}.
    Example:   f :: (forall b. Ord b => b -> b -> Bool) -> ...
       Call:   f (==)
    we must skolemise the argument type (forall b. Ord b => b -> b -> Bool)
    before emitting the [W] Eq alpha constraint arising from the call to (==).
    It will be solved from the Ord b!

  - quickLookArg may or may not have done `qlUnify` with the calling context.
    If not (eaql_encl = False) must do so now.  Example:  choose [] ids,
            where ids :: [forall a. a->a]
                  choose :: a -> a -> a
    We instantiate choose with `kappa` and discover from `ids` that
    (kappa = [forall a. a->a]).  Now we resume typechecking argument [], and
    we must take advantage of what we have now discovered about `kappa`,
    to typecheck   [] :: [forall a. a->a]

(QLA5) In the quicklook pass, we don't scale multiplicities. Since arguments
    aren't typechecked yet, we don't know their free variable usages
    anyway. But, in a nested call, the head of an application chain is fully
    typechecked.

    In order for the multiplicities in the head to be properly scaled, we store
    the head's usage environment in the eaql_fun_ue field. Then, when we do the
    full-typechecking pass, we can emit the head's usage environment where we
    would have typechecked the head in a naive algorithm.

(QLA6) `quickLookArg` is supposed to capture the result of partially typechecking
   the argument, so it can be resumed later.  "Capturing" should include all
   generated type-class/equality constraints and Linear-Haskell usage info. There
   are two calls in `quickLookArg1` that might generate such constraints:

     - `tcInferAppHead_maybe`.  This can generat Linear-Haskell usage info, via
       the call to `tcEmitBindingUsage` in `check_local_id`, which is called
       indirectly by `tcInferAppHead_maybe`.

       In contrast, `tcInferAppHead_maybe` does not generate any type-class or
       equality constraints, because it doesn't instantiate any functions.  [But
       see #25493 and #25494 for why this isn't quite true today.]

    - `tcInstFun` generates lots of type-class and equality constraints, as it
      instantiates the function.  But it generates no usage info, because that
      comes only from the call to `check_local_id`, whose usage info is captured
      in the call to `tcInferAppHead_maybe` in `quickLookArg1`.

  Conclusion: in quickLookArg1:
    - capture usage information (but not constraints)
        for the call to `tcInferAppHead_maybe`
    - capture constraints (but not usage information)
        for the call to `tcInstFun`

-}

quickLookArg :: QLFlag -> Int
             -> HsExprLoc -- ^ location span of the whole application
             -> (HsExpr GhcRn, SrcSpan) -- ^ Head of the application chain and its source span
             -> LHsExpr GhcRn          -- ^ Argument
             -> Scaled TcSigmaTypeFRR  -- ^ Type expected by the function
             -> TcM (HsExprArg 'TcpInst)
-- See Note [Quick Look at value arguments]
quickLookArg NoQL _ app_lspan _ larg orig_arg_ty
  = skipQuickLook app_lspan larg orig_arg_ty
quickLookArg DoQL pos app_lspan fun_and_lspan larg orig_arg_ty
  = do { setSrcSpanA (getLoc larg) $ rejectNativeQuickLook (scaledThing orig_arg_ty)
       ; is_rho <- qlArgHasRhoType (scaledThing orig_arg_ty)
       ; traceTc "qla" (ppr orig_arg_ty $$ ppr is_rho)
       ; if not is_rho
         then skipQuickLook app_lspan larg orig_arg_ty
         else quickLookArg1 pos app_lspan fun_and_lspan larg orig_arg_ty }

qlArgHasRhoType :: TcType -> TcM Bool
-- `qlArgHasRhoType` checks that the expected argument type in rule
-- App-lightning-bolt (Fig 5 in the paper) is indeed a rho-type.
--
-- It must apply the current QL substitution, so it any QLInstTyVar that it
-- comes across.   Why?  See Section 5.7 in the paper; argument order matters.
--
-- What if we find an /un-filled/ QLInstVar?  We treat this as a rho-type
-- even though a later argument might force it to be sigma-type.  See
-- Section 9 in the paper.
--
-- With -XDeepSubsunption we need a /deep/ rho-type.
-- (We don't need getDeepSubsumptionFlag_DataConHead here because this
-- is only about QuickLook.)

qlArgHasRhoType ty
  = do { ds_flag <- getDeepSubsumptionFlag
       ; go ds_flag ty }
  where
    go ds_flag ty
      | isSigmaTy ty
      = return False

      | Just kappa <- getTyVar_maybe ty
      , isQLInstTyVar kappa
      = do { info <- readMetaTyVar kappa
           ; case info of
               Indirect arg_ty' -> go ds_flag arg_ty'
               Flexi            -> return True }

      | Deep {} <- ds_flag
      , Just (_, _, res_ty) <- tcSplitFunTy_maybe ty
      = go ds_flag res_ty

      | otherwise
      = return True

isGuardedTy :: TcType -> Bool
isGuardedTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | otherwise                               = False

quickLookArg1 :: Int
              -> HsExprLoc
              -> (HsExpr GhcRn, SrcSpan)
              -> LHsExpr GhcRn
              -> Scaled TcRhoType  -- Deeply skolemised
              -> TcM (HsExprArg 'TcpInst)
-- quickLookArg1 implements the "QL Argument" judgement in Fig 5 of the paper
quickLookArg1 pos app_lspan rn_head larg@(L _ arg) sc_arg_ty@(Scaled _ orig_arg_rho)
  = addArgCtxt pos rn_head larg $ -- Context needed for constraints
                                           -- generated by calls in arg
    do { traceTc "qla1" (ppr arg)

       ; (rn_fun_arg, rn_args) <- splitHsApps arg

       ; traceTc "qla2" (ppr arg)

       ; fun_lspan_arg <- getFunSrcSpan rn_args

       -- Step 1: get the type of the head of the argument
       ; (fun_ue, mb_fun_ty) <- if any isExplicitRetainedArg rn_args
                               then return (zeroUE, Nothing)
                               else tcCollectingUsage $ tcInferAppHead_maybe rn_fun_arg
         -- tcCollectingUsage: the use of an Id at the head generates usage-info
         -- See the call to `tcEmitBindingUsage` in `check_local_id`.  So we must
         -- capture and save it in the `EValArgQL`.  See (QLA6) in
         -- Note [Quick Look at value arguments]

       ; lvl <- getTcLevel
       ; traceTc "quickLookArg {" $
         vcat [ text "arg:" <+> ppr arg
              , text "orig_arg_rho:" <+> ppr orig_arg_rho
              , text "head:" <+> ppr rn_fun_arg <+> dcolon <+> ppr mb_fun_ty
              , text "args:" <+> ppr rn_args
              , text "level:" <+> ppr lvl ]

       ; case mb_fun_ty of {
           Nothing -> skipQuickLook app_lspan larg sc_arg_ty ;    -- fun is too complicated
           Just (tc_fun_arg_head, fun_sigma_arg_head) ->

       -- step 2: use |-inst to instantiate the head applied to the arguments
    do { let arg_tc_head = (tc_fun_arg_head, fun_lspan_arg)
       ; do_ql <- wantQuickLook rn_fun_arg

       ; ((inst_args, app_res_rho), wanted)
             <- captureConstraints $
                tcInstFun do_ql True (rn_fun_arg, fun_lspan_arg) tc_fun_arg_head fun_sigma_arg_head rn_args
                -- We must capture type-class and equality constraints here, but
                -- not usage constraints.  See (QLA6) in Note [Quick Look at
                -- value arguments]

       ; traceTc "quickLookArg 2" $
         vcat [ text "arg:" <+> ppr arg
              , text "orig_arg_rho:" <+> ppr orig_arg_rho
              , text "app_res_rho:" <+> ppr app_res_rho ]

       -- Step 3: Check the two other premises of APP-lightning-bolt (Fig 5 in the paper)
       --         Namely: (A) is orig_arg_rho is guarded
       --             or: (B) fiv(app_res_rho) = emptyset
       -- then Step 4: do quick-look unification if either (A) or (B) hold
       --
       -- For (B) see Note [The fiv test in quickLookArg]
       -- This tells us if the quick look at the argument yields information that
       -- influences the enclosing function call
       -- NB: `isGuardedTy` is computed based on the original,
       --         unzonked orig_arg_rho, so that we deliberately do not exploit
       --         guardedness that emerges a result of QL on earlier args
       --     In contrast, we must do the `anyFreeKappa` test /after/ tcInstFun; see (QLA3).
       ; arg_influences_enclosing_call
            <- if isGuardedTy orig_arg_rho
               then return True
               else not <$> anyFreeKappa app_res_rho  -- (B)
                    -- For (B) see Note [The fiv test in quickLookArg]

       -- Step 4: do quick-look unification if either (A) or (B) hold
       -- This is part of APP-lightning-bolt (Fig 5 in the paper)
       -- NB: orig_arg_rho may not be zonked, but that's ok
       ; when arg_influences_enclosing_call $
         qlUnify app_res_rho orig_arg_rho

       ; traceTc "quickLookArg done }" (ppr rn_fun_arg)

       ; return (EValArgQL { eaql_loc_span = app_lspan
                           , eaql_arg_ty   = sc_arg_ty
                           , eaql_larg     = larg
                           , eaql_tc_fun   = arg_tc_head
                           , eaql_rn_fun   = rn_fun_arg
                           , eaql_fun_ue   = fun_ue
                           , eaql_args     = inst_args
                           , eaql_wanted   = wanted
                           , eaql_encl     = arg_influences_enclosing_call
                           , eaql_res_rho  = app_res_rho }) }}}

{- *********************************************************************
*                                                                      *
                 Folding over instantiation variables
*                                                                      *
********************************************************************* -}

{- Note [Monomorphise instantiation variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are done with Quick Look on a call, we must turn any un-unified
/instantiation/ variables into regular /unification/ variables.  This is the
lower-case 'theta' (a mono-substitution) in the APP-DOWN rule of Fig 5 of the
Quick Look paper.

We do this in `qlMonoInstArgs` by looking at the arguments, monomorphising
the free instantiation variables of the /type/ arguments of the call.  Those
type arguments appear (only) in
  * the `WpTyApp` components of
  * the `HsWrapper` of
  * a `EWrap` argument

All instantiation variables for a call will be reachable from the type(s)
at which the function is instantiated -- i.e. those WpTyApps.  Even instantiation
variables allocated by tcInstFun itself, such as in the IRESULT rule, end up
connected to the original type(s) at which the function is instantiated.

To monomorphise the free QL instantiation variables of a type, we use
`foldQLInstVars`.

Wrinkles:

(MIV1) When monomorphising an instantiation variable, don't forget to
   monomorphise its kind. It might have type (a :: TYPE k), where both
  `a` and `k` are instantiation variables.

(MIV2) In `qlUnify`, `make_kinds_ok` may unify
    a :: k1  ~  b :: k2
  making a cast
    a := b |> (co :: k1 ~ k2)
  But now suppose k1 is an instantiation variable.  Then that coercion hole
  `co` is the only place that `k1` will show up in the traversal, and yet
  we want to monomrphise it.  Hence the do_hole in `foldQLInstTyVars`
-}


qlMonoInstArgs :: [HsExprArg 'TcpInst] -> ZonkM ()
-- See Note [Monomorphise instantiation variables]
qlMonoInstArgs args = mapM_ qlMonoInstArg args

qlMonoInstArg :: HsExprArg 'TcpInst -> ZonkM ()
-- See Note [Monomorphise instantiation variables]
qlMonoInstArg (EWrap (EHsWrap w)) = qlMonoHsWrapper w
qlMonoInstArg _                   = return ()

qlMonoHsWrapper :: HsWrapper -> ZonkM ()
-- See Note [Monomorphise instantiation variables]
qlMonoHsWrapper (WpCompose w1 w2) = qlMonoHsWrapper w1 >> qlMonoHsWrapper w2
qlMonoHsWrapper (WpTyApp ty)      = qlMonoTcType ty
qlMonoHsWrapper _                 = return ()

qlMonoTcType :: TcType -> ZonkM ()
-- Monomorphise any free instantiation variables of `ty`
-- This functions zonks as it goes
-- See Note [Monomorphise instantiation variables]
qlMonoTcType ty
  = do { traceZonk "monomorphiseQLInstVars {" (ppr ty)
       ; go_ty ty
       ; traceZonk "monomorphiseQLInstVars }" empty }
  where
    go_ty :: TcType -> ZonkM ()
    go_ty ty = unTcMUnit (foldQLInstVars go_tv ty)

    go_tv :: TcTyVar -> TcMUnit
    -- Precondition: tv is a QL instantiation variable
    -- If it is already unified, look through it and carry on
    -- If not, monomorphise it, by making a fresh unification variable,
    -- at the ambient level
    go_tv tv
      | MetaTv { mtv_ref = ref, mtv_tclvl = lvl, mtv_info = info } <- tcTyVarDetails tv
      = assertPpr (case lvl of QLInstVar -> True; _ -> False) (ppr tv) $
        TCMU $ do { traceZonk "qlMonoTcType" (ppr tv)
                  ; flex <- readTcRef ref
                  ; case flex of {
                      Indirect ty -> go_ty ty ;
                      Flexi       ->
               do { let kind = tyVarKind tv
                  ; go_ty kind  -- See (MIV1) in Note [Monomorphise instantiation variables]
                  ; ref2  <- newTcRef Flexi
                  ; lvl2  <- getZonkTcLevel
                  ; let details = MetaTv { mtv_info  = info
                                         , mtv_ref   = ref2
                                         , mtv_tclvl = lvl2 }
                        tv2  = mkTcTyVar (tyVarName tv) kind details
                 ; writeTcRef ref (Indirect (mkTyVarTy tv2)) }}}
      | otherwise
      = pprPanic "qlMonoTcType" (ppr tv)

newtype TcMUnit = TCMU { unTcMUnit :: ZonkM () }
instance Semigroup TcMUnit where
  TCMU ml <> TCMU mr = TCMU (ml >> mr)
instance Monoid TcMUnit where
  mempty = TCMU (return ())

{- Note [The fiv test in quickLookArg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In rule APP-lightning-bolt in Fig 5 of the paper, we have to test rho_r
for having no free instantiation variables.  We do this in Step 3 of quickLookArg1,
using anyFreeKappa.  Example:
    Suppose       ids :: [forall a. a->a]
    and consider  Just (ids++ids)
We will instantiate Just with kappa, say, and then call
    quickLookArg1 False {kappa} (ids ++ ids) kappa
The call to tcInstFun will return with app_res_rho = [forall a. a->a]
which has no free instantiation variables, so we can QL-unify
  kappa ~ [forall a. a->a]
-}

anyFreeKappa :: TcType -> TcM Bool
-- True if there is a free instantiation variable
-- in the argument type, after zonking
-- See Note [The fiv test in quickLookArg]
-- The function is monadic; it does not require a zonked type as input
anyFreeKappa ty = do { rejectNativeQuickLook ty
                     ; unTcMBool (foldQLInstVars go_tv ty) }
  where
    go_tv tv = TCMB $ do { info <- readMetaTyVar tv
                         ; case info of
                             Indirect ty -> anyFreeKappa ty
                             Flexi       -> return True }

newtype TcMBool = TCMB { unTcMBool :: TcM Bool }
instance Semigroup TcMBool where
  TCMB ml <> TCMB mr = TCMB (do { l <- ml; if l then return True else mr })
instance Monoid TcMBool where
  mempty = TCMB (return False)

foldQLInstVars :: forall a. Monoid a => (TcTyVar -> a) -> TcType -> a
{-# INLINE foldQLInstVars #-}
foldQLInstVars check_tv ty
  = runFV (do_ty ty) ()
  where
    (do_ty, _, _, _) = foldTyCo folder

    folder :: TyCoFolder (FV () a)
    folder = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                                             -- in GHC.Core.TyCo.FVs
                        , tcf_lit        = mempty
                        , tcf_tyvar      = do_tv
                        , tcf_termvar    = \v -> pprPanic "foldQLInstVars: native term index not implemented" (ppr v)
                        , tcf_termbinder = \v _ -> pprPanic "foldQLInstVars: native dependent binder not implemented" (ppr v)
                        , tcf_covar      = mempty
                        , tcf_hole       = do_hole
                        , tcf_tycobinder = do_bndr }

    do_bndr _ = id

    do_hole hole = do_ty (coVarKind (coHoleCoVar hole))
                     -- See (MIV2) in Note [Monomorphise instantiation variables]

    do_tv :: TcTyVar -> FV () a
    do_tv tv | isQLInstTyVar tv = MkFV $ \_ -> check_tv tv
             | otherwise        = mempty

{- *********************************************************************
*                                                                      *
                 QuickLook unification
*                                                                      *
********************************************************************* -}

-- | Reject retained types before Quick Look inspects or assigns them.
-- See Note [Retained types and Quick Look].
rejectNativeQuickLook :: TcType -> TcM ()
rejectNativeQuickLook ty
  = do { ty <- liftZonkM $ zonkTcType ty
       ; when (hasNativeDependentExpr (Core.Type ty)) $
           failWithTc (TcRnRetainedQuantificationError (RetainedQuickLook ty)) }

-- | Check the type arguments that Quick Look will monomorphise.
rejectNativeQLInstArgs :: [HsExprArg 'TcpInst] -> TcM ()
rejectNativeQLInstArgs = mapM_ check_arg
  where
    check_arg (EWrap (EHsWrap wrap)) = check_wrap wrap
    check_arg _ = return ()

    check_wrap (WpCompose wrap1 wrap2) = check_wrap wrap1 >> check_wrap wrap2
    check_wrap (WpTyApp ty) = rejectNativeQuickLook ty
    check_wrap _ = return ()

{- Note [Retained types and Quick Look]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quick Look assigns instantiation variables without the ordinary unifier's
runtime-index escape check. Its instantiation-variable folders do not support
retained types either. Reject those types before both operations. Zonk first
so that the check also sees a type behind a filled metavariable. Check the
WpTyApp inputs before qlMonoInstArgs enters its ZonkM folder.

This check depends on the actual type, not ExperimentalDependentTypes.
Imported contracts can contain native types in a client without that flag.
An incomplete immutable application plan can retain DoQL in such a client.
The native-type rejection must still run before a Quick Look assignment or
unsupported traversal. See Note [Imported native application contracts].
This restriction does not add Quick Look support for retained types.
See Note [Rigid retained indices] in GHC.Tc.Utils.Unify.
-}

qlUnify ::  TcType -> TcType -> TcM ()
-- Unify ty1 with ty2:
--   * It unifies /only/ instantiation variables (see (UQL6)), hopefully with polytypes
--   * It does not return a coercion (unlike unifyType); it is called
--     for the sole purpose of unifying instantiation variables, although it
--     may also (opportunistically) unify regular unification variables.
--   * It never produces errors, even for mis-matched types
--   * It may return without having made the argument types equal, of course;
--     it just makes best efforts.
qlUnify ty1 ty2
  = do { rejectNativeQuickLook ty1
       ; rejectNativeQuickLook ty2
       ; lvl <- getTcLevel
       ; traceTc "qlUnify" (ppr lvl $$ ppr ty1 $$ ppr ty2)
       ; go ty1 ty2 }
  where
    go :: TcType -> TcType -> TcM ()

    go t1 t2 = do { traceTc "qlUinfy:go" (ppr t1 <+> char '~' <+> ppr t2)
                  ; go' t1 t2 }

    -- Decompose (arg1 -> res1) ~ (arg2 -> res2)
    -- and         (c1 => res1) ~   (c2 => res2)
    -- But for the latter we only learn instantiation info from res1~res2
    go' (FunTy { ft_af = af1, ft_ma = ma1, ft_arg = arg1, ft_res = res1 })
       (FunTy { ft_af = af2, ft_ma = ma2, ft_arg = arg2, ft_res = res2 })
      | af1 == af2 -- Match the arrow TyCon
      = do { traceTc "go_fun" (ppr arg1 $$ ppr arg2)
           ; go ma1 ma2
           ; when (isVisibleFunArg af1) (go arg1 arg2)

        -- NB: we do not unify the multiplicities; that would be too strong.
        -- We might only require mult1 ⩽ mult2, as in Note [Multiplicity in deep subsumption].
        -- ; when (isFUNArg af1)        (go mult1 mult2)

           ; go res1 res2 }

    -- Make sure to not unify "kappa := (a %1 -> b)". See (UQL5).
    -- NB: we /do/ want to be able to unify "kappa := a => b", as that's
    -- the main point of QuickLook (allowing meta-variables to be unified
    -- with qualified types).
    go' (FunTy { ft_mult = OneTy }) _ = return ()
    go' _ (FunTy { ft_mult = OneTy }) = return ()

    -- Type variables; see (UQL6) in Note [QuickLook unification]
    go' (TyVarTy tv) ty2 | isMetaTyVar tv = go_meta tv ty2
    go' ty1 (TyVarTy tv) | isMetaTyVar tv = go_meta tv ty1

    go' (CastTy ty1 _) ty2 = go ty1 ty2
    go' ty1 (CastTy ty2 _) = go ty1 ty2

    go' (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2 -- See GHC.Tc.Utils.Unify
      = return ()  -- Note [Expanding synonyms during unification]

    -- Now, and only now, expand synonyms
    go' rho1 rho2
      | Just rho1 <- coreView rho1 = go rho1 rho2
      | Just rho2 <- coreView rho2 = go rho1 rho2

    go' (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , not (isTypeFamilyTyCon tc1)
      , tys1 `equalLength` tys2
      = zipWithM_ go tys1 tys2

    -- Don't allow unifying (a => b) with the AppTy 'arr[tau] a b'.
    -- To ensure this, use 'tcSplitAppTyNoView_maybe' which does not split (=>).
    go' (AppTy t1a t1b) ty2
      | Just (t2a, t2b) <- tcSplitAppTyNoView_maybe ty2
      = do { go t1a t2a; go t1b t2b }
    go' ty1 (AppTy t2a t2b)
      | Just (t1a, t1b) <- tcSplitAppTyNoView_maybe ty1
      = do { go t1a t2a; go t1b t2b }

    -- Type families have a monomorphic result, so if we see (say)
    --     Maybe kappa ~ F ty
    -- we know that kappa must be a monotype
    go' (TyConApp tc1 _) ty2 | isTypeFamilyTyCon tc1 = monomorphise ty2
    go' ty1 (TyConApp tc2 _) | isTypeFamilyTyCon tc2 = monomorphise ty1

    go' _ _ = return ()
       -- Don't look under foralls; see (UQL4) of Note [QuickLook unification]

    ----------------
    go_meta tv1 ty2
      = assertPpr (isMetaTyVar tv1) (ppr tv1) $
        do { info <- readMetaTyVar tv1
           ; case info of  -- See (UQL9)
               Indirect ty1 -> go ty1 ty2
               Flexi        -> do { ty2 <- liftZonkM $ zonkTcType ty2
                                  ; go_flexi tv1 ty2 } }

    ----------------
    -- Both tv1 and ty2 are zonked; tv1 is a unfilled meta-tyvar
    -- Zonking ty2 makes the occurs-check easier
    go_flexi tv1 ty2
      | TyVarTy tv2 <- ty2
      , lhsPriority tv2 > lhsPriority tv1
      = go_flexi1 tv2 (TyVarTy tv1)
      | otherwise
      = go_flexi1 tv1 ty2

    -- Both tv1 and ty2 are zonked; tv1 is a unfilled meta-tyvar
    go_flexi1 tv1 ty2
      | isQLInstTyVar tv1 = go_kappa tv1 ty2
      | otherwise         = monomorphise ty2

    go_kappa kappa ty2
      | isConcreteTyVar kappa  -- See (UQL7) in Note [QuickLook unification]
      = return ()
      | anyFreeVarsOfType (== kappa) ty2
      = return ()  -- Occurs check.  See (UQL10) in Note [QuickLook unification]
      | otherwise
      = do { co <- unifyKind (Just (TypeThing ty2)) ty2_kind kappa_kind
                   -- unifyKind: see (UQL2) in Note [QuickLook unification]
                   --            and (MIV2) in Note [Monomorphise instantiation variables]
           ; let ty2' = mkCastTy ty2 co
           ; traceTc "qlUnify:update" $
             ppr kappa <+> text ":=" <+> ppr ty2
           ; liftZonkM $ writeMetaTyVar kappa ty2'
           ; return () }
      where
        kappa_kind = tyVarKind kappa
        ty2_kind   = typeKind ty2

    monomorphise :: TcType -> TcM ()
    monomorphise ty  -- See (UQL8)
      = do { _ <- liftZonkM $ qlMonoTcType ty; return () }

{- Note [QuickLook unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In qlUnify, if we find (kappa ~ ty), we are going to update kappa := ty.
That is the entire point of qlUnify!   Wrinkles:

(UQL1) Before unifying an instantiation variable in `go_kappa`, we must check
  the usual unification conditions. In particular:
    * An occurs-check
    * An attempt to unify a concrete type variable with a non-concrete type.
  We don't need a level-check because the level of an instantiation variable
  is infinity -- see (UQL3).

(UQL2) What if kappa and ty have different kinds?  We simply call the
  ordinary unifier and use the coercion to connect the two.

  If that coercion is not Refl, it is all in vain: The whole point of
  qlUnify is to impredicatively unify (kappa := forall a. blah). It is
  no good to unify (kappa := (forall a.blah) |> co) because we can't
  use that casted polytype.

  BUT: unifyKind has emitted constraint(s) into the Tc monad, so we may as well
  use them.  (An alternative; use uType directly, if the result is not Refl,
  discard the constraints and the coercion, and do not update the instantiation
  variable.  But see "Sadly discarded design alternative" below.)

(UQL3) Instantiation variables don't really have a settled level yet;
  they have level QLInstVar (see Note [QuickLook instantiation variables]
  in GHC.Tc.Utils.TcType.  You might worry that we might unify
      alpha[1] := Maybe kappa[qlinst]
  and later this kappa turns out to be a level-2 variable, and we have committed
  a skolem-escape error.

  But happily this can't happen: QL instantiation variables have level infinity,
  and we never unify a variable with a type from a deeper level.

(UQL4) Should we look under foralls in qlUnify? The use-case would be
     (forall a.  beta[qlinst] -> a)  ~  (forall a. (forall b. b->b) -> a)
  where we might hope for
     beta := forall b. b

  But in fact we don't attempt this:

  * The normal on-the-fly unifier doesn't look under foralls, so why
    should qlUnify?

  * Looking under foralls means we'd have to track the bound variables on both
    sides.  Tiresome but not a show stopper.

  * We might call the *regular* unifier (via unifyKind) under foralls, and that
    doesn't know about those bound variables (it controls scope through level
    numbers) so it might go totally wrong.  At least we'd have to instantaite
    the forall-types with skolems (with level numbers).  Maybe more.

  It's just not worth the trouble, we think (for now at least).

(UQL5) qlUnify must be careful about linear function arrows. Suppose for example
  we have a program like:

    data A = MkA Int Bool
      -- As per Note [Data constructors are linear by default],
      -- this means that MkA :: Int %1 -> Bool -> A

    foo :: Maybe (Bool -> A) -> ()
    foo _ ()
    bar = foo just_pap
      where
        just_pap = Just $ B 3

  Here, when typechecking the application 'B A', we will get a call to qlUnify:

    qlUnify
      (Bool %1 -> A)
      𝜈[tau:qlinst]

  We want to hold off on unifying 𝜈 := Bool %1 -> A: instead, we want to accept
  the program with the more lenient subtype check

    Bool %Many -> A  ⩽  𝜈

  The general principle is: treat linear arrows %1 -> similar to foralls and
  constraint arrows =>, so that (UQL4) applies to them as well.
  See Note [Multiplicity in deep subsumption].

(UQL6) What if `qlUnify` sees a regular (monotyped) unification variable, `alpha`, rather
  than an instantiation variable `kappa`?  Thus (alpha ~ ty).   It's tempting to just
  unify it, but the regular, on-the-fly unifier has lots of careful checks (levels,
  concreteness etc).  In regular unification it doesn't matter whether we unify on-the-fly
  or later -- it's just an efficiency issue -- but we must maintain all the careful invariants.

  The simplest thing is to say that `qlUnify` unifies /only/ instantiation variables
  (see calls to `isQLInstTyVar`).  We could be more ambitious, perhaps, in future, but
  beware of accidentally causing obscure /user-visible/ consequences (#26543),
  e.g. via Note [Occurs check and coercions].

  Remember, though, that the /only/ reason for QuickLoook is to unify instantiation variables
  with polytypes; anything else is optional, and will be picked up with later "ordinary"
  typechecking.  So unifing only instantiation variables is enough.

(UQL7) If `qlUnify` sees a concrete `kaapa`, thus
            kappa[conc] ~ some-type
  we just give up. It's tiresome to do all the checks for concreteness; and concrete
  type variables are never unified with polytypes, so it's fine to leave it for the
  regular unifier.

(UQL8) If we see (Maybe kappa ~ alpha), where `kappa` is an instantiation variable
  and `alpha` is a regular unification variable, we know that `kappa` must be a
  monotype, so we can use `qlMonoTcType` to monomorphise it.  This is important,
  because rule APP-LIGHTNING in the QuickLook paper only fires when there are no free
  instantiation variables.  Unifying `kappa` with a monotype makes it more possible
  for APP-LIGHTNING to fire.

  Same if we see (Maybe kappa ~ F tya) where F is a type function, becuase the result
  of a type function is always a monotype.

(UQL9) If we have (Maybe kappa ~ alpha) and we have /already/ unified alpha:=Maybe beta,
  then it's more efficient to look through that unification and decompose, so that
  we get (kappa ~ alpha).  That way we don't have to monomorphise `kappa`.  So we
  "look through" all meta-tyvars, not just the instantiation vars.

(UQL10) For the occurs check in `qlUnify` we simply use `anyFreeVarsOfType`.  For
  coercions that looks at the free vars of the coercion; but for coercion /holes/
  all it can do is look at the free vars of the hole's type.
  See Note [Occurs check and coercions] in GHC.Tc.Utils.Unify

Sadly discarded design alternative
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is very tempting to use `unifyType` rather than `qlUnify`, killing off the
latter.  (Extending `unifyType` slightly to allow it to unify an instantiation
variable with a polytype is easy.).  But I could not see how to make it work:

 * `unifyType` makes the types /equal/, and returns a coercion, and it is hard to
   marry that up with DeepSubsumption.  Absent deep subsumption, this approach
   might just work.

 * I considered making a wrapper for `uType`, which simply discards any deferred
   equality constraints.  But we can't do that: in a heterogeneous equality we might
   have unified a unification variable (alpha := ty |> co), where `co` is only bound
   by those constraints.
-}

{- *********************************************************************
*                                                                      *
                 tagToEnum#
*                                                                      *
********************************************************************* -}

{- Note [tagToEnum#]
~~~~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  It's crude, because it relies on our
knowing *now* that the type is ok, which in turn relies on the
eager-unification part of the type checker pushing enough information
here.  In theory the Right Thing to do is to have a new form of
constraint but I definitely cannot face that!  And it works ok as-is.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

When data type families are involved it's a bit more complicated.
     data family F a
     data instance F [Int] = A | B | C
Then we want to generate something like
     tagToEnum# R:FListInt 3# |> co :: R:FListInt ~ F [Int]
Usually that coercion is hidden inside the wrappers for
constructors of F [Int] but here we have to do it explicitly.

It's all grotesquely complicated.
-}

isTagToEnum :: HsExpr GhcTc -> Bool
isTagToEnum (HsVar _ (L _ fun_id)) = fun_id `hasKey` tagToEnumKey
isTagToEnum _ = False

tcTagToEnum :: HsExpr GhcTc -> [HsExprArg 'TcpTc] -> TcRhoType
            -> TcM (HsExpr GhcTc)
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum tc_fun tc_args res_ty
  | [val_arg] <- dropWhile (not . isHsValArg) tc_args
  = do { res_ty <- liftZonkM $ zonkTcType res_ty

       -- Check that the type is algebraic
       ; case tcSplitTyConApp_maybe res_ty of {
           Nothing -> do { addErrTc (TcRnTagToEnumUnspecifiedResTy res_ty)
                         ; vanilla_result } ;
           Just (tc, tc_args) ->

    do { -- Look through any type family
       ; fam_envs <- tcGetFamInstEnvs
       ; case tcLookupDataFamInst_maybe fam_envs tc tc_args of {
           Nothing -> do { check_enumeration res_ty tc
                         ; vanilla_result } ;
           Just (rep_tc, rep_args, coi) ->

    do { -- coi :: tc tc_args ~R rep_tc rep_args
         check_enumeration res_ty rep_tc
       ; let rep_ty  = mkTyConApp rep_tc rep_args
             tc_fun' = mkHsWrap (WpTyApp rep_ty) tc_fun
             df_wrap = mkWpCastR (mkSymCo coi)
             tc_expr = rebuildHsApps tc_fun' [val_arg]
       ; return (mkHsWrap df_wrap tc_expr) }}}}}

  | otherwise
  = failWithTc TcRnTagToEnumMissingValArg

  where
    vanilla_result = return (rebuildHsApps tc_fun tc_args)

    check_enumeration ty' tc
      | -- isTypeDataTyCon: see wrinkle (W1) in
        -- Note [Type data declarations] in GHC.Rename.Module
        isTypeDataTyCon tc    = addErrTc (TcRnTagToEnumResTyTypeData ty')
      | isEnumerationTyCon tc = return ()
      | otherwise             = addErrTc (TcRnTagToEnumResTyNotAnEnum ty')

{- *********************************************************************
*                                                                      *
             Pragmas on expressions
*                                                                      *
********************************************************************* -}

tcExprPrag :: HsPragE GhcRn -> HsPragE GhcTc
tcExprPrag (HsPragSCC x1 ann) = HsPragSCC x1 (tcStringLit ann)
