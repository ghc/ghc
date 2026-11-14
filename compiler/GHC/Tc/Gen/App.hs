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
       , tcExprPrag ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcPolyExpr, tcInferRho )

import GHC.Hs

import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Sig ( isCompleteHsSig )
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Env ( tcLookupIdMaybe, tcLookupConLike )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst_maybe )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Concrete  ( unifyConcrete, idConcreteTvs )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types ( TcGblEnv(tcg_type_env) )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint (WantedConstraints(wc_simple), ctPred)
import GHC.Tc.Types.ErrCtxt ( FunAppCtxtFunArg(..) )
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Zonk.TcType

import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon ( dataConConcreteTyVars, isNewDataCon, dataConOrigArgTys, dataConRepArgBinders, dataConTyCon, dataConWorkId )
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.Utils ( applyTypeToArgs )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Subst ( substTyWithInScope )
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Unify ( tcMatchTyX_BM, tcMatchRetainedTyX_BM, BindFlag(..) )
import GHC.Core.Type
import GHC.Core.UsageEnv ( UsageEnv )
import GHC.Core.Coercion

import GHC.Builtin.WiredIn.Types ( multiplicityTy, runtimeRepTy )
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.KnownKeys

import GHC.Types.TyThing ( TyThing(AnId) )
import GHC.Types.TypeEnv ( lookupTypeEnv )
import GHC.Types.Var
import GHC.Types.Var.Set ( elemVarSet )
import GHC.Types.Var.FV
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.Var.Env  ( emptyTidyEnv, mkInScopeSet )

import GHC.Data.Maybe
import GHC.Data.FastString

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Data ( Data, cast, gmapQ )
import Data.Function
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
       ; (tc_fun1, fun_sigma1) <- tcRetainedExpectedInst wanted_ql exp_res_ty rn_args
                                                     tc_fun0 fun_sigma0
       ; (tc_fun2, fun_sigma2, initial_cache, inst_plan) <-
           tcRetainedArgInst wanted_ql exp_res_ty rn_head rn_args tc_fun1 fun_sigma1
       ; (dollar_inst, dollar_cache) <-
           tcRetainedDollarInst exp_res_ty rn_head rn_args tc_fun2 fun_sigma2
       ; let (tc_fun, fun_sigma) = fromMaybe (tc_fun2, fun_sigma2) dollar_inst
             arg_cache = initial_cache ++ dollar_cache
             do_ql | null dollar_cache = wanted_ql
                   | otherwise = NoQL
             -- inst_final: top-instantiate the result type of the application,
             -- EXCEPT if we are trying to infer a sigma-type
             expected_rho = case exp_res_ty of
                              Check rho -> Just rho
                              Infer {}  -> Nothing
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
              , text "do_ql:" <+> ppr do_ql]

       ; tcRetainedAppEvidence $
         do { -- Step 2: Instantiate the function type (taking a quick look at args)
              -- See Note [Quick Look for particular Ids]
              (inst_args, app_res_rho) <-
                tcInstFunWithExpected expected_rho inst_plan do_ql inst_final
                                      rn_head tc_fun fun_sigma rn_args

            ; -- Step 3: Take a quick look at the result type
              -- See Note [QuickLook: arguments before result]
              case (do_ql, exp_res_ty) of
                 (DoQL, Check exp_rho) -> quickLookResultType app_res_rho exp_rho
                 _                     -> return ()

            ; finishAppWithCache arg_cache do_ql rn_expr fun_lspan rn_fun tc_fun
                                       inst_args app_res_rho exp_res_ty } }

-- | Bind native dictionary evidence at the application that needs it.
tcRetainedAppEvidence :: TcM (HsExpr GhcTc) -> TcM (HsExpr GhcTc)
tcRetainedAppEvidence thing_inside
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; if not enabled then thing_inside else
    do { (expr, wanted) <- captureConstraints thing_inside
       ; if any (hasNativeDependentExpr . Core.Type . ctPred) (wc_simple wanted)
         then do { tclvl <- getTcLevel
                 ; (implic, ev_binds) <- buildImplicationFor tclvl RetainedAppSkol [] [] wanted
                 ; emitImplications implic
                 ; return (mkHsWrap (mkWpLet ev_binds) expr) }
         else do { emitConstraints wanted
                 ; return expr } } }

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
tcValArgsWithCache cache do_ql rn_head args = go do_ql 0 args
  where
    go :: QLFlag -> Int -> [HsExprArg 'TcpInst] -> TcM [HsExprArg 'TcpTc]
    go _ _ [] = return []
    go do_ql pos (arg : args)
      = do { arg'  <- case (arg, lookup pos' cache) of
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
           ; args' <- go do_ql pos' args
           ; return (arg' : args') }
      where
        -- Increment position if the argument is user-written type or value argument
        !pos' | user_visible arg = pos + 1
              | otherwise        = pos

    user_visible (EValArg{})                   = True
    user_visible (EValArgQL{})                 = True
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


         -- Now check the argument
       ; arg' <- tcScalingUsage mult $
                 tcPolyExpr arg (mkCheckExpType exp_arg_ty)
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
tcInstFunWithExpected :: Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                      -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                      -> TcSigmaType -> [HsExprArg 'TcpRn]
                      -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithExpected expected_rho inst_plan do_ql inst_final rn_head@(_, fun_lspan)
                      tc_fun fun_sigma rn_args
  = do { lvl <- getTcLevel
       ; traceTc "tcInstFun" (vcat [ text "tc_fun" <+> ppr tc_fun
                                   , text "rn_fun" <+> ppr rn_head
                                   , text "fun_sigma" <+> ppr fun_sigma
                                   , text "args:" <+> ppr rn_args
                                   , text "do_ql" <+> ppr do_ql
                                   , text "lvl:" <+> ppr lvl ])
       ; case do_ql of
           NoQL -> return ()
           DoQL -> setSrcSpan fun_lspan $ rejectNativeQuickLook fun_sigma
       ; fun_origin <- mk_origin rn_head
       ; res@(_, fun_ty) <- go inst_plan fun_origin 1 [] fun_sigma rn_args
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
    inst_fun _                = isInferredForAllTyFlag

    -----------
    go, go1 :: RetainedInstPlan          -- Immutable choices in binder order
            -> CtOrigin                 -- Of the function
            -> Int                      -- Value-argument position of next arg
            -> [HsExprArg 'TcpInst]     -- Accumulator, reversed
            -> TcSigmaType -> [HsExprArg 'TcpRn]
            -> TcM ([HsExprArg 'TcpInst], TcSigmaType)

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
              Nothing -> go1 plan fun_orig pos acc fun_ty args }

    -- go1: fun_ty is not filled-in instantiation variable
    --      ('go' dealt with that case)

    -- Handle out-of-scope functions gracefully
    go1 plan fun_orig pos acc fun_ty (arg : rest_args)
      | fun_is_out_of_scope, looks_like_type_arg arg   -- See Note [VTA for out-of-scope functions]
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
                          subst = Subst.zipTvSubst tvs inst_tys
                    ; unless (tvs `equalLength` inst_tys
                              && and (zipWith (\tv ty ->
                                   eqType (Subst.substTy subst (tyVarKind tv))
                                          (typeKind ty)) tvs inst_tys)) $
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
       = do { (wrap_co, arg_ty, res_ty) <-
                  matchActualFunTy (FRRRepPolyUnliftedNewtype dc)
                    (Just $ HsExprTcThing tc_fun)
                    (n_val_args, fun_sigma) fun_ty
             ; let acc' = addArgWrap (mkWpCastN wrap_co) acc
             ; return (reverse acc', tcMkScaledFunTy arg_ty res_ty) }
      | otherwise
      = return (reverse acc, fun_ty)

    -- See Note [Retained argument application].
    go1 plan fun_orig pos acc fun_ty
        (EValArg { ea_arg = arg, ea_loc_span = ctxt } : rest_args)
      | DepFunTy binder _ <- coreFullView fun_ty
      = do { core_arg <- tcRetainedArg arg
           ; arg' <- skipQuickLook ctxt arg (Scaled ManyTy (varType binder))
           ; let body_ty = applyTypeToArgs fun_ty [core_arg]
           ; go plan fun_orig (pos + 1) (arg' : acc) body_ty rest_args }

    -- Rule ITVDQ from the GHC Proposal #281
    go1 plan fun_orig pos acc fun_ty ((EValArg { ea_arg = arg }) : rest_args)
      | Just (tvb, body) <- tcSplitForAllTyVarBinder_maybe fun_ty
      = assertPpr (isVisibleForAllTyFlag (binderFlag tvb)) (ppr fun_ty $$ ppr arg) $
        -- Any invisible binders have been instantiated by IALL above,
        -- so this forall must be visible (i.e. Required)
        do { (ty_arg, inst_body) <- tcVDQ fun_conc_tvs (tvb, body) arg
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
           ; let fun_ty' = mkScaledFunTys arg_tys res_ty

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
           ; (fun_co, arg_ty, res_ty) <-
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
           ; arg' <- quickLookArg do_ql pos ctxt rn_head arg arg_ty
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


-- | Resolve a retained argument without evaluating or typechecking its value.
tcRetainedArg :: LHsExpr GhcRn -> TcM Core.CoreExpr
tcRetainedArg arg
  = setSrcSpanA (getLoc arg) $
    do { core_arg <- resolve arg
       ; case Core.exprToNativeIndex core_arg of
           Just _ -> return core_arg
           Nothing -> unsupported }
  where
    resolve :: LHsExpr GhcRn -> TcM Core.CoreExpr
    resolve (L loc expr)
      = setSrcSpanA loc $ case expr of
          HsPar _ arg -> resolve arg
          XExpr (ExpandedThingRn (HSE { hse_exp = arg })) -> resolve arg
          HsApp _ fun arg -> Core.App <$> resolve fun <*> resolve arg
          HsVar _ lname
            | isDataOcc (nameOccName (unLocWithUserRdr lname))
            -> do { con <- tcLookupConLike (unLoc lname)
                  ; case con of
                      RealDataCon dc -> return (Core.Var (dataConWorkId dc))
                      _ -> unsupported }
            | otherwise
            -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
                  ; case mb_id of
                      Just arg_id
                        -> do { arg_id <- liftZonkM (zonkId arg_id)
                              ; if isNativeIndexId arg_id
                                then return (Core.Var arg_id)
                                else unsupported }
                      _ -> unsupported }
          _ -> unsupported

    unsupported :: TcM a
    unsupported = failWithTc (TcRnRetainedQuantificationError RetainedIndexExpression)

{- Note [Retained argument application]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained argument is a value. Resolve a local unrestricted Id or a saturated
constructor tree before instantiating the codomain. applyTypeToArgs uses the
same index substitution as Core type recovery. Do not use tcVDQ: it translates
an expression into an erased type argument.

Keep a normal EValArg with the binder's domain as its expected type. tcValArg
checks the value once and records its runtime usage. Resolution does not inspect
unfoldings or evaluate values. Later argument types use the same runtime Id,
including a lazy binding whose RHS is bottom. No singleton is added.
See docs/dependent-haskell/REFERENCES.md#agda-provenance and
docs/dependent-haskell/CORE.md#laziness-and-eta.
-}

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
  , (tvs, body) <- tcSplitForAllInvisTyVars sigma
  , not (null tvs)
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) body
  , isTauTy expected, isTauTy body
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
  , (tvs, body) <- tcSplitForAllInvisTyVars sigma
  , not (null tvs)
  , let (theta, tau) = tcSplitPhiTy body
  , null theta || hasNoConcreteTyVars fun
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) tau
  , isTauTy expected || (hasNoConcreteTyVars fun && isRetainedValueType expected)
  , isTauTy result
  , not (hasNativeDependentExpr (Core.Type body))
  , Just subst <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst result expected
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
  , (tvs, body) <- tcSplitForAllInvisTyVars sigma
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
    constructor_result (Just _ : fields) (DepFunTy _ body)
      = constructor_result fields body
    constructor_result (Just index : fields) ty
      | FunTy flag mult domain body <- coreFullView ty
      , isVisibleFunArg flag
      , isManyTy mult
      , eqType domain (varType index)
      = constructor_result fields body
    constructor_result (Nothing : fields) ty
      | Just (_, body) <- splitVisibleFunTy_maybe ty
      = constructor_result fields body
    constructor_result _ _ = Nothing
tcRetainedWorkerResultMatch _ _ _ _ _ = return Nothing

-- | Admit known runtime function values without erased polymorphism.
-- Constraint arrows are supported only below a retained binder.
isRetainedValueType :: Type -> Bool
isRetainedValueType = go False
  where
    go _ ty | isTauTy ty = True
    go under_pi ty = case coreFullView ty of
      DepFunTy index body -> isNativeIndexId index
                         && typeKind body `eqType` liftedTypeKind
                         && go True body
      FunTy flag mult arg res -> isTauTy mult
                              && (isVisibleFunArg flag || under_pi)
                              && go under_pi arg && go under_pi res
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
accepts constraint arrows below a retained binder. Reject erased foralls and
other non-tau type heads. Require a function head without concreteness variables.
The template remains ordinary and its result remains tau. The existing NoQL,
type-argument, complete-substitution, kind, runtime-scope and exact-result checks
still apply. Preserve the original body and its constraints in the substitution.
The ordinary argument and result subsumption checks still run afterward.

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
from its argument. Under ExperimentalDependentTypes and NoQL, infer arguments
whose formal types mention outer invisible index binders. Match their actual
types against the formal types with an immutable substitution. A retained
expected result can supply additional bindings. Apply the substitution only
when every binder is determined and its kind and local runtime scope agree.

Save the typed arguments and their usage environments. tcValArgsWithCache
checks each saved type by subsumption and emits its usage under the final
argument multiplicity. It does not typecheck the source expression again.
Other arguments retain ordinary expected-type checking. If matching fails or
leaves binders undetermined, ordinary instantiation checks the same cached
arguments. The ordinary unifier still rejects retained meta-variable fills.

Only saturated ordinary monomorphic arrows without explicit type arguments
enter this route. Preserve any leading constraint context as described in
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
from Nothing remain ordinary meta-variables. Seed the immutable substitution
from the expected result first. Do not infer a domain whose outer binders
are already determined by that result.

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

-- | Classify named or completely signed arguments without elaboration.
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

-- | Specialize the wired-in dollar operator from a checked result and one
-- cached native-anchored RHS. Keep its original type and evidence application.
-- See Note [Immutable native dollar applications].
tcRetainedDollarInst :: ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                    -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                    -> TcM (Maybe (HsExpr GhcTc, TcSigmaType), RetainedArgCache)
tcRetainedDollarInst (Check expected) rn_head@(HsVar _ (L _ (WithUserRdr _ name)), _)
                    args fun@(HsVar _ (L _ dollar)) sigma
  | name `hasKey` dollarIdKey, dollar `hasKey` dollarIdKey
  , not (any is_type_arg args)
  , [lhs, rhs] <- [arg | EValArg { ea_arg = arg } <- args]
  , (tvs, body) <- tcSplitForAllInvisTyVars sigma
  , not (null tvs)
  , ([_, domain], result) <- splitFunTys body
  , isTauTy body, not (hasNativeDependentExpr (Core.Type body))
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; result_ty <- if not enabled then return Nothing else
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
    tau_domains (ForAllTy _ body) = tau_domains body
    tau_domains (CastTy ty _) = tau_domains ty
    tau_domains (DepFunTy binder body) = isTauTy (varType binder) && tau_domains body
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
                              ; let (tvs, body) = tcSplitForAllInvisTyVars sigma
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
tcRetainedDollarInst _ _ _ _ _ = return (Nothing, [])

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

Require a named, native-anchored RHS under ExperimentalDependentTypes.
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
  | (tvs, body) <- tcSplitForAllInvisTyVars sigma
  , (theta, tau) <- tcSplitPhiTy body
  , not (null tvs && null theta)
  = let (rest_tvs, rest_theta, result) = tcRetainedInstTelescope tau
    in (tvs ++ rest_tvs, theta ++ rest_theta, result)
  | otherwise = ([], [], sigma)

-- | Arguments inferred once before immutable index instantiation.
-- The argument position counts source value arguments from one.
type RetainedArgCache = [(Int, (LHsExpr GhcTc, TcRhoType, UsageEnv))]

-- | Infer index-bearing arguments once and match only outer invisible binders.
tcRetainedArgInst :: QLFlag -> ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                  -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                  -> TcM (HsExpr GhcTc, TcSigmaType, RetainedArgCache, RetainedInstPlan)
tcRetainedArgInst do_ql expected rn_head args fun sigma
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; if enabled
         then tcRetainedArgInstEnabled do_ql expected rn_head args fun sigma
         else return (fun, sigma, [], Nothing) }

-- | Match cached index arguments when the experimental extension is enabled.
tcRetainedArgInstEnabled :: QLFlag -> ExpRhoType -> (HsExpr GhcRn, SrcSpan)
                         -> [HsExprArg 'TcpRn] -> HsExpr GhcTc -> TcSigmaType
                         -> TcM (HsExpr GhcTc, TcSigmaType, RetainedArgCache, RetainedInstPlan)
tcRetainedArgInstEnabled NoQL expected rn_head args fun sigma
  | not (any is_type_arg args)
  , (outer_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let (tvs, theta, tau) = tcRetainedInstTelescope sigma
        nested = length tvs /= length outer_tvs
  , let index_tvs = filter (isNativeIndexType . tyVarKind) tvs
  , not (null tvs)
  , null theta || hasNoConcreteTyVars fun
  , let (domains, result) = splitFunTys tau
  , length domains == count isHsValArg args
  , isTauTy tau, not (isFunTy result)
  , not (hasNativeDependentExpr (Core.Type body))
  = do { let index_route = not (null index_tvs)
             initial_subst = result_subst index_route tvs result
             needed_tvs | index_route = index_tvs
                        | otherwise = filter (isNothing . Subst.lookupTyVar initial_subst) tvs
       ; eligible <- if index_route
                     then return (Just (repeat True))
                     else if hasNoConcreteTyVars fun
                             && isJust (tcRetainedOrdinaryResult (count isHsValArg args) tau)
                          then do { shapes <- mapM tcRetainedInferableArg
                                                [arg | EValArg { ea_arg = arg } <- args]
                                  ; return (if any snd shapes then Just (map fst shapes)
                                            else Nothing) }
                          else return Nothing
       ; case eligible of
           Nothing -> return (fun, sigma, [], Nothing)
           Just inferable -> do
             { cache <- infer_args needed_tvs inferable domains 1 args
             ; actual_tys <- mapM (liftZonkM . zonkTcType . actual_type . snd) cache
             ; let templates = [ scaledThing (domains !! (pos-1)) | (pos, _) <- cache ]
                   matched = foldM (\subst (formal, actual) ->
                               tcMatchTyX_BM (bind_outer tvs) subst formal actual)
                             initial_subst (zip templates actual_tys)
             ; case matched of
                 Just subst
                   | Just inst_tys <- traverse (Subst.lookupTyVar subst) tvs
                   , index_route || any (hasNativeDependentExpr . Core.Type) inst_tys
                   , not nested || any (hasNativeDependentExpr . Core.Type) inst_tys
                   , and (zipWith (kind_agrees subst) tvs inst_tys)
                   , and (zipWith (\formal actual -> eqType (Subst.substTy subst formal) actual)
                                  templates actual_tys)
                   -> do { valid <- tcRetainedInstScope inst_tys
                         ; if valid
                           then if nested
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

    result_subst index_route tvs result
      | Check expected_ty <- expected
      , isTauTy expected_ty
      , not index_route || hasNativeDependentExpr (Core.Type expected_ty)
      , Just subst <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst result expected_ty
      = subst
      | otherwise = Subst.emptySubst

    actual_type (_, ty, _) = ty

    infer_args :: [TyVar] -> [Bool] -> [Scaled Type] -> Int -> [HsExprArg 'TcpRn]
               -> TcM RetainedArgCache
    infer_args _ _ _ _ [] = return []
    infer_args tvs (inferable : rest) (domain : domains) pos
               (EValArg { ea_arg = arg } : args)
      = do { entry <- if inferable && any (`elemVarSet` tyCoVarsOfType (scaledThing domain)) tvs
                      then addArgCtxt pos rn_head arg $
                           do { (usage, (typed, actual)) <- tcCollectingUsage (tcInferRho arg)
                              ; return [(pos, (typed, actual, usage))] }
                      else return []
           ; entries <- infer_args tvs rest domains (pos+1) args
           ; return (entry ++ entries) }
    infer_args tvs inferable domains pos (_ : args)
      = infer_args tvs inferable domains pos args
tcRetainedArgInstEnabled _ _ _ _ fun sigma = return (fun, sigma, [], Nothing)

-- | Check runtime identifiers against their current lexical or module bindings.
-- Normalize type and multiplicity metadata without changing identifier identity.
-- See Note [Native index bindings during typechecking].
tcRetainedInstScope :: [Type] -> TcM Bool
tcRetainedInstScope tys
  = do { local_env <- getLclTypeEnv
       ; global_env <- tcg_type_env <$> getGblEnv
       ; let current_binding v
               = case lookupNameEnv local_env (getName v) of
                   Just (ATcId { tct_id = local_id }) -> Just local_id
                   Just _ -> Nothing
                   Nothing -> case lookupTypeEnv global_env (getName v) of
                     Just (AnId module_id) -> Just module_id
                     _ -> Nothing
             valid_runtime_id v
               | not (isId v) = return True
               | Just binding <- current_binding v
               = do { occurrence <- liftZonkM (zonkId v)
                    ; binding <- liftZonkM (zonkId binding)
                    ; return (occurrence == binding
                              && isNativeIndexId occurrence && isNativeIndexId binding
                              && eqType (varType occurrence) (varType binding)
                              && isManyTy (idMult occurrence) && isManyTy (idMult binding)) }
               | otherwise = return False
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

Look in the local environment first, then the existing module type environment.
Do not load interfaces for a missing or stale name. Normalize both the index
occurrence and its authoritative binding before checking their identity,
closed native domain and Many multiplicity. Imported GlobalIds, unresolved or
rigid domains, linear indices and missing bindings still fail. The existing
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
                   -> [TyVar]
                   -> TcThetaType -> TcSigmaType
                   -> TcM (HsWrapper, TcSigmaType)
-- (instantiateSigmaQL orig tvs theta ty)
--     instantiates the type variables tvs, emits the (instantiated)
--     constraints theta, and returns the (instantiated) type ty
-- See Note [Instantiating type variables in QuickLook]
instantiateSigmaQL do_ql orig concs tvs theta body_ty
  = do { rec (subst, inst_tvs) <- mapAccumLM (new_meta subst) empty_subst tvs
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
  | Just (tvb, inner_ty) <- tcSplitForAllTyVarBinder_maybe fun_ty
  , isSpecifiedForAllTyFlag (binderFlag tvb)
  = do { tc_inst_forall_arg conc_tvs (tvb, inner_ty) hs_ty }

  | otherwise
  = do { (_, fun_ty) <- liftZonkM $ zonkTidyTcType emptyTidyEnv fun_ty
       ; failWith $ TcRnInvalidTypeApplication fun_ty hs_ty }

-- See Note [Visible type application and abstraction]
tcVDQ :: ConcreteTyVars              -- See Note [Representation-polymorphism checking built-ins]
      -> (ForAllTyBinder, TcType)    -- Function type
      -> LHsExpr GhcRn               -- Argument type
      -> TcM (TcType, TcType)
tcVDQ conc_tvs (tvb, inner_ty) arg
  = do { hs_wc_ty <- expr_to_type arg
       ; tc_inst_forall_arg conc_tvs (tvb, inner_ty) hs_wc_ty }

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
        unwrap_sig (L l (HsSig _ HsOuterExplicit{hso_bndrs=bndrs} body)) =
          return $ L l (HsForAllTy noExtField (HsForAllInvis noAnn bndrs) body)
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
        -> (ForAllTyBinder, TcType)    -- Function type
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
                   -> (ForAllTyBinder, TcType)  -- Function type
                   -> LHsWcType GhcRn           -- Argument type
                   -> TcM (TcType, TcType)
tc_inst_forall_arg conc_tvs (tvb, inner_ty) hs_ty
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

       ; let fun_ty    = mkForAllTy tvb inner_ty
             in_scope  = mkInScopeSet (tyCoVarsOfTypes [fun_ty, ty_arg])
             insted_ty = substTyWithInScope in_scope [tv] [ty_arg] inner_ty
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
      , Just (_, res_ty) <- tcSplitFunTy_maybe ty
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
       ; (fun_ue, mb_fun_ty) <- tcCollectingUsage $ tcInferAppHead_maybe rn_fun_arg
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
rejectNativeQuickLook :: TcType -> TcM ()
rejectNativeQuickLook ty
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; when enabled $
         do { ty <- liftZonkM $ zonkTcType ty
            ; when (hasNativeDependentExpr (Core.Type ty)) $
                failWithTc (TcRnRetainedQuantificationError (RetainedQuickLook ty)) } }

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
This source restriction does not add Quick Look support for retained types.
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
    go' (FunTy { ft_af = af1, ft_arg = arg1, ft_res = res1 })
       (FunTy { ft_af = af2, ft_arg = arg2, ft_res = res2 })
      | af1 == af2 -- Match the arrow TyCon
      = do { traceTc "go_fun" (ppr arg1 $$ ppr arg2)
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
