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
       ( tcApp, tcRuleAppPrefix, RuleAppPrefix(..)
       , tcExprPrag
       , matchPolytypeExistsWitness, matchExactPolytypeWitness, tcPreviewQuickLookResult
       , tcImplicitOperand, tcLexicalPackageOperand
       , tcRetainedInferableArg, tcRetainedArg, tcOptionalLetOperand ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcCheckMonoExpr, tcCheckPolyExpr, tcPolyExpr, tcInferRho )

import GHC.Hs

import GHC.Rename.Env ( addUsedGRE, DeprecationWarnings(NoDeprecationWarnings) )
import GHC.Rename.Utils ( mkExpandedLExpr, wrapGenSpan )
import GHC.Tc.Gen.Head
import GHC.Tc.Gen.Match ( tcMatches, tcBody, tcOpenImplicitPackage, tcOpenImplicitPackageAt
                        , implicitPayloadType, tcCloseRuntimeScope )
import GHC.Tc.Gen.SourceTable ( tcLookupSourceTableId, sourceTableOccurrence, recordSourceMethodUse, recordSourceLiteralUse )
import GHC.Tc.Gen.Sig ( isCompleteHsSig )
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr
import GHC.Tc.Utils.Monad
import GHC.Tc.Solver ( tcRewriteFamilyAxioms )
import GHC.Tc.Solver.Monad ( bumpReductionDepth, runTcSEqualities )
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Env ( isExistsPackName, tcLookupIdMaybe, tcLookupConLike
                        , tcLookup, tcLookupGlobal, tcLookupLcl_maybe, tcExtendIdEnv, tcExtendGeneratedIdEnv )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst_maybe )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Concrete  ( unifyConcrete, idConcreteTvs )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.TcRef ( TcRef )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint (WantedConstraints(wc_simple), ctPred, ctEvLoc, ctEvEvId,
                                mkNonCanonical, DelayedError(DE_SourceMethod))
import GHC.Tc.Types.ErrCtxt ( FunAppCtxtFunArg(..), UserTypeCtxt(ExprSigCtxt)
                           , ReportRedundantConstraints(NoRRC) )
import GHC.Tc.Types.CtLoc ( CtLoc )
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Zonk.TcType

import GHC.Core.Class ( Class, classTyVars )
import GHC.Core.ConLike ( ConLike(..) )
import GHC.Core.DataCon ( DataCon, dataConConcreteTyVars, dataConFullSig, dataConName, isNewDataCon, isVanillaDataCon, dataConOrigArgTys, dataConOrigResTy, dataConSourceArity, dataConHasRetainedResult, dataConRepArgBinders, dataConTyCon, dataConUserTyVars, dataConWorkId, dataConWrapId )
import qualified GHC.Core as Core
import GHC.Core.Dependent ( hasNativeDependentExpr, hasScopedDictionaryType
                          , isScopedSelectorReceiver, supportsQuickLookType )
import GHC.Core.FamInstEnv (FamInstEnvs, reduceTyFamApp_maybe)
import GHC.Core.Make ( mkCharExpr, mkNaturalExpr )
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Utils ( applyTypeToArgs, exprType, isVacuousRetainedBinder )
import GHC.Core.FVs ( exprFreeVarsList )
import GHC.Core.InstEnv ( lookupUniqueInstEnv, instanceDFunId )
import GHC.Core.Predicate ( classMethodInstTy )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Unify ( tcMatchTyX_BM, tcMatchRetainedTyX_BM, tcUnifyTys, BindFlag(..), BindTvFun )
import GHC.Core.Type
import GHC.Core.UsageEnv ( UsageEnv, popUE, singleUsageUE, zeroUE )
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom (coAxiomBranches, coAxBranchLHS, coAxBranchRHS, coAxBranchCoVars, fromBranches)

import GHC.Builtin.WiredIn.Types ( charTy, integerTy, multiplicityTy, runtimeRepTy, nilDataCon, consDataCon, mkListTy, unmatchableDataConTy )
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.KnownKeys

import GHC.Driver.DynFlags ( GeneralFlag(Opt_DeferTypeErrors), DynFlags(reductionDepth), HasDynFlags(getDynFlags) )
import GHC.Types.TyThing ( TyThing(AnId, AConLike, ATyCon) )
import GHC.Types.Basic ( RecFlag(NonRecursive), TypeOrKind(TypeLevel), IntWithInf, intGtLimit )
import GHC.Types.TypeEnv ( lookupTypeEnv )
import GHC.Types.Var
import GHC.Types.Id ( idName, idType, mkLocalId, isClassOpId_maybe, isDFunId, sourceTableId_maybe, sourceMethodTarget_maybe, sourceLiteralCons_maybe )
import GHC.Types.Var.Set ( IdSet, elemVarSet, emptyVarSet, extendVarSet, isEmptyVarSet, mkVarSet, sizeVarSet )
import GHC.Types.Var.FV
import GHC.Types.Name
import GHC.Unit.Types ( isInteractiveModule )
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Types.SourceText ( SourceText(NoSourceText) )
import GHC.Types.Var.Env  ( VarEnv, emptyTidyEnv, mkInScopeSet, delVarEnv, lookupVarEnv )

import GHC.Data.Maybe
import GHC.Data.Bag ( unitBag )
import GHC.Data.FastString
import Language.Haskell.Syntax.Text ( unpackHText )

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Data ( Data, cast, gmapQ )
import Data.Either ( isLeft, isRight )
import Data.Function
import Data.List ( mapAccumL )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
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
  = do { pack <- case rn_fun of
                   HsVar _ (L _ (WithUserRdr _ name)) -> isExistsPackName name
                   _ -> return False
       ; if pack then tcExistsPack rn_expr rn_args exp_res_ty
                 else tcOrdinaryApp rn_expr rn_fun rn_args exp_res_ty }

tcOrdinaryApp :: HsExpr GhcRn -> HsExpr GhcRn -> [HsExprArg 'TcpRn]
              -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryApp rn_expr rn_fun rn_args exp_res_ty
  = do { fun_lspan <- getFunSrcSpan rn_args
       ; let rn_head = (rn_fun, fun_lspan)
       ; (tc_fun0, fun_sigma0) <- tcInferAppHead rn_head
       ; let check_call fun sigma args expected =
               do { let check = tcOrdinaryApp' rn_expr rn_head
                  ; mixed <- tcOpenMixedBarArguments fun sigma args expected check
                  ; case mixed of
                      Just opened -> return opened
                      Nothing -> tcOpenImplicitArgs rn_expr (implicitProductSpine sigma args expected)
                                                   args expected (check fun sigma) }
             -- See Note [Type arguments before an implicit package head].
             (type_prefix, payload_args)
               | let (prefix, rest) = span prefix_arg rn_args
               , any type_arg prefix = (prefix, rest)
               | otherwise = ([], rn_args)
       ; case implicitSpineHead fun_sigma0 of
           ExistsTy ImplicitExists _ body
             | all ordinary_arg payload_args
             , opensImplicitHead (count isHsValArg payload_args) body
             -> do { (prefix, schema) <- tcInstFun NoQL True rn_head tc_fun0 fun_sigma0 type_prefix
                   ; prefix' <- tcValArgsWithCache [] NoQL rn_head prefix
                   ; package_loc <- getFunSrcSpan payload_args
                   ; let package = L (noAnnSrcSpan package_loc) (rebuildHsApps tc_fun0 prefix')
                   ; result <- expTypeToType exp_res_ty
                   ; tcOpenImplicitPackage package_loc schema package result $ \payload ->
                       check_call (mkHsVar (noLocA payload)) (varType payload)
                                  payload_args (mkCheckExpType result) }
           _ -> check_call tc_fun0 fun_sigma0 rn_args exp_res_ty }
  where
    prefix_arg EWrap{} = True
    prefix_arg arg = type_arg arg

    type_arg ETypeArg{} = True
    type_arg _ = False

    ordinary_arg EValArg{} = True
    ordinary_arg EWrap{} = True
    ordinary_arg _ = False

-- | Open dot operands for a consumer of their explicit packages.
-- See Note [Implicit opening for an explicit package argument].
tcOpenMixedBarArguments
  :: HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn] -> ExpRhoType
  -> (HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn] -> ExpRhoType -> TcM (HsExpr GhcTc))
  -> TcM (Maybe (HsExpr GhcTc))
tcOpenMixedBarArguments fun sigma args expected check_app
  | hasNoConcreteTyVars fun
  = runMaybeT $ do
      (binders, rho, spine) <- MaybeT (tcFindMixedBarArguments sigma args)
      liftMaybeT $ do
        result <- expTypeToType expected
        open binders rho result [] [] spine
  | otherwise = return Nothing
  where
    open binders rho result rev_args opened [] =
      let (fun', ty) = case matchBarPackageWitnesses binders (reverse opened) of
            Just (witnesses, _) ->
              (mkHsWrap (mkWpTyApps witnesses) fun, substTyWith binders witnesses rho)
            Nothing -> (fun, sigma)
      in check_app fun' ty (reverse rev_args) (mkCheckExpType result)
    open binders rho result rev_args opened (Left arg : rest) =
      open binders rho result (arg : rev_args) opened rest
    open binders rho result rev_args opened (Right (domain, loc, operand, wanted, _) : rest) =
      do { (checked, schema) <- tcScalingUsage ManyTy $
               case wanted of
                 Just schema -> (, schema) <$> tcCheckMonoExpr operand schema
                 Nothing -> do { (checked, actual) <- tcInferRho operand
                               ; schema <- liftZonkM (zonkTcType actual)
                               ; return (checked, schema) }
         ; tcOpenImplicitPackage (getLocA operand) schema checked result $ \payload ->
             let ref = fmap (\e -> mkExpandedLExpr e (nlHsVar (getName payload))) operand
             in tcExtendIdEnv [payload] $
                  open binders rho result (EValArg loc noExtField ref : rev_args)
                       ((domain, varType payload) : opened) rest }

-- | Classify the mixed operands before checking any application arguments.
-- See Note [Implicit opening for an explicit package argument].
tcFindMixedBarArguments
  :: TcSigmaType -> [HsExprArg 'TcpRn] -> TcM (Maybe ([TyVar], TcRhoType, [MixedBarArg]))
tcFindMixedBarArguments sigma args
  | (tvs@(_:_), rho) <- tcSplitForAllInvisTyVars sigma
  , let binders = map snd tvs
  , all isTyVar binders
  = runMaybeT $ do
      spine <- mixed_spine rho args
      let packages = [(domain, payload) | Right (domain, _, _, _, payload) <- spine]
          templates = mapMaybe (barPackageSignature . fst) packages
      guard (all (\binder -> any (isInjectiveInType binder) templates) binders)
      _ <- MaybeT (pure (matchBarPackageWitnesses binders packages))
      return (binders, rho, spine)
  | otherwise = return Nothing
  where
    mixed_spine :: TcRhoType -> [HsExprArg 'TcpRn] -> MaybeT TcM [MixedBarArg]
    mixed_spine _ [] = return []
    mixed_spine ty (arg@(EWrap EPar{}) : rest) =
      (Left arg :) <$> mixed_spine ty rest
    mixed_spine ty (arg@EValArg{ea_loc_span = loc, ea_arg = operand} : rest)
      | FunTy{ft_af = af, ft_mult = mult, ft_arg = domain, ft_res = res} <- coreFullView ty
      , isVisibleFunArg af
      = do
          package <- liftMaybeT $
            if isManyTy mult && isJust (barPackageSignature domain)
              then tcPackageOperandResult False operand
              else return Nothing
          let slot = case package of
                Just (declared_ty, check_schema)
                  | schema@(ExistsTy ImplicitExists _ inner) <- coreFullView declared_ty
                  -> Right (domain, loc, operand, if check_schema then Just schema else Nothing,
                            implicitPayloadType inner)
                _ -> Left arg
          (slot :) <$> mixed_spine res rest
    mixed_spine _ _ = mzero

-- | An ordinary argument or a mixed-package argument with its bar domain,
-- source operand, optional checked schema and declared payload type.
type MixedBarArg = Either (HsExprArg 'TcpRn)
                          (TcType, HsExprLoc, LHsExpr GhcRn, Maybe TcType, TcType)

{- Note [Implicit opening for an explicit package argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For inspectInner (forward (make flag)), the formal domain is
exists b | (a, b) under forall a. If make returns a mixed package and
forward passes its argument's type to its result, the opened bar type
can determine the erased type application. Read these declarations
before creating inference variables. The ordinary unifier treats the
bar type as opaque.

Follow a leading invisible result parameter through written value
arguments whose complete domain is that parameter. For example,
forall a b. Show b => b -> a -> a identifies the second argument.
This probe selects the inference path. The ordinary expression
checker still instantiates the head and checks all constraints.
A family application or an unconstrained result parameter cannot
identify a package through this probe.

For select :: Bool -> a -> a -> a, both alternatives must supply
the same closed dot schema. Comparing declared results with free
variables could mistake a binder stripped from a producer signature
for a type already in scope in the caller. Agreement on an open schema
would also need evidence that each free variable is rigid in the
caller and independent of the producer's omitted arguments.

Use the common schema as the expected type of the complete selector
call. Its ordinary checker checks every operand and all uses at that
instantiation. Open only the checked result. The alternatives can
hide different witnesses. The selector determines which value is
returned and how much of each operand it evaluates.

A lexical tuple binding can use the schema from one forwarding slot as
its expected RHS type. See Note [Scope of a forwarded package schema].
Allow leading type arguments for specified multiplicity binders. These
arguments leave the forwarded type parameter for ordinary inference.
The original RHS still checks the written type arguments and usage.

An independent higher-rank callback does not obscure the result
parameter. For relay :: (forall x. x -> x) -> a -> a, the second
argument supplies a. Accept a higher-rank sibling with a leading
invisible telescope and an ordinary monomorphic body. Its type cannot
mention the forwarded parameter, including in a kind or a constraint.
Check the complete relay call before opening the resulting package.
The ordinary checker checks the callback against its higher-rank
domain, emits the relay's constraints, and accounts for argument use.

For readBoth :: forall a b. Inner a b -> Int and an operand with type
exists a. Inner a Char, the opened bar type determines both a and b.
Require an injective occurrence of each parameter in the bar binder's
kind or body. Match all parameters together. Check each selected type's
kind under the same substitution, including earlier parameters in the
telescope. Then compare the complete bar type. A selected type can refer
to the opened dot witness, but it cannot capture the bar binder.

In choose :: forall a b. Bool -> Inner a -> Inner b -> Int, distinct
operands can fix a and b. Match all declared bar domains against the
corresponding dot payloads before opening the operands. Keep the ordinary
arguments in the spine. Each selected domain has multiplicity Many, and
every head binder needs an injective occurrence across the selected domains.

Check each selected operand once. Give the checked expressions to the
existing lazy unpacks. Recompute the joint match under the openings, so
each operand gives a separate fresh witness. A declaration shared by the
operands cannot justify equating their witnesses. Keep the result type
outside every opening. Check the original application in these scopes.
The callee still controls the explicit unpacking.

Proposal #473's IExist rule opens a typed expression. Its alternative Core
form uses a lazy unpack to keep the witness in scope. The ordinary source
operand remains in the diagnostic context.

Use the same read-only preflight to defer Quick Look when it would skip
the opening. See Note [Quick Look and ordinary argument checking].
-}

tcOrdinaryApp' :: HsExpr GhcRn -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
               -> TcSigmaType -> [HsExprArg 'TcpRn] -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryApp' = tcOrdinaryAppWithPrefix []

-- | Suspend the check at each written type argument until the preceding
-- rule variables are in scope. Reuse the checked arguments at the next step.
-- See Note [Dependent RULE binder domains] in GHC.Tc.Gen.Sig.
data RuleAppPrefix
  = CheckRulePrefix (TcM (TcSigmaType, RuleAppPrefix))
  | FinishRuleApp (TcM (LHsExpr GhcTc, TcRhoType))

-- | Infer the head of the retained RULE application.
tcRuleAppPrefix :: LHsExpr GhcRn -> TcM (TcSigmaType, RuleAppPrefix)
tcRuleAppPrefix (L loc expr) = context $
  do { (fun, args) <- splitHsApps expr
     ; fun_span <- getFunSrcSpan args
     ; let head_ctxt = (fun, fun_span)
     ; (checked_fun, sigma) <- tcInferAppHead head_ctxt
     ; let check prefix residual remaining = case break looks_like_type_arg remaining of
             (values, ty_arg : rest) -> CheckRulePrefix $ context $
               do { (checked, sigma') <- tcInstFunWithPrefix quickLookArg True prefix Nothing Nothing Nothing
                                          NoQL False head_ctxt checked_fun residual (values ++ [ty_arg])
                  ; return (sigma', check checked sigma' rest) }
             _ -> FinishRuleApp $ context $
               do { (lhs, rho) <- runInferRho $ \expected ->
                      tcOpenImplicitArgs expr residual remaining expected $
                        tcOrdinaryAppWithPrefix prefix expr head_ctxt checked_fun residual
                  ; return (L loc lhs, rho) }
     ; return (sigma, check [] sigma args) }
  where
    context :: TcM a -> TcM a
    context action = setSrcSpanA loc $ addExprCtxt expr action

tcOrdinaryAppWithPrefix :: [HsExprArg 'TcpInst] -> HsExpr GhcRn -> (HsExpr GhcRn, SrcSpan)
                        -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn]
                        -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryAppWithPrefix checked_prefix rn_expr rn_head tc_fun fun_sigma rn_args expected
  = do { (args, fields) <- if null checked_prefix
                          then tcRetainedSpineFields rn_expr fun_sigma rn_args
                          else return (rn_args, [])
       ; let check names = tcOrdinaryAppScoped names checked_prefix rn_expr rn_head
                                              tc_fun fun_sigma args expected
       ; if null fields then check [] else tcWithSpineFields fields check }

-- | Check each field once and share it with the retained application below.
tcWithSpineFields :: [(Id, LHsExpr GhcRn)] -> ([Name] -> TcM (HsExpr GhcTc))
                  -> TcM (HsExpr GhcTc)
tcWithSpineFields fields check
  = do { rhss <- forM fields $ \(field, rhs) ->
           tcScalingUsage ManyTy (tcCheckPolyExpr rhs (idType field))
       ; let ids = map fst fields
       ; (usage, result) <- tcCollectingUsage $
           foldr tcExtendGeneratedIdEnv (check (map getName ids)) ids
       ; tcEmitBindingUsage (foldl' (\ue field -> snd (popUE ue field)) usage ids)
       ; let binds = HsValBinds noAnn (XValBindsLR (HsVBG
                       [(NonRecursive, [mk_bind field rhs]) | (field, rhs) <- zip ids rhss] []))
       ; return (HsLet noExtField (noLocA binds) (noLocA result)) }
  where
    mk_bind :: Id -> LHsExpr GhcTc -> LHsBind GhcTc
    mk_bind field rhs = noLocA PatBind
      { pat_ext = XPatBindTc (idType field) [] [] ManyTy
      , pat_lhs = noLocA (LazyPat noExtField (noLocA (VarPat noExtField (noLocA field))))
      , pat_mods = []
      , pat_rhs = unguardedGRHSs (getLocA rhs) rhs noAnn }

-- | Select plain list fields when the remainder of the contract forgets all fields.
-- See Note [Opaque operands for a retained list spine].
tcRetainedSpineFields :: HsExpr GhcRn -> Type -> [HsExprArg 'TcpRn]
                     -> TcM ([HsExprArg 'TcpRn], [(Id, LHsExpr GhcRn)])
tcRetainedSpineFields expr sigma args
  | not (any needs_fields args) = return (args, [])
  | (_, _, tau) <- tcRetainedInstTelescope sigma
  , DepFunTy DepFunVisible _ _ <- coreFullView tau
  = do { let (prefix, operands) = span spine_prefix args
       ; plan <- tryTcDiscardingErrs (return Nothing) $
           runMaybeT (scan tau operands [])
       ; case plan of
           Nothing -> return (args, [])
           Just count -> do
             { changed <- traverse prepare (take count operands)
             ; return (prefix ++ map fst changed ++ drop count operands, concatMap snd changed) } }
  | otherwise = return (args, [])
  where
    spine_prefix ETypeArg{} = True
    spine_prefix (EWrap EPar{}) = True
    spine_prefix _ = False

    needs_fields = maybe False (any opaque . snd) . list_fields
    list_fields EValArg{ea_arg = arg}
      | L loc (ExplicitList _ elements) <- stripParensLHsExpr arg = Just (loc, elements)
    list_fields _ = Nothing

    opaque element = case unLoc (stripParensLHsExpr element) of
      HsVar{} -> False
      HsLit _ HsChar{} -> False
      HsLit _ HsString{} -> False
      _ -> True

    scan ty (EWrap EPar{} : rest) fields
      = (1 +) <$> scan ty rest fields
    scan ty (arg : rest) fields
      | DepFunTy DepFunVisible binder _ <- coreFullView ty
      , Just (_, elements) <- list_fields arg
      , Just [element_ty] <- nativeIndexDataConTyArgs (idType binder) nilDataCon
      = do { ids <- liftMaybeT $ mapM (const (newSysLocalId (fsLit "element") ManyTy element_ty)) elements
           ; let nil = Core.mkTyApps (Core.Var (dataConWorkId nilDataCon)) [element_ty]
                 cons = Core.mkTyApps (Core.Var (dataConWorkId consDataCon)) [element_ty]
                 operand = foldr (\field tail -> Core.mkApps cons [Core.Var field, tail]) nil ids
           ; guard (isJust (Core.retainedArgIndex operand))
           ; next <- scan (applyTypeToArgs ty [operand]) rest (ids ++ fields)
           ; return (1 + next) }
    scan ty _ fields
      = do { guard (not (null fields))
           ; _ <- liftMaybeT (tcCloseRuntimeScope (map getName fields) (exprCtOrigin expr) ty)
           ; return 0 }

    prepare arg
      | Just (loc, elements) <- list_fields arg
      , any opaque elements
      = do { element_ty <- newFlexiTyVarTy liftedTypeKind
           ; (elements', fields) <- mapAndUnzipM (name_field element_ty) elements
           ; let arg' = arg { ea_arg = L loc (ExplicitList noExtField elements') }
           ; return (arg', concat fields) }
      | otherwise = return (arg, [])

    name_field ty rhs
      | opaque rhs
      = do { field <- newSysLocalId (fsLit "element") ManyTy ty
           ; let reference = mkExpandedLExpr (unLoc rhs) (nlHsVar (idName field))
           ; return (L (getLoc rhs) reference, [(field, rhs)]) }
      | otherwise = return (rhs, [])

{- Note [Opaque operands for a retained list spine]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A list's checked length can depend on its constructors without depending on
its values. Check the whole remaining telescope once with independent opaque
fields before changing an argument. Checked family reduction must remove
these fields from the entire telescope, including its constraints.
A result or constraint that compares contents keeps the original checking
rule. A successful projection does not establish any new field equality.

Ignore leading written type arguments and parentheses in this scan. A
conversion valid before substituting the quantified type variables is valid
after substitution. Keep the written arguments and parentheses in the
application. The usual type application rules still check the type arguments
and determine the actual field types. A checked RULE prefix can already
contain retained value operands. This rewrite applies to the source spine
before checking such operands.

Check each new RHS once as a monomorphic ordinary expression. A lazy binding
owns the fresh Id used by both the retained index and the runtime list. Bind
the application evidence underneath those Ids. Normalize its actual outward
type under the same scope and use the nominal coercion there. See
Note [Closing an inferred runtime index scope] in GHC.Tc.Gen.Match.
-}

tcOrdinaryAppScoped :: [Name] -> [HsExprArg 'TcpInst] -> HsExpr GhcRn -> (HsExpr GhcRn, SrcSpan)
                    -> HsExpr GhcTc -> TcSigmaType -> [HsExprArg 'TcpRn]
                    -> ExpRhoType -> TcM (HsExpr GhcTc)
tcOrdinaryAppScoped fields checked_prefix rn_expr rn_head@(rn_fun, fun_lspan) tc_fun0 fun_sigma0 rn_args written_res_ty
  -- See Note [Constructors in a nested dependent sum] in GHC.Tc.Utils.Unify.
  = do { wanted_ql <- wantQuickLook rn_fun
       ; exp_res_ty <- case (tc_fun0, written_res_ty) of
           (XExpr (ConLikeTc (RealDataCon dc)), Check ty)
             | not (null (dataConRepArgBinders dc))
             , let tc = dataConTyCon dc
             , Just (_, params) <- matchSumPayload tc ty
             -> return (mkCheckExpType (mkTyConApp tc params))
           (_, Check ty) | DoQL <- wanted_ql
             -> do { result <- setSrcSpan fun_lspan $
                              tcKnownRetainedConstructorResult rn_args tc_fun0 fun_sigma0 ty
                   ; return (maybe written_res_ty mkCheckExpType result) }
           _ -> return written_res_ty
       ; traceTc "tcApp {" $
           vcat [ text "rn_fun:" <+> ppr rn_fun
                , text "fun_lspan:" <+> ppr fun_lspan
                , text "rn_args:" <+> ppr rn_args ]


       -- See Note [Imported native application contracts].
       ; native_app <- tcRetainedAppCapability fun_sigma0 exp_res_ty rn_args
       -- See Note [Known native variables with ImpredicativeTypes].
       ; head_ql <- if not (null fields) || any isExplicitRetainedArg rn_args
                        || (native_app && hasScopedDictionaryPrefix fun_sigma0)
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
            ; let instantiate =
                    tcInstFunWithPrefix quickLookArg False (checked_prefix ++ prefix) vta_cache expected_rho inst_plan
                      do_ql inst_final rn_head tc_fun fun_sigma remaining_args
            ; ((inst_args, app_res_rho), emit_wanteds) <- case do_ql of
                NoQL -> (, return ()) <$> instantiate
                DoQL -> do { (instantiated, wanted) <- captureConstraints instantiate
                           ; return (instantiated, emitConstraints wanted) }
            ; extra_cache <- case vta_cache of
                Nothing -> return []
                Just ref -> fromMaybe [] <$> readTcRef ref
            ; let prepare = do
                    emit_wanteds
                    -- See Note [QuickLook: arguments before result].
                    case (do_ql, exp_res_ty) of
                      (DoQL, Check exp_rho) -> quickLookResultType app_res_rho exp_rho
                      _                    -> return ()
            ; finishAppWithScopedCache fields (arg_cache ++ extra_cache) prepare do_ql rn_expr fun_lspan
                source_fun tc_fun inst_args app_res_rho written_res_ty } }
  where
    -- See Note [Type arguments before an implicit package head].
    source_fun
      | HsVar _ name <- rn_fun
      , isSystemName (unLocWithUserRdr name)
      , EWrap (EExpand _ (ExprCtxt original@HsAppType{})) : _ <- rn_args
      = original
      | otherwise = rn_fun

-- | Use expected product fields to decide which operands request opening.
-- See Note [Implicit opening for a product payload].
implicitProductSpine :: TcSigmaType -> [HsExprArg 'TcpRn] -> ExpRhoType -> TcSigmaType
implicitProductSpine sigma args expected
  | Check schema <- expected
  , ExistsTy ImplicitExists _ payload <- coreFullView schema
  , Just (tc, fields) <- tcSplitTyConApp_maybe (implicitPayloadType payload)
  , Just dc <- tyConSingleDataCon_maybe tc
  , isVanillaDataCon dc, not (isNewDataCon dc)
  , Just (_, components) <- tcSplitTyConApp_maybe (dataConOrigResTy dc)
  , eqTypes (map scaledThing (dataConOrigArgTys dc)) components
  , (binders, body) <- tcSplitForAllInvisTyVars sigma
  , let (_, domains, result) = splitFunTys body
  , count isHsValArg args == length domains
  , Just (result_tc, ts) <- tcSplitTyConApp_maybe result
  , result_tc == tc
  , eqTypes (map scaledThing domains) ts
  , Just vars <- traverse getTyVar_maybe ts
  , all (`elem` map snd binders) vars
  , sizeVarSet (mkVarSet vars) == length vars
  , vars `equalLength` fields
  = substTyWith vars fields body
  | otherwise = sigma

{- Note [Implicit opening for a product payload]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For (filterVec p xs, first) checked at
  exists n. (Vec a n, Vec a n -> Maybe a)
the constructor's declared arguments are just type variables. The first
field of the expected product requests a vector payload. Read those
fields before checking the application and use their shapes to select
openings.

The same rule applies to Product x y or makeProduct x y at
  exists n. Product (Vec a n) (Vec a n -> Maybe a)
when the callable declares a -> b -> Product a b. Require a plain
data product with one constructor and one field for each parameter.
The declared argument types must be the distinct parameters of the
nominal result in field order. A partial application does not select
this rule.

The substitution supplies a shape only to the read-only argument
classifier. The checker uses the original function type and argument
multiplicities. It instantiates the output witness inside the openings.
Independent openings introduce distinct skolems, so two filtered vectors
cannot stand for fields at the same length. A field whose declared type
is itself a package still forwards that package. Each opening keeps
the original argument lazy.
-}

-- | Scope implicit existential value arguments over the rest of the application.
-- See Note [Implicit existential arguments].
tcOpenImplicitArgs :: HsExpr GhcRn -> TcSigmaType -> [HsExprArg 'TcpRn] -> ExpRhoType
                   -> ([HsExprArg 'TcpRn] -> ExpRhoType -> TcM (HsExpr GhcTc))
                   -> TcM (HsExpr GhcTc)
tcOpenImplicitArgs rn_expr fun_ty args expected check_app
  = do { marked <- tcImplicitAppArgs fun_ty args
       ; if all isLeft marked
         then check_app args expected
         else check_opened marked }
  where
    check_opened marked
      | Infer ir <- expected
      = do { level <- getTcLevel
           ; template <- if level `sameDepthAs` ir_lvl ir
                          then declared_result marked
                          else return Nothing
           ; case template of
               Just result ->
                 do { let free = tyCoVarsOfType result
                          result_vars = [v | (_, v) <- head_binders, v `elemVarSet` free]
                    ; (subst, _) <- newMetaTyVarsX (Subst.mkEmptySubst (mkInScopeSet free))
                                                  result_vars
                    ; let schema = Subst.substTy subst result
                    ; body <- open schema Map.empty [] marked
                    ; tcWrapResult rn_expr body schema expected }
               Nothing -> check_mono marked }
      | otherwise = check_mono marked

    check_mono marked = do
      result_ty <- expTypeToType expected
      open result_ty Map.empty [] marked

    (head_binders, head_sigma) = tcSplitForAllInvisTyVars fun_ty
    (head_theta, head_body) = tcSplitPhiTy head_sigma

    declared_result marked
      | Just result <- spine_result head_body args = return (Just result)
      | otherwise = ranked_result marked

    spine_result result [] = package_result result
    spine_result ty (EWrap{} : rest) = spine_result ty rest
    spine_result ty (EValArg{} : rest)
      | Just (_, res) <- splitVisibleFunTy_maybe ty = spine_result res rest
    spine_result _ _ = Nothing

    package_result result
      | ExistsTy ImplicitExists _ _ <- coreFullView result
      , isFamFreeTy result
      , ordinary_package result
      = Just result
      | otherwise = Nothing

    ordinary_package ty
      | ExistsTy ImplicitExists binder body <- coreFullView ty
      = ordinary_type (varType binder) && ordinary_package body
      | otherwise = ordinary_type ty

    ordinary_type ty = isTauTy ty && not (hasNativeDependentExpr (Core.Type ty))

    -- See Note [Inferring an erased package from a higher-rank callback].
    ranked_result marked = runMaybeT $ do
      guard =<< liftMaybeT (xoptM LangExt.ImpredicativeTypes)
      Left EValArg{ea_arg = callback} : opened <-
        pure [arg | arg <- marked, is_value arg]
      guard (not (null opened) && all directly_opened opened)
      let (mas, domains, result) = splitFunTys head_body
      Scaled cb_use cb : payloads <- pure domains
      guard (opened `equalLength` payloads)
      TyVarTy result_var <- pure (coreFullView result)
      guard (result_var `elem` map snd head_binders)
      let (cb_binders, cb_body) = tcSplitForAllInvisTyVars cb
      guard (not (null cb_binders))
      let (cb_matches, formals, cb_result) = tcSplitFunTys cb_body
      guard (formals `equalLength` payloads && eqType cb_result result)
      let other = cb_use : mas ++ head_theta ++ cb_matches ++ map fst cb_binders
                    ++ concatMap (\(Scaled mult ty) -> [mult, ty]) (payloads ++ formals)
                    ++ map (varType . snd) (head_binders ++ cb_binders)
      guard (not (result_var `elemVarSet` tyCoVarsOfTypes other))
      (ident, operands) <- MaybeT (tcDeclaredOperandSpine False callback)
      (provided, residual) <- MaybeT . pure $ declared_residual (varType ident) operands
      let (_, actuals, callback_result) = tcSplitFunTys residual
          actual_types = map scaledThing actuals
          formal_types = map scaledThing formals
          inputs = formal_types ++ actual_types
          input_vars = provided ++ map snd head_binders
          bind_input tv _ | tv `elem` input_vars = BindMe
                          | otherwise = DontBindMe
      guard (actuals `equalLength` formals)
      guard (all isFamFreeTy inputs && all supportsQuickLookType inputs)
      guard (all noQuickLookVars (callback_result : inputs))
      subst <- MaybeT (pure (tcUnifyTys bind_input actual_types formal_types))
      guard (and (zipWith eqType (Subst.substTys subst actual_types)
                                 (Subst.substTys subst formal_types)))
      let template = Subst.substTy subst callback_result
          free = tyCoVarsOfType template
      guard (all (not . (`elemVarSet` free)) (provided ++ map snd cb_binders))
      MaybeT (pure (package_result template))
      where
        is_value (Left EValArg{}) = True
        is_value Right{} = True
        is_value _ = False
        directly_opened (Right (_, _, _, Nothing, _)) = True
        directly_opened _ = False

        -- IALL can instantiate another group after each supplied value.
        declared_residual sigma args = case args of
          [] -> Just (tvs, body)
          _ : rest -> do
            (_, result) <- splitVisibleFunTy_maybe body
            (later_tvs, residual) <- declared_residual result rest
            return (tvs ++ later_tvs, residual)
          where
            (tvs, _, body) = tcRetainedInstTelescope sigma

    open result_ty opened rev_args [] = do
      let args = reverse rev_args
      args' <- if Map.null opened then return args else mapM (reuse_opening opened) args
      check_app args' (mkCheckExpType result_ty)
    open result_ty opened rev_args (Left arg : rest) =
      open result_ty opened (arg : rev_args) rest
    open result_ty opened rev_args (Right (loc, mult, arg, rebuild, known_schema) : rest) =
      do { (checked, actual) <- tcScalingUsage mult $
               case known_schema of
                 Just schema -> (, schema) <$> tcCheckMonoExpr arg schema
                 Nothing -> tcInferRho arg
         ; schema <- liftZonkM (zonkTcType actual)
         ; key <- if isManyTy mult && isNothing rebuild
                  then openingKey arg checked
                  else return Nothing
         ; let shared = do
                 identity <- key
                 (previous, payload) <- Map.lookup identity opened
                 guard (eqType previous schema)
                 return payload
               continue env payload =
                 let payload_ref = wrapGenSpan (mkHsVar (wrapGenSpan (getName payload)))
                     ref = EValArg loc noExtField $
                       fromMaybe id rebuild (fmap (\e -> mkExpandedLExpr e payload_ref) arg)
                 in open result_ty env (ref : rev_args) rest
         ; case shared of
             Just payload -> continue opened payload
             Nothing -> tcOpenImplicitPackage (getLocA arg) schema checked result_ty $ \payload ->
               let env = case key of
                     Just identity -> Map.insert identity (schema, payload) opened
                     Nothing -> opened
               in tcExtendGeneratedIdEnv payload $ continue env payload }

    openingKey :: LHsExpr GhcRn -> LHsExpr GhcTc -> TcM (Maybe (Name, Maybe Name))
    openingKey original checked
      | HsVar _ (L _ ident) <- bare_expr checked
      , ExistsTy ImplicitExists _ _ <- coreFullView (varType ident)
      = return ((\name -> (name, Nothing)) <$> lexicalName ident)
      | HsApp _ fun operand <- bare_expr checked
      , HsVar _ (L _ producer) <- bare_expr fun
      , HsVar _ (L _ input) <- bare_expr operand
      , let producer_name = getName producer
      , isExternalName producer_name
      , Just input_name <- lexicalName input
      , FunTy{ft_af = af, ft_mult = mult, ft_arg = domain, ft_res = result}
          <- coreFullView (varType producer)
      , isVisibleFunArg af, isManyTy mult
      , isTauTy domain
      , ExistsTy ImplicitExists _ _ <- coreFullView result
      , let key = (producer_name, Just input_name)
      , renamedKey original == Just key
      = do { input_type <- liftZonkM (zonkTcType (varType input))
           ; return (if eqType domain input_type then Just key else Nothing) }
      | otherwise = return Nothing

    lexicalName :: Id -> Maybe Name
    lexicalName ident
      | let name = getName ident
      , isLocalId ident, not (isExternalName name) = Just name
      | otherwise = Nothing

    renamedKey :: LHsExpr GhcRn -> Maybe (Name, Maybe Name)
    renamedKey expr = case bare_expr expr of
      HsVar _ name -> Just (unLocWithUserRdr name, Nothing)
      HsApp _ fun arg
        | HsVar _ producer <- bare_expr fun
        , HsVar _ input <- bare_expr arg
        -> Just (unLocWithUserRdr producer, Just (unLocWithUserRdr input))
      _ -> Nothing

    bare_expr :: LHsExpr (GhcPass p) -> HsExpr (GhcPass p)
    bare_expr = unLoc . stripParensLHsExpr

    reuse_opening :: Map.Map (Name, Maybe Name) (TcType, Id)
                  -> HsExprArg 'TcpRn -> TcM (HsExprArg 'TcpRn)
    reuse_opening opened arg@EValArg{ea_arg = expr} =
      do { expr' <- project expr
         ; return arg{ea_arg = expr'} }
      where
        project (L loc (HsPar ann nested)) =
          L loc . HsPar ann <$> project nested
        project original@(L loc (HsApp ann fun operand))
          | Just key <- renamedKey operand
          , Just (_, payload) <- Map.lookup key opened
          , HsVar _ head_name <- bare_expr fun
          , let name = unLocWithUserRdr head_name
          , not (isDataOcc (nameOccName name))
          = do { ident <- tcLookupIdMaybe name
               ; return $ case implicitSpineHead . varType <$> ident of
                   Just FunTy{ft_af = af, ft_mult = mult, ft_arg = domain}
                     | isVisibleFunArg af, isManyTy mult, demands_payload (coreFullView domain)
                     -> let ref = mkExpandedLExpr (unLoc operand) (nlHsVar (getName payload))
                        in L loc (HsApp ann fun (L (getLoc operand) ref))
                   _ -> original }
        project original = return original

    reuse_opening _ arg = return arg


-- | Find operands that need an implicit package opening before checking a call.
-- See Note [Implicit existential arguments].
-- See Note [Quick Look and ordinary argument checking].
tcImplicitAppArgs :: TcType -> [HsExprArg 'TcpRn]
                  -> TcM [Either (HsExprArg 'TcpRn)
                                 (HsExprLoc, Mult, LHsExpr GhcRn,
                                  Maybe (LHsExpr GhcRn -> LHsExpr GhcRn), Maybe TcType)]
tcImplicitAppArgs ty (wrapper@EWrap{} : rest) = (Left wrapper :) <$> tcImplicitAppArgs ty rest
tcImplicitAppArgs ty (ordinary@EValArg{ea_loc_span = loc, ea_arg = arg} : rest)
  | FunTy { ft_af = af, ft_mult = mult, ft_arg = domain, ft_res = result }
      <- implicitSpineHead ty
  , isVisibleFunArg af
  = do { implicit <- if (isManyTy mult || isOneTy mult)
                        && demands_payload (coreFullView domain)
                     then classify mult domain
                     else return Nothing
       ; let operand = maybe (Left ordinary) Right implicit
       ; (operand :) <$> tcImplicitAppArgs result rest }
  where
    classify mult domain
      | isGuardedTy domain
      , Just branches <- resultBranches arg
      = do { declared <- tcPackageOperandResult False arg
           ; case declared of
               Just (schema, _) -> return (Just (loc, mult, arg, Nothing, Just schema))
               Nothing -> do { known <- allM declared_package (NE.toList branches)
                             ; return (if known then Just (loc, mult, arg, Nothing, Nothing)
                                                else Nothing) } }
      | otherwise
      = fmap (fmap (\(package, replace) -> (loc, mult, package, replace, Nothing)))
             (tcImplicitOperand arg)

    declared_package expr
      | Just branches <- resultBranches expr = allM declared_package (NE.toList branches)
      | otherwise = do { result <- tcDeclaredOperandResult expr
                       ; return $ case result of
                           Just (ExistsTy ImplicitExists _ _) -> True
                           _ -> False }
tcImplicitAppArgs _ rest = return (map Left rest)

-- | Check if a declared argument domain requests the payload of a package.
demands_payload :: TcType -> Bool
demands_payload TyConApp{} = True
demands_payload AppTy{} = True
demands_payload FunTy{} = True
demands_payload _ = False

-- | Read the declaration for an implicit package or a call with a package head.
-- The replacement function rebuilds a call on a checked payload.
-- See Note [Type arguments before an implicit package head].
tcImplicitOperand :: LHsExpr GhcRn
                  -> TcM (Maybe (LHsExpr GhcRn, Maybe (LHsExpr GhcRn -> LHsExpr GhcRn)))
tcImplicitOperand arg = find_head [] id arg
  where
    find_head operands rebuild (L loc (HsApp x fun operand)) =
      find_head (operand : operands) (\f -> rebuild (L loc (HsApp x f operand))) fun
    find_head operands rebuild (L loc (HsPar x fun)) =
      find_head operands (\f -> rebuild (L loc (HsPar x f))) fun
    find_head [] _ (L _ (ExprWithTySig _ _ sig))
      = do { implicit <- tcImplicitOperandSignature sig
           ; return (if implicit then Just (arg, Nothing) else Nothing) }
    find_head operands rebuild head_expr
      | (base, written) <- package_head [] head_expr
      , HsVar _ name <- unLoc base
      , let id_name = unLocWithUserRdr name
      , not (isDataOcc (nameOccName id_name))
      = do { ident <- tcLookupIdMaybe id_name
           ; let n = length operands
           ; case implicitSpineHead . varType <$> ident of
               Just (ExistsTy ImplicitExists _ body)
                 | opensImplicitHead n body -> return (Just (head_expr, Just rebuild))
               Just ty | HsVar{} <- unLoc head_expr
                       , Just result <- implicitSpineResult n ty
                       , ExistsTy ImplicitExists _ _ <- result -> return (Just (arg, Nothing))
               _ | Just var <- ident, [operand] <- operands
                 -> do { implicit <- tcImplicitOperandProjection (varType var) written operand
                       ; return (if implicit then Just (arg, Nothing) else Nothing) }
               _ -> return Nothing }
    find_head _ _ _ = return Nothing

    package_head written (L _ (HsAppType _ fun ty)) = package_head (ty : written) fun
    package_head written (L _ (HsPar _ fun)) = package_head written fun
    package_head written expr = (expr, written)

-- | Read the result after the written ordinary arguments.
implicitSpineResult :: Int -> TcType -> Maybe TcType
implicitSpineResult 0 ty = Just (implicitSpineHead ty)
implicitSpineResult n ty
  | Just (_, result) <- splitVisibleFunTy_maybe (implicitSpineHead ty)
  = implicitSpineResult (n - 1) result
  | otherwise = Nothing

-- | Read the declared result of a named head and its ordinary value arguments.
-- The caller still checks the whole operand, including its constraints.
tcDeclaredOperandResult :: LHsExpr GhcRn -> TcM (Maybe TcType)
tcDeclaredOperandResult operand =
  fmap (>>= \(ident, args) -> implicitSpineResult (length args) (varType ident))
       (tcDeclaredOperandSpine False operand)

-- | Read the result expressions of a branching operand. Pattern and guard
-- binders are still outside the typechecking environment of this probe.
-- See Note [Implicit existential arguments].
resultBranches :: LHsExpr GhcRn -> Maybe (NE.NonEmpty (LHsExpr GhcRn))
resultBranches operand = case unLoc (stripParensLHsExpr operand) of
    HsIf _ _ left right -> Just (left NE.:| [right])
    HsMultiIf _ rhss -> Just (fmap rhs rhss)
    HsCase _ _ (MG { mg_alts = L _ alts }) -> do
      matches <- NE.nonEmpty alts
      results <- traverse match_results matches
      pure (join results)
    _ -> Nothing
  where
    match_results (L _ Match { m_grhss = GRHSs _ rhss (L _ EmptyLocalBinds{}) })
      = Just (fmap rhs rhss)
    match_results _ = Nothing

    rhs :: LGRHS GhcRn (LHsExpr GhcRn) -> LHsExpr GhcRn
    rhs (L _ (GRHS _ _ body)) = body

-- | Read a lexical package operand's declared type. A forwarded result
-- supplies an expected type for checking the original operand.
tcLexicalPackageOperand :: LHsExpr GhcRn -> TcM (Maybe (TcType, Bool))
tcLexicalPackageOperand = tcPackageOperandResult True

-- | Read a package result from a declaration, forwarding call or branching expression.
-- The result flag asks the caller to check the whole operand at the schema.
-- See Note [Implicit opening for an explicit package argument].
tcPackageOperandResult :: Bool -> LHsExpr GhcRn -> TcM (Maybe (TcType, Bool))
tcPackageOperandResult lexical = go False
  where
    go _ operand
      | HsLet _ (L _ (HsValBinds _ (XValBindsLR (HsVBG [(NonRecursive, [L _ binding])] [])))) body
          <- unLoc (stripParensLHsExpr operand)
      , FunBind { fun_id = L _ name, fun_matches = matches } <- binding
      , Just rhs <- lazyLetRhs matches
      , all (returnsLocal name) (fromMaybe (NE.singleton body) (resultBranches body))
      = do { declared <- go True rhs
           ; pure ((\(schema, _) -> (schema, True)) <$> declared) }
    go _ operand
      | Just (first NE.:| rest) <- resultBranches operand
      = runMaybeT $ do
          (schema, _) <- MaybeT (go True first)
          others <- traverse (MaybeT . go True) rest
          guard (all (eqType schema . fst) others)
          pure (schema, True)
    go forwarded operand = runMaybeT $ do
      (ident, args) <- MaybeT (tcDeclaredOperandSpine lexical operand)
      let sigma = varType ident
          arity = length args
      result <- MaybeT (pure (implicitSpineResult arity sigma))
      case result of
        TyVarTy v
          | isEmptyNameEnv (idConcreteTvs ident)
          , (tvs, _, body) <- tcRetainedInstTelescope sigma
          , v `elem` tvs
          , (matches, domains, final) <- splitFunTys body
          , domains `equalLength` args
          , getTyVar_maybe (coreFullView final) == Just v
          , all isTauTy matches, all (ordinary_domain v) domains
          , first : rest <- [arg | (Scaled _ domain, arg) <- zip domains args
                                 , getTyVar_maybe (coreFullView domain) == Just v]
          -> do { selected@(schema, _) <- MaybeT (go (forwarded || lexical) first)
                ; case rest of
                    [] | lexical -> pure (schema, True)
                    [] -> pure selected
                    _ -> do { guard (not lexical && closed_dot_schema schema)
                            ; alternatives <- traverse (MaybeT . go False) rest
                            ; guard (all (agrees_with schema . fst) alternatives)
                            ; pure (schema, True) } }
        _ -> do { when forwarded $ guard (scoped_dot_schema sigma result)
                ; pure (result, False) }

    scoped_dot_schema sigma ty
      | schema@(ExistsTy ImplicitExists _ _) <- coreFullView ty
      = all rigid (tyCoVarsOfTypeList schema)
      | otherwise = False
      where
        scope = tyCoVarsOfType sigma
        rigid v = isTcTyVar v && isSkolemTyVar v && v `elemVarSet` scope

    closed_dot_schema ty
      | schema@(ExistsTy ImplicitExists _ _) <- coreFullView ty
      = noFreeVarsOfType schema
      | otherwise = False

    agrees_with schema other = closed_dot_schema other && eqType schema other

    ordinary_domain v (Scaled mult domain) =
      isTauTy mult && (isTauTy domain || independent_callback v domain)

    independent_callback v domain
      | (binders@(_:_), _, body) <- tcRetainedInstTelescope domain
      = all isTyVar binders && isTauTy body
        && not (v `elemVarSet` tyCoVarsOfType domain)
        && not (hasNativeDependentExpr (Core.Type domain))
      | otherwise = False

{- Note [Scope of a forwarded package schema]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A nonrecursive local binding can forward its declared package through a case.
For example, in let p = chooseMixed flag in case flag of { True -> p; False -> p },
each result is the same renamed variable. Read the schema from the defining
expression. Check the original let at that schema once. A shadowing pattern
has a different Name and cannot select the outer binding's schema here.

A lexical tuple binding checks a forwarding call at the selected package
schema before opening it. The schema can mention a rigid type variable from
the caller, or a witness opened by an enclosing binding. Use the complete
declaration of the selected operand as the scope check. A variable already
free in that declaration stays in scope when the probe removes its arrows.
An operand's quantified variables belong to its own telescope. Compare
variables by identity and include the free variables of their kinds.
See Note [Computing deep free variables] in GHC.Core.TyCo.FVs.

Apply this check at the leaf of a single forwarding chain. A direct RHS
keeps ordinary instantiation, and multiple forwarding operands keep their
existing rule. Ordinary checking of the full call still checks its sibling
arguments, evidence, and usage at the selected schema. The opening keeps
the complete let result outside the new witness scope.
-}

-- | Read a named head and its ordinary arguments without elaboration.
-- In a lexical package binding, allow leading specified multiplicity arguments.
tcDeclaredOperandSpine :: Bool -> LHsExpr GhcRn -> TcM (Maybe (Id, [LHsExpr GhcRn]))
tcDeclaredOperandSpine lexical = go []
  where
    go args (L _ (HsApp _ fun arg)) = go (arg : args) fun
    go args (L _ (HsPar _ fun)) = go args fun
    go args expr
      | (written, L _ (HsVar _ lname)) <- leading_types 0 expr
      , written == 0 || lexical
      , let name = unLocWithUserRdr lname
      , not (isDataOcc (nameOccName name))
      = runMaybeT $ do
          ident <- MaybeT (lookup_declared name)
          let (_, binders, _) = tcSplitForAllTyVarsReqTVBindersN 0 (varType ident)
              specified = take written [binderVar b | (_, b) <- binders
                                                    , isSpecifiedForAllTyFlag (binderFlag b)]
          guard (length specified == written && all (isMultiplicityTy . tyVarKind) specified)
          pure (ident, args)
    go _ _ = return Nothing

    lookup_declared name
      | isExternalName name = tcLookupIdMaybe name
      | otherwise = do { local <- tcLookupLcl_maybe name
                       ; return $ case local of
                           Just ATcId{tct_id = ident} -> Just ident
                           _ -> Nothing }

    leading_types n (L _ (HsPar _ fun)) = leading_types n fun
    leading_types n (L _ (HsAppType _ fun _)) = leading_types (n + 1) fun
    leading_types n expr = (n, expr)

-- | Recognize a written dot type without checking the expression or signature.
-- See Note [Projected implicit existential operands].
tcImplicitOperandSignature :: LHsSigWcType GhcRn -> TcM Bool
tcImplicitOperandSignature sig@(HsWC { hswc_body = L _ (HsSig { sig_body = body }) })
  | isCompleteHsSig sig = tcImplicitOperandType body
  | otherwise = return False

-- | Read the outer shape of a written type.
tcImplicitOperandType :: LHsType GhcRn -> TcM Bool
tcImplicitOperandType (L _ (HsParTy _ nested)) = tcImplicitOperandType nested
tcImplicitOperandType (L _ (XHsType (HsTypeExistsRn ImplicitExists _ _))) = return True
tcImplicitOperandType ty
  | Just (L _ (WithUserRdr _ name)) <- hsTyGetAppHead_maybe ty
  , isTcOcc (nameOccName name)
  = do { thing <- tcLookup name
       ; return $ case thing of
           AGlobal (ATyCon tc)
             | Just rhs <- synTyConRhs_maybe tc
             , ExistsTy ImplicitExists _ _ <- coreFullView rhs -> True
           _ -> False }
  | otherwise = return False

-- | Recognize a dot result selected by a named projection.
-- See Note [Projected implicit existential operands].
tcImplicitOperandProjection :: TcSigmaType -> [LHsWcType GhcRn] -> LHsExpr GhcRn -> TcM Bool
tcImplicitOperandProjection sigma written operand
  | (_, binders, body) <- tcSplitForAllTyVarsReqTVBindersN 0 sigma
  , let tvs = map (binderVar . snd) binders
        specified = [binderVar b | (_, b) <- binders, isSpecifiedForAllTyFlag (binderFlag b)]
  , length written <= length specified
  , isTauTy body
  , not (hasNativeDependentExpr (Core.Type body))
  , Just (domain, result) <- splitVisibleFunTy_maybe body
  , TyVarTy result_var <- coreFullView result, result_var `elem` tvs
  , Just (tc, domain_args) <- tcSplitTyConApp_maybe domain
  , isGenerativeTyCon tc Nominal
  , any (eqType (mkTyVarTy result_var)) domain_args
  = do { declared <- tcDeclaredOperandResult operand
       ; case declared >>= match_dot tvs tc domain result_var of
           Just () -> case lookup result_var (zip specified written) of
             Just (HsWC { hswc_body = ty }) -> tcImplicitOperandType ty
             Nothing -> return True
           Nothing -> return False }
  | otherwise = return False
  where
    match_dot tvs tc domain result_var actual = do
      (actual_tc, _) <- tcSplitTyConApp_maybe actual
      guard (actual_tc == tc && supportsQuickLookType actual && noQuickLookVars actual)
      guard (not (any (`elemVarSet` tyCoVarsOfType actual) tvs))
      let in_scope = mkInScopeSet (tyCoVarsOfTypes [domain, actual])
      subst <- tcMatchTyX_BM (bind_outer tvs) (Subst.mkEmptySubst in_scope) domain actual
      guard (eqType (Subst.substTy subst domain) actual)
      selected <- Subst.lookupTyVar subst result_var
      case coreFullView selected of
        ExistsTy ImplicitExists _ _ -> Just ()
        _ -> Nothing

    bind_outer tvs tv _ | tv `elem` tvs = BindMe
                        | otherwise = DontBindMe

-- | Find the declared shape after ordinary invisible instantiation.
implicitSpineHead :: TcType -> TcType
implicitSpineHead ty = case coreFullView ty of
  ForAllTy _ (Bndr _ vis) body | isInvisibleForAllTyFlag vis -> implicitSpineHead body
  FunTy { ft_af = af, ft_res = body } | isInvisibleFunArg af -> implicitSpineHead body
  other -> other

-- | Check a value spine against the declared payload of an implicit head.
opensImplicitHead :: Int -> TcType -> Bool
opensImplicitHead arity body = arity > 0 && go arity (implicitPayloadType body)
  where
    go 0 _ = True
    go n ty = case splitVisibleFunTy_maybe ty of
      Just (_, result) -> go (n - 1) result
      Nothing -> False

{- Note [Type arguments before an implicit package head]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given builder :: forall a. C a => exists t. a -> Callback t,
the prefix builder @Bool produces a package. The value argument then
applies to the function inside the package.

Use the declared shape to identify the prefix. Check its written type
arguments and instantiate its ordinary invisible binders through IALL
and ITYARG. Save the checked type applications and dictionary wrappers
on the package expression. Check the value arguments only after opening
the package, with the result type outside the opening.

The case and observer paths keep the prefix in their source view and
check it with the ordinary application checker. They rebuild only the
value spine inside the opening. Source spans on the prefix still identify
type argument and dictionary errors. Use the original prefix from the
expression expansion in value-argument diagnostics for the rebuilt call.
-}

{- Note [Implicit existential arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The declared domain must ask for a payload. A package domain or an
unconstrained variable keeps forwarding an existing package. This
first rule recognizes named argument heads and ordinary value spines.
The declaration selects the route. The ordinary checker verifies the
actual argument, including its use and all of its constraints, once.
Scale that check by the concrete multiplicity of the formal arrow.

In listCells (if flag then id left else right), the declared branches
can supply the same dot schema. Read both without checking the terms.
Each must expose the same complete package type with only rigid free
variables. Check the conditional at that schema before opening it.
The ordinary conditional checker keeps its branch usage and demand.

For fromList calls in the branches of an if or case, each producer
declaration promises a dot result. The element type comes from the
operands. Infer the entire branching expression once and open the
checked result. Ordinary inference checks the scrutinee, patterns and
guards, including any splices. The probe only reads result types from
heads already in scope. A head bound by the checked case or guard is
unavailable to the probe. A renamed rebindable ifThenElse is an
application expansion and keeps its ordinary rules.

A guarded formal domain may still supply essential type information.
For consume (case flag of True -> Env (\f -> (f 1, f True)); ...),
the type of Env's field tells us that f is polymorphic. If a branch
does not identify a dot result, check at the formal domain. Keep this
rule also for non-guarded domains, such as a type family that may reduce
to a package after ordinary instantiation.

This probe selects a checking path before Quick Look. A Quick Look
cache can record the result type of an instantiated application spine.
It has no result for a case expression. The opening uses the complete
checked operand and keeps the same application continuation for both
declared and inferred package types.

A dot package with an arrow payload can also head a value application.
Instantiate the head's ordinary invisible prefix before opening the package.
For a direct call, check its original operands against that payload inside
the opening. A named argument call can need the scope across an enclosing
consumer. When its formal domain requests a payload, open only the inner
head and replace that head in the original expression. The ordinary
checker then checks the rewritten call once, as part of the outer call.

Introduce a fresh skolem and lazy payload binding for each distinct
opening. Two unrestricted operands in this application can share an
opening when both are the same checked local package variable at the
same schema. Reuse the payload Id under its existing skolem. This is
the lexical-variable case of IExistCore in An Existential Crisis
Resolved. It preserves the source use check for each operand.
Instantiated variables and linear operands have their own opening.
The map is local to the application spine.

For snd p (fst p), the first argument opens p for the application.
A nested unary value application can reuse that payload when its
declared domain also asks for the same local package to be opened.
Only reuse an opening from an unrestricted operand in an unrestricted
domain. Its first source use already contributes Many, so the nested
use cannot change its checked multiplicity. Check the nested call
at the outer argument type. The projections determine payload demand.

A named producer can also supply the same package to both selectors.
Require an exact, monomorphic unary call with an unrestricted domain
and the same bare local input. Use the resolved names of the checked
producer and input. Instantiation, dictionary selection and argument
conversion are absent from this equality check. Demand still comes
from the surrounding calls. The opening shares the producer thunk,
so it can reduce the number of evaluations. See Aside 2 and section
9.2.2 of An Existential Crisis Resolved.

Instantiate the application under all of the skolems. The
outer result type is present before any of these scopes, so the
ordinary implication solver rejects a witness that would escape.
If the result expects another dot package, check and pack the result
inside the scopes. An opening substitutes its skolem through the
remaining schema before opening the next dot binder. The result type
stays outside the whole chain. The erasure of Unpack preserves the
payload's demand. Bar packages go through the ordinary checker.

An inference slot at this level can receive a directly declared dot
result. For example, `filterVec p (builder x)` returns the package
declared by filterVec. Create outer metavariables for the result's
free head binders only. Check the application against this copy of
its declared package. The ordinary head instantiation still takes
place inside the opening, so argument-only variables can match the
witness. Fill the usual inference slot after closing the implication.
The outer result's metavariables cannot capture the opened witness.
-}

{- Note [Inferring an erased package from a higher-rank callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a separately compiled consumer of a filtered vector.

  consume :: (forall j. Vec a j -> r) -> Vec a k -> r
  let ys = consume (filterVec (> 3)) (filterVec odd xs) in cells ys

Opening the second operand needs a result type outside its witness scope.
An ordinary inference slot cannot contain the dot result of filterVec.
Quick Look skips the callback because its expected type starts with forall.
See Note [Quick Look at value arguments].

When the consumer has a ranked callback followed by directly opened
operands, unify the callback's declared input types with the expected
callback domains. Bind variables of the two head declarations only. This
lets a concrete callback, such as Vec Int j -> Filtered Int, fix the
consumer's element type. Use one substitution for the whole spine.
Both sides must agree after substitution. A declared
callback that uses one length for two inputs cannot match distinct rigid
lengths. Require the result parameter
to occur only as the callback result and the complete application result.
This avoids choosing a package for a parameter that another argument or
constraint can select. Use the substituted result only when it declares a
dot package and contains no binder from the callback's local forall or
unmatched binder from the callback's declaration. Read only the leading
telescope. A forall below an applied value arrow can introduce another
binder, so the probe cannot strip that forall. For the example, matching
Vec a j transports the element parameter into the consumer's telescope.
The callback length j does not occur in the package.

Freshen the consumer's result parameters outside the opening. Check the
original callback at that result through the ordinary argument checker.
This still checks its supplied arguments, constraints, and multiplicity.
For a result with consecutive dot binders, check each binder kind and
the final payload. An inner body can refer to a witness bound by an
earlier dot. Keep the whole result type over these witnesses. The
producer thunk stays lazy.

A declaration can place its inferred lengths after an ordinary argument,
as in select :: Predicate a -> forall i j. Vec a i -> Vec a j -> HiddenRows a.
Read the invisible binders exposed after each supplied value, including
the last one. Match only variables bound by these groups. Check the final
package for escapes from all of them. The ordinary application checker
instantiates each group and checks the arguments and constraints in order.
-}

{- Note [Projected implicit existential operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In foldr step z (fst (partition p xs)), the declared result of fst is a
type variable. A declared result of partition has two dot packages under
the rigid pair constructor. The argument type of fst determines which one
the application returns.

Use the same guarded premise as Quick Look (Serrano et al., Fig. 5).
Read the producer's declared spine and match the projection's rigid
domain. Use the immutable matcher. The result parameter must be a direct
argument of the nominally injective domain constructor. A match under a
type family would not determine the result parameter. Select the route
only if that parameter has a dot package at the head. A written type for
that parameter must also show a dot package, directly or through a known
synonym. The same check applies to a written expression signature. These
checks consume no source usages and create no constraints or Quick Look
variables.

The selected type is only a shape. It can mention binders from the
declaration of the producer. The ordinary inference of the original
operand checks the arguments, type applications, and constraints once.
tcOpenImplicitArgs scales that check by the formal multiplicity and uses
its actual result to open the package. It keeps the consumer's result
outside the new skolem. See Note [Implicit existential arguments].
-}

-- | Check a saturated explicit existential constructor application.
-- See Note [Checking explicit existential packages].
tcExistsPack :: HsExpr GhcRn -> [HsExprArg 'TcpRn] -> ExpRhoType -> TcM (HsExpr GhcTc)
tcExistsPack rn_expr args expected
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; unless enabled $ failWithTc (TcRnExistsPack ExistsPackExtension)
       ; let (type_args, rest) = span is_type_arg (filter_not_parens args)
       ; case rest of
           [EValArg { ea_arg = witness }, EValArg { ea_arg = payload }] ->
             do { schema <- check_schema [ty | ETypeArg { ea_hs_ty = ty } <- type_args]
                ; case coreFullView schema of
                    ExistsTy ExplicitExists binder body ->
                      do { arg <- expr_to_type witness
                         ; inferred <- case arg of
                             HsWC [] ty | HsWildCardTy (HoleVar (L _ name)) <- unLoc (ignoreParens ty)
                                        , name == unnamedHoleRdrName
                               -> infer_retained_witness binder body payload
                             _ -> return Nothing
                         ; witness_ty <- maybe (tcHsTypeApp arg (tyVarKind binder)) return inferred
                         ; let payload_ty = substTyWith [binder] [witness_ty] body
                         ; payload' <- tcScalingUsage ManyTy $ tcCheckPolyExpr payload payload_ty
                         ; let packed = XExpr (NativeExistsPack schema witness_ty payload')
                         ; (result_expected, pack) <- implicitResult NoQL rn_expr schema expected
                         ; pack <$> tcWrapResult rn_expr packed schema result_expected }
                    _ -> failWithTc (TcRnExistsPack (ExistsPackInvalidSchema schema)) }
           _ -> failWithTc (TcRnExistsPack ExistsPackArguments) }
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _          = False

    filter_not_parens = filter $ \case
      EWrap (EPar _) -> False
      _ -> True

    infer_retained_witness binder body payload = runMaybeT $ do
      actual <- MaybeT (tcRetainedDeclaredArgType return payload)
      matchRetainedExistsWitness (lexprCtOrigin payload) binder body actual

    expected_schema = traverse (liftZonkM . zonkTcType) =<< readExpType_maybe expected
    read_schema = expected_schema >>= maybe (failWithTc (TcRnExistsPack ExistsPackMissingSchema)) return

    check_schema [] = read_schema
    check_schema [arg] = do
      ty <- tcHsTypeApp arg liftedTypeKind
      case coreFullView ty of
        ExistsTy{} -> return ty
        _ -> do
          mb_schema <- expected_schema
          case mb_schema of
            Just schema | ExistsTy{} <- coreFullView schema -> check_kind ty schema
            _ | eqType ty liftedTypeKind -> read_schema
              | otherwise -> return ty
    check_schema [kind, schema] =
      do { k <- tcHsTypeApp kind liftedTypeKind
         ; ty <- tcHsTypeApp schema liftedTypeKind
         ; check_kind k ty }
    check_schema _ = failWithTc (TcRnExistsPack ExistsPackArguments)
    check_kind k schema = case coreFullView schema of
      ExistsTy ExplicitExists binder _ | not (eqType k (tyVarKind binder)) ->
        failWithTc (TcRnExistsPack (ExistsPackInvalidKind k (tyVarKind binder)))
      _ -> return schema

{- Note [Checking explicit existential packages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recognize Pack by its resolved name in base. A constructor named Pack in a
different unit or module uses ordinary application checking.

The existential schema determines the type of the payload. Read it from an
explicit type argument or the expected result type. A single visible argument
can be the schema or its witness kind. Check the kind against the expected
package. With two visible arguments the second gives the schema. Check
the witness with the required type argument translation used by tcVDQ. Check
the payload at the instantiated body type. Keep the schema and witness for
Core Lint. CoreToStg erases both and keeps the payload's demand.

For `Pack _ flag`, the declared type of flag may already refer to a
retained field of an enclosing pattern. A wildcard metavariable cannot be
filled with that field. See Note [Rigid retained indices] in
GHC.Tc.Utils.Unify. Instead, match the package body against the declared
payload type after family reduction. The existential binder is the only
template variable. Check the chosen witness in the current scope, then
typecheck the payload against the instantiated body as usual. Reading its
declaration neither evaluates the payload nor consumes a second use.

Skip ordinary inference of the head, including the Quick Look shortcut.
Pack has no ordinary function type with which the solver could discover the
existential schema from the payload.
-}

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
           then do { (checked, ty) <- tcInstFunWithPrefix quickLookArg True [] Nothing Nothing
                                       Nothing DoQL False rn_head fun sigma leading
                   ; return (checked, ty, rest) }
           else return ([], sigma, args)
       ; expected <- readExpType_maybe exp_ty
       ; expected <- mapM (liftZonkM . zonkTcType) expected
       ; residual <- liftZonkM (zonkTcType residual)
       ; let keep_ql = return (prefix, DoQL, residual, remaining, Nothing)
             declared_plan =
               do { plan <- tcRetainedKnownArgPlan declared_arg_type expected fun residual remaining
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
                         && maybe True known_result expected)
                 then keep_ql else
                 if not (any (hasNativeDependentExpr . Core.Type) tys)
                 then declared_plan else
            do { let (tvs, _, tau) = tcRetainedInstTelescope residual
               ; if null tvs && isGroundRetainedCallType tau
                 then return (prefix, NoQL, residual, remaining, Nothing)
                 else do
                   { matched <- tcRetainedResultInst NoQL expected remaining fun residual
                   ; case matched of
                       Just (choices, body)
                         | let (remaining_tvs, _, body_tau) = tcRetainedInstTelescope body
                         , null remaining_tvs
                         , isRetainedValueType body_tau
                             || (maybe False isRankedRetainedContainer expected
                                 && isGroundRetainedCallType body_tau)
                         -> do { choices <- liftZonkM (zonkTcTypes choices)
                               ; body <- liftZonkM (zonkTcType body)
                               ; valid_choices <- tcRetainedInstScope (body : choices)
                               ; if valid_choices && all no_ql_vars (body : choices)
                                 then return (prefix ++ [EWrap (EHsWrap (mkWpTyApps choices))],
                                              NoQL, body, remaining, Nothing)
                                 else keep_ql }
                       _ -> declared_plan } } } }
  where
    -- Use the complete result and cached RHS in tcRetainedDollarInst.
    -- See Note [Immutable native dollar applications].
    declared_arg_type
      | HsVar _ (L _ dollar) <- fun, dollar `hasKey` dollarIdKey
      = tcRetainedDeclaredArgType tcRetainedDeclaredTypeView
      | otherwise
      = tcRetainedQuickLookArgType

    known_head HsVar{} = True
    known_head (XExpr (ConLikeTc (RealDataCon _))) = True
    known_head _ = False
    prefix_arg ETypeArg{} = True
    prefix_arg EPrag{} = True
    prefix_arg EWrap{} = True
    prefix_arg _ = False
    type_arg ETypeArg{} = True
    type_arg _ = False
    known_result ty = isRetainedValueType ty || isRankedRetainedContainer ty
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
                         && not (isRetainedValueType ty || isRankedRetainedCallback ty)
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
tcRetainedKnownArgPlan :: (LHsExpr GhcRn -> TcM (Maybe Type))
                      -> Maybe TcRhoType -> HsExpr GhcTc -> TcSigmaType
                      -> [HsExprArg 'TcpRn] -> TcM RetainedInstPlan
tcRetainedKnownArgPlan declared expected fun sigma args
  | hasNoConcreteTyVars fun
  , not (any is_type_arg args)
  = tcRetainedKnownValuePlan declared expected sigma [arg | EValArg { ea_arg = arg } <- args]
  | otherwise = return Nothing
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

-- | Match a known declaration and a supplied sequence of value arguments.
tcRetainedKnownValuePlan :: (LHsExpr GhcRn -> TcM (Maybe Type))
                        -> Maybe TcRhoType -> TcSigmaType
                        -> [LHsExpr GhcRn] -> TcM RetainedInstPlan
tcRetainedKnownValuePlan declared expected sigma values
  | (tvs, theta, tau) <- tcRetainedInstTelescope sigma
  , not (null tvs)
  , let (scoped_preds, body) = scopedDictionaryContext tau
  , isRetainedValueType body
  , not (null values)
  = case coreFullView tau of
      DepFunTy DepFunInvisible binder body ->
        fmap (fmap snd) (tcInferInvisibleRetained tvs binder body values)
      _ -> choose_erased scoped_preds tvs theta tau values (retainedDeclaredDomains (length values) tau)
  where
    choose_erased scoped_preds tvs theta tau values (domains, result, masked)
      = do { pairs <- fmap catMaybes $ mapM (declared_arg tvs) (zip domains values)
         ; let actuals = map snd pairs
               seed | length domains == length values
                    , Just (bound, result_ty) <- result
                    , Just rho <- expected
                    , not (null pairs) || not (isEmptyVarSet bound)
                    = fromMaybe Subst.emptySubst $
                        case coreFullView rho of
                          ExistsTy ImplicitExists witness payload
                            | not (isEmptyVarSet bound)
                            -> match_packed_result_fields tvs witness result_ty payload
                          _ | null pairs || anyFreeVarsOfType (`elemVarSet` bound) result_ty
                            -> match_result_fields tvs Subst.emptySubst result_ty rho
                            | otherwise
                            -> matchRetainedAppType tvs Subst.emptySubst result_ty rho
                    | otherwise = Subst.emptySubst
               match subst = foldM (\s (formal, actual) ->
                                      matchRetainedAppType tvs s formal actual)
                                   subst pairs
               matched = case match seed of
                           Nothing -> match Subst.emptySubst
                           other -> other
         ; case matched of
             Just subst
               | any (hasNativeDependentExpr . Core.Type)
                     (sigma : actuals ++ maybeToList expected)
               -> runMaybeT $ do
                    -- See Note [Applying a scoped dictionary].
                    choices_subst <- case scoped_preds of
                      [] -> pure subst
                      _  -> do
                        let missing = filter (isNothing . Subst.lookupTyVar subst) tvs
                            required = tyCoVarsOfTypes scoped_preds
                        guard (not (any (`elemVarSet` required) missing))
                        fst <$> liftMaybeT (newMetaTyVarsX subst missing)
                    let fixed = mapMaybe (Subst.lookupTyVar subst) tvs
                        -- See Note [Multiplicity of a supplied native argument].
                        fresh_mult tv = eqType (tyVarKind tv) multiplicityTy
                          && tv `elemVarSet` tyCoVarsOfType tau
                          && not (any (anyFreeVarsOfType (== tv))
                                      (masked : theta ++ map tyVarKind tvs))
                          && any (hasNativeDependentExpr . Core.Type) fixed
                        choice tv = case Subst.lookupTyVar choices_subst tv of
                          Just ty -> pure ty
                          Nothing -> guard (fresh_mult tv) >> liftMaybeT newMultiplicityVar
                    choices <- traverse choice tvs
                    guard (not (any (`elemVarSet` tyCoVarsOfTypes choices) tvs))
                    guard (and (zipWith (\tv ty ->
                              eqType (Subst.substTy choices_subst (tyVarKind tv)) (typeKind ty)) tvs choices))
                    guard (all (\(formal, actual) -> eqType (Subst.substTy choices_subst formal) actual) pairs)
                    let tys = sigma : actuals ++ choices ++ maybeToList expected
                    guard (all noQuickLookVars tys)
                    valid <- liftMaybeT (tcRetainedInstScope tys)
                    guard valid
                    return choices
             _ -> return Nothing }

    -- See Note [Erased arguments in a dependent result].
    match_packed_result_fields tvs witness formal payload
      | Just (tc, templates) <- splitTyConApp_maybe formal
      , Just (tc', actuals) <- splitTyConApp_maybe payload
      , tc == tc', isAlgTyCon tc, templates `equalLength` actuals
      , Just fields <- result_fields tc templates actuals
      = foldM (\s (template, actual) -> match_result_fields tvs s template actual)
              Subst.emptySubst (mapMaybe independent fields)
      | otherwise = Nothing
      where
        independent entry@(template, actual)
          | not (mentions_witness actual) = Just entry
          | let expanded = expandTypeSynonyms actual
          , not (mentions_witness expanded) = Just (template, expanded)
          | otherwise = Nothing
        mentions_witness = anyFreeVarsOfType (== witness)

    match_result_fields tvs subst formal actual
      | Just tv <- getTyVar_maybe formal, tv `elem` tvs
      = matchRetainedAppType tvs subst formal actual
      | Just (tc, templates) <- splitTyConApp_maybe formal
      , Just (tc', fields) <- splitTyConApp_maybe actual
      , tc == tc', templates `equalLength` fields
      , Just pairs <- result_fields tc templates fields
      = foldM (\s (template, field) -> match_result_fields tvs s template field)
              subst pairs
      | otherwise = Just subst

    -- See Note [Inferring the suffix of a checked split].
    result_fields tc templates fields
      | isAlgTyCon tc, isGenerativeTyCon tc Nominal
      , length templates == tyConArity tc
      = Just (zip templates fields)
      | Just (_, True) <- closedFamilyScope_maybe tc
      , length templates + 1 == tyConArity tc
      , Just axiom <- closedFamilyTyConCoAxiom_maybe tc
      , [branch] <- fromBranches (coAxiomBranches axiom)
      , null (coAxBranchCoVars branch)
      , Just parameters <- traverse getTyVar_maybe (coAxBranchLHS branch)
      , length parameters == tyConArity tc
      , length parameters == sizeVarSet (mkVarSet parameters)
      = let determined = injectiveVarsOfType False (coAxBranchRHS branch)
        in Just [(template, field) | (parameter, template, field) <- zip3 parameters templates fields
                                   , parameter `elemVarSet` determined]
      | otherwise = Nothing

    declared_arg tvs (Just (retained, formal), arg)
      | any (`elemVarSet` tyCoVarsOfType formal) tvs
      = do { actual <- if retained then tcRetainedConstructorArgType arg
                         else declared arg
           ; return ((formal,) . declared_domain_view retained formal <$> actual) }
      | otherwise = return Nothing
    declared_arg _ _ = return Nothing

    -- See Note [Declared retained-domain choices].
    declared_domain_view True formal actual
      | Just (tc, _) <- tcSplitTyConApp_maybe formal
      , isAlgTyCon tc
      , Just (_, params) <- matchSumPayload tc actual
      = mkTyConApp tc params
    declared_domain_view _ _ actual = actual

tcRetainedKnownValuePlan _ _ _ _ = return Nothing

-- | Read a declared operand type before Quick Look instantiates a native call.
-- See Note [Nested immutable retained applications].
tcRetainedQuickLookArgType :: LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedQuickLookArgType arg
  | L _ (HsVar _ con_name) <- head_expr
  , not (null args)
  , isDataOcc (nameOccName (unLocWithUserRdr con_name))
  = runMaybeT $ do
      RealDataCon dc <- liftMaybeT (tcLookupConLike (unLoc con_name))
      guard (dataConHasRetainedResult dc && length args == dataConSourceArity dc)
      result <- constructor_result (idType (dataConWrapId dc)) args
      guard (isAlgTyConApp result && noQuickLookVars result)
      guard =<< liftMaybeT (tcRetainedInstScope [result])
      return result
  | L _ (HsVar _ lname) <- head_expr
  , values@(_ : _) <- args
  , let name = unLocWithUserRdr lname
  , not (isDataOcc (nameOccName name))
  = runMaybeT $ do
      fun <- MaybeT (tcLookupIdMaybe name)
      guard (isEmptyNameEnv (idConcreteTvs fun))
      sigma <- liftMaybeT (liftZonkM (zonkTcType (varType fun)))
      let (tvs, theta, tau) = tcRetainedInstTelescope sigma
          bare = all bare_operand values
      guard (null theta && not (null tvs))
      guard (bare || takesDirectIndexArgument tau)
      result <- MaybeT (pure (tcRetainedOrdinaryResult (length values) tau))
      guard (isAlgTyConApp result)
      let read_operand = if bare then declared else tcRetainedQuickLookArgType
      choices <- MaybeT (tcRetainedKnownValuePlan read_operand Nothing sigma values)
      let ty = substTyWith tvs choices result
      guard (hasNativeDependentExpr (Core.Type ty) && noQuickLookVars ty)
      valid <- liftMaybeT (tcRetainedInstScope [ty])
      guard valid
      return ty
  | otherwise = declared arg
  where
    (head_expr, args) = split_named [] arg
    declared = tcRetainedDeclaredArgType tcRetainedDeclaredTypeView

    constructor_result ty [] = pure ty
    constructor_result ty (operand : rest) = case coreFullView ty of
      DepFunTy DepFunVisible binder _ -> do
        HsVar _ name <- pure (unLoc (stripParensLHsExpr operand))
        ATcId { tct_id = local } <- MaybeT (tcLookupLcl_maybe (unLocWithUserRdr name))
        actual <- liftMaybeT (liftZonkM (zonkId local))
        let domain = varType binder
        guard (all noQuickLookVars [varType actual, typeKind (varType actual), idMult actual])
        value <- if isNativeOpaqueValueId actual && varType actual `eqType` domain
                 then pure actual
                 else do
                   level <- liftMaybeT getTcLevel
                   let flexible ty = case coreFullView ty of
                         TyVarTy tv | isTcTyVar tv, isMetaTyVar tv
                                    , TauTv <- metaTyVarInfo tv
                                    -> not (isQLInstTyVar tv) && isTouchableMetaTyVar level tv
                         _ -> False
                   guard (flexible (varType actual) || varType actual `eqType` domain)
                   guard (isManyTy (idMult actual) || isOneTy (idMult actual)
                          || flexible (idMult actual))
                   (Core.Var checked, _) <- liftMaybeT (tcRetainedArg domain operand)
                   guard (checked == actual && isNativeOpaqueValueId checked)
                   pure checked
        constructor_result (applyTypeToArgs ty [Core.Var value]) rest
      FunTy { ft_af = af, ft_res = result } | isVisibleFunArg af ->
        constructor_result result rest
      _ -> mzero

    split_named values (L _ (HsPar _ inner)) = split_named values inner
    split_named values (L _ (HsApp _ fun value)) = split_named (value : values) fun
    split_named values fun = (fun, values)

    bare_operand operand = case unLoc (stripParensLHsExpr operand) of
      HsVar{} -> True
      _ -> False

    isAlgTyConApp ty = case tcSplitTyConApp_maybe ty of
      Just (tc, _) -> isAlgTyCon tc && not (isClassTyCon tc)
      _ -> False

-- | Does a value argument accept a constructor indexed by its own retained field?
-- See Note [Nested immutable retained applications].
takesDirectIndexArgument :: Type -> Bool
takesDirectIndexArgument ty
  | Just (domain, result) <- splitVisibleFunTy_maybe ty
  = case tcSplitTyConApp_maybe domain of
      Just (tc, _) | any dataConHasRetainedResult (tyConDataCons tc) -> True
      _ -> takesDirectIndexArgument result
  | otherwise = False

{- Note [Nested immutable retained applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `toList (appendVec left right)`, the declared vector types can
fix all erased arguments of appendVec. Its result still contains a
scoped runtime index. Quick Look's nested application rule would
enter the inner application through tcInstFun, without the complete
immutable plan available when checking it directly. Instantiation
variables cannot contain that retained index.

Read the declared result of a simple, fully applied, unconstrained
inner call using the same immutable selector as a direct call. Inspect
only operands that determine an erased argument. Each recursive call
examines a proper subexpression. Recurse through a named call only if a
declared domain has a constructor with a retained result index. Repeated
ordinary calls such as choose (choose (Just id)) must not inspect every
suffix of the argument tree. This also handles an ordinary projection
of a rebuilt value, such as `readLayout (roundCell (Cell xs value))`.
Use the algebraic argument type only to complete the outer caller's plan.
Otherwise leave normal Quick Look in place. The normal checker still
checks the inner and outer applications, including their argument types,
lexical scopes and usage. The plan does not evaluate any operand or
assign a native type to a Quick Look variable.

A constructor can return its own field as an index. In
`rebuild (IndexedList xs)`, the outer function must instantiate its erased
index with the caller's xs. Its signature alone cannot supply this index.
The constructor's result equality enables the same argument inference as
a named value with a retained type. With Quick Look, read the constructor
wrapper's codomain at the scoped field Id before instantiating rebuild.
An ordinary payload arrow does not bind an index. Step past each such
arrow without checking its operand. For example, in
`adjust f (Reading calibrated value)`, only calibrated fixes the result
type. For an unsigned local variable, use the ordinary retained-variable
check to fix its type or multiplicity from the constructor domain. Only
touchable ordinary metavariables qualify. Check the operand's kind too.
The saturated constructor requires these assignments even if the outer
plan fails and ordinary checking resumes. The normal argument check
records usage. Report a linear operand at its source occurrence.
See Note [Retained constructor result equality] in GHC.Core.DataCon.
-}

-- | Read a monomorphic declaration for immutable retained argument matching.
-- See Note [Declared retained-domain choices].
tcRetainedDeclaredArgType :: (Type -> TcM Type) -> LHsExpr GhcRn -> TcM (Maybe Type)
tcRetainedDeclaredArgType view (L _ (HsPar _ arg)) = tcRetainedDeclaredArgType view arg
tcRetainedDeclaredArgType view (L _ (XExpr (ExpandedThingRn (HSE _ arg))))
  = tcRetainedDeclaredArgType view arg
tcRetainedDeclaredArgType view (L _ (ExplicitList _ fields))
  = fmap (fmap mkListTy) (tcRetainedListElementType (tcRetainedDeclaredArgType view) fields)
tcRetainedDeclaredArgType _ (L _ (HsLit _ HsChar{})) = return (Just charTy)
tcRetainedDeclaredArgType _ (L _ (HsLit _ HsString{})) = return (Just (mkListTy charTy))
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
                | null (dataConRepArgBinders dc)
                , (tvs, [], [], [], fields, result) <- dataConFullSig dc
                , fields `equalLength` args
                , isJust (nativeIndexDataConTyArgs result dc)
                -> do { subst <- foldM (tcMatchDeclaredField tvs) Subst.emptySubst (zip fields args)
                      ; return $ do
                          { choices <- traverse (Subst.lookupTyVar subst) tvs
                          ; let ty = substTyWith tvs choices result
                          ; guard (isNativeIndexType ty)
                          ; return ty } }
              _ -> return Nothing }
    collect [] arg = tcRetainedDeclaredArgType tcRetainedDeclaredTypeView arg
    collect _ _ = return Nothing

-- | Use a field's declaration only while it can determine a missing binder.
tcMatchDeclaredField :: [TyVar] -> Subst.Subst -> (Scaled Type, LHsExpr GhcRn) -> TcM Subst.Subst
tcMatchDeclaredField tvs subst (Scaled _ formal, arg)
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
       ; let (domains, _, _) = retainedDeclaredDomains (length args) suffix
             choose view = do
               { pairs <- catMaybes <$> mapM (declared_pair view indices) (zip domains args)
               ; let determines (formal, _) = template `elemVarSet` injectiveVarsOfType True formal
                     anchors | null tvs  = filter determines pairs
                             | otherwise = pairs  -- Erased argument prepass
               ; select template (map snd indices ++ tvs) anchors }
       ; givens <- getNativeGivens
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
tcRetainedDeclaredTypeView ty = coercionRKind <$> tcRetainedTypeCoercion (const True) ty

-- | Transport a type using constructor proofs about the selected indices.
tcRetainedTypeCoercion :: (Id -> Bool) -> Type -> TcM CoercionN
tcRetainedTypeCoercion select ty
  = do { givens <- getNativeGivens
       ; givens <- mapM (liftZonkM . zonkId) givens
       ; foldM refine (mkNomReflCo ty) (reverse givens) }
  where
    refine result_co given
      | isCoVar given
      , Just (index, co) <- orient (mkCoVarCo given)
      , select index
      , isRetainedConstructorProof index co
      , let result = coercionRKind result_co
      , anyFreeVarsOfType (== index) result
      = do { name <- newSysName (mkTyVarOcc "retained_view")
           ; let template = mkTyVar name (varType index)
           ; case abstractNativeIndices [(TermVarTy index, template)] result of
               Nothing -> return result_co
               Just _ -> mkTransCo result_co <$> tcRetainedCoercion index co result }
      | otherwise = return result_co

    orient co
      | TermVarTy index <- coercionLKind co = Just (index, co)
      | TermVarTy index <- coercionRKind co = Just (index, mkSymCo co)
      | otherwise = Nothing

{- Note [Declared retained argument refinement]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor branch can have retained ~# S k while a declared argument keeps
its original type [Vec Int retained]. Select the erased element parameter of
genericVector from the proof-backed view [Vec Int (S k)]. Otherwise Count at
the original and refined element types can block result checking before the
ordinary solver checks the argument.

Use only the actual lexical evidence parameters returned by getNativeGivens. This
list is newest first. Apply checked nominal constructor proofs from outer to
inner scope. Orient an actual nominal proof toward the runtime index before
checking its constructor endpoint. For example, matching Refl can supply
S Z ~# size rather than size ~# S Z. The symmetric coercion proves the same
view. The existing tcRetainedCoercion supplies the type view. Check the
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

-- | Check for a dependent dictionary before the first written value argument.
hasScopedDictionaryPrefix :: TcSigmaType -> Bool
hasScopedDictionaryPrefix sigma
  | (_, _, rho) <- tcRetainedInstTelescope sigma
  , DepDictTy{} <- coreFullView rho = True
  | otherwise = False

-- | Collect the predicates whose evidence occurs in the body of this prefix.
scopedDictionaryContext :: Type -> ([PredType], Type)
scopedDictionaryContext ty = case coreFullView ty of
  DepDictTy dict body ->
    let (preds, rho) = scopedDictionaryContext body
    in (varType dict : preds, rho)
  FunTy af _ _ _ body | isInvisibleFunArg af -> scopedDictionaryContext body
  _ -> ([], ty)

-- | Collect declared domains for the written value-argument prefix.
-- Mark visible retained domains with True. Skip dependent argument domains.
-- Keep the set of crossed retained binders with the result.
-- See Note [Declared retained-domain choices].
retainedDeclaredDomains :: Int -> Type -> ([Maybe (Bool, Type)], Maybe (IdSet, Type), Type)
retainedDeclaredDomains supplied = go supplied emptyVarSet
  where
    go 0 bound result = result_view bound result
    go remaining bound ty
      | Just ty' <- coreView ty = go remaining bound ty'
    go remaining bound (DepDictTy dict body)
      = let (domains, result, masked) = go remaining (extendVarSet bound dict) body
        in (domains, result, DepDictTy dict masked)
    go _ _ ty@(DepFunTy DepFunInvisible _ _) = ([], Nothing, ty)
    go remaining bound (DepFunTy DepFunVisible binder body)
      = let domain = independent bound True (varType binder)
            (rest, result, masked) = go (remaining - 1) (extendVarSet bound binder) body
        in (domain : rest, result, DepFunTy DepFunVisible binder masked)
    go remaining bound ty@(FunTy af _ mult arg res)
      | isVisibleFunArg af
      = let (rest, result, masked) = go (remaining - 1) bound res
            weight | Just _ <- getTyVar_maybe mult = ManyTy
                   | otherwise = mult
        in (independent bound False arg : rest, result, ty { ft_mult = weight, ft_res = masked })
      | otherwise
      = let (rest, result, masked) = go remaining bound res
        in (rest, result, ty { ft_res = masked })
    go _ bound result = result_view bound result

    result_view bound result = ([], Just (bound, result), result)

    independent bound retained domain
      | mentions bound domain = Nothing
      | otherwise = Just (retained, domain)
    mentions bound = anyFreeVarsOfType (`elemVarSet` bound)

{- Note [Multiplicity of a supplied native argument]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In indexGeneric [v] pos, v can have type Vec Int n for a runtime n.
The element type must be selected before introducing ordinary metavariables.
The position arrow also quantifies a multiplicity p. Reading the type of pos
does not determine how often the function uses it.

Keep the known native element type. Give p an ordinary fresh multiplicity
variable when it occurs only as the weight of supplied value arrows. The
third result of retainedDeclaredDomains masks these weights to check that
condition. Check the original function type and its argument usage with
the fresh variable. Neither the result nor a cached argument-type view
can expose a variable introduced by this rule.
-}

{- Note [Erased arguments in a dependent result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider splitAtVec :: forall a m. foreach (n :: Nat)
                     -> Vec a (Add n m) -> (Vec a n, Vec a m).
The first vector in the result uses the retained binder. The second
determines the erased suffix m from the caller's expected pair type.
Match erased variables in the arguments of matching generative algebraic
type constructors. A type family result gives no such projection.
The source arguments can supply the remaining erased choices as usual.
The same rule applies when only the result supplies information. For
example, dropVec returns Vec a m and its input contains Add n m.

An expected dot package can also supply independent erased parameters.
For Tile rows columns xs checked against exists shape. Tile a shape,
take the element type a from the expected payload. Skip arguments that
still mention the hidden witness after expanding type synonyms. Use the
expanded type when a synonym discards the witness. The ordinary result
would otherwise have a fresh element metavariable when implicitResult
tries to match the payload. It cannot use an ordinary metavariable for
the retained shape. First choose the element type, then let
implicitResult check the actual result and choose the shape it contains.

Only the erased parameters receive type arguments. Keep the declared
retained telescope. The argument checker still checks the input at
Add n m, and result subsumption checks the actual first vector length.
-}

{- Note [Inferring the suffix of a checked split]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expected result of SplitBothAtWidth determines both input vectors.
Select those constructor arguments before checking the two fields. If
we infer a field first and cache its type, its splitAtValue call loses
the expected SplitAtValue a cut suffix input. A result match that emits
kind equalities is suitable here because the caller commits to it. The
speculative Quick Look prefix still uses tcRetainedResultInst.

SplitAtValue returns a sum whose payload is another sum. The domain of
the inner sum is Vec a suffix. This fixes suffix even though the input
width Add cut suffix does not. For two applications of the same checked
payload family, inspect its single covering equation. Match only the
captured parameters that occur injectively in the right-hand side.
Use the ordinary nominal injective variable analysis and treat user type
families as opaque. A capture under an unannotated family Hide is still
ambiguous. This selects type applications; final subsumption and the
ordinary field checks must still succeed.
-}

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
Skip later domains that mention a crossed runtime binder. An exact match
there would need the actual runtime argument. For a dependent result, see
Note [Erased arguments in a dependent result]. Keep a skipped domain's
position for source argument alignment. Original argument checking still resolves every actual index and
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

A retained operand bound by Sigma.Pair can have the generated family type
of its payload. For a known algebraic domain, reuse matchSumPayload to find
the nominal result of that generated family equation. The family arguments
supply no injectivity evidence. The result can determine erased parameters from
Vec a size while size is still in scope. Keep the original declared type
for argument checking and for its nominal cast.

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

A checked runtime argument can expose another erased prefix. For example,
in keepAfter b m the remaining type first binds a, then m :: Maybe a.
At IALL, reuse the declaration check on m before instantiating that prefix.
Use only the next retained operand. A later written type argument belongs
to a different position. Written type arguments at this position still use
the usual visible type application rule.

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

A written type application can supply a ranked retained callback to an
algebraic constructor. In Just @Callback reflect, ITYARG has already
checked Callback. The residual type Callback -> Maybe Callback declares
the field and the result. Keep this type argument in the checked prefix
and use ordinary checking for that field. The callback keeps its binders,
constraints and scope. Quick Look does not assign an instantiation
variable to Callback.

An expected result can determine the same choice for Just reflect. Match
the ordinary constructor result Maybe alpha against Maybe Callback.
With ImpredicativeTypes, this match can use a ranked retained type argument
from an algebraic result. Require the same exact result, complete choices,
kinds and scope as a written application. Check the field against Callback
afterwards. The expected result supplies the choice before argument checking.

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

-- | Use ordinary checking for a known native value or callback.
-- See Note [Known native variables with ImpredicativeTypes].
tcRetainedKnownHeadQL :: Bool -> QLFlag -> [HsExprArg 'TcpRn] -> ExpRhoType
                     -> HsExpr GhcTc -> TcSigmaType -> TcM QLFlag
tcRetainedKnownHeadQL True DoQL [] exp_res_ty HsVar{} actual
  = do { actual <- liftZonkM (zonkTcType actual)
       ; expected <- readExpType_maybe exp_res_ty
       ; expected <- mapM (liftZonkM . zonkTcType) expected
       ; let tys = actual : maybeToList expected
             known_value
               = not (isSigmaTy actual || isForAllTy actual)
                   && any (hasNativeDependentExpr . Core.Type) tys
             known_callback
               = isRankedRetainedCallback actual
                   && maybe False isRetainedCallbackRho expected
       ; if (known_value || known_callback) && all noQuickLookVars tys
         then if known_value
              then return NoQL
              else do { scoped <- tcRetainedInstScope tys
                      ; return (if scoped then NoQL else DoQL) }
         else return DoQL }
tcRetainedKnownHeadQL _ do_ql _ _ _ _ = return do_ql

-- | Accept a complete call telescope with declared higher-rank inputs.
-- See Note [Known native variables with ImpredicativeTypes].
isGroundRetainedCallType :: Type -> Bool
isGroundRetainedCallType ty
  | FunTy flag ma mult arg res <- coreFullView ty
  , isVisibleFunArg flag
  = isTauTy ma && isTauTy mult
    && (isRetainedValueType arg || isRankedRetainedArgument arg)
    && isGroundRetainedCallType res
  | otherwise
  = isRetainedValueType ty || isRankedRetainedContainer ty

-- | Recognize an algebraic result with a ranked retained type argument.
-- See Note [Ground retained applications].
isRankedRetainedContainer :: Type -> Bool
isRankedRetainedContainer ty
  | TyConApp tc args <- coreFullView ty
  , isAlgTyCon tc, not (isClassTyCon tc)
  , length args == tyConArity tc
  , typeKind ty `eqType` liftedTypeKind
  = any isRankedRetainedCallback args
    && all (\arg -> isRetainedValueType arg || isRankedRetainedCallback arg) args
  | otherwise
  = False

isRankedRetainedCallback :: Type -> Bool
isRankedRetainedCallback ty
  = case tcRetainedInstTelescope ty of
      (_ : _, _, rho) -> isRetainedCallbackRho rho
      _              -> False

isRetainedCallbackRho :: Type -> Bool
isRetainedCallbackRho ty
  | DepFunTy{} <- coreFullView ty = isRetainedValueType ty
  | otherwise                    = False

-- | Recognize a declared retained callback or a container for it.
isRankedRetainedArgument :: Type -> Bool
isRankedRetainedArgument ty = isRankedRetainedCallback ty || isRankedRetainedContainer ty

-- | Defer a nested call with a fixed ordinary result and a ranked retained input.
-- See Note [Quick Look and ordinary argument checking].
isGroundRankedRetainedCall :: Int -> Type -> Bool
isGroundRankedRetainedCall arity ty
  | has_ranked_input arity ty
  , Just result <- tcRetainedOrdinaryResult arity ty
  = supportsQuickLookType result && noQuickLookVars ty
      && isGroundRetainedCallType ty
  | otherwise = False
  where
    has_ranked_input n ty
      | n > 0, Just (arg, res) <- splitVisibleFunTy_maybe ty
      = isRankedRetainedArgument arg || has_ranked_input (n-1) res
      | otherwise = False

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

A complete call telescope can also declare a polymorphic retained parameter.
If the head has no type variables to instantiate, use the ordinary argument
checker for that call. It pushes the parameter's type into its operand and
skolemises it. When that operand is a bare name with a retained telescope,
instantiate it predicatively against the resulting retained type. Check its
scope and keep the complete subsumption check and dictionary obligations.

The call-telescope check permits a retained polytype or its algebraic
container at a declared argument. For example, consume can supply
Maybe Callback to its operand Just reflect. The constructor then uses
the expected result to choose Callback. See Note [Ground retained applications].
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

finishApp :: TcM ()                 -- Saved constraints and result Quick Look
          -> QLFlag
          -> HsExpr GhcRn           -- The whole application
          -> SrcSpan                -- Span for the /head/ of the application
          -> HsExpr GhcRn           -- The head of the application (renamed)
          -> HsExpr GhcTc           -- The head of the application (typechecked)
          -> [HsExprArg 'TcpInst]   -- Arguments
          -> TcRhoType -> ExpRhoType
          -> TcM (HsExpr GhcTc)
-- Instantiation and Quick Look are done.  Now do typechecking,
-- of both the result type and the arguments, before rebuilding.
finishApp = finishAppWithScopedCache [] []

-- | Finish an application below any generated runtime field bindings.
-- See Note [Opening a package found by Quick Look].
finishAppWithScopedCache :: [Name] -> RetainedArgCache -> TcM () -> QLFlag
                         -> HsExpr GhcRn -> SrcSpan -> HsExpr GhcRn -> HsExpr GhcTc
                         -> [HsExprArg 'TcpInst] -> TcRhoType -> ExpRhoType
                         -> TcM (HsExpr GhcTc)
finishAppWithScopedCache fields cache prepare do_ql rn_expr fun_loc rn_fun tc_fun inst_args
                        app_res_rho exp_res_ty
  | DoQL <- do_ql
  , null fields, null cache
  , [arg@EValArgQL{eaql_loc_span = loc, eaql_arg_ty = scaled@(Scaled mult domain)
                  , eaql_larg = original, eaql_res_rho = actual}]
       <- filter (not . wrapper) inst_args
  , Just (tc, _) <- tcSplitTyConApp_maybe domain
  , isGenerativeTyCon tc Nominal
  , isOneTy mult || isManyTy mult
  = do { actual <- liftZonkM (zonkTcType actual)
       ; outer_result <- case exp_res_ty of
           Check result -> noQuickLookVars <$> liftZonkM (zonkTcType result)
           Infer{} -> return True
       ; case coreFullView actual of
           ExistsTy ImplicitExists _ _ | noQuickLookVars actual, outer_result
             -> do { EValArg{ea_arg = checked} <-
                       tcValArg do_ql 1 (rn_fun, fun_loc)
                         arg{eaql_arg_ty = Scaled mult actual, eaql_encl = True}
                   ; result_ty <- expTypeToType exp_res_ty
                   ; tcOpenImplicitPackageAt mult (getLocA original) actual checked result_ty $ \payload ->
                       do { qlUnify (varType payload) domain
                          ; let replace EValArgQL{} = EValArg loc scaled original
                                replace arg = arg
                                payload_cache = [(1, (nlHsVar payload, varType payload, zeroUE))]
                          ; finishAppBody fields payload_cache prepare do_ql rn_expr fun_loc rn_fun
                              tc_fun (map replace inst_args) app_res_rho (mkCheckExpType result_ty) } }
           _ -> ordinary }
  | otherwise = ordinary
  where
    wrapper EWrap{} = True
    wrapper _ = False
    ordinary = finishAppBody fields cache prepare do_ql rn_expr fun_loc rn_fun tc_fun
                             inst_args app_res_rho exp_res_ty

{- Note [Opening a package found by Quick Look]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In firstCell (id x), where x has type exists n. Vec n a, Quick Look
has already instantiated id at the package type. The boxed vector
domain asks for its payload. Use the cached result before argument
subsumption compares the quantified type directly with Vec.

Resume the original argument once at its cached package type. This
restores its saved usage and constraints, including any checking of
arguments that Quick Look skipped. Open the result under a fresh witness.
Quick Look can instantiate the outer reader's remaining variables
before their level is fixed. Emit the reader's saved obligations under
the opening. Finish its result and argument checks there. The outward
result type must have its type variables at ordinary levels. Allocate
an inference metavariable outside the opening when the result is inferred.
Otherwise Quick Look could assign the hidden witness to an outward
instantiation variable, whose level is infinity.

Require one cached value argument, a generative formal head and a
concrete multiplicity. No second operand or written type argument needs
to be moved into that scope. The cached package must have no unresolved
Quick Look variables. Retain the original checking path without such a
cache. In particular, do not enable Quick Look on any additional calls.
-}

-- | Check the instantiated result and arguments, then rebuild the call.
finishAppBody :: [Name] -> RetainedArgCache -> TcM () -> QLFlag
              -> HsExpr GhcRn -> SrcSpan -> HsExpr GhcRn -> HsExpr GhcTc
              -> [HsExprArg 'TcpInst] -> TcRhoType -> ExpRhoType
              -> TcM (HsExpr GhcTc)
finishAppBody fields cache prepare do_ql rn_expr fun_loc rn_fun tc_fun inst_args
              app_res_rho exp_res_ty
  = do { prepare
       ; case do_ql of
           NoQL -> return ()
           DoQL -> do { rejectNativeQuickLook app_res_rho
                      ; setSrcSpan fun_loc $ rejectNativeQLInstArgs inst_args }

         -- See Note [Introducing an implicit existential].
       ; (app_res_rho, scope_wrap) <- case fields of
           [] -> return (app_res_rho, idHsWrapper)
           _ -> do { Reduction co reduced <-
                       tcCloseRuntimeScope fields (exprCtOrigin rn_expr) app_res_rho
                   ; return (reduced, mkWpCastN co) }
       ; (result_expected, pack) <- implicitResult do_ql rn_expr app_res_rho exp_res_ty

         -- Step 4.1: monomorphise un-unified QuickLook instantiation variables
         -- Step 4.2: zonk app_res_rho to expose the polymorphism hidden under
         --           instantiation variables in `app_res_rho`
       ; app_res_rho <- case do_ql of
           NoQL -> return app_res_rho
           DoQL -> liftZonkM $
                   do { qlMonoInstArgs inst_args
                      ; zonkTcType app_res_rho }

         -- Step 5: subsumption check against the expected type
         -- See Note [Unify with expected type before typechecking arguments]
       ; branch_co <- case (do_ql, result_expected) of
           (NoQL, Check expected)
             | isTauTy expected, not (anyFreeVarsOfType (const True) expected)
             -> tcRetainedResultCoercion inst_args app_res_rho
           _ -> return (mkNomReflCo app_res_rho)
       ; res_wrap <- checkResultTy rn_expr (tc_fun, fun_loc) inst_args
                                   (coercionRKind branch_co) result_expected

         -- Step 6: Typecheck the arguments
       ; case do_ql of
           NoQL | null cache, classHasUnmatchableReceiver tc_fun
                , all valueOrWrapper inst_args ->
             void (seedClassReceiver NoQL (rn_fun, fun_loc) (valueDomains inst_args))
           _ -> return ()
       ; tc_args <- tcValArgsWithCache cache do_ql (rn_fun, fun_loc) inst_args

         -- Step 7: Horrible special case for tagToEnum#.
       ; res_expr <- if isTagToEnum tc_fun
                     then tcTagToEnum tc_fun tc_args app_res_rho
                     else return (rebuildHsApps tc_fun tc_args)

       ; traceTc "End tcApp }" (ppr tc_fun)
       ; return (pack (mkHsWrap (res_wrap <.> mkWpCastN branch_co <.> scope_wrap) res_expr)) }

-- | Refine the result of applying a retained function to the branch index.
-- See Note [Source retained case refinement] in GHC.Tc.Gen.Match.
tcRetainedResultCoercion :: [HsExprArg 'TcpInst] -> Type -> TcM CoercionN
tcRetainedResultCoercion args ty
  = do { indices <- catMaybes <$> mapM index_arg args
       ; if null indices then return (mkNomReflCo ty)
         else tcRetainedTypeCoercion (`elem` indices) ty }
  where
    index_arg (ERetainedArg DepFunVisible _ _ arg _ _)
      | HsVar _ name <- unLoc (stripParensLHsExpr arg)
      = tcLookupIdMaybe (unLocWithUserRdr name)
    index_arg _ = return Nothing

-- | Identify class selectors with an unmatchable-capable class parameter.
classHasUnmatchableReceiver :: HsExpr GhcTc -> Bool
classHasUnmatchableReceiver (HsVar _ (L _ selector))
  | Just cls <- isClassOpId_maybe selector
  = any unmatchable_parameter (classTyVars cls)
  where
    unmatchable_parameter tv
      | Just (ma, _, _) <- tcSplitFunTy_maybe (tyVarKind tv)
      = isTyVarTy ma || ma `eqType` unmatchableDataConTy
      | otherwise = False
classHasUnmatchableReceiver _ = False

-- | Recognize ordinary arguments for a declaration-only receiver hint.
valueOrWrapper :: HsExprArg 'TcpInst -> Bool
valueOrWrapper EValArg{} = True
valueOrWrapper EWrap{} = True
valueOrWrapper ETypeArg{} = True
valueOrWrapper _ = False

-- | Preserve positions of checked arguments after type applications.
valueDomains :: [HsExprArg 'TcpInst] -> [(Int, LHsExpr GhcRn, Type)]
valueDomains = go 1
  where
    go _ [] = []
    go pos (EValArg{ea_arg = arg, ea_arg_ty = Scaled _ expected} : rest) =
      (pos, arg, expected) : go (pos + 1) rest
    go pos (ETypeArg{ea_loc_span = l} : rest)
      | not (isGeneratedSrcSpan (locA l)) = go (pos + 1) rest
    go pos (_ : rest) = go pos rest

-- | Learn a closed unmatchable class receiver before checking its callback.
-- See Note [Unmatchable class receivers].
seedClassReceiver :: QLFlag -> (HsExpr GhcRn, SrcSpan)
                  -> [(Int, LHsExpr GhcRn, Type)] -> TcM Bool
seedClassReceiver do_ql rn_head = go []
  where
    go _ [] = return False
    go callbacks ((pos, arg, expected) : rest) =
      do { selected <- if null callbacks
                       then return False
                       else addArgCtxt pos rn_head arg $
                              seed_receiver callbacks arg expected
         ; callbacks' <- if isJust (tcSplitFunTy_maybe expected)
                         then do { ty <- liftZonkM (zonkTcType expected)
                                 ; return (ty : callbacks) }
                         else return callbacks
         ; selected_rest <- go callbacks' rest
         ; return (selected || selected_rest) }

    seed_receiver callbacks arg expected = fmap isJust $ runMaybeT $ do
      (tc, indices) <- MaybeT (tcSplitTyConApp_maybe <$> liftZonkM (zonkTcType expected))
      guard (isAlgTyCon tc)
      let shared f = any (elemVarSet f . tyCoVarsOfType) callbacks
      [(f, kind)] <- return
        [ (f, kind) | TyVarTy f <- indices, eligible f
                    , Just kind@(ma, _, _) <- [tcSplitFunTy_maybe (tyVarKind f)]
                    , isTyVarTy ma || ma `eqType` unmatchableDataConTy
                    , shared f ]
      open <- liftMaybeT (isUnfilledMetaTyVar f)
      guard open
      actual <- declared_receiver 0 arg
      (outer, outer_args) <- liftMaybe (tcSplitTyConApp_maybe actual)
      guard (any family_prefix outer_args)
      (actual_tc, actual_indices) <- if outer == tc
        then return (outer, outer_args)
        else do
          guard (isTypeFamilyTyCon outer)
          envs <- liftMaybeT tcGetFamInstEnvs
          reduction <- liftMaybe (reduceTyFamApp_maybe envs Nominal outer outer_args)
          liftMaybe (tcSplitTyConApp_maybe (reductionReducedType reduction))
      guard (tc == actual_tc)
      [target] <- return
        [ t | (TyVarTy v, t) <- zip indices actual_indices, v == f, family_prefix t ]
      guard (not (hasNativeDependentExpr (Core.Type target)))
      (target_ma, Scaled target_mult target_domain, target_result) <-
        liftMaybe (tcSplitFunTy_maybe (typeKind target))
      let (ma, Scaled mult domain, result) = kind
      guard (target_ma `eqType` unmatchableDataConTy && mult `eqType` target_mult
             && domain `eqType` target_domain && result `eqType` target_result)
      compatible <- case getTyVar_maybe ma of
        Just tv | eligible tv -> liftMaybeT (isUnfilledMetaTyVar tv)
        _ -> return (ma `eqType` target_ma)
      guard compatible
      liftMaybeT $ case do_ql of
        NoQL -> void (unifyType Nothing target (mkTyVarTy f))
        DoQL -> qlUnify target (mkTyVarTy f)

    declared_receiver n (L _ (HsPar _ fun)) = declared_receiver n fun
    declared_receiver n (L _ (HsApp _ fun _)) = declared_receiver (n + 1) fun
    declared_receiver n arg = do
      (_, L _ (HsVar _ lname)) <- liftMaybe (retainedNamedCallback arg)
      ident <- MaybeT (tcLookupIdMaybe (unLocWithUserRdr lname))
      let ty = varType ident
      guard (isTauTy ty && noFreeVarsOfType ty)
      liftMaybe (tcRetainedOrdinaryResult n ty)

    eligible tv = isTcTyVar tv && isMetaTyVar tv &&
      case do_ql of
        NoQL -> not (isQLInstTyVar tv)
        DoQL -> isQLInstTyVar tv

    liftMaybe :: Maybe a -> MaybeT TcM a
    liftMaybe = MaybeT . pure

    family_prefix t
      | Just (tc, args) <- tcSplitTyConApp_maybe t = isUnsaturatedUserFamily tc args
      | otherwise = False

{- Note [Unmatchable class receivers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider mapIndexed (fmap not) familyInput. The class method checks its
callback against f Bool -> f Bool. If f is unknown, the ordinary check
can select a matchable f before it checks familyInput :: Batch (Pick True).

Check only classes with a parameter that can have an unmatchable arrow
kind. Use the receiver's closed declared type when it contains a family
prefix. For makeReceiver (), peel the visible arrows of the monomorphic
producer's declared type. This reads no producer operands and permits no
type instantiation or dependent arrows. Look through at most one nominal
family equation to expose the same
algebraic constructor. Avoid recursive reduction in this optional step.
An earlier callback must use the same receiver index. Unify that index
with the partial unmatchable family. The ordinary receiver check supplies
the family coercion and checks the other indices. Its context can contain
witnesses from opening earlier implicit operands. Keep checking terms and
recording usage in source order. For Quick Look, use the same hint on
the already instantiated visible telescope before looking at the callback.
Fill only its instantiation variables. Zonk the callback type after the
hint so an application of that index takes the family path in qlUnify.
The callback already had a visible arrow, so its outer shape and the
guardedness test stay the same. Continue with the original Quick Look
order and cache.
-}

-- | Introduce a dot package when the application has a fixed payload shape.
-- See Note [Introducing an implicit existential].
implicitResult :: QLFlag -> HsExpr GhcRn -> TcRhoType -> ExpRhoType
               -> TcM (ExpRhoType, HsExpr GhcTc -> HsExpr GhcTc)
implicitResult do_ql rn_expr actual expected@(Check wanted)
  | ExistsTy ImplicitExists _ _ <- coreFullView wanted
  = do { payload <- liftZonkM (zonkTcType actual)
       ; schema <- liftZonkM (zonkTcType wanted)
       ; impred <- xoptM LangExt.ImpredicativeTypes
       ; ds_flag <- getDeepSubsumptionFlag
       ; let quick = impred && supportsQuickLookType schema && supportsQuickLookType payload
                            && noQuickLookVars schema
                            && case do_ql of
                                 DoQL -> True
                                 NoQL -> noQuickLookVars payload
       ; case coreFullView payload of
           TyConApp{} -> pack quick payload schema
           AppTy{} -> pack quick payload schema
           FunTy{} | Deep{} <- ds_flag -> pack False payload schema
                   | otherwise         -> pack quick payload schema
           ExistsTy ExplicitExists _ _
             | NoQL <- do_ql
             , Just (witness, inner) <- matchMixedExistsResult schema payload
             -> return (mkCheckExpType inner,
                        \expr -> XExpr (NativeExistsPack schema witness (noLocA expr)))
           _ -> return (expected, id) }
  where
    pack quick payload schema
      | ExistsTy ImplicitExists binder body <- coreFullView schema
      = do { witness <- if quick
               then case (coreFullView body, coreFullView payload) of
                      (FunTy{}, FunTy{})
                        -- See Note [Matching a witness below forall].
                        | Just exact <- matchExactPolytypeWitness binder body payload
                        , ForAllTy{} <- coreFullView exact
                        -> return exact
                      _ -> newMetaTyVarTyAtLevel QLInstVar (tyVarKind binder)
               else do { retained <- runMaybeT $
                           matchRetainedExistsWitness (exprCtOrigin rn_expr) binder body payload
                       ; maybe (newFlexiTyVarTy (tyVarKind binder)) return retained }
           ; let payload_ty = substTyWith [binder] [witness] body
           ; (expected', inner) <- pack quick payload payload_ty
           ; when quick $
               do { rejectNativeQuickLook witness
                  ; liftZonkM (qlMonoTcType witness) }
           ; return (expected', \expr -> XExpr (NativeExistsPack schema witness (noLocA (inner expr)))) }
      | otherwise
      = do { when quick (qlUnify schema payload)
           ; return (mkCheckExpType schema, id) }
implicitResult _ _ _ expected = return (expected, id)

-- | Choose a dot witness for an already checked bar package.
-- See Note [Introducing an implicit existential].
matchMixedExistsResult :: Type -> Type -> Maybe (Type, Type)
matchMixedExistsResult schema actual
  | ExistsTy ImplicitExists outer formal <- coreFullView schema
  = do { ([witness], [inner]) <- matchBarPackageWitnesses [outer] [(formal, actual)]
       ; return (witness, inner) }
  | otherwise = Nothing

-- | Match free template variables across explicit existential packages.
-- See Note [Implicit opening for an explicit package argument].
matchBarPackageWitnesses :: [TyVar] -> [(Type, Type)] -> Maybe ([Type], [Type])
matchBarPackageWitnesses binders packages
  | all supportsQuickLookType types
  , all noQuickLookVars types
  = do { templates <- traverse barPackageSignature formals
       ; targets <- traverse barPackageSignature actuals
       ; let bind_outer tv _ | tv `elem` binders = BindMe
                             | otherwise = DontBindMe
             empty_subst = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes (templates ++ targets)))
             actual_fvs = tyCoVarsOfTypes actuals
       ; subst <- foldM (\s (template, target) -> tcMatchTyX_BM bind_outer s template target)
                        empty_subst (zip templates targets)
       ; witnesses <- traverse (Subst.lookupTyVar subst) binders
       ; guard (all (`elemVarSet` actual_fvs) (tyCoVarsOfTypesList witnesses))
       ; let kinds = map (Subst.substTy subst . tyVarKind) binders
             inners = map (Subst.substTy subst) formals
       ; guard (eqTypes (map typeKind witnesses) kinds && eqTypes inners actuals)
       ; return (witnesses, inners) }
  | otherwise = Nothing
  where
    (formals, actuals) = unzip packages
    types = formals ++ actuals

-- | Compare the binder kind and body without changing the original mode.
barPackageSignature :: Type -> Maybe Type
barPackageSignature ty
  | ExistsTy ExplicitExists binder body <- coreFullView ty
  = Just (mkTyCoForAllTy unmatchableDataConTy binder (Invisible SpecifiedSpec) body)
  | otherwise = Nothing

-- | Select a higher-rank witness from equal invariant result types.
-- See Note [Implicit existential lambda payloads] in GHC.Tc.Gen.Expr.
matchPolytypeExistsWitness :: TyVar -> Type -> Type -> Maybe Type
matchPolytypeExistsWitness binder formal actual
  | TyConApp formal_tc _ <- coreFullView formal
  , TyConApp actual_tc _ <- coreFullView actual
  , formal_tc == actual_tc
  , isGenerativeTyCon formal_tc Nominal
  = matchExactPolytypeWitness binder formal actual
  | otherwise = Nothing

-- | Match one fixed polytype witness without binding any other variables.
matchExactPolytypeWitness :: TyVar -> Type -> Type -> Maybe Type
matchExactPolytypeWitness binder formal actual
  | all supportsQuickLookType [tyVarKind binder, formal, actual]
  , all noQuickLookVars [tyVarKind binder, formal, actual]
  = do { subst <- tcMatchTyX_BM bind_witness empty_subst formal actual
       ; witness <- Subst.lookupTyVar subst binder
       ; guard (not (isTauTy witness)
                && eqType (typeKind witness) (tyVarKind binder)
                && eqType (substTyWith [binder] [witness] formal) actual)
       ; return witness }
  | otherwise = Nothing
  where
    empty_subst = Subst.mkEmptySubst (mkInScopeSet (tyCoVarsOfTypes [formal, actual]))
    bind_witness tv _ | tv == binder = BindMe
                      | otherwise = DontBindMe

{- Note [Matching a witness below forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Under shallow subsumption, a declared payload can determine a polytype
inside a callback's forall. For example, to pack

  (forall r. (PolyId -> r) -> Maybe r) -> Int

as exists t. (forall r. (t -> r) -> Maybe r) -> Int, choose
PolyId = forall a. a -> a.

Quick Look stops at a forall because it cannot track the bound variables.
See (UQL4) in Note [QuickLook unification].
For this match, both payload types have no Quick Look variables.
The Core matcher renames the forall binders and rejects a substitution
that mentions a variable bound inside the target. A witness
forall a. a -> r therefore cannot refer to the callback's r.
See (CU2) in Note [The Core unifier] in GHC.Core.Unify.

Check the witness kind and compare the complete substituted payload
with the declared type. Then check the original expression as usual.
-}

-- | Match a stored index without assigning its local Id to a metavariable.
-- See Note [Introducing an implicit existential].
matchRetainedExistsWitness :: CtOrigin -> TyVar -> Type -> Type -> MaybeT TcM Type
matchRetainedExistsWitness ct_origin binder body actual = do
  guard (hasNativeDependentExpr (Core.Type actual))
  loc <- liftMaybeT $ getCtLocM ct_origin (Just TypeLevel)
  view <- liftMaybeT $ reductionReducedType <$> tcRewriteFamilyAxioms loc actual
  witness <- MaybeT $ return $ do
    subst <- matchRetainedAppType [binder] Subst.emptySubst body view
    Subst.lookupTyVar subst binder
  guard (hasNativeDependentExpr (Core.Type witness)
         && eqType (typeKind witness) (tyVarKind binder)
         && eqType (substTyWith [binder] [witness] body) view)
  scoped <- liftMaybeT (tcRetainedInstScope [witness])
  guard scoped
  return witness

{- Note [Introducing an implicit existential]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check the application result at the dot payload type before packing it.
Choose a witness for each consecutive dot binder in order. Substitute
each witness through the remaining schema before checking the next binder
kind. Check the final payload once and pack it from the inside out.
See Elab-Gen in An Existential Crisis Resolved.

With ImpredicativeTypes the witness itself can be a polytype. For
`exists t. Box t`, a payload of type `Box (forall a. a -> a)` determines
that full type as the witness. Follow GenImpredicative in Figure 10 of the
paper. Allocate the instantiation variables in telescope order, then
Quick Look unifies the final payload type with the application's type.
The fresh variables are local to this check. Monomorphise each one on
the way out, including witnesses absent from the payload. Check the
zonked witness before traversing it. finishAppWithScopedCache still checks
the actual result type against the instantiated payload.

Use the checked package type before Step 4 monomorphises the application
variables. For example, check Duo id calls against exists t. Duo t (Calls t),
where calls :: Calls (forall a. a -> a). Quick Look at the arguments finds
the type of calls. Matching the result then fixes both the witness and
the type of id. finishAppWithScopedCache also handles nested applications after
tcValArg has restored the enclosing constraints, multiplicity and usage.

The package schema must have no free Quick Look variables. Both types and
their coercions must support Quick Look. Other packages use ordinary
witness inference. See Note [Retained types and Quick Look].

Quick Look assumes an invariant function arrow (section 5.8 of its paper).
With DeepSubsumption the domain and result of an outer arrow can need
different subsumption checks. For example, a payload of type
`((forall a. a) -> Bool) -> Int` fits `exists t. (t -> Bool) -> t`
at t = Int. Equating the domains first would fix t to the polytype.
Use ordinary witness inference for outer arrows in this mode. Type
constructor applications still compare their arguments for equality.

Usually the solver can fill a fresh witness metavariable. A payload that
already mentions a retained index needs an immutable choice instead.
See Note [Rigid retained indices] in GHC.Tc.Utils.Unify.

Match the body against the result type with only the existential binder
as a template. Reduce type families before matching. Check the selected
witness in the current scope. The match does not supply evidence. The
ordinary application checker still checks subsumption and every argument.

For `exists a. exists b | (a, b)`, the checked bar package may already fix
the dot witness. Match its binder kind and body using a scoped forall
template. Keep the bar binder out of the selected witness. Substitute the
witness into the original bar type and check nominal equality of the whole
type. Store the original modes in Core. The comparison supplies a candidate
witness only and does not change the treatment of existentials in unification.
-}

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

tcValArg do_ql pos rn_head (ERetainedArg DepFunVisible loc (Scaled mult ty) arg res_ty co)
  = do { let checked_ty = maybe ty coercionLKind co
       ; scoped <- tcScopedClassOperand checked_ty arg
       ; checked <- case scoped of
           Just (selector, inst, dict, _, operand) ->
             do { checked_operand <- tcValArgExpr do_ql pos rn_head loc (Scaled mult inst) operand
                ; let wrapper = mkWpEvApps [EvExpr (Core.Var dict)] <.> mkWpTyApps [inst]
                      method = noLocA (mkHsWrap wrapper (mkHsVar (noLocA selector)))
                ; return (mkHsApps method [checked_operand]) }
           _ -> tcValArgExpr do_ql pos rn_head loc (Scaled mult checked_ty) arg
       ; return (ERetainedArg DepFunVisible loc noExtField
                   (maybe checked (`mkLHsWrapCo` checked) co) res_ty noExtField) }
tcValArg _ pos rn_head (ERetainedArg DepFunInvisible loc (Scaled mult ty) arg res_ty co)
  = addArgCtxt pos rn_head arg $
    do { let checked_ty = maybe ty coercionLKind co
       ; checked <- tcScalingUsage mult $
                     traverse (\e -> tcPolyExpr e (mkCheckExpType checked_ty)) arg
       ; return (ERetainedArg DepFunInvisible loc noExtField
                   (maybe checked (`mkLHsWrapCo` checked) co) res_ty noExtField) }

tcValArg do_ql pos rn_head (EValArg loc ty arg)
  = EValArg loc noExtField <$> tcValArgExpr do_ql pos rn_head loc ty arg

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
               do { -- Emit saved-up usages /under/ the tcScalingUsage.
                    -- See (QLA5) in Note [Quick Look at value arguments]
                    tcEmitBindingUsage head_ue
                  ; let prepare = do
                          -- See (QLA4) in Note [Quick Look at value arguments].
                          emitConstraints wanted
                          unless arg_influences_enclosing_call $
                            qlUnify app_res_rho exp_arg_rho
                  ; finishApp prepare DoQL rn_expr arg_head_loc rn_fun tc_fun inst_args app_res_rho
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

-- | Check a written visible value at its argument type and multiplicity.
tcValArgExpr :: QLFlag -> Int -> (HsExpr GhcRn, SrcSpan) -> HsExprLoc
             -> Scaled TcSigmaTypeFRR -> LHsExpr GhcRn -> TcM (LHsExpr GhcTc)
tcValArgExpr do_ql pos rn_fun_head lspan sc_arg_ty larg@(L arg_loc arg)
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
           NoQL | callback_enabled -> tcRetainedCallbackArgs [] [(1, Scaled mult exp_arg_ty, larg)]
                                                            Subst.emptySubst []
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
       ; return (L arg_loc arg') }

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
tcInstFunWithExpectedCache = tcInstFunWithPrefix quickLookArg False []

-- | Resume instantiation after a checked prefix, or check that prefix only.
-- A prefix-only call stops before instantiating the result. Retained RULEs
-- may include value operands before a written type argument.
-- See Note [Ground retained applications].
tcInstFunWithPrefix :: QuickLookArg -> Bool -> [HsExprArg 'TcpInst]
                    -> Maybe (TcRef (Maybe RetainedArgCache))
                    -> Maybe TcRhoType -> RetainedInstPlan -> QLFlag -> Bool
                    -> (HsExpr GhcRn, SrcSpan) -> HsExpr GhcTc
                    -> TcSigmaType -> [HsExprArg 'TcpRn]
                    -> TcM ([HsExprArg 'TcpInst], TcSigmaType)
tcInstFunWithPrefix look_arg prefix_only prefix vta_cache expected_rho inst_plan do_ql inst_final
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
       ; res@(_, fun_ty) <- go inst_plan fun_origin (1 + prefix_values) (reverse prefix) fun_sigma rn_args
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
    n_val_args = prefix_values + count isHsValArg rn_args
    prefix_values = count consumed_value prefix
    consumed_value EValArg{} = True
    consumed_value ERetainedArg{ ea_retained_vis = DepFunVisible } = True
    consumed_value _ = False

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
     = do { matched <- tcRetainedResultInstWithCasts do_ql expected_rho args tc_fun fun_ty
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
                ; choices <- case inferred of
                    Just _ -> return inferred
                    Nothing -> tcRetainedPartialResultPlan do_ql expected_rho args tc_fun fun_ty
                ; go1 choices fun_orig pos acc fun_ty args } }

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
      = do { selected <- case plan of
                Just (_ : _) -> return plan
                _ | not prefix_only, not no_tvs
                  , DepDictTy{} <- coreFullView body2
                  -- See Note [Applying a scoped dictionary].
                  -> do { sigma <- liftZonkM (zonkTcType fun_ty)
                        ; choice <- tcRetainedKnownArgPlan (tcRetainedDeclaredArgType pure)
                                                            Nothing tc_fun sigma args
                        ; case choice of
                            Just _ -> return choice
                            Nothing -> failWithTc $ TcRnRetainedQuantificationError
                                                     (RetainedSolverType fun_ty) }
                _ | NoQL <- do_ql, not prefix_only, pos > 1, not no_tvs
                  , DepFunTy DepFunVisible _ _ <- coreFullView body2
                  , arg@EValArg{} : _ <- args
                  -> do { sigma <- liftZonkM (zonkTcType fun_ty)
                        ; tcRetainedKnownArgPlan (tcRetainedDeclaredArgType tcRetainedDeclaredTypeView)
                                                 Nothing tc_fun sigma [arg] }
                _ -> return plan
           ; (wrap, fun_rho, rest_plan) <-
                -- setSrcSpan of the function: important for the class constraints
                -- that may be emitted from instantiating fun_sigma
                setSrcSpan fun_lspan $
                case selected of
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
    go1 plan fun_orig pos acc fun_ty (ERetainedArg _ loc _ arg _ _ : rest_args)
      | DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      = do { (runtime_arg, res_ty, arg_co) <-
               addArgCtxt (pos + count written_prefix_arg acc) rn_head arg $
               if isVacuousRetainedBinder binder body
               then return (arg, body, Nothing)
               else do { (operand, runtime_arg) <- tcRetainedOperand binder body arg
                       ; res <- retainedAppResult fun_ty binder operand
                       ; co <- retainedVariableCast operand runtime_arg
                       ; return (runtime_arg, res, co) }
           ; let checked = ERetainedArg DepFunInvisible loc (Scaled ManyTy (varType binder))
                                        runtime_arg res_ty arg_co
           ; go plan fun_orig pos (checked : acc) res_ty rest_args }
      | otherwise = addArgCtxt (pos + count written_prefix_arg acc) rn_head arg $
                    failWithTc (TcRnRetainedQuantificationError (RetainedExplicitArgument fun_ty))

    -- See Note [Inferring an invisible retained argument].
    go1 plan fun_orig pos acc fun_ty args@(EValArg { ea_arg = first } : _)
      | DepFunTy DepFunInvisible binder body <- coreFullView fun_ty
      = do { reflected <- tcInstInvisibleTypeWitness fun_orig Nothing fun_ty
           ; case reflected of
               Just (wrap, res) -> go plan fun_orig pos (addArgWrap wrap acc) res args
               Nothing -> infer_index binder body }
      where
        infer_index binder body
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

    -- See Note [Applying a scoped dictionary].
    go1 plan fun_orig pos acc fun_ty args
      | DepDictTy binder _ <- coreFullView fun_ty
      , not (null args), inst_fun args Inferred
      = do { givens <- liftZonkM . mapM zonkId =<< getLexicalGivens
           ; predicate <- liftZonkM (zonkTcType (varType binder))
           ; dict <- case filter (\d -> isNativeClassDictionaryId d
                                    && varType d `tcEqType` predicate) givens of
               [dict] -> return dict
               [] | noFreeVarsOfType predicate ->
                 do { defer <- goptM Opt_DeferTypeErrors
                    ; when defer $
                        failWithTc $ TcRnRetainedQuantificationError (RetainedSolverType fun_ty)
                    ; ev <- newWanted fun_orig Nothing predicate
                    ; emitSimple (mkNonCanonical ev)
                    ; return (ctEvEvId ev) }
               _ -> failWithTc $ TcRnRetainedQuantificationError (RetainedSolverType fun_ty)
           ; let evidence = Core.Var dict
                 wrap = mkWpEvApps [EvExpr evidence]
           ; go plan fun_orig pos (addArgWrap wrap acc) (applyTypeToArgs fun_ty [evidence]) args }
    go1 _ _ _ _ fun_ty _
      | DepDictTy{} <- coreFullView fun_ty
      = failWithTc $ TcRnRetainedQuantificationError (RetainedSolverType fun_ty)

    -- Rule IRESULT from Fig 4 of the QL paper; no more arguments
    go1 plan fun_orig pos acc fun_ty []
       | Just (_ : _) <- plan
       = pprPanic "tcInstFun: unused retained instantiations" (ppr plan)
       | inst_final
       , DepFunTy DepFunInvisible _ _ <- coreFullView fun_ty
       , Just expected <- expected_rho
       = do { reflected <- tcInstInvisibleTypeWitness fun_orig (Just expected) fun_ty
            ; case reflected of
                Just (wrap, res) -> go plan fun_orig pos (addArgWrap wrap acc) res []
                Nothing -> return (reverse acc, fun_ty) }
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
      | DepFunTy DepFunVisible binder body <- coreFullView fun_ty
      = do { (core_arg, runtime_arg) <- tcRetainedOperand binder body arg
           ; body_ty <- retainedAppResult fun_ty binder core_arg
           ; arg_co <- retainedVariableCast core_arg runtime_arg
           ; let arg' = ERetainedArg DepFunVisible ctxt (Scaled ManyTy (varType binder)) runtime_arg body_ty
                                         arg_co
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
           ; let arg_pos = pos + count written_prefix_arg acc
           ; selected <- case do_ql of
               DoQL | classHasUnmatchableReceiver tc_fun
                    , Just{} <- tcSplitFunTy_maybe (scaledThing arg_ty)
                    , Just domains <- receiverDomains (arg_pos + 1) res_ty rest_args
                    , not (null domains) ->
                 seedClassReceiver DoQL rn_head ((arg_pos, arg, scaledThing arg_ty) : domains)
               _ -> return False
           ; arg_ty <- if selected then liftZonkM (zonkScaledTcType arg_ty) else return arg_ty
           ; arg' <- look_arg do_ql arg_pos ctxt rn_head arg arg_ty
           ; let acc' = arg' : addArgWrap (mkWpCastN fun_co) acc
           ; go plan fun_orig (pos+1) acc' res_ty rest_args }

    -- Read only the visible domains that already follow this callback.
    receiverDomains _ _ [] = Just []
    receiverDomains pos ty (EValArg{ea_arg = arg} : rest)
      | Just (_, Scaled _ domain, result) <- tcSplitFunTy_maybe ty
      = ((pos, arg, domain) :) <$> receiverDomains (pos + 1) result rest
    receiverDomains pos ty (EWrap{} : rest) = receiverDomains pos ty rest
    receiverDomains _ _ _ = Nothing

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


-- | Recover an optional alias operand from its already checked RHS.
-- See Note [Optional typed let operands].
tcOptionalLetOperand :: LHsExpr GhcTc
                     -> TcM (Maybe (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)]))
tcOptionalLetOperand rhs = runMaybeT $ do
    result@(operand, _, _) <- go (unLoc rhs)
    _ <- liftMaybe (Core.retainedArgIndex operand)
    return result
  where
    go :: HsExpr GhcTc
       -> MaybeT TcM (Core.CoreExpr, [Id], [(EvVar, Id, Id, Maybe Id)])
    go expr = case expr of
      HsPar _ body -> go (unLoc body)
      ExprWithTySig _ body _ -> go (unLoc body)
      XExpr (ExpandedThingTc (HSE _ body)) -> go (unLoc body)
      XExpr (WrapExpr WpHole body) -> go body
      XExpr (WrapExpr wrap body)
        | Just (selector, domain, wanted) <- method_head wrap body -> do
            domain <- liftMaybeT (liftZonkM (zonkTcType domain))
            (sel, dfun, ev) <- method selector domain wanted
            target_name <- liftMaybe (sourceMethodTarget_maybe dfun sel)
            target <- MaybeT (tcLookupIdMaybe target_name) >>= certified
            guard (varType target `tcEqType` classMethodInstTy sel [domain])
            return (Core.Var target, [target], [(ev, dfun, sel, Just target)])
        | otherwise -> do
            (body', heads, guards) <- go body
            wrapped <- type_apps wrap body'
            return (wrapped, heads, guards)
      HsAppType ty fun _ -> do
        (fun', heads, guards) <- go (unLoc fun)
        applied <- type_apps (WpTyApp ty) fun'
        return (applied, heads, guards)
      HsApp _ fun arg -> do
        (fun', fun_heads, fun_guards) <- go (unLoc fun)
        (arg', arg_heads, arg_guards) <- go (unLoc arg)
        return (Core.App fun' arg', fun_heads ++ arg_heads, fun_guards ++ arg_guards)
      ExplicitList element_ty elements -> do
        element_ty <- liftMaybeT (liftZonkM (zonkTcType element_ty))
        values <- mapM (go . unLoc) elements
        let nil = Core.mkTyApps (Core.Var (dataConWorkId nilDataCon)) [element_ty]
            cons = Core.mkTyApps (Core.Var (dataConWorkId consDataCon)) [element_ty]
        return (foldr (\(value, _, _) rest -> Core.mkApps cons [value, rest]) nil values,
                concatMap (\(_, heads, _) -> heads) values,
                concatMap (\(_, _, guards) -> guards) values)
      HsVar _ (L _ occurrence) -> do
        occurrence <- liftMaybeT (liftZonkM (zonkId occurrence))
        if not (isExternalName (getName occurrence)) && isNativeOpaqueValueId occurrence
          then do
            thing <- MaybeT (tcLookupLcl_maybe (getName occurrence))
            case thing of
              ATcId { tct_id = actual } -> do
                actual <- liftMaybeT (liftZonkM (zonkId actual))
                guard (actual == occurrence && isNativeOpaqueValueId actual
                       && varType actual `tcEqType` varType occurrence)
                return (Core.Var actual, [], [])
              _ -> mzero
          else do
            owner <- certified occurrence
            return (Core.Var owner, [owner], [])
      XExpr (ConLikeTc (RealDataCon con)) -> return (Core.Var (dataConWorkId con), [], [])
      HsOverLit _ (OverLit { ol_ext = OverLitTc { ol_rebindable = False
                                              , ol_witness = witness
                                              , ol_type = result_ty }
                          , ol_val = HsIntegral literal })
        | let n = il_value literal, n >= 0
        , HsApp _ fun arg <- strip witness
        , XExpr (WrapExpr wrap body) <- strip (unLoc fun)
        , Just (selector, domain, wanted) <- method_head wrap body
        , HsLit _ (XLit (HsInteger _ actual integer_ty)) <- strip (unLoc arg)
        , actual == n, isIntegerTy integer_ty -> do
            domain <- liftMaybeT (liftZonkM (zonkTcType domain))
            result_ty <- liftMaybeT (liftZonkM (zonkTcType result_ty))
            guard (domain `tcEqType` result_ty)
            (sel, dfun, ev) <- method selector domain wanted
            (zero, succ_con) <- liftMaybe (sourceLiteralCons_maybe dfun sel)
            dflags <- liftMaybeT getDynFlags
            guard (n <= toInteger (maxBound :: Int)
                   && not (intGtLimit (fromInteger n) (reductionDepth dflags)))
            let build 0 = Core.Var (dataConWorkId zero)
                build k = Core.App (Core.Var (dataConWorkId succ_con)) (build (k - 1))
            return (build (fromInteger n :: Int), [], [(ev, dfun, sel, Nothing)])
      _ -> mzero

    strip :: HsExpr GhcTc -> HsExpr GhcTc
    strip (HsPar _ body) = strip (unLoc body)
    strip (XExpr (ExpandedThingTc (HSE _ body))) = strip (unLoc body)
    strip (XExpr (WrapExpr WpHole body)) = strip body
    strip expr = expr

    method_head :: HsWrapper -> HsExpr GhcTc -> Maybe (Id, Type, EvVar)
    method_head (WpCompose (WpEvApp (EvExpr (Core.Var wanted))) (WpTyApp domain)) body
      | HsVar _ (L _ selector) <- strip body = Just (selector, domain, wanted)
    method_head (WpEvApp (EvExpr (Core.Var wanted))) body
      | HsAppType domain fun _ <- strip body
      , HsVar _ (L _ selector) <- strip (unLoc fun) = Just (selector, domain, wanted)
    method_head _ _ = Nothing

    method :: Id -> Type -> EvVar -> MaybeT TcM (Id, Id, EvVar)
    method selector domain wanted = do
      selector <- liftMaybeT (liftZonkM (zonkId selector))
      domain <- liftMaybeT (liftZonkM (zonkTcType domain))
      wanted <- liftMaybeT (liftZonkM (zonkId wanted))
      cls <- liftMaybe (isClassOpId_maybe selector)
      case tcSplitSigmaTy (varType selector) of
        ([(_, tv)], [theta], rho)
          | let (_, _, result) = splitFunTys rho
          , result `tcEqType` mkTyVarTy tv
          , theta `tcEqType` mkClassPred cls [mkTyVarTy tv]
          , noFreeVarsOfType domain, typeKind domain `tcEqType` tyVarKind tv
          , varType wanted `tcEqType` mkClassPred cls [domain] -> return ()
        _ -> mzero
      insts <- liftMaybeT tcGetInstEnvs
      dfun_name <- case lookupUniqueInstEnv insts cls [domain] of
        Right (inst, []) -> return (getName (instanceDFunId inst))
        _ -> mzero
      env <- liftMaybeT getGblEnv
      guard (not (nameIsLocalOrFrom (tcg_semantic_mod env) dfun_name))
      dfun <- MaybeT (tcLookupIdMaybe dfun_name)
      guard (isDFunId dfun && varType dfun `tcEqType` varType wanted
             && noFreeVarsOfType (varType dfun))
      return (selector, dfun, wanted)

    certified :: Id -> MaybeT TcM Id
    certified actual = do
      env <- liftMaybeT getGblEnv
      owner <- liftMaybe (sourceTableOccurrence env actual)
      _ <- liftMaybe (sourceTableId_maybe owner)
      owner <- liftMaybeT (liftZonkM (zonkId owner))
      guard (varType owner `tcEqType` varType actual)
      return owner

    type_apps :: HsWrapper -> Core.CoreExpr -> MaybeT TcM Core.CoreExpr
    type_apps WpHole body = return body
    type_apps (WpTyApp ty) body = do
      ty <- liftMaybeT (liftZonkM (zonkTcType ty))
      return (Core.App body (Core.Type ty))
    type_apps (WpCompose outer inner) body = type_apps inner body >>= type_apps outer
    type_apps _ _ = mzero

    liftMaybe :: Maybe a -> MaybeT TcM a
    liftMaybe = MaybeT . pure

{- Note [Optional typed let operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary let RHS has already selected its syntax, type applications and
class dictionary Wanteds. Recover those exact choices without checking the
source again. Replace a method by a certified target only provisionally;
the caller must guard the alias equality with the original Wanted evidence.
A literal must retain its fromInteger witness and exact Integer payload.
Primitive literal short cuts supply no such evidence and are not candidates.

Read the current source-table owner without registering an equation use.
The caller checks rewrite rules before offering the alias equality and records
uses only if its proof is used. Reject unsupported wrappers and expressions
without a diagnostic.
Finally, retainedArgIndex checks the saturated canonical tree and its domains.
This operation emits no constraints, usage obligations or method checks.
-}

{- Note [Applying a scoped dictionary]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Use declared value domains to fix erased parameters before choosing any
dictionary. Use the declaration path also with ImpredicativeTypes.
Quick Look cannot assign a type with this private evidence prefix.
The declaration pass reads through the evidence prefix. It keeps the
dictionaries bound and cannot infer an argument from a selector
application in the result. A dictionary in an expected result must
match the identity and predicate of an actual lexical Given.

A signature can also have ordinary constraints and payload parameters.
Select the parameters of every dependent dictionary before opening it.
Instantiate remaining parameters in dependency order. For example, in
(Inspect r, Label a b) => foreach (x :: r) -> a -> Payload (ordering x),
the receiver fixes r. The ordinary solver can use the functional
dependency a -> b to determine b.

At the call, prefer the unique lexical Given with the exact predicate.
A Wanted for the same predicate can produce a fresh let-bound alias.
That alias would change the index and could escape its binder.

The Core index currently represents a selector on a variable operand.
Reject a constructor or other non-variable before substituting it.

A closed predicate with no direct Given can use an ordinary Wanted. Its
fresh evidence ID belongs to the existing evidence bindings. An ordinary
result can consume the indexed call while the ID is in scope. The
retained-index escape checks still apply if the ID occurs in the result.
Disallow deferred type errors on this path. The source prototype cannot
assert that an unsolved predicate supplies a class dictionary.

A constructor pattern can instantiate its dictionary with type metavariables
that the scrutinee has already fixed. Zonk the predicates before comparing
them. Keep the original evidence Id and reject competing lexical Givens
before asking the ordinary solver for a ground instance.
-}

-- | Resolve the index at the binder's checked scalar or function domain.
tcRetainedOperand :: Id -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedOperand binder body arg = do
  result@(core_arg, _) <- if isNativeIndexFunctionId binder
    then (,arg) <$> tcRetainedFunctionArg domain arg
    else tcRetainedArg domain arg
  case core_arg of
    Core.Var index | isNativeOpaqueValueId index -> return result
    _ | isScopedSelectorReceiver binder body -> setSrcSpanA (getLoc arg) $
          failWithTc (TcRnRetainedQuantificationError RetainedSelectorReceiver)
      | otherwise -> return result
  where
    domain = varType binder

-- | Reuse the nominal conversion of the exact written variable at value checking.
-- See Note [Retained argument application].
retainedVariableCast :: Core.CoreExpr -> LHsExpr GhcRn -> TcM (Maybe CoercionN)
retainedVariableCast (Core.Cast (Core.Var index) (SubCo co)) arg
  | HsVar _ name <- unLoc (stripParensLHsExpr arg)
  , unLocWithUserRdr name == getName index
  = if isNativeIndexFunctionId index then return (Just co)
    else do { givens <- getNativeGivens
            ; return (if anyFreeVarsOfCo (`elem` givens) co then Just co else Nothing) }
retainedVariableCast _ _ = return Nothing

-- | Use the same domain for index checking and codomain substitution.
-- See Note [Wildcards in retained operand domains].
retainedAppResult :: Type -> Id -> Core.CoreExpr -> TcM Type
retainedAppResult fun_ty binder arg
  | anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) (varType binder)
  = do { fun_ty' <- liftZonkM (zonkTcType fun_ty)
       ; return (applyTypeToArgs fun_ty' [arg]) }
  | otherwise = return (applyTypeToArgs fun_ty [arg])

-- | Check a value operand and specialise a class selector to its checked method.
-- See Note [Dictionary selection in retained arguments].
tcRetainedValueOperand :: (LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn))
                       -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedValueOperand resolve domain arg@(L loc original)
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
          scoped <- tcScopedClassOperand domain arg
          case scoped of
            Just (selector, inst, dict, index, _) ->
              pure (Core.mkApps (Core.Var selector) [Core.Type inst, Core.Var dict, Core.Var index], arg)
            Nothing -> do
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
              platform <- getPlatform
              case args of
                [operand]
                  | Just (zero, successor) <- sourceLiteralCons_maybe dfun sel
                  , HsOverLit _ (OverLit OverLitRn{ol_rebindable = False} literal)
                      <- unLoc (stripParensLHsExpr operand)
                  , Just (HsLit _ (XLit (HsInteger _ n _)))
                      <- shortCutLit platform literal integerTy
                  , n >= 0
                  -> do { (core, generated) <- buildRetainedNumeral dfun sel zero successor n
                        ; return (core, L loc (mkExpandedLExpr original generated)) }
                _ -> do
                  target_name <- maybe unchecked return (sourceMethodTarget_maybe dfun sel)
                  target <- tcLookupIdMaybe target_name >>= maybe unchecked return
                  unless (varType target `tcEqType` classMethodInstTy sel [domain]) unchecked

                  let target_head = L head_loc (HsVar noExtField (fmap (const (noUserRdr target_name)) lname))
                      runtime_arg = L loc (mkExpandedLExpr original (mkHsApps target_head args))
                  checked <- resolve runtime_arg
                  recordSourceMethodUse dfun sel target
                  wantRetainedMethod sel dfun
                  return checked
        _ -> ordinary
  | otherwise = ordinary
  where
    ordinary = resolve arg
    split_call :: LHsExpr GhcRn -> [LHsExpr GhcRn] -> (LHsExpr GhcRn, [LHsExpr GhcRn])
    split_call (L _ (HsPar _ e)) args = split_call e args
    split_call (L _ (HsApp _ f a)) args = split_call f (a:args)
    split_call (L _ (OpApp _ a op b)) args = split_call op (a:b:args)
    split_call e args = (e, args)

-- | Resolve a direct selector call to the exact Given used in its index.
-- See Note [Scoped Given indices].
tcScopedClassOperand :: Type -> LHsExpr GhcRn
                     -> TcM (Maybe (Id, Type, Id, Id, LHsExpr GhcRn))
tcScopedClassOperand result_ty call
  | HsApp _ method operand <- unLoc (stripParensLHsExpr call)
  , HsVar _ method_name <- unLoc (stripParensLHsExpr method)
  , HsVar _ operand_name <- unLoc (stripParensLHsExpr operand)
  = do { enabled <- xoptM LangExt.ExperimentalDependentTypes
       ; if not enabled then return Nothing else do
       { selector <- tcLookupIdMaybe (unLocWithUserRdr method_name)
       ; actual <- tcLookupLcl_maybe (unLocWithUserRdr operand_name)
       ; index_id <- case actual of
           Just ATcId{tct_id = index} -> Just <$> liftZonkM (zonkId index)
           _ -> return Nothing
       ; givens <- liftZonkM . mapM zonkId =<< getLexicalGivens
       ; return $ do
           sel <- selector
           index <- index_id
           guard (isNativeOpaqueValueId index)
           let inst = varType index
           (predicate, domain, result) <- Core.nativeClassSelectorTys sel inst
           guard (inst `tcEqType` domain && result `tcEqType` result_ty)
           [dict] <- pure (filter (\d -> varType d `tcEqType` predicate) givens)
           let core = Core.mkApps (Core.Var sel) [Core.Type inst, Core.Var dict, Core.Var index]
           _ <- Core.nativeClassSelectorIndex core
           pure (sel, inst, dict, index, operand) } }
tcScopedClassOperand _ _ = return Nothing

{- Note [Scoped Given indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The complete signature can refer to a direct class Given in the result
of a retained arrow. During application instantiation, use that same
Given to form the index. During argument checking, apply the selector
to the Given explicitly and check the value operand as usual.

An ordinary Wanted can solve to the right Given but produce a fresh
dictionary let binder. Keeping that alias in the result would allow a
let-bound index to escape. Do not make this alias equal to the Given.
Only a unique lexical Given of the selector's exact predicate qualifies.
Read the operand's instantiated type and multiplicity before matching a
constructor field. Use the original operand and evidence Ids.
This path does not look for superclasses or global instances.
-}

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
  ev <- newWanted (RetainedMethodOrigin name) Nothing (varType dfun)
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
  if isNaturalTy domain
    then do
      platform <- getPlatform
      wantRetainedMethod sel dfun
      return (mkNaturalExpr platform n, nlHsLit (HsNatural noExtField (mkIntegralLit n)))
    else do
      (zero, succ_con) <- maybe unverified return (sourceLiteralCons_maybe dfun sel)
      buildRetainedNumeral dfun sel zero succ_con n

-- | Build the index and runtime operand from a checked nonnegative conversion.
buildRetainedNumeral :: Id -> Id -> DataCon -> DataCon -> Integer
                     -> TcM (Core.CoreExpr, LHsExpr GhcRn)
buildRetainedNumeral dfun sel zero succ_con n = do
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

An explicit call to the same certified method can use the ordinary Integer
literal shortcut for its argument. Require a non-rebound literal that the
shortcut accepts at Integer. The outer method still needs the certificate
and the exact dictionary. A rebound literal or a computed Integer argument
can have a different meaning.

The wired-in Natural domain uses the ordinary boxed literal and the
DataKinds NumTyLit at the same kind. The standard Num Natural method maps a
nonnegative Integer to exactly that value. Require its dictionary before
using the literal shortcut. A local fromInteger cannot select this branch.
A different Given will fail the dictionary check.

A result signature such as Vec a 3 resolves its numeral independently through
FromNatural. Ordinary type equality checks that the two meanings agree.
Each successor adds one constructor. Check the reduction-depth budget before
allocating the tree, including for a literal too large to fit in an Int.

A complete monomorphic expression signature on the literal must denote the
same retained domain. Check it before elaborating the literal. A qualified
signature could introduce a Given and change the conversion. Keep the
original annotation in the expansion for diagnostics. The constructor tree
already has the checked domain. The ordinary operand check uses it directly.
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

Run this selection at each recursive operand's expected domain. For example,
in n * (m + k), select multiplication at the outer domain and addition at
the second argument's domain. The resolver continuation keeps the same local
binder environment. This is required when a lambda binds an index used by a
nested method call. Selection inspects the head but does not check operands.
The continuation checks the rewritten expression once. Its head is the exact
source target, so it cannot select the same class method again.

Check the target call and its rewrite rules before emitting the method
obligation. An invalid operand or rule can already explain the error.
Use the target for both the index and runtime. The expanded expression keeps the user's
operator and its source span. The later argument check must not resolve
a second dictionary. M81's source-call check verifies saturation and
operand kinds and preserves the lazy call through erasure.
-}

-- | Resolve a lexical callback or an exact function with checked equations.
-- See Notes [Checking a cast retained callback] and [Named retained callbacks].
tcRetainedFunctionArg :: Type -> LHsExpr GhcRn -> TcM Core.CoreExpr
tcRetainedFunctionArg domain arg = setSrcSpanA (getLoc arg) $
  case retainedSourceHead arg of
    Just (name, written)
      | isExternalName name -> do
          mb_owner <- tcLookupIdMaybe name >>= maybe (pure Nothing) tcLookupSourceTableId
          case mb_owner of
            Just owner -> instantiate_named owner written
            Nothing -> failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall name))
      | null written -> do
          thing <- tcLookupLcl_maybe name
          case thing of
            Just (ATcId { tct_id = actual }) -> liftZonkM (zonkId actual) >>= tcRetainedFunctionVar domain
            _ -> unsupported
    _ -> unsupported
  where
    unsupported :: TcM a
    unsupported = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument

    instantiate_named owner written = do
      expected <- liftZonkM (zonkTcType domain)
      owner <- liftZonkM (zonkId owner)
      (core, sigma) <- foldM (applyRetainedSourceType unsupported owner)
                             (Core.Var owner, varType owner) written
      let (binders, body) = tcSplitForAllInvisTyVars sigma
      choices <- chooseRetainedSourceTypes unsupported owner sigma (map snd binders) body expected
      let arg = Core.mkTyApps core choices
      case Core.nativeIndexFunctionArgIndex arg of
        Just index | typeKind index `tcEqType` expected -> return arg
        _ -> unsupported

{- Note [Named retained callbacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained callback can use a named function with checked source equations.
Resolve the certificate through the written occurrence's defining Id.
Record that use and its dependencies with tcLookupSourceTableId. This
checks the same rewrite rules as a saturated source call.

A unary source family takes the same erased arguments as its term owner.
Leave its value binder unsaturated at a function-valued retained argument.
Substituting the index for the callback saturates that family wherever the
callback was applied to a value. The U arrow prevents using the application
as evidence for the identity of either input. The ordinary argument check
keeps the original function occurrence and its selected type arguments.
A lexical variable still has its own index, even when its right-hand side
names a certified function. A lambda has no certificate at this admission.

The simplifier can substitute another lexical callback and preserve a strict
case on an opaque call. It cannot substitute a source family into that case
evidence. The latter needs a checked ordinary retained application which
records the source equations and the active rule policy. Keep the lexical
judgment for beta exposure, rule matching and strict case transport.
-}

-- | Check the nominal equality for the actual lexical callback.
tcRetainedFunctionVar :: TcType -> Id -> TcM Core.CoreExpr
tcRetainedFunctionVar expected actual
  | not (isExternalName (getName actual))
  , isNativeIndexFunctionId actual
  , isJust (nativeIndexFunctionTypeTys_maybe expected)
  = if actual_ty `tcEqType` expected then pure (Core.Var actual)
    else do
      co <- unifyType (Just (NameThing (idName actual))) actual_ty expected
      nominal <- liftZonkM (zonkCo co)
      if isReflCo nominal
        then pure (Core.Var (setVarType actual (coercionLKind nominal)))
        else do
          let range = Core.Cast (Core.Var actual) (SubCo nominal)
          case Core.nativeIndexFunctionArgIndex range of
            Just index | typeKind index `tcEqType` expected -> pure range
            _ -> mismatch
  | otherwise = failWithTc $ TcRnRetainedQuantificationError RetainedFunctionArgument
  where
    actual_ty = varType actual
    mismatch = failWithTc $ TcRnRetainedQuantificationError $
                 RetainedIndexArgumentCast actual_ty expected

{- Note [Checking a cast retained callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary caller can declare cmp :: Bool -> Bool -> Ordering while
the provider expects Bool -> Result 'True. A type family can also
determine the input type. The ordinary unifier must prove the nominal
equality of the arrows. Use its coercion for both the retained index
and the value argument. Both refer to the same local Id.

Unification can also fix erased metavariables in the expected arrow.
If the coercion zonks to reflexivity, keep the local Id at the resolved
type. The Core judgment for callback casts requires non-reflexive
evidence. See Note [Wildcards in retained operand domains].
-}

-- | Resolve a retained argument at its known domain without checking its value.
-- See Note [Retained argument application].
tcRetainedArg :: Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
tcRetainedArg domain arg
  = setSrcSpanA (getLoc arg) $
    do { domain <- constructor_domain domain arg
       ; result@(core_arg, _) <- resolve emptyNameEnv domain arg
       ; case Core.retainedArgIndex core_arg of
           Just index
             | typeKind index `tcEqType` domain -> return result
             | otherwise -> do { domain' <- liftZonkM (zonkTcType domain)
                               ; if typeKind index `tcEqType` domain'
                                 then return result else unsupported }
           _ -> unsupported }
  where
    -- See Note [Inferred domains of written retained constructors].
    constructor_domain domain arg
      | anyFreeVarsOfType flexible domain
      , case unLoc (stripParensLHsExpr arg) of
          HsVar _ name -> isDataOcc (nameOccName (unLocWithUserRdr name))
          _ -> True
      = do { actual <- tcRetainedConstructorArgType arg
           ; case actual of
               Just ty
                 | not (hasNativeDependentExpr (Core.Type ty))
                 , isJust (tcMatchTyX_BM bind Subst.emptySubst domain ty)
                 -> do { _ <- unifyType Nothing domain ty
                       ; liftZonkM (zonkTcType domain) }
               _ -> return domain }
      | otherwise = return domain
      where
        flexible tv = isTcTyVar tv && isMetaTyVar tv
        bind tv _ | flexible tv = BindMe
                  | otherwise   = DontBindMe

    resolve :: NameEnv Id -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
    resolve env expected original = do
      result@(core, checked) <- tcRetainedValueOperand (resolve_expr env expected) expected original
      case Core.splitNativeIndexedConApp core of
        Just _ | let actual = exprType core, not (actual `tcEqType` expected) -> do
          co <- unifyType Nothing actual expected
          return (Core.Cast core (SubCo co), checked)
        _ -> return result

    resolve_expr :: NameEnv Id -> Type -> LHsExpr GhcRn -> TcM (Core.CoreExpr, LHsExpr GhcRn)
    resolve_expr env expected original@(L loc expr)
      = setSrcSpanA loc $ case expr of
          HsPar x arg -> do
            (core, arg') <- resolve env expected arg
            return (core, L loc (HsPar x arg'))
          XExpr (ExpandedThingRn (HSE ctxt arg)) -> do
            (core, arg') <- resolve env expected arg
            return (core, L loc (XExpr (ExpandedThingRn (HSE ctxt arg'))))
          HsLit _ (HsChar _ c)
            | charTy `tcEqType` expected
            -> return (mkCharExpr c, original)
          HsLit _ (HsString _ string)
            | mkListTy charTy `tcEqType` expected
            -> do { -- See Note [Quoted strings in retained operands].
                    let chars = map (nlHsLit . HsChar NoSourceText) (unpackHText string)
                  ; (core, expanded) <- resolve env expected (L loc (ExplicitList noExtField chars))
                  ; return (core, L loc (mkExpandedLExpr expr expanded)) }
          HsOverLit _ (OverLit OverLitRn{ol_from_fun = L _ from} (HsIntegral lit))
            | let n = il_value lit, n >= 0
            -> do { (core, generated) <- tcRetainedNumeral expected from n
                  ; return (core, L loc (mkExpandedLExpr expr generated)) }
          ExprWithTySig _ operand sig
            | HsOverLit _ (OverLit _ (HsIntegral lit)) <- unLoc (stripParensLHsExpr operand)
            , il_value lit >= 0
            , isCompleteHsSig sig
            -> do { -- See Note [Retained integral literals].
                    annotated <- tcHsSigWcType False (ExprSigCtxt NoRRC) sig
                  ; unless (isTauTy annotated) unsupported
                  ; expected' <- liftZonkM (zonkTcType expected)
                  ; unless (annotated `tcEqType` expected') $
                      failWithTc $ TcRnRetainedQuantificationError
                        (RetainedIndexArgumentCast annotated expected')
                  ; (core, operand') <- resolve env expected' operand
                  ; return (core, L loc (mkExpandedLExpr expr operand')) }
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
            -> (,original) <$> tcRetainedArgVar (lexprCtOrigin original) expected arg_id
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
                              ; arg_id <- case coreFullView (varType arg_id) of
                                  TyVarTy tv
                                    | isLocalId arg_id, isTcTyVar tv, isMetaTyVar tv
                                    , not (isNativeOpaqueValueType (varType arg_id))
                                    , isNativeOpaqueValueType expected
                                    -> do { -- See Note [Inferring a local retained operand].
                                            co <- unifyKind Nothing (tyVarKind tv) liftedTypeKind
                                          ; co' <- liftZonkM (zonkCo co)
                                          ; unless (isReflCo co') unsupported
                                          ; liftZonkM (zonkId arg_id) }
                                  _ -> return arg_id
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
                                    then (,original) <$> tcRetainedArgVar (lexprCtOrigin original) expected arg_id
                                    else failWithTc $ TcRnRetainedQuantificationError
                                           (RetainedIndexMultiplicity (idMult arg_id)) }
                                else case nameModule_maybe (getName arg_id) of
                                  Just m | isGlobalId arg_id, isInteractiveModule m ->
                                    failWithTc $ TcRnRetainedQuantificationError
                                      (RetainedInteractiveIndex (getName arg_id))
                                  _ -> unsupported }
                      _ -> unsupported }
          _ -> unsupported

    -- Instantiate the erased parameters from the complete expected domain.
    constructor expected dc = case nativeIndexDataConTyArgs expected dc of
      Just tys -> return (Core.mkTyApps (Core.Var (dataConWorkId dc)) tys)
      Nothing -> case indexed expected of
        Just app -> return app
        Nothing -> do { refined <- tcRetainedDeclaredTypeView expected
                      ; maybe unsupported return (indexed refined) }
      where
        indexed domain = do
          let tvs = dataConUserTyVars dc
              bind tv _ | tv `elem` tvs = BindMe
                        | otherwise     = DontBindMe
          subst <- tcMatchTyX_BM bind Subst.emptySubst (dataConOrigResTy dc) domain
          tys <- traverse (Subst.lookupTyVar subst) tvs
          guard (substTyWith tvs tys (dataConOrigResTy dc) `eqType` domain)
          case Core.nativeFamilyConHead_maybe dc tys of
            Just app -> return app
            Nothing -> do
              (prefix, _) <- Core.nativeIndexedConArgs dc tys
              return (Core.mkApps (Core.Var (dataConWorkId dc)) prefix)

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
      = do { (core_fun, fun') <- case retainedSourceHead fun of
               Just (n, written) -> do
                     certified <- if isExternalName n
                       then tcLookupIdMaybe n >>= maybe (pure Nothing) tcLookupSourceTableId
                       else pure Nothing
                     case certified of
                       Just v -> do { core <- instantiate_source v written arg
                                    ; pure (core, fun) }
                       _ -> setSrcSpanA (getLoc fun) $
                              failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall n))
               Nothing -> resolve env expected fun
           ; (app, arg') <- apply env core_fun arg
           ; return (app, fun', arg') }

    -- See Note [Erased arguments of certified source operands].
    instantiate_source :: Id -> [LHsWcType GhcRn] -> LHsExpr GhcRn -> TcM Core.CoreExpr
    instantiate_source owner written arg = do
      owner <- liftZonkM (zonkId owner)
      (core, sigma) <- foldM (applyRetainedSourceType unsupported owner)
                             (Core.Var owner, varType owner) written
      let (binders, body) = tcSplitForAllInvisTyVars sigma
      if null binders
        then return core
        else do
          actual <- tcRetainedDeclaredArgType pure arg
          case (splitFunTy_maybe body, actual) of
            (Just (_, _, _, formal, _), Just actual) ->
              Core.mkTyApps core <$>
                chooseRetainedSourceTypes unsupported owner sigma (map snd binders) formal actual
            _ -> unsupported

    -- Resolve the witness before checking the dependent payload. The next
    -- application reads its domain from the type of this partial Core call.
    apply env fun arg
      | DepFunTy DepFunVisible binder _ <- coreFullView (exprType fun)
      = do { (core_arg, arg') <- resolve env (varType binder) arg
           ; case Core.retainedArgIndex core_arg of
               Just index | typeKind index `tcEqType` varType binder ->
                 return (Core.App fun core_arg, arg')
               _ -> setSrcSpanA (getLoc arg) unsupported }
      | otherwise = apply_ordinary env fun arg

    apply_ordinary env fun arg = case splitFunTy_maybe (exprType fun) of
      Just (_, _, _, arg_ty, _) -> do
        (core_arg, arg') <- resolve env arg_ty arg
        let app = Core.App fun core_arg
        -- See Note [Retained argument cast diagnostics].
        case core_arg of
          Core.Cast (Core.Var actual) co
            | (Core.Var owner, args) <- Core.collectArgs app
            , Just family <- sourceTableId_maybe owner
            , length args == tyConArity family
            , Nothing <- Core.nativeSourceCallIndex app
            , not (varType actual `tcEqType` coercionRKind co)
            -> setSrcSpanA (getLoc arg) $
                 failWithTc (TcRnRetainedQuantificationError
                   (RetainedIndexArgumentCast (varType actual) (coercionRKind co)))
          _ -> return (app, arg')
      _ -> unsupported

    unsupported :: TcM a
    unsupported = failWithTc (TcRnRetainedQuantificationError RetainedIndexExpression)

-- | Split a written source head into its name and visible type arguments.
-- See Note [Erased arguments of certified source operands].
retainedSourceHead :: LHsExpr GhcRn -> Maybe (Name, [LHsWcType GhcRn])
retainedSourceHead = go []
  where
    go written (L _ (HsPar _ fun)) = go written fun
    go written (L _ (HsAppType _ fun ty)) = go (ty : written) fun
    go written (L _ (HsVar _ name))
      | let n = unLocWithUserRdr name
      , not (isDataOcc (nameOccName n)) = Just (n, written)
    go _ _ = Nothing

-- | Check a written type without inferring an earlier erased argument.
-- See Note [Erased arguments of certified source operands].
applyRetainedSourceType :: TcM (Core.CoreExpr, TcSigmaType) -> Id
                        -> (Core.CoreExpr, TcSigmaType) -> LHsWcType GhcRn
                        -> TcM (Core.CoreExpr, TcSigmaType)
applyRetainedSourceType unsupported owner (core, sigma) ty
  | Just (_, binder, _) <- tcSplitForAllTyVarBinder_maybe sigma
  , isSpecifiedForAllTyFlag (binderFlag binder)
  = do { (choice, body) <- tcVTA (idConcreteTvs owner) sigma ty
       ; choice <- liftZonkM (zonkTcType choice)
       ; body <- liftZonkM (zonkTcType body)
       ; valid <- tcRetainedInstScope [choice, body]
       ; if valid && all noQuickLookVars [choice, body]
                  && not (any (\v -> isTcTyVar v && isMetaTyVar v)
                              (tyCoVarsOfTypesList [choice, body]))
         then return (Core.App core (Core.Type choice), body)
         else unsupported }
  | otherwise = unsupported

-- | Determine erased type arguments from a known type.
-- See Note [Erased arguments of certified source operands].
chooseRetainedSourceTypes :: TcM [Type] -> Id -> Type -> [TyVar]
                          -> Type -> Type -> TcM [Type]
chooseRetainedSourceTypes unsupported owner sigma tvs formal actual
  | null tvs, eqType formal actual
  = return []
  | isEmptyNameEnv (idConcreteTvs owner)
  , Just subst <- matchRetainedAppType tvs Subst.emptySubst formal actual
  , Just choices <- traverse (Subst.lookupTyVar subst) tvs
  , not (any (`elemVarSet` tyCoVarsOfTypes choices) tvs)
  , and (zipWith (\tv ty -> eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                 tvs choices)
  , eqType (Subst.substTy subst formal) actual
  , all noQuickLookVars (sigma : actual : choices)
  = do { valid <- tcRetainedInstScope (sigma : actual : choices)
       ; if valid then return choices else unsupported }
  | otherwise = unsupported

{- Note [Inferred domains of written retained constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In zipWithExact [x,y] [u,v] (,), the two input lists determine their
element types. The result type of the callback is still unknown. Ordinary
forall instantiation can therefore supply a fresh element type for each
list. If we build the (:) worker at that type before checking x, its Core
type argument still contains the old metavariable when we recover the index.

Read the declared type of the constructor tree before building its workers.
Match only metavariables of the expected domain. Let the ordinary unifier
check that match and read its result. An ordinary metavariable cannot stand
for a type containing a runtime index. The existing immutable instantiation
path handles those types. The operand check still validates each field and
its usage. A bare local variable retains its lexical conversion evidence.
-}

{- Note [Erased arguments of certified source operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A retained operand can call a checked source function with an erased element
parameter. For example, sourceLength xs has type Nat when xs has a declared
type [a]. Its certified head still has type forall a. [a] -> Nat. Instantiate
that erased binder before reading the first runtime argument's domain.

Read only the operand's existing monomorphic declaration. Match kinds and
types with an immutable substitution. Every unwritten erased binder must be
determined by this first argument. Check the substituted domain, binder kinds,
lexical scope, and concreteness requirements before constructing Core type arguments.
Do not allocate inference variables or check the operand to obtain advice.
Keep the written expression for the later ordinary check and usage recording.

A written prefix such as sourceLength @a xs uses tcVTA at each leading
specified binder. Require known types after zonking. Do not apply tcVTA to
an inferred binder or create a guessed instantiation before the written
prefix. Remaining erased binders can use the same declaration matching.
The runtime syntax keeps each written type argument. Its ordinary check uses
the same specified binders and the same known types as these Core arguments.
The argument's actual type must still match the instantiated domain.

This step follows tcLookupSourceTableId. It does not certify a function or
change declaration-group ordering. The defining group must have completed
its source equations before another group can use them as an operand.
The final retainedArgIndex check still validates saturation and all domains.
-}

{- Note [Inferring a local retained operand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An unsigned binding such as `get xs i = mkListIndex xs i` begins with
a fresh type variable for xs. Its runtime representation can also be
unknown. The expected list domain fixes that representation to LiftedRep.
Unify the kinds before checking whether xs can be an opaque local index.
Require a reflexive coercion after zonking. The usual variable check then
checks the full domain, scope and multiplicity of the operand.
-}

-- | Resolve an index variable at its expected domain. Function values keep
-- their complete fixed type and lexical identity.
-- See Note [Retained argument application].
tcRetainedArgVar :: CtOrigin -> TcType -> Id -> TcM Core.CoreExpr
tcRetainedArgVar origin expected arg_id
  | isNativeIndexFunctionId arg_id = tcRetainedFunctionVar expected arg_id
  | varType arg_id `tcEqType` expected = return (Core.Var arg_id)
  | otherwise
  = do { branch_co <- tcRetainedDomainConversion origin (varType arg_id) expected
       ; co <- maybe (unifyType Nothing (varType arg_id) expected) return branch_co
       ; co' <- liftZonkM (zonkCo co)
       ; if isReflCo co'
         then return (Core.Var (setVarType arg_id (coercionLKind co')))
         else return (Core.Cast (Core.Var arg_id) (SubCo co')) }

-- | Prove a domain conversion from lexical constructor evidence and family axioms.
-- See Note [Retained argument application].
tcRetainedDomainConversion :: CtOrigin -> TcType -> TcType -> TcM (Maybe CoercionN)
tcRetainedDomainConversion origin actual expected
  | anyFreeVarsOfType isNonCoVarId actual
  = tryTcDiscardingErrs (return Nothing) $ runMaybeT $ do
      branch_co <- liftMaybeT (tcRetainedTypeCoercion (const True) actual)
      guard (not (isReflCo branch_co))
      let actual' = coercionRKind branch_co
      if actual' `tcEqType` expected
        then return branch_co
        else do
          loc <- liftMaybeT (getCtLocM origin (Just TypeLevel))
          Reduction actual_co actual_nf <- liftMaybeT (tcRewriteFamilyAxioms loc actual')
          Reduction expected_co expected_nf <- liftMaybeT (tcRewriteFamilyAxioms loc expected)
          guard (actual_nf `tcEqType` expected_nf)
          return (branch_co `mkTransCo` actual_co `mkTransCo` mkSymCo expected_co)
  | otherwise = return Nothing

{- Note [Wildcards in retained operand domains]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In keepMaybe @_ m x, the type of m can determine the wildcard. The
ordinary unifier checks the declared operand against the retained domain.
If zonking then gives the same type on both sides, use the original
runtime Id with its resolved type. This supplies an index for the
codomain. A cast for a non-reflexive equality cannot supply that index.
Use the resolved domain when applying the dependent function type too.
-}

{- Note [Retained argument cast diagnostics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sourceLength @Bool xs requires [Bool] when xs has type [Int]. Variable
resolution emits the ordinary equality wanted and keeps its nominal cast.
The computed-index grammar cannot use that cast. Its final rejection would
otherwise hide the argument types behind an unsupported-expression error.

When a saturated certified call cannot recover its index and its argument is
a casted variable, report the actual and expected types at that operand's
existing span. Do not solve constraints, check the operand again, or discard
a successful conversion. The message describes the required cast, rather
than asserting that no nominal equality can hold in the current context.
-}

{- Note [Quoted strings in retained operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A plain quoted string at [Char] has the same index as its written list
of characters. Decode the parsed HText with its existing modified UTF-8
decoder. This preserves embedded NUL and each Unicode code point.

Check and desugar the expansion through the retained list path. It keeps
the original literal for source diagnostics and the list constructors
for Core index recovery. The constructor fields stay lazy. Ordinary
string expressions still use GHC.Core.Make.mkStringExprFSWith.

With OverloadedStrings the renamer produces HsOverLit, even when the
expected type is [Char]. A fromString instance or a rebindable fromString
can change the value or demand. Only a plain HsString uses this expansion.
-}

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

A variable can have a nominally equal domain with a different family
application. For example, a constructor equation supplies cut ~# Z while a
later retained variable still has domain Vec Bool (Add cut m). Lift the
lexical constructor proof through the original domain. Family axioms can
then reduce Add. Use this conversion only if it proves the complete domain
equality. Otherwise, give the original types to the ordinary unifier so it
can use other Given equalities or infer metavariables as usual.

Keep the nominal domain cast in the resolved Core argument. If that cast
uses the lexical branch evidence, reuse it on the same written variable
in tcValArg. Check the occurrence at the cast's input type and account
for its runtime usage once. Other arguments use the existing argument
check. A failed domain equality remains a located type error.
See Note [Nominal native index casts] in GHC.Core.

Resolution does not inspect unfoldings or evaluate values. Later argument
types use the same runtime Id, including a lazy binding whose RHS is bottom.
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
When the result mentions the binder, resolve the written operand by the same
index rules as a visible retained argument and substitute it into the result.
A vacuous binder needs only a checked runtime argument. See Note [Vacuous
retained applications] in GHC.Core.Utils. An inferred operand cannot replace
the written one. Check the expression at the binder's domain and multiplicity
Many exactly once. Store the result type in HsAppRetained and desugar the
operand to a runtime value application.

Keep the written visibility in ERetainedArg and HsAppRetained. A visible
operand uses ordinary value checking. Its stored result type keeps later
passes from having to substitute the operand again. Breakpoint insertion
recognises both visibilities. See Note [Breakpoints in retained operands]
in GHC.HsToCore.Ticks. A written invisible operand counts in source
diagnostic positions without consuming an ordinary argument cache position. Skip Quick Look for this application so that
the retained result is checked without Quick Look instantiation variables.
-}

-- | Is this a written runtime argument for an invisible retained binder?
isExplicitRetainedArg :: HsExprArg p -> Bool
isExplicitRetainedArg ERetainedArg{ ea_retained_vis = DepFunInvisible } = True
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
  = do { matched <- tcRetainedResultInstWithCasts do_ql expected_rho args fun sigma
       ; case matched of
           Just (_, body)
             | XExpr (ConLikeTc (RealDataCon _)) <- fun
             , let (_, tau) = tcSplitPhiTy body
             , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) tau
             , isFunTy result
             -- See Note [Retained choices for partial constructors].
             -> return (fun, sigma)
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
application route and tcInstFunWithExpected continue to finishAppWithScopedCache.
Its checkResultTy call checks subsumption against the original expected type
and emits the required equality evidence. For example, a non-reflexive Refl
still fails without a suitable Given. Never replace the actual result with
the expected result, or use a reflexive coercion for this check.

For a retained sum, the payload family can reduce to an ordinary data
type such as appendVec left right :~: input. Read its arguments through
the checked scoped family equation in matchSumPayload. This lets Refl
choose an endpoint. Final nominal subsumption must still prove that the
two endpoints agree in this branch. A missing induction premise must
therefore fail at Refl.

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

-- | Choose result arguments at a site that commits to the application.
-- See Note [Inferring the suffix of a checked split].
tcRetainedResultInstWithCasts :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                              -> HsExpr GhcTc -> TcSigmaType
                              -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedResultInstWithCasts do_ql expected args fun sigma
  = do { matched <- tcRetainedResultInst do_ql expected args fun sigma
       ; case matched of
           Just _ -> return matched
           Nothing -> tcRetainedResultIndexCast do_ql expected args fun sigma }

-- | Instantiate erased constructor binders from exact retained result indices.
-- See Note [Transporting retained result indices].
tcRetainedResultIndexCast :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                         -> HsExpr GhcTc -> TcSigmaType
                         -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedResultIndexCast NoQL (Just expected) args fun sigma
  | XExpr (ConLikeTc (RealDataCon dc)) <- fun
  , hasNoConcreteTyVars fun
  , not (any is_type_arg args)
  , (binders@(_:_), body) <- tcSplitForAllInvisTyVars sigma
  , isTauTy body
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) body
  , Just (tc, templates) <- splitTyConApp_maybe result
  , tc == dataConTyCon dc, isGenerativeTyCon tc Nominal
  , length templates == tyConArity tc
  , Just actuals <- retainedResultArgs tc expected
  , templates `equalLength` actuals
  , let tvs = map snd binders
  , Just indices <- traverse (result_index (zip templates actuals)) tvs
  , any is_retained_var indices
  , or (zipWith (\index tv -> not (typeKind index `eqType` tyVarKind tv)) indices tvs)
  , all noQuickLookVars [body, expected]
  = do { valid <- tcRetainedInstScope (body : expected : indices)
       ; if not valid then return Nothing else
         do { let in_scope = mkInScopeSet (tyCoVarsOfTypes (body : map tyVarKind tvs))
            ; (subst, choices) <- mapAccumLM transport (Subst.mkEmptySubst in_scope) (zip tvs indices)
            ; return (Just (choices, Subst.substTy subst body)) } }
  where
    result_index pairs tv
      | [index] <- [actual | (formal, actual) <- pairs, getTyVar_maybe formal == Just tv]
      = Just index
      | otherwise = Nothing

    is_retained_var TermVarTy{} = True
    is_retained_var _ = False

    transport subst (tv, index)
      = do { let kind = Subst.substTy subst (tyVarKind tv)
           ; co <- unifyKind Nothing (typeKind index) kind
           ; let choice = mkCastTy index co
           ; return (Subst.extendTvSubstAndInScope subst tv choice, choice) }

    is_type_arg ETypeArg{} = True
    is_type_arg _ = False
tcRetainedResultIndexCast _ _ _ _ _ = return Nothing

-- | Read the expected data arguments, including a checked sum payload.
-- See Note [Transporting retained result indices].
retainedResultArgs :: TyCon -> Type -> Maybe [Type]
retainedResultArgs tc ty
  | Just (head_tc, params) <- splitTyConApp_maybe ty, head_tc == tc = Just params
  | otherwise = snd <$> matchSumPayload tc ty

{- Note [Transporting retained result indices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a checked split, a constructor returns Width (Add cut suffix) p
while the expected result is Width total v. The length proof can
establish the kind equality Vec a total ~# Vec a (Add cut suffix).
The expected result supplies the exact value v. An instantiation
metavariable cannot hold that value, even after kind transport.

For a saturated real constructor, take each remaining invisible binder
from its unique direct argument in the same data TyCon. At least one
argument must identify a retained value. Other positions can determine
ordinary parameters such as the element type of both vectors. Copy the
argument from its position without decomposing it. Find every choice
before emitting any evidence. Check the indices in binder order. An
earlier transport can occur in a later kind. Use ordinary nominal kind
checking, as for a written type application, and cast each scoped index.
This produces an immutable application. Emit the kind equalities when
the caller commits to this application. The enclosing implication must
solve them. The application checker still checks all field arguments
and the entire result.
For a generated sum payload, read the result arguments after the checked
family reduction in matchSumPayload. That reduction goes from the family
application to the expected data type. Only the data type has a slot that
selects the index. See Note [Rigid retained indices] in GHC.Tc.Utils.Unify.
-}

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
        (_, tau) = tcSplitPhiTy body
  , not (null tvs), isTauTy tau
  , not (hasNativeDependentExpr (Core.Type body))
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) tau
  , Just pairs <- candidate_pairs body result expected
  , let candidates = [ subst | (formal, actual) <- pairs
                            , Just kinds <- [tcMatchTyX_BM (bind_outer tvs)
                                               Subst.emptySubst (typeKind formal) (typeKind actual)]
                            , Just subst <- [tcMatchTyX_BM (bind_outer tvs) kinds formal actual] ]
  , Just inst_tys <- traverse (candidate candidates) tvs
  , not (any (`elemVarSet` tyCoVarsOfTypes inst_tys) tvs)
  , let subst = Subst.zipTvSubst tvs inst_tys
  , and (zipWith (\tv ty -> eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                tvs inst_tys)
  = do { valid <- tcRetainedInstScope inst_tys
       ; return (if valid then Just (inst_tys, Subst.substTy subst body) else Nothing) }
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _ | tv `elem` tvs = BindMe
                        | otherwise = DontBindMe

    candidate substs tv = listToMaybe [ ty | subst <- substs
                                         , Just ty <- [Subst.lookupTyVar subst tv] ]

    candidate_pairs body result expected
      | XExpr (ConLikeTc (RealDataCon _)) <- fun
      , isFunTy result, isRetainedValueType expected
      = callback_pairs result expected
      | isTauTy body
      , Just (tc, templates) <- splitTyConApp_maybe result
      , isAlgTyCon tc
      , case fun of
          HsVar{} -> True
          XExpr (ConLikeTc (RealDataCon dc)) -> tc == dataConTyCon dc
          _ -> False
      , length templates == tyConArity tc
      , Just actuals <- retainedResultArgs tc expected
      , templates `equalLength` actuals, isRetainedValueType (mkTyConApp tc actuals)
      = Just (zip templates actuals)
      | otherwise = Nothing

    -- See Note [Retained choices for partial constructors].
    callback_pairs actual expected
      | FunTy af ma mult domain result <- coreFullView actual
      , FunTy expected_af expected_ma expected_mult expected_domain expected_result
          <- coreFullView expected
      , isVisibleFunArg af, isVisibleFunArg expected_af
      , eqType ma expected_ma
      , eqType mult expected_mult || (isOneTy mult && isManyTy expected_mult)
      = ((domain, expected_domain) :) <$> callback_pairs result expected_result
      | isFunTy actual || isFunTy expected = Nothing
      | otherwise = Just [(actual, expected)]
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

A record selector also carries a checked Id. For example, applying
getConst to a known Const Int n can instantiate its erased index with the
local runtime n. Inspect the selector Id's concreteness requirements in
the same way as for a variable head.
-}

-- | Check that a known application head has no concreteness requirements.
hasNoConcreteTyVars :: HsExpr GhcTc -> Bool
hasNoConcreteTyVars (HsVar _ (L _ var)) = isEmptyNameEnv (idConcreteTvs var)
hasNoConcreteTyVars (XExpr (HsRecSelTc (FieldOcc _ (L _ sel))))
  = isEmptyNameEnv (idConcreteTvs sel)
hasNoConcreteTyVars (XExpr (ConLikeTc (RealDataCon dc)))
  = isEmptyNameEnv (dataConConcreteTyVars dc)
hasNoConcreteTyVars _ = False

-- | Match a retained expected result and preserve its instantiated constraints.
tcRetainedResultMatch :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                      -> HsExpr GhcTc -> TcSigmaType
                      -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedResultMatch NoQL (Just expected) args fun sigma
  | Just (tvs, body, subst, ranked_constructor) <- retainedOrdinaryResultSubst expected args fun sigma
  , Just inst_tys <- traverse (Subst.lookupTyVar subst) tvs
  , and (zipWith (\tv ty -> eqType (Subst.substTy subst (tyVarKind tv)) (typeKind ty))
                tvs inst_tys)
  = do { enabled <- if ranked_constructor then xoptM LangExt.ImpredicativeTypes else return True
       ; valid <- if enabled && (not ranked_constructor || all noQuickLookVars (expected : inst_tys))
                  then tcRetainedInstScope inst_tys
                  else return False
       ; return (if valid then Just (inst_tys, Subst.substTy subst body) else Nothing) }
tcRetainedResultMatch do_ql expected args fun sigma
  = tcRetainedWorkerResultMatch do_ql expected args fun sigma

-- | Match the result without choosing absent parameters. The flag identifies
-- a ranked constructor result that requires complete instantiation.
retainedOrdinaryResultSubst :: TcRhoType -> [HsExprArg 'TcpRn] -> HsExpr GhcTc
                            -> TcSigmaType -> Maybe ([TyVar], Type, Subst.Subst, Bool)
retainedOrdinaryResultSubst expected args fun sigma
  | hasNativeDependentExpr (Core.Type expected)
  , not (any is_type_arg args)
  , (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , not (null tvs)
  , let (theta, tau) = tcSplitPhiTy body
  , null theta || hasNoConcreteTyVars fun
  , Just result <- tcRetainedOrdinaryResult (count isHsValArg args) tau
  , let tau_expected = isTauTy expected
        ranked_constructor
          | not tau_expected
          , XExpr (ConLikeTc (RealDataCon _)) <- fun = isRankedRetainedContainer expected
          | otherwise = False
  , tau_expected || (hasNoConcreteTyVars fun
                     && (isRetainedValueType expected || ranked_constructor
                         || exactPackageResult tvs result expected))
  , isTauTy result
  , not (hasNativeDependentExpr (Core.Type body))
  , Just kind_subst <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst
                                   (typeKind result) (typeKind expected)
  , Just subst <- tcMatchTyX_BM (bind_outer tvs) kind_subst result expected
  , eqType (Subst.substTy subst result) expected
  = Just (tvs, body, subst, ranked_constructor)
  | otherwise = Nothing
  where
    is_type_arg ETypeArg{} = True
    is_type_arg _ = False

    bind_outer tvs tv _
      | tv `elem` tvs = BindMe
      | otherwise = DontBindMe

{- Note [Partial ordinary applications at a native result type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In fmap (sum . toList) vectors, the input of the callback can be
Vec Int (sourceLength xs). The declared result of (.) after two arguments is
a -> c. It determines a and c, but does not mention the intermediate b.
If IALL allocates a meta-variable for a, it cannot assign the native input.
This also prevents normal argument checking from passing that input to
toList.

Use the exact expected-result substitution when it fixes a native type.
Use it just before ordinary instantiation, after complete result and
argument-based inference. Allow only ordinary lifted type parameters to
remain unknown. Their kinds and the kinds of the chosen parameters must
already agree. Allocate ordinary meta-variables for those unknowns and
pass the complete plan to IALL. Normal argument checks use the selected
input and infer the intermediate type. The normal meta-variable check
still rejects a native assignment to an unknown parameter. Keep the
original head, argument multiplicities, constraints and final subsumption.
-}

-- | Keep exact native choices and infer the remaining ordinary parameters.
-- See Note [Partial ordinary applications at a native result type].
tcRetainedPartialResultPlan :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                            -> HsExpr GhcTc -> TcSigmaType -> TcM RetainedInstPlan
tcRetainedPartialResultPlan NoQL (Just expected) args fun sigma
  | HsVar{} <- fun
  , hasNoConcreteTyVars fun
  , isFunTy expected
  , hasNativeDependentExpr (Core.Type expected)
  , any isHsValArg args
  = runMaybeT $ do
      expected <- liftMaybeT (liftZonkM (zonkTcType expected))
      guard (isTauTy expected)
      -- A stored impredicative field needs all its choices from the checked result.
      (tvs, _, subst, False) <- MaybeT (pure (retainedOrdinaryResultSubst expected args fun sigma))
      selected <- MaybeT (pure (retainedPartialResultChoices tvs subst))
      guard (noQuickLookVars expected)
      scoped <- liftMaybeT (tcRetainedInstScope selected)
      guard scoped
      snd <$> MaybeT (tcRetainedArgChoices False tvs subst)
tcRetainedPartialResultPlan _ _ _ _ _ = return Nothing

-- | Validate the native choices before allocating the remaining ordinary arguments.
retainedPartialResultChoices :: [TyVar] -> Subst.Subst -> Maybe [Type]
retainedPartialResultChoices tvs subst
  | not (null pending), all ((`eqType` liftedTypeKind) . kind) pending
  , any (hasNativeDependentExpr . Core.Type) selected
  , not (any (`elemVarSet` tyCoVarsOfTypes (selected ++ map kind tvs)) tvs)
  , and [kind tv `eqType` typeKind ty | (tv, Just ty) <- choices]
  , all noQuickLookVars selected
  = Just selected
  | otherwise = Nothing
  where
    choices = [(tv, Subst.lookupTyVar subst tv) | tv <- tvs]
    selected = [ty | (_, Just ty) <- choices]
    pending = [tv | (tv, Nothing) <- choices]
    kind tv = Subst.substTy subst (tyVarKind tv)

-- | Recognize a package expected at one invisible result parameter.
-- See Note [Exact expected existential packages].
exactPackageResult :: [TyVar] -> Type -> Type -> Bool
exactPackageResult tvs (TyVarTy result_var) expected
  | ExistsTy{} <- coreFullView expected
  = result_var `elem` tvs
exactPackageResult _ _ _ = False

{- Note [Exact expected existential packages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary polymorphic producer can already return a whole package.
When its result is one of its invisible type parameters, match that
parameter against the expected package. That type already specifies each
binder, its mode, and the payload. A type family in the payload can stay
stuck. A polymorphic payload can stay polymorphic. Matching the bare
result parameter copies that entire type into an ordinary type application.
It does not choose a witness or infer an argument of a type family.

All needed outer parameters must have
well-kinded choices. When the result leaves unrelated ordinary
parameters, seed the existing argument matcher with the same kind
and type choice. The original context, argument and result checks
remain authoritative. See
Note [Ordinary parameters in native applications].
-}

{- Note [Retained choices for partial constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VCons x has a linear remaining field arrow. Its expected callback can have
an unrestricted arrow. Exact matching of these arrows cannot select the
erased native index. For real constructors, collect independent choices from
the expected field domains, then from the expected algebraic result. Use the
first choice for each erased parameter. An input Vec a m therefore selects m
even when the expected output is Vec a (S n). The final check must prove the
required equality between m and n. Require equal arrow matchability and
multiplicities, except for an actual One arrow against an expected Many arrow.
Require complete choices, matching kinds, and valid runtime scope.

Return the original instantiated body with its linear arrows. The final
tcSubTypeApp check supplies the constructor multiplicity subsumption.
Defer early selection for function-valued constructor results to IALL.
IALL stores type applications in the argument spine and preserves the
ConLikeTc head that enables this subsumption. Do not wrap the head early.
-}

-- | Specialize a known retained worker from its complete expected result.
-- See Note [Expected retained worker instantiation].
tcRetainedWorkerResultMatch :: QLFlag -> Maybe TcRhoType -> [HsExprArg 'TcpRn]
                           -> HsExpr GhcTc -> TcSigmaType
                           -> TcM (Maybe ([Type], TcSigmaType))
tcRetainedWorkerResultMatch NoQL (Just expected) args fun sigma
  | hasNoConcreteTyVars fun
  , not (any is_type_arg args)
  , (ma_tvs, body) <- tcSplitForAllInvisTyVars sigma
  , let tvs = map snd ma_tvs
  , not (null tvs)
  , let (_, tau) = tcSplitPhiTy body
  , hasNativeDependentExpr (Core.Type tau)
  , isRetainedValueType expected, isRetainedValueType tau
  , Just result <- worker_result (count isHsValArg args) tau
  , isRetainedValueType result
  , hasNativeDependentExpr (Core.Type expected) || uniform_constructor_result tvs result
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
    -- Sigma a p exposes both erased parameters, even when the expected
    -- Sigma type contains no runtime Id. The fields still check the index.
    uniform_constructor_result tvs result
      | XExpr (ConLikeTc (RealDataCon dc)) <- fun
      , let fields = dataConRepArgBinders dc
      , any isJust fields, length fields == count isHsValArg args
      , Just (tc, params) <- splitTyConApp_maybe result
      , tc == dataConTyCon dc, isGenerativeTyCon tc Nominal
      , Just vars <- traverse getTyVar_maybe params
      = all (`elem` vars) tvs
      | otherwise = False

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

-- | Admit known retained values and complete erased package types.
-- Constraint arrows are supported only below a retained binder.
-- See Note [Known retained function instantiation].
isRetainedValueType :: Type -> Bool
isRetainedValueType = go False
  where
    go _ ty | isTauTy ty = True
    go under_pi ty = case coreFullView ty of
      ExistsTy{} -> supportsQuickLookType ty
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

The expected Sigma Nat p can have no runtime Id in it. Its generative
constructor exposes both erased parameters of Pair as direct arguments.
Read them from the expected type before checking the computed witness in
Pair (sourceLength xs) (sourceVector xs). Keep the exact kind, result and
scope checks. The second field must still use the first field's index.

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
finishAppWithScopedCache. They check the extra arguments, evidence order, usage and
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
              ; return (case con of RealDataCon dc -> (True, dataConHasRetainedResult dc)
                                    _ -> (False, False)) }
        | otherwise
        -> do { mb_id <- tcLookupIdMaybe (unLocWithUserRdr lname)
              ; return (case mb_id of
                          Just var -> (True, hasNativeDependentExpr (Core.Type (varType var)))
                          Nothing -> (False, False)) }
      HsApp _ fun arg -> application fun [arg]
      OpApp _ lhs op rhs ->
        do { ql <- wantQuickLook (unLoc op)
           ; case ql of
               DoQL -> return (False, False)
               NoQL -> do
                 { let operands = [lhs, rhs]
                 ; shape <- application op operands
                 ; if shape /= (True, True) then return (False, False) else
                   do { prefixes <- mapM tcPartialCallback operands
                      ; return (if any carries_result (catMaybes prefixes)
                                then shape else (False, False)) } } }
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
    -- See Note [Partially applied callbacks].
    carries_result (_, tvs, _, FunTy { ft_res = result }) =
      any (`elemVarSet` tyCoVarsOfType result) tvs
    carries_result _ = False

    application fun args =
      do { (inferable, anchor) <- tcRetainedInferableArg fun
         ; if not inferable then return (False, False)
           else do { shapes <- mapM tcRetainedInferableArg args
                   ; return (True, anchor || any snd shapes) } }

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
checking and finishAppWithScopedCache. The existing argument cache emits RHS usage
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
                ; tcRetainedKnownArgPlan (tcRetainedDeclaredArgType tcRetainedDeclaredTypeView)
                                       expected' fun sigma' args }
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
        equality_first_arg
          | [first_arg, _] <- value_args
          , hasNoConcreteTyVars fun
          , maybe False isTauTy expected
          , is_nominal_continuation domains result
          = Just first_arg
          | otherwise = Nothing
        equality_continuation = isJust equality_first_arg
        complete_choices = native_worker || equality_continuation
  , arity == length value_args
    || (overapplied && arity > 0 && hasNoConcreteTyVars fun)
  , equality_continuation || if native_worker
      then hasNoConcreteTyVars fun && isRetainedValueType tau
      else isTauTy tau && not (isFunTy result)
  = do { expected_ty <- mapM (liftZonkM . zonkTcType) expected
       ; let index_route = not complete_choices && not (null index_tvs)
             initial_subst | equality_continuation
                           , Just rho <- expected_ty
                           = fromMaybe Subst.emptySubst
                               (matchRetainedAppType tvs Subst.emptySubst result rho)
                           | overapplied = Subst.emptySubst
                           | otherwise = result_subst expected_ty index_route native_worker tvs result
             result_anchor = any (hasNativeDependentExpr . Core.Type)
                               [ty | tv <- tvs, Just ty <- [Subst.lookupTyVar initial_subst tv]]
             needed_tvs | index_route = index_tvs
                        | otherwise = filter (isNothing . Subst.lookupTyVar initial_subst) tvs
             arguments = zip3 [1..] domains value_args
       ; index_prefixes <- if index_route && hasNoConcreteTyVars fun
                           then filterM (\(_, domain, arg) -> defer_callback False domain arg) arguments
                           else return []
       ; eligible <- case equality_first_arg of
           { Just first_arg ->
             do { (inferable, anchor) <- tcRetainedInferableArg first_arg
                ; return (if result_anchor || anchor
                          then Just [inferable, False] else Nothing) }
           ; Nothing -> if native_worker
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
                                  ; let eligible (pos, domain, _) (inferable, _)
                                          = all (\(i, _, _) -> i /= pos) index_prefixes
                                            && (inferable || any (`elemVarSet` tyCoVarsOfType
                                                 (scaledThing domain)) index_tvs)
                                  ; return (Just (zipWith eligible arguments shapes)) }
                          else return (Just (repeat True))
                     else if hasNoConcreteTyVars fun
                             && isJust (tcRetainedOrdinaryResult arity tau)
                          then do { shapes <- mapM tcRetainedInferableArg
                                                (take arity value_args)
                                  ; deferred <- zipWithM (defer_callback True) domains (take arity value_args)
                                  ; return (if result_anchor || any snd shapes
                                            then Just (zipWith (\shape defer -> fst shape && not defer)
                                                               shapes deferred)
                                            else Nothing) }
                          else return Nothing }
       ; case eligible of
           Nothing -> return (fun, sigma, [], Nothing)
           Just inferable -> do
             { let domain_at pos = scaledThing (domains !! (pos-1))
                   matching_arg (pos, entry) =
                     let arg = value_args !! (pos-1)
                     in addArgCtxt (pos + fromMaybe 0 after_vta) rn_head arg $
                        do { ty <- liftZonkM (zonkTcType (actual_type entry))
                           ; loc <- getCtLocM (lexprCtOrigin arg) (Just TypeLevel)
                           ; view <- if not native_worker && not index_route
                                        && hasScopedDictionaryType ty && isTauTy ty && isFamFreeTy ty
                                     then return ty
                                     else reductionReducedType <$> tcRewriteFamilyAxioms loc ty
                           ; return (loc, (domain_at pos, callback_view (domain_at pos) view)) }
                   match_indices = tcMatchRetainedIndexArgs (bind_outer tvs)
             ; initial_cache <- infer_args needed_tvs inferable domains 1 args
             ; initial_args <- mapM matching_arg initial_cache
             ; let initial_pairs = map snd initial_args
                   match_worker subst = foldM (\s (formal, actual) ->
                       matchRetainedAppType tvs s formal actual) subst initial_pairs
             ; (matched, cache, pairs) <- if index_route
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
                              ; let cache = initial_cache ++ extra_cache
                              ; case extra of
                                  Just (s, extra_views) | not (null index_prefixes) -> do
                                    { (matched, cache') <- tcRetainedCallbackArgs tvs index_prefixes s cache
                                    ; let new_entries = filter (\(i, _) -> isNothing (lookup i cache)) cache'
                                    ; prefix_pairs <- mapM (fmap snd . matching_arg) new_entries
                                    ; return (matched, cache', views ++ extra_views ++ prefix_pairs) }
                                  _ -> return (fst <$> extra, cache, views ++ maybe [] snd extra) }
                         | otherwise -> return (Just subst, initial_cache, views) }
                 else do
                   { initial_match <- if complete_choices
                       then return (case match_worker initial_subst of
                                      Nothing -> match_worker Subst.emptySubst
                                      other -> other)
                       else do { m <- tcRetainedMatchArgs tvs initial_subst initial_pairs
                               ; case m of
                                   Nothing -> tcRetainedMatchArgs tvs Subst.emptySubst initial_pairs
                                   other -> return other }
                   ; (m, cache) <- case initial_match of
                       Just subst | not complete_choices ->
                         tcRetainedCallbackArgs tvs arguments subst initial_cache
                       _ -> return (initial_match, initial_cache)
                   ; pairs <- mapM (fmap snd . matching_arg) cache
                   ; return (m, cache, pairs) }
             ; choices <- case matched of
                            Just subst | complete_choices
                              -> return $ do { inst_tys <- traverse (Subst.lookupTyVar subst) tvs
                                             ; return (subst, inst_tys) }
                            Just subst -> tcRetainedArgChoices index_route tvs subst
                            Nothing -> return Nothing
             ; case choices of
                 Just (subst, inst_tys)
                   | index_route || any (hasNativeDependentExpr . Core.Type) inst_tys
                   , not nested || any (hasNativeDependentExpr . Core.Type) inst_tys
                   , not complete_choices || not (any (`elemVarSet` tyCoVarsOfTypes inst_tys) tvs)
                   , not equality_continuation
                     || (all isTauTy inst_tys
                         && not (any isMetaTyVar (tyCoVarsOfTypesList inst_tys))
                         && maybe False (eqType (Subst.substTy subst result)) expected_ty)
                   , and (zipWith (kind_agrees subst) tvs inst_tys)
                   , all (\(formal, actual) -> eqType (Subst.substTy subst formal) actual) pairs
                   -> do { valid <- tcRetainedInstScope inst_tys
                         ; if valid
                           then if nested || isJust after_vta
                                then return (fun, sigma, cache, Just inst_tys)
                                else return (mkHsWrap (mkWpTyApps inst_tys) fun,
                                             Subst.substTy subst body, cache, Nothing)
                           else return (fun, sigma, cache, Nothing) }
                 _ -> return (fun, sigma, cache, Nothing) } }
  where
    -- See Note [Checking a nominal equality continuation].
    is_nominal_continuation [Scaled _ first_arg, Scaled _ continuation] result
      | isTauTy first_arg
      , FunTy { ft_af = af, ft_arg = pred_ty, ft_res = body } <- coreFullView continuation
      , isInvisibleFunArg af
      , Just (tc, _) <- splitTyConApp_maybe pred_ty
      , tc `hasKey` eqTyConKey
      = isTauTy body && eqType body result
    is_nominal_continuation _ _ = False

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
      , not index_route
      , exactPackageResult tvs result expected_ty
      , Just kinds <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst
                                    (typeKind result) (typeKind expected_ty)
      , Just subst <- tcMatchTyX_BM (bind_outer tvs) kinds result expected_ty
      = subst
      | Just expected_ty <- expected
      , not native_worker
      , isTauTy expected_ty
      , not index_route || hasNativeDependentExpr (Core.Type expected_ty)
      , Just subst <- tcMatchTyX_BM (bind_outer tvs) Subst.emptySubst result expected_ty
      = subst
      | otherwise = Subst.emptySubst

    -- See Note [Matching a callback with an inferred multiplicity].
    callback_view FunTy{ft_af = expected_af, ft_ma = expected_ma, ft_mult = expected_mult} actual
      | isVisibleFunArg expected_af
      , isOneTy expected_mult || isManyTy expected_mult
      , fun@FunTy{ft_af = af, ft_ma = ma, ft_mult = mult} <- coreFullView actual
      , af == expected_af, eqType ma expected_ma
      , Just tv <- getTyVar_maybe mult, isMetaTyVar tv
      , hasNativeDependentExpr (Core.Type actual)
      = fun { ft_mult = expected_mult }
    callback_view _ actual = actual

    actual_type (_, ty, _) = ty

    -- See Note [Partially applied callbacks].
    defer_callback named domain arg
      | FunTy { ft_af = af, ft_mult = mult } <- coreFullView (scaledThing domain)
      , isVisibleFunArg af
      = do { prefix <- tcPartialCallback arg
           ; case prefix of
               Just _ -> return True
               Nothing | named && isManyTy mult -> isJust <$> tcRetainedNamedCallbackType arg
                       | otherwise     -> return False }
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

{- Note [Checking a nominal equality continuation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gcastWith consumes a proof and a continuation with the corresponding (~)
constraint. An ordinary type metavariable cannot hold a type that mentions
a retained index. Its use inside the continuation is also untouchable under
the Given. See Note [Rigid retained indices] in GHC.Tc.Utils.Unify.

For a saturated known head taking an ordinary argument and a continuation
with one nominal equality, select the erased types from the first argument
and the checked result. The head's signature supplies the continuation's
equality. The first argument can be a witness, as in gcastWith, or have a
type that is independent of the equality. The continuation must return the
same tau type as the application.

Check the continuation by ordinary subsumption under its original Given.
Require each erased argument to have a scoped tau choice with no unfilled
metavariables. Use NoQL and its existing argument cache. The cache supplies
the checked first argument and its usage. The application keeps its head,
arguments and evaluation order.
-}

{- Note [Matching a callback with an inferred multiplicity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In fmap (indexGeneric xs) (mkFinGeneric xs n), the partial lookup can
have type Fin (sourceLength xs) %p -> a with a fresh multiplicity p.
The declared callback argument of fmap uses Many. Match the domain and
result to choose fmap's erased arguments before its input metavariable
would have to hold sourceLength xs. Use the expected concrete arrow
multiplicity only in that comparison. Keep the cached callback at its
actual type. tcValArgsWithCache then checks its multiplicity and emits
the ordinary subsumption wrapper. Rigid multiplicities are never guessed.
-}

-- | Match cached index arguments and keep both views used for selection.
-- See Note [Matching a computed nonempty length].
tcMatchRetainedIndexArgs :: BindTvFun -> Subst.Subst -> [(CtLoc, (Type, Type))]
                         -> TcM (Maybe (Subst.Subst, [(Type, Type)]))
tcMatchRetainedIndexArgs bind seed entries = runMaybeT (match_group seed entries)
  where
    match_group subst pairs = mapAccumLM finish selected pending
      where
        (selected, pending) = mapAccumL select subst pairs

    -- A later argument (or field) can select an input to an earlier family.
    select subst entry@(_, pair@(formal, actual))
      | Just subst' <- tcMatchTyX_BM bind subst formal actual = (subst', Left pair)
      | otherwise = (subst, Right entry)

    finish subst (Left pair) = return (subst, pair)
    finish subst (Right (loc, (formal, actual)))
      | Just subst' <- tcMatchTyX_BM bind subst instantiated actual
      = return (subst', (instantiated, actual))
      | not (hasNativeDependentExpr (Core.Type actual)) = mzero
      | otherwise = do
          view <- case instantiated of
            TyConApp tc _ | isTypeFamilyTyCon tc ->
              liftMaybeT (reductionReducedType <$> tcRewriteFamilyAxioms loc instantiated)
            _ -> return instantiated
          match_view subst loc view actual
      where
        instantiated = coreFullView (Subst.substTy subst formal)

    match_view subst loc formal actual
      | Just subst' <- tcMatchTyX_BM bind subst formal actual
      = return (subst', (formal, actual))
      | TermVarTy v <- coreFullView actual
      = do { exposed <- MaybeT (tcSourceLetHead v)
           ; match_view subst loc formal exposed }
      | TyConApp tc fields <- formal, isGenerativeTyCon tc Nominal
      , TyConApp tc' args <- coreFullView actual, tc == tc', fields `equalLength` args
      = do { (subst', views) <- match_group subst [(loc, pair) | pair <- zip fields args]
           ; return (subst', (mkTyConApp tc (map fst views), mkTyConApp tc (map snd views))) }
      | otherwise = mzero

-- | Expose the outer shape of a checked local RHS for immutable selection.
-- See Note [Inferring through local definitions].
tcSourceLetHead :: Id -> TcM (Maybe Type)
tcSourceLetHead value = do
    declared <- tcRetainedDeclaredTypeView (TermVarTy value)
    if not (eqType declared (TermVarTy value))
      then return (Just declared)
      else do
        givens <- mapM (liftZonkM . zonkId) =<< getNativeGivens
        case mapMaybe (constructor_given . varType) givens of
          view : rest | all (eqType view) rest -> return (Just view)
          _ -> do
            views <- tcl_source_let_views . tcl_lcl_ctxt <$> getLclEnv
            case lookupVarEnv views value of
              Nothing -> return Nothing
              Just rhs -> do
                fam_envs <- tcGetFamInstEnvs
                limit <- reductionDepth <$> getDynFlags
                return (sourceLetHead fam_envs limit (delVarEnv views value) rhs)
  where
    -- See Note [Nominal Givens in retained argument selection].
    constructor_given pred_ty = do
      (tc, [kind, lhs, rhs]) <- tcSplitTyConApp_maybe pred_ty
      guard (tc `hasKey` eqTyConKey && isNativeIndexId value
             && kind `eqType` varType value)
      view <- if lhs `eqType` TermVarTy value then Just rhs
              else if rhs `eqType` TermVarTy value then Just lhs
              else Nothing
      TyConApp promoted _ <- pure (coreFullView view)
      dc <- isPromotedDataCon_maybe promoted
      _ <- nativeIndexDataConTyArgs kind dc
      return view

{- Note [Nominal Givens in retained argument selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A GADT pattern can introduce count ~# S Z. The continuation of gcastWith
binds the corresponding boxed constraint count ~ S Z. Both can determine
the missing tail of an argument declared as Vec a (1 + n). Choose the
constructor stated in the scoped Given before exposing a source let
definition such as count = 1 + extent.

Use a direct homogeneous nominal Given with the same runtime Id and a
constructor in its index domain. Require agreement when several boxed
Givens apply. This view only selects the erased type arguments. The cached
value retains its original type. Ordinary subsumption still uses the
actual Given to build the argument coercion.
-}

-- | Find a constructor by following local definitions and checked source families.
sourceLetHead :: FamInstEnvs -> IntWithInf -> VarEnv Type -> Type -> Maybe Type
sourceLetHead fam_envs limit views rhs = simplify_fields <$> expose 1 views rhs
  where
    expose depth env ty
      | intGtLimit depth limit = Nothing
      | TermVarTy v <- head_ty, Just def <- lookupVarEnv env v
      = expose (depth + 1) (delVarEnv env v) def
      | TyConApp tc args <- head_ty, source_family tc
      = case reduce tc args of
          Just result -> next result
          Nothing     -> try_args tc [] args
      | otherwise = Just head_ty
      where
        head_ty = coreFullView ty
        next = expose (depth + 1) env
        try_args _  _   [] = Just head_ty
        try_args tc rev (arg:rest) = do
          arg' <- next arg
          let rev' = arg' : rev
          case if eqType arg' arg then Nothing else reduce tc (reverse rev' ++ rest) of
            Just result -> next result
            Nothing     -> try_args tc rev' rest

    source_family tc = isTypeFamilyTyCon tc &&
                       isJust (sourceTableTermOcc (nameOccName (tyConName tc)))
    reduce tc args = reductionReducedType <$> reduceTyFamApp_maybe fam_envs Nominal tc args

    -- plus (S Z) n exposes S (plus Z n). Select n directly. Only discard
    -- a family application when one axiom returns one of its own operands.
    simplify_fields (TyConApp tc fields) | isJust (isPromotedDataCon_maybe tc)
      = mkTyConApp tc (map simplify_operand fields)
    simplify_fields ty = ty
    simplify_operand ty@(TyConApp tc args)
      | source_family tc, Just result <- reduce tc args, any (eqType result) args = result
      | otherwise = ty
    simplify_operand ty = ty

{- Note [Inferring through local definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider safeHead :: Vec a (1 + n) -> a at a vector of length total.
If total = S area, the omitted argument can be area even when the
definition of area uses an arbitrary dictionary. Try the structural
match first. At an opaque variable, prefer an actual lexical constructor
Given. Otherwise consult the checked RHS of its definition. Follow aliases
and reduce generated source family heads until the outer constructor is
available. For plus one n, try the equation before inspecting the arguments.
If necessary, expose one and retry before inspecting n. Leave constructor
fields for structural matching.

The local environment contains candidate types only. A method on the
RHS might ultimately select a caller-supplied dictionary. Reading the
candidate supplies no equality and records no source-equation use. Keep
the original inferred expression, type and usage in RetainedArgCache.
tcValArgsWithCache must check that type against the instantiated domain.
The M92 implication solver can supply a defining equality only when its
ordinary evidence justifies it. Static and arrow scopes delimit the
candidate environment in the same way as lexical native evidence.
-}

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

For a family-free tau type with a scoped selector, keep the zonked type.
For example, the first field of the pair passed to snd can contain
compareOrigin @c dict x. The family rewriter cannot rewrite that selector.
Match it as part of the original type. The complete choices still pass
tcRetainedInstScope, which checks the receiver and the exact lexical Given.
This path does not provide a coercion for a different selector application.

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
tcRetainedCallbackArgs :: [TyVar] -> [(Int, Scaled Type, LHsExpr GhcRn)]
                       -> Subst.Subst -> RetainedArgCache
                       -> TcM (Maybe Subst.Subst, RetainedArgCache)
tcRetainedCallbackArgs tvs args subst cache = go subst cache args
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
      | isNothing (lookup pos cache)
      , let formal = scaledThing domain
      , callback_type@FunTy { ft_af = af, ft_arg = input, ft_res = output }
           <- coreFullView (Subst.substTy subst formal)
      , isVisibleFunArg af, mentions_templates input || mentions_templates output
      = do { prefix <- tcPartialCallback arg
           ; case prefix of
               Nothing -> go subst cache rest
               Just prefix -> do
                 { valid <- select_prefix subst input output prefix
                 ; case valid of
                     Just chosen -> go chosen cache rest
                     Nothing -> do
                       { deferred <- defer_prefix callback_type prefix
                       ; if deferred then go subst cache rest else
                         do { (usage, (typed, actual)) <- tcCollectingUsage (tcInferRho arg)
                            ; matched <- tcRetainedMatchArgs tvs subst [(formal, actual)]
                            ; let cache' = cache ++ [(pos, (typed, actual, usage))]
                            ; case matched of
                                Just chosen -> go chosen cache' rest
                                Nothing -> return (Nothing, cache') } } } }
      | otherwise = go subst cache rest

    select_prefix subst input output
      (_, parameters, fields, FunTy { ft_arg = formal_input, ft_res = formal_output })
      = runMaybeT $ do
      let initial | mentions_templates input = Subst.emptySubst
                  | otherwise = fromMaybe Subst.emptySubst
                                (matchRetainedAppType parameters Subst.emptySubst formal_input input)
      selected <- liftMaybeT (foldM (tcMatchDeclaredField parameters) initial fields)
      choices <- MaybeT (pure (traverse (Subst.lookupTyVar selected) parameters))
      guard (not (any (`elemVarSet` tyCoVarsOfTypes choices) parameters))
      guard (and (zipWith (\tv ty -> eqType (Subst.substTy selected (tyVarKind tv)) (typeKind ty))
                          parameters choices))
      let actual_input  = Subst.substTy selected formal_input
          actual_output = Subst.substTy selected formal_output
      guard (all noQuickLookVars (actual_input : actual_output : choices))
      scoped <- liftMaybeT (tcRetainedInstScope choices)
      guard scoped
      with_input <- MaybeT (pure (match_type subst input actual_input))
      MaybeT (pure (match_type with_input (Subst.substTy with_input output) actual_output))
    select_prefix _ _ _ _ = return Nothing

    -- See Note [Partially applied callbacks].
    defer_prefix expected@FunTy { ft_mult = mult, ft_arg = input, ft_res = output }
                 (True, parameters, fields, residual)
      | isManyTy mult
      , hasNativeDependentExpr (Core.Type input)
      , not (any (isNativeIndexType . tyVarKind) tvs)
      , not (mentions_templates input)
      , isTauTy input
      , Just result_var <- getTyVar_maybe output
      , result_var `elem` tvs, tyVarKind result_var `eqType` liftedTypeKind
      , noQuickLookVars expected
      , Just subst <- matchRetainedAppType parameters Subst.emptySubst residual expected
      , Subst.substTy subst residual `eqType` expected
      , Just selected <- retainedPartialResultChoices parameters subst
      = do { ordinary <- allM ordinary_operand (map snd fields)
           ; if ordinary then tcRetainedInstScope selected else return False }
    defer_prefix _ _ = return False

    ordinary_operand arg =
      do { declared <- tcRetainedNamedCallbackType arg
         ; return (maybe False (not . hasNativeDependentExpr . Core.Type) declared) }

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

-- | Read the residual function type of a known, partially applied value.
-- See Note [Partially applied callbacks].
tcPartialCallback :: LHsExpr GhcRn
                  -> TcM (Maybe (Bool, [TyVar], [(Scaled Type, LHsExpr GhcRn)], Type))
tcPartialCallback = collect True []
  where
    collect plain fields (L _ (HsPar _ fun)) = collect plain fields fun
    collect plain fields (L _ (HsApp _ fun arg)) = collect plain (arg : fields) fun
    collect _     fields (L _ (HsAppType _ fun _)) = collect False fields fun
    collect plain fields@(_:_) (L loc (HsVar _ lname)) = setSrcSpanA loc $ do
      let name = unLocWithUserRdr lname
      owner <- if isDataOcc (nameOccName name)
        then do { con <- tcLookupConLike (unLoc lname)
                ; return $ case con of
                    RealDataCon dc | isEmptyNameEnv (dataConConcreteTyVars dc) -> Just (dataConWrapId dc)
                    _ -> Nothing }
        else tcLookupIdMaybe name
      return $ do
        var <- owner
        guard (isEmptyNameEnv (idConcreteTvs var))
        let (tvs, _, tau) = tcRetainedInstTelescope (varType var)
            (_, domains, _) = splitFunTys tau
        guard (not (null tvs) && not (hasNativeDependentExpr (Core.Type tau)))
        residual@FunTy { ft_af = af }
          <- coreFullView <$> tcRetainedOrdinaryResult (length fields) tau
        guard (isVisibleFunArg af)
        return (plain && not (isDataOcc (nameOccName name)), tvs, zip domains fields, residual)
    collect _ _ _ = return Nothing

{- Note [Partially applied callbacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In fmap (VCons x) (fromListExact n xs), the second argument determines
the callback input Vec a n. Inferring VCons x first would allocate a
mutable tail index that cannot later be filled with the runtime n.
Defer this application alongside bare named callbacks. If the result type
has already fixed the outer parameters, ordinary checking now sees
the complete callback type.

An enclosing application can infer this infix expression as a data argument
when it has a native anchor and a partial callback whose result depends on
its instantiation. Leave other infix expressions on their checking path.
In particular, ($) needs its expected result for the evidence in
expectMarker label marker $ evaluate (sameNat n m). See
Note [Immutable native dollar applications].

When the output parameter is still unknown, match the partially applied function's
remaining input against that known input. Declared prefix fields can
supply missing erased parameters. Use the declared residual result
as a candidate for the outer call. This step neither checks a field
occurrence nor supplies evidence.

For fmap ((.) sum toList) vectors, the callback result can be an
undetermined lifted type parameter. The input alone does not determine
the intermediate type of (.). If the callback's residual arrow exactly
matches the known native input and this result parameter, defer the
callback. Limit this check to an ordinary function prefix without written
type arguments and with named polymorphic operands whose types contain
no native syntax. These operands supply no independent native anchor.
Keep the known input and let the generic route allocate the ordinary
result parameter. Normal callback checking then has the expected arrow.
See Note [Partial ordinary applications at a native result type].

If declarations do not determine the callback and it cannot be deferred,
infer the original expression and cache its type and usage once. This
preserves applications where the prefix supplies a native index. Normal
callback checking consumes the original syntax at its real multiplicity.
See Note [Retained choices for partial constructors].
-}

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
       ; givens <- getNativeGivens
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
               | isNativeClassDictionaryId v
               = do { scoped <- getLexicalGivens
                    ; occurrence <- liftZonkM (zonkId v)
                    ; actuals <- liftZonkM (mapM zonkId (filter (== v) scoped))
                    ; return $ case actuals of
                        [actual] -> isNativeClassDictionaryId actual
                                 && varType occurrence `eqType` varType actual
                        _ -> False }
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
-- Use it to mark written type operands before checking the application.
-- See Note [VTA for out-of-scope functions] and Note [Dependent RULE binder domains]
-- in GHC.Tc.Gen.Sig.
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

-- | Take a quick look at a value argument during application instantiation.
type QuickLookArg = QLFlag -> Int
                 -> HsExprLoc -- ^ Location span of the whole application
                 -> (HsExpr GhcRn, SrcSpan) -- ^ Head and source span
                 -> LHsExpr GhcRn          -- ^ Argument
                 -> Scaled TcSigmaTypeFRR  -- ^ Type expected by the function
                 -> TcM (HsExprArg 'TcpInst)

quickLookArg :: QuickLookArg
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

-- | Find a lambda result type before the patterns bring new binders into scope.
-- See Note [Quick Look before lambda patterns].
tcPreviewQuickLookResult :: LHsExpr GhcRn -> TcM (Maybe TcRhoType)
tcPreviewQuickLookResult rhs =
  discardErrs $ discardConstraints $ tryTcDiscardingErrs (return Nothing) $
    runMaybeT $ do
      result <- preview rhs >>= liftMaybeT . liftZonkM . zonkTcType
      guard (fixed result)
      return result
  where
    preview :: LHsExpr GhcRn -> MaybeT TcM TcRhoType
    preview expr = do
      guard (plain_spine expr)
      (rn_fun, args) <- liftMaybeT (splitHsApps (unLoc expr))
      (tc_fun, declared) <- preview_head rn_fun
      sigma <- liftMaybeT (liftZonkM (zonkTcType declared))
      guard (fixed sigma)
      loc <- liftMaybeT (getFunSrcSpan args)
      snd <$> liftMaybeT
        (tcInstFunWithPrefix preview_arg False [] Nothing Nothing Nothing
          DoQL True (rn_fun, loc) tc_fun sigma args)

    preview_head :: HsExpr GhcRn -> MaybeT TcM (HsExpr GhcTc, TcSigmaType)
    preview_head (HsVar _ (L loc (WithUserRdr _ name))) = do
      thing <- if isExternalName name
               then liftMaybeT (tcLookup name)
               else MaybeT (tcLookupLcl_maybe name)
      let from_id ident = (mkHsVar (L loc ident), varType ident)
      case thing of
        ATcId{tct_id = ident} -> return (from_id ident)
        AGlobal (AnId ident) -> return (from_id ident)
        AGlobal (AConLike (RealDataCon dc)) ->
          return (XExpr (ConLikeTc (RealDataCon dc)), varType (dataConWrapId dc))
        _ -> mzero
    preview_head _ = mzero

    preview_arg :: QuickLookArg
    preview_arg _ _ loc _ arg scaled = do
      let expected = scaledThing scaled
      is_rho <- qlArgHasRhoType expected
      when is_rho $ void $ runMaybeT $ do
        actual <- preview arg
        influences <- liftMaybeT (quickLookInfluencesCall expected actual)
        when influences (liftMaybeT (qlUnify actual expected))
      skipQuickLook loc arg scaled

    plain_spine :: LHsExpr GhcRn -> Bool
    plain_spine expr = case unLoc (stripParensLHsExpr expr) of
      HsVar{} -> True
      HsApp _ fun _ -> plain_spine fun
      _ -> False

    fixed :: TcType -> Bool
    fixed ty = supportsQuickLookType ty && not (hasCoercionHole ty)
               && not (anyFreeVarsOfType isMetaTyVar ty)

{- Note [Quick Look before lambda patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider checking a lambda against `exists t. Bool -> Callback t`.
For `\flag -> id (prepare flag)`, the declaration of id cannot fix t.
Quick Look at its arguments can find the result `Callback PolyId`.
The result supplies t by invariant matching, as in Figure 10 of
"An Existential Crisis Resolved".

Run the ordinary application instantiator with the same rho, guardedness
and free instantiation variable checks as Quick Look. Read only plain
named application spines. A pattern or guard name has no declaration at
this point, so it supplies no Quick Look information. Leave complex
expressions and explicit type arguments to the ordinary checker.

Require head and result types without free metavariables or coercion holes.
Read head types without checking their uses. Discard the preview's
constraints and diagnostics. Then check the original lambda once, with
all its patterns, guards and operands in scope.
-}

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

-- | Check premises (A) and (B) of APP-QL using the original argument type.
-- See Note [Quick Look at value arguments] and Note [The fiv test in quickLookArg].
quickLookInfluencesCall :: TcRhoType -> TcRhoType -> TcM Bool
quickLookInfluencesCall expected actual
  | isGuardedTy expected = return True
  | otherwise = not <$> anyFreeKappa actual

isGuardedTy :: TcType -> Bool
isGuardedTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | otherwise                               = False

{- Note [Quick Look and ordinary argument checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In take 1 (toList (fst (partitionVec odd xs))), checking the call to
toList opens the package at the head of its argument. Quick Look can
prepare toList as an EValArgQL for take. Resuming that argument calls
finishApp directly. It skips the opening in tcOrdinaryApp.

For a plain named value application, consult the same read-only
argument classifier before Quick Look checks the head and arguments.
If the call needs to open an argument, keep its original syntax for
ordinary checking. This leaves the opening, result scope, constraints
and multiplicities with tcOpenImplicitArgs. Calls without an opening
can still perform the APP-lightning-bolt step of Quick Look (Figure 5).
See Note [Quick Look at value arguments].

A plain call such as print (run reflect input) can check run's higher-rank
argument using the same ordinary path as a direct call. Defer only when its
declared telescope supplies that argument type and its result is an ordinary
monotype. Do not change Quick Look's native instantiation or result checks.

A nested projection may have a complete erased argument plan of its own.
For example, an argument of (&&) may read a field from a rebuilt indexed
constructor. If that plan selects a runtime index, keep the original
application for ordinary checking. Instantiating its head again with
tcInstFun would replace the known index with a Quick Look variable.
See Note [Nested immutable retained applications].

The classifier accepts only written value arguments and parentheses.
An expansion in the spine can already have registered an untyped Template
Haskell finalizer. The ordinary check must not split that expansion again.
-}

-- | Leave checked native arguments in plain named calls to the ordinary checker.
-- See Note [Quick Look and ordinary argument checking].
tcQuickLookNeedsOrdinaryArgs :: HsExpr GhcRn -> [HsExprArg 'TcpRn] -> TcM Bool
tcQuickLookNeedsOrdinaryArgs (HsVar _ lname) args
  | let name = unLocWithUserRdr lname
  , not (isDataOcc (nameOccName name))
  , any isHsValArg args, all isPlainQuickLookArg args
  = do { ident <- tcLookupIdMaybe name
       ; case ident of
           Just var -> do
             { let sigma = varType var
             ; mixed <- if isEmptyNameEnv (idConcreteTvs var)
                        then tcFindMixedBarArguments sigma args
                        else return Nothing
             ; if isJust mixed || isGroundRankedRetainedCall (length (filter isHsValArg args)) sigma
               then return True
               else any isRight <$> tcImplicitAppArgs sigma args }
           Nothing -> return False }
tcQuickLookNeedsOrdinaryArgs _ _ = return False

-- | These arguments can be left for ordinary checking without replaying an expansion.
isPlainQuickLookArg :: HsExprArg 'TcpRn -> Bool
isPlainQuickLookArg EValArg{} = True
isPlainQuickLookArg (EWrap EPar{}) = True
isPlainQuickLookArg _ = False

-- | Select a constructor result from a complete expected type.
-- See Note [Retained constructors with a known Quick Look result].
tcKnownRetainedConstructorResult :: [HsExprArg 'TcpRn] -> HsExpr GhcTc
                                 -> TcSigmaType -> TcRhoType -> TcM (Maybe TcRhoType)
tcKnownRetainedConstructorResult args fun@(XExpr (ConLikeTc (RealDataCon dc))) sigma expected
  | let fields = dataConRepArgBinders dc
  , any isJust fields
  , length fields == count isHsValArg args
  , all isPlainQuickLookArg args
  = runMaybeT $ do
      expected <- liftMaybeT (liftZonkM (zonkTcType expected))
      guard (not (anyFreeVarsOfType (\v -> isTcTyVar v && isMetaTyVar v) expected))
      result <- MaybeT (resolve (dataConTyCon dc) expected)
      _ <- MaybeT (tcRetainedWorkerResultMatch NoQL (Just result) args fun sigma)
      pure result
  where

    resolve parent ty
      | Just (tc, _) <- tcSplitTyConApp_maybe ty
      , tc == parent
      = return (Just ty)
      | Just (family, _) <- tcSplitTyConApp_maybe ty
      , can_reduce family
      = do { envs <- tcGetFamInstEnvs
           ; loc <- getCtLocM (OccurrenceOf (tyConName family)) (Just TypeLevel)
           ; tryTcDiscardingErrs (return Nothing) $
               runTcSEqualities (follow envs loc ty) }
      | otherwise = return Nothing
      where
        follow envs loc ty
          = case tcSplitTyConApp_maybe ty of
              Just (tc, _) | tc == parent -> return (Just ty)
              Just (tc, tys)
                | can_reduce tc
                , Just (Reduction _ rhs) <- reduceTyFamApp_maybe envs Nominal tc tys
                -> do { loc <- bumpReductionDepth loc ty
                      ; follow envs loc rhs }
              _ -> return Nothing

    can_reduce tc = isTypeFamilyTyCon tc && not (isConstrainedFamilyTyCon tc)
tcKnownRetainedConstructorResult _ _ _ _ = return Nothing

{- Note [Retained constructors with a known Quick Look result]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fully applied retained constructor can get its erased parameters from a
known expected result, as in consume (Pair True payload). A type family in
the expected type can hide the constructor's parent. Follow nominal family
equations at the head only. The preview does not record an axiom use. Final
subsumption still checks the original result and records any evidence use.

Quick Look on the argument would instantiate the constructor without this
expected result. If the existing worker matcher finds every erased
parameter, leave the entire application to ordinary argument checking.
It checks the retained operand and each field against the constructor
telescope. No part of this preview assigns a metavariable or inspects a
written operand. Restrict it to plain value applications so the final
check does not repeat a source expansion.
-}

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
       ; ordinary_args <- tcQuickLookNeedsOrdinaryArgs rn_fun_arg rn_args
       ; (fun_ue, mb_fun_ty) <- if ordinary_args || any isExplicitRetainedArg rn_args
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

       ; known_result <- case mb_fun_ty of
           Just (fun, sigma) -> setSrcSpan fun_lspan_arg $ do
             result <- tcKnownRetainedConstructorResult rn_args fun sigma orig_arg_rho
             if isJust result then return True else
               case fun of
                 HsVar{} | all isPlainQuickLookArg rn_args
                         , let (_, _, tau) = tcRetainedInstTelescope sigma
                         , takesDirectIndexArgument tau -> do
                   plan <- tcRetainedKnownArgPlan tcRetainedQuickLookArgType Nothing fun sigma rn_args
                   return (maybe False (any (hasNativeDependentExpr . Core.Type)) plan)
                 _ -> return False
           Nothing -> return False

       ; case mb_fun_ty of {
           _ | known_result -> skipQuickLook app_lspan larg sc_arg_ty ;
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
       ; arg_influences_enclosing_call <- quickLookInfluencesCall orig_arg_rho app_res_rho

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
       ; unless (supportsQuickLookType ty) $
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
An erased existential binds a type variable. foldQLInstVars visits its kind
and body with the same type-binder operation it uses for forall. qlUnify
can assign the entire package to an instantiation variable. Its ordinary
occurs and concreteness checks apply. It does not match two packages by
opening them or by equating their witnesses. Final subsumption checks the
actual package type, including the binder mode.

Quick Look assigns instantiation variables without the ordinary unifier's
runtime-index escape check. Its instantiation-variable folders do not support
retained types either. Reject runtime indices and retained binders before both operations, including
those inside an erased package. Zonk first
so that the check also sees a type behind a filled metavariable. Check the
WpTyApp inputs before qlMonoInstArgs enters its ZonkM folder.

This check depends on the actual type, not ExperimentalDependentTypes.
Imported contracts can contain native types in a client without that flag.
An incomplete immutable application plan can retain DoQL in such a client.
The native-type rejection must still run before a Quick Look assignment or
unsupported traversal. See Note [Imported native application contracts].
Eisenberg et al., An Existential Crisis Resolved, section 8, integrates
erased existential types with the Quick Look rules. See also
Note [QuickLook unification].
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
    -- Both arrows must already be matchable. Otherwise the argument or
    -- result comparison can choose a type constructor before the solver
    -- learns that the receiver supplies an unmatchable family. See
    -- Note [Decomposing AppTy equalities] in GHC.Tc.Solver.Equality.
    go' (AppTy t1a t1b) ty2
      | Just (t2a, t2b) <- tcSplitAppTyNoView_maybe ty2
      = go_app t1a t1b t2a t2b
    go' ty1 (AppTy t2a t2b)
      | Just (t1a, t1b) <- tcSplitAppTyNoView_maybe ty1
      = go_app t1a t1b t2a t2b

    -- Type families have a monomorphic result, so if we see (say)
    --     Maybe kappa ~ F ty
    -- we know that kappa must be a monotype
    go' (TyConApp tc1 _) ty2 | isTypeFamilyTyCon tc1 = monomorphise ty2
    go' ty1 (TyConApp tc2 _) | isTypeFamilyTyCon tc2 = monomorphise ty1

    go' _ _ = return ()
       -- Don't look under foralls; see (UQL4) of Note [QuickLook unification]

    go_app f1 a1 f2 a2 =
      do { m1 <- matchable_arrow f1
         ; when m1 $
           do { m2 <- matchable_arrow f2
              ; when m2 $ do { go f1 f2; go a1 a2 } } }

    matchable_arrow fun
      | isMatchableFunTy kind = return True
      | otherwise = isMatchableFunTy <$> liftZonkM (zonkTcType kind)
      where kind = typeKind fun

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
