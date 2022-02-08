
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

module GHC.Tc.Gen.App
       ( tcApp
       , tcInferSigma
       , tcExprPrag ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcPolyExpr )

import GHC.Types.Basic ( Arity )
import GHC.Types.Id ( idArity, idName, hasNoBinding )
import GHC.Types.Name ( isWiredInName )
import GHC.Types.Var
import GHC.Builtin.Types ( multiplicityTy )
import GHC.Core.ConLike  ( ConLike(..) )
import GHC.Core.DataCon ( dataConRepArity
                        , isNewDataCon, isUnboxedSumDataCon, isUnboxedTupleDataCon )
import GHC.Tc.Gen.Head
import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep )
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst_maybe )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Subst (substTyWithInScope)
import GHC.Core.TyCo.FVs( shallowTyCoVarsOfType )
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Types.Var.Set
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.Names
import GHC.Driver.Session
import GHC.Types.SrcLoc
import GHC.Types.Var.Env  ( emptyTidyEnv, mkInScopeSet )
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.Function

import GHC.Prelude

{- *********************************************************************
*                                                                      *
                 Quick Look overview
*                                                                      *
********************************************************************* -}

{- Note [Quick Look]
~~~~~~~~~~~~~~~~~~~~
The implementation of Quick Look closely follows the QL paper
   A quick look at impredicativity, Serrano et al, ICFP 2020
   https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/

All the moving parts are in this module, GHC.Tc.Gen.App, so named
because it deal with n-ary application.  The main workhorse is tcApp.

Some notes relative to the paper

* The "instantiation variables" of the paper are ordinary unification
  variables.  We keep track of which variables are instantiation variables
  by keeping a set Delta of instantiation variables.

* When we learn what an instantiation variable must be, we simply unify
  it with that type; this is done in qlUnify, which is the function mgu_ql(t1,t2)
  of the paper.  This may fill in a (mutable) instantiation variable with
  a polytype.

* When QL is done, we don't need to turn the un-filled-in
  instantiation variables into unification variables -- they
  already /are/ unification varibles!  See also
  Note [Instantiation variables are short lived].

* We cleverly avoid the quadratic cost of QL, alluded to in the paper.
  See Note [Quick Look at value arguments]

Note [Instantiation variables are short lived]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By the time QL is done, all filled-in occurrences of instantiation
variables have been zonked away (see "Crucial step" in tcValArgs),
and so the constraint /generator/ never subsequently sees a meta-type
variable filled in with a polytype -- a meta type variable stands
(only) for a monotype.  See Section 4.3 "Applications and instantiation"
of the paper.

However, the constraint /solver/ can see a meta-type-variable filled
in with a polytype (#18987). Suppose
  f :: forall a. Dict a => [a] -> [a]
  xs :: [forall b. b->b]
and consider the call (f xs).  QL will
* Instantiate f, with a := kappa, where kappa is an instantiation variable
* Emit a constraint (Dict kappa), via instantiateSigma, called from tcInstFun
* Do QL on the argument, to discover kappa := forall b. b->b

But by the time the third step has happened, the constraint has been
emitted into the monad.  The constraint solver will later find it, and
rewrite it to (Dict (forall b. b->b)). That's fine -- the constraint
solver does no implicit instantiation (which is what makes it so
tricky to have foralls hiding inside unification variables), so there
is no difficulty with allowing those filled-in kappa's to persist.
(We could find them and zonk them away, but that would cost code and
execution time, for no purpose.)

Since the constraint solver does not do implicit instantiation (as the
constraint generator does), the fact that a unification variable might
stand for a polytype does not matter.
-}


{- *********************************************************************
*                                                                      *
              tcInferSigma
*                                                                      *
********************************************************************* -}

tcInferSigma :: Bool -> LHsExpr GhcRn -> TcM TcSigmaType
-- Used only to implement :type; see GHC.Tc.Module.tcRnExpr
-- True  <=> instantiate -- return a rho-type
-- False <=> don't instantiate -- return a sigma-type
tcInferSigma inst (L loc rn_expr)
  | (fun@(rn_fun,_), rn_args) <- splitHsApps rn_expr
  = addExprCtxt rn_expr $
    setSrcSpanA loc     $
    do { do_ql <- wantQuickLook rn_fun
       ; (_tc_fun, fun_sigma) <- tcInferAppHead fun rn_args
       ; (_delta, inst_args, app_res_sigma) <- tcInstFun do_ql inst fun fun_sigma rn_args
       ; _tc_args <- tcValArgs do_ql inst_args
       ; return app_res_sigma }

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
      |  $([| head |])    -- HsSpliceE+HsSpliced+HsSplicedExpr: untyped TH expression splices
      |  other_expr       -- Other expressions

When tcExpr sees something that starts an application chain (namely,
any of the constructors in 'app' or 'head'), it invokes tcApp to
typecheck it: see Note [tcApp: typechecking applications].  However,
for HsPar and HsPragE, there is no tcWrapResult (which would
instantiate types, bypassing Quick Look), so nothing is gained by
using the application chain route, and we can just recurse to tcExpr.

A "head" has three special cases (for which we can infer a polytype
using tcInferAppHead_maybe); otherwise is just any old expression (for
which we can infer a rho-type (via tcInfer).

There is no special treatment for HsUnboundVar, HsOverLit etc, because
we can't get a polytype from them.

Left and right sections (e.g. (x +) and (+ x)) are not yet supported.
Probably left sections (x +) would be easy to add, since x is the
first arg of (+); but right sections are not so easy.  For symmetry
reasons I've left both unchanged, in GHC.Tc.Gen.Expr.

It may not be immediately obvious why ExprWithTySig (e::ty) should be
dealt with by tcApp, even when it is not applied to anything. Consider
   f :: [forall a. a->a] -> Int
   ...(f (undefined :: forall b. b))...
Clearly this should work!  But it will /only/ work because if we
instantiate that (forall b. b) impredicatively!  And that only happens
in tcApp.

We also wish to typecheck application chains with untyped Template Haskell
splices in the head, such as this example from #21038:
    data Foo = MkFoo (forall a. a -> a)
    f = $([| MkFoo |]) $ \x -> x
This should typecheck just as if the TH splice was never in the way—that is,
just as if the user had written `MkFoo $ \x -> x`. We could conceivably have
a case for typed TH expression splices too, but it wouldn't be useful in
practice, since the types of typed TH expressions aren't allowed to have
polymorphic types, such as the type of MkFoo.

Note [tcApp: typechecking applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcApp implements the APP-Downarrow/Uparrow rule of
Fig 3, plus the modification in Fig 5, of the QL paper:
"A quick look at impredicativity" (ICFP'20).

It treats application chains (f e1 @ty e2) specially:

* So we can report errors like "in the third arument of a call of f"

* So we can do Visible Type Application (VTA), for which we must not
  eagerly instantiate the function part of the application.

* So that we can do Quick Look impredicativity.

tcApp works like this:

1. Use splitHsApps, which peels off
     HsApp, HsTypeApp, HsPrag, HsPar
   returning the function in the corner and the arguments

   splitHsApps can deal with infix as well as prefix application,
   and returns a Rebuilder to re-assemble the the application after
   typechecking.

   The "list of arguments" is [HsExprArg], described in Note [HsExprArg].
   in GHC.Tc.Gen.Head

2. Use tcInferAppHead to infer the type of the function,
     as an (uninstantiated) TcSigmaType
   There are special cases for
     HsVar, HsRecSel, and ExprWithTySig
   Otherwise, delegate back to tcExpr, which
     infers an (instantiated) TcRhoType

3. Use tcInstFun to instantiate the function, Quick-Looking as we go.
   This implements the |-inst judgement in Fig 4, plus the
   modification in Fig 5, of the QL paper:
   "A quick look at impredicativity" (ICFP'20).

   In tcInstFun we take a quick look at value arguments, using
   quickLookArg.  See Note [Quick Look at value arguments].

4. Use quickLookResultType to take a quick look at the result type,
   when in checking mode.  This is the shaded part of APP-Downarrow
   in Fig 5.

5. Use unifyResultType to match up the result type of the call
   with that expected by the context.  See Note [Unify with
   expected type before typechecking arguments]

6. Use tcValArgs to typecheck the value arguments

7. After a gruesome special case for tagToEnum, rebuild the result.


Some cases that /won't/ work:

1. Consider this (which uses visible type application):

    (let { f :: forall a. a -> a; f x = x } in f) @Int

   Since 'let' is not among the special cases for tcInferAppHead,
   we'll delegate back to tcExpr, which will instantiate f's type
   and the type application to @Int will fail.  Too bad!

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

The latter is much better. That is why we call unifyExpectedType
before tcValArgs.
-}

tcApp :: HsExpr GhcRn -> ExpRhoType -> TcM (HsExpr GhcTc)
-- See Note [tcApp: typechecking applications]
tcApp rn_expr exp_res_ty
  | (fun@(rn_fun, fun_ctxt), rn_args) <- splitHsApps rn_expr
  = do { (tc_fun, fun_sigma) <- tcInferAppHead fun rn_args

       -- Instantiate
       ; do_ql <- wantQuickLook rn_fun
       ; (delta, inst_args, app_res_rho) <- tcInstFun do_ql True fun fun_sigma rn_args

       -- Check for representation polymorphism in the case that
       -- the head of the application is a primop or data constructor
       -- which has argument types that are representation-polymorphic.
       -- This amounts to checking that the leftover argument types,
       -- up until the arity, are not representation-polymorphic,
       -- so that we can perform eta-expansion later without introducing
       -- representation-polymorphic binders.
       --
       -- See Note [Checking for representation-polymorphic built-ins]
       ; traceTc "tcApp FRR" $
           vcat
             [ text "tc_fun =" <+> ppr tc_fun
             , text "inst_args =" <+> ppr inst_args
             , text "app_res_rho =" <+> ppr app_res_rho ]
       ; hasFixedRuntimeRep_remainingValArgs inst_args app_res_rho tc_fun

       -- Quick look at result
       ; app_res_rho <- if do_ql
                        then quickLookResultType delta app_res_rho exp_res_ty
                        else return app_res_rho

       -- Unify with expected type from the context
       -- See Note [Unify with expected type before typechecking arguments]
       --
       -- perhaps_add_res_ty_ctxt: Inside an expansion, the addFunResCtxt stuff is
       --    more confusing than helpful because the function at the head isn't in
       --    the source program; it was added by the renamer.  See
       --    Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr
       ; let  perhaps_add_res_ty_ctxt thing_inside
                 | insideExpansion fun_ctxt
                 = thing_inside
                 | otherwise
                 = addFunResCtxt rn_fun rn_args app_res_rho exp_res_ty $
                   thing_inside

       ; res_co <- perhaps_add_res_ty_ctxt $
                   unifyExpectedType rn_expr app_res_rho exp_res_ty

       ; whenDOptM Opt_D_dump_tc_trace $
         do { inst_args <- mapM zonkArg inst_args  -- Only when tracing
            ; traceTc "tcApp" (vcat [ text "rn_fun"       <+> ppr rn_fun
                               , text "inst_args"    <+> brackets (pprWithCommas pprHsExprArgTc inst_args)
                               , text "do_ql:  "     <+> ppr do_ql
                               , text "fun_sigma:  " <+> ppr fun_sigma
                               , text "delta:      " <+> ppr delta
                               , text "app_res_rho:" <+> ppr app_res_rho
                               , text "exp_res_ty:"  <+> ppr exp_res_ty
                               , text "rn_expr:"     <+> ppr rn_expr ]) }

       -- Typecheck the value arguments
       ; tc_args <- tcValArgs do_ql inst_args

       -- Reconstruct, with a special cases for tagToEnum#.
       ; tc_expr <-
          if isTagToEnum rn_fun
          then tcTagToEnum tc_fun fun_ctxt tc_args app_res_rho
          else do return (rebuildHsApps tc_fun fun_ctxt tc_args)

       -- Wrap the result
       ; return (mkHsWrapCo res_co tc_expr) }

{-
Note [Checking for representation-polymorphic built-ins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We cannot have representation-polymorphic or levity-polymorphic
function arguments. See Note [Representation polymorphism invariants]
in GHC.Core.  That is checked by the calls to `hasFixedRuntimeRep ` in
`tcEValArg`.

But some /built-in/ functions are representation-polymorphic.  Users
can't define such Ids; they are all GHC built-ins or data
constructors.  Specifically they are:

1. A few wired-in Ids like unsafeCoerce#, with compulsory unfoldings.
2. Primops, such as raise#
3. Newtype constructors with `UnliftedNewtypes` that have
   a representation-polymorphic argument.
4. Representation-polymorphic data constructors: unboxed tuples
   and unboxed sums.

For (1) consider
  badId :: forall r (a :: TYPE r). a -> a
  badId = unsafeCoerce# @r @r @a @a

The wired-in function
  unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                   (a :: TYPE r1) (b :: TYPE r2).
                   a -> b
has a convenient but representation-polymorphic type. It has no
binding; instead it has a compulsory unfolding, after which we
would have
  badId = /\r /\(a :: TYPE r). \(x::a). ...body of unsafeCorece#...
And this is no good because of that rep-poly \(x::a).  So we want
to reject this.

On the other hand
  goodId :: forall (a :: Type). a -> a
  goodId = unsafeCoerce# @LiftedRep @LiftedRep @a @a

is absolutely fine, because after we inline the unfolding, the \(x::a)
is representation-monomorphic.

Test cases: T14561, RepPolyWrappedVar2.

For primops (2) the situation is similar; they are eta-expanded in
CorePrep to be saturated, and that eta-expansion must not add a
representation-polymorphic lambda.

Test cases: T14561b, RepPolyWrappedVar, UnliftedNewtypesCoerceFail.

For (3), consider a representation-polymorphic newtype with
UnliftedNewtypes:

  type Id :: forall r. TYPE r -> TYPE r
  newtype Id a where { MkId :: a }

  bad :: forall r (a :: TYPE r). a -> Id a
  bad = MkId @r @a             -- Want to reject

  good :: forall (a :: Type). a -> Id a
  good = MkId @LiftedRep @a   -- Want to accept

Test cases: T18481, UnliftedNewtypesLevityBinder

So these three cases need special treatment. We add a special case
in tcApp to check whether an application of an Id has any remaining
representation-polymorphic arguments, after instantiation and application
of previous arguments.  This is achieved by the hasFixedRuntimeRep_remainingValArgs
function, which computes the types of the remaining value arguments, and checks
that each of these have a fixed runtime representation using hasFixedRuntimeRep.

Wrinkles

* Because of Note [Typechecking data constructors] in GHC.Tc.Gen.Head,
  we desugar a representation-polymorphic data constructor application
  like this:
     (/\(r :: RuntimeRep) (a :: TYPE r) \(x::a). K r a x) @LiftedRep Int 4
  That is, a rep-poly lambda applied to arguments that instantiate it in
  a rep-mono way.  It's a bit like a compulsory unfolding that has been
  inlined, but not yet beta-reduced.

  Because we want to accept this, we switch off Lint's representation
  polymorphism checks when Lint checks the output of the desugarer;
  see the lf_check_fixed_repy flag in GHC.Core.Lint.lintCoreBindings.

* Arity.  We don't want to check for arguments past the
  arity of the function.  For example

      raise# :: forall {r :: RuntimeRep} (a :: Type) (b :: TYPE r). a -> b

  has arity 1, so an instantiation such as:

      foo :: forall w r (z :: TYPE r). w -> z -> z
      foo = raise# @w @(z -> z)

  is unproblematic.  This means we must take care not to perform a
  representation-polymorphism check on `z`.

  To achieve this, we consult the arity of the 'Id' which is the head
  of the application (or just use 1 for a newtype constructor),
  and keep track of how many value-level arguments we have seen,
  to ensure we stop checking once we reach the arity.
  This is slightly complicated by the need to include both visible
  and invisible arguments, as the arity counts both:
  see GHC.Tc.Gen.Head.countVisAndInvisValArgs.

  Test cases: T20330{a,b}

-}

-- | Check for representation-polymorphism in the remaining argument types
-- of a variable or data constructor, after it has been instantiated and applied to some arguments.
--
-- See Note [Checking for representation-polymorphic built-ins]
hasFixedRuntimeRep_remainingValArgs :: [HsExprArg 'TcpInst] -> TcRhoType -> HsExpr GhcTc -> TcM ()
hasFixedRuntimeRep_remainingValArgs applied_args app_res_rho = \case

  HsVar _ (L _ fun_id)

    -- (1): unsafeCoerce#
    -- 'unsafeCoerce#' is peculiar: it is patched in manually as per
    -- Note [Wiring in unsafeCoerce#] in GHC.HsToCore.
    -- Unfortunately, even though its arity is set to 1 in GHC.HsToCore.mkUnsafeCoercePrimPair,
    -- at this stage, if we query idArity, we get 0. This is because
    -- we end up looking at the non-patched version of unsafeCoerce#
    -- defined in Unsafe.Coerce, and that one indeed has arity 0.
    --
    -- We thus manually specify the correct arity of 1 here.
    | idName fun_id == unsafeCoercePrimName
    -> check_thing fun_id 1 (FRRNoBindingResArg fun_id)

    -- (2): primops and other wired-in representation-polymorphic functions,
    -- such as 'rightSection', 'oneShot', etc; see bindings with Compulsory unfoldings
    -- in GHC.Types.Id.Make
    | isWiredInName (idName fun_id) && hasNoBinding fun_id
    -> check_thing fun_id (idArity fun_id) (FRRNoBindingResArg fun_id)
       -- NB: idArity consults the IdInfo of the Id. This can be a problem
       -- in the presence of hs-boot files, as we might not have finished
       -- typechecking; inspecting the IdInfo at this point can cause
       -- strange Core Lint errors (see #20447).
       -- We avoid this entirely by only checking wired-in names,
       -- as those are the only ones this check is applicable to anyway.


  XExpr (ConLikeTc (RealDataCon con) _ _)
  -- (3): Representation-polymorphic newtype constructors.
    | isNewDataCon con
  -- (4): Unboxed tuples and unboxed sums
    || isUnboxedTupleDataCon con
    || isUnboxedSumDataCon con
    -> check_thing con (dataConRepArity con) (FRRDataConArg con)

  _ -> return ()

  where
    nb_applied_vis_val_args :: Int
    nb_applied_vis_val_args = count isHsValArg applied_args

    nb_applied_val_args :: Int
    nb_applied_val_args = countVisAndInvisValArgs applied_args

    arg_tys :: [TyCoBinder]
    arg_tys = fst $ splitPiTys app_res_rho
    -- We do not need to zonk app_res_rho first, because the number of arrows
    -- in the (possibly instantiated) inferred type of the function will
    -- be at least the arity of the function.

    check_thing :: Outputable thing => thing -> Arity -> (Int -> FRROrigin) -> TcM ()
    check_thing thing arity mk_frr_orig = do
      traceTc "tcApp remainingValArgs check_thing" (debug_msg thing arity)
      go (nb_applied_vis_val_args + 1) (nb_applied_val_args + 1) arg_tys
      where
        go :: Int -- ^ visible value argument index
                  -- (only used to report the argument position in error messages)
           -> Int -- ^ value argument index
                  -- used to count up to the arity to ensure we don't check too many argument types
           -> [TyCoBinder]
           -> TcM ()
        go _ i_val _
          | i_val > arity
          = return ()
        go _ _ []
          -- Should never happen: it would mean that the arity is higher
          -- than the number of arguments apparent from the type
          = pprPanic "hasFixedRuntimeRep_remainingValArgs" (debug_msg thing arity)
        go i_visval !i_val (Anon af (Scaled _ arg_ty) : tys)
          = case af of
              InvisArg ->
                go i_visval (i_val + 1) tys
              VisArg   -> do
                _concrete_ev <- hasFixedRuntimeRep (mk_frr_orig i_visval) arg_ty
                go (i_visval + 1) (i_val + 1) tys
        go i_visval i_val (_: tys)
          = go i_visval i_val tys

    -- A message containing all the relevant info, in case this functions
    -- needs to be debugged again at some point.
    debug_msg :: Outputable thing => thing -> Arity -> SDoc
    debug_msg thing arity =
      vcat
        [ text "thing =" <+> ppr thing
        , text "arity =" <+> ppr arity
        , text "applied_args =" <+> ppr applied_args
        , text "nb_applied_vis_val_args =" <+> ppr nb_applied_vis_val_args
        , text "nb_applied_val_args =" <+> ppr nb_applied_val_args
        , text "arg_tys =" <+> ppr arg_tys ]

--------------------
wantQuickLook :: HsExpr GhcRn -> TcM Bool
-- GHC switches on impredicativity all the time for ($)
wantQuickLook (HsVar _ (L _ f))
  | getUnique f `elem` quickLookKeys = return True
wantQuickLook _                      = xoptM LangExt.ImpredicativeTypes

quickLookKeys :: [Unique]
-- See Note [Quick Look for particular Ids]
quickLookKeys = [dollarIdKey, leftSectionKey, rightSectionKey]

zonkQuickLook :: Bool -> TcType -> TcM TcType
-- After all Quick Look unifications are done, zonk to ensure that all
-- instantiation variables are substituted away
--
-- So far as the paper is concerned, this step applies
-- the poly-substitution Theta, learned by QL, so that we
-- "see" the polymorphism in that type
--
-- In implementation terms this ensures that no unification variable
-- linger on that have been filled in with a polytype
zonkQuickLook do_ql ty
  | do_ql     = zonkTcType ty
  | otherwise = return ty

-- zonkArg is used *only* during debug-tracing, to make it easier to
-- see what is going on.  For that reason, it is not a full zonk: add
-- more if you need it.
zonkArg :: HsExprArg 'TcpInst -> TcM (HsExprArg 'TcpInst)
zonkArg eva@(EValArg { eva_arg_ty = Scaled m ty })
  = do { ty' <- zonkTcType ty
       ; return (eva { eva_arg_ty = Scaled m ty' }) }
zonkArg arg = return arg



----------------
tcValArgs :: Bool                    -- Quick-look on?
          -> [HsExprArg 'TcpInst]    -- Actual argument
          -> TcM [HsExprArg 'TcpTc]  -- Resulting argument
tcValArgs do_ql args
  = mapM tc_arg args
  where
    tc_arg :: HsExprArg 'TcpInst -> TcM (HsExprArg 'TcpTc)
    tc_arg (EPrag l p)           = return (EPrag l (tcExprPrag p))
    tc_arg (EWrap w)             = return (EWrap w)
    tc_arg (ETypeArg l hs_ty ty) = return (ETypeArg l hs_ty ty)

    tc_arg eva@(EValArg { eva_arg = arg, eva_arg_ty = Scaled mult arg_ty
                        , eva_ctxt = ctxt })
      = do { -- Crucial step: expose QL results before checking arg_ty
             -- So far as the paper is concerned, this step applies
             -- the poly-substitution Theta, learned by QL, so that we
             -- "see" the polymorphism in that argument type. E.g.
             --    (:) e ids, where ids :: [forall a. a->a]
             --                     (:) :: forall p. p->[p]->[p]
             -- Then Theta = [p :-> forall a. a->a], and we want
             -- to check 'e' with expected type (forall a. a->a)
             -- See Note [Instantiation variables are short lived]
             arg_ty <- zonkQuickLook do_ql arg_ty

             -- Now check the argument
           ; arg' <- tcScalingUsage mult $
                     do { traceTc "tcEValArg" $
                          vcat [ ppr ctxt
                               , text "arg type:" <+> ppr arg_ty
                               , text "arg:" <+> ppr arg ]
                        ; tcEValArg ctxt arg arg_ty }

           ; return (eva { eva_arg    = ValArg arg'
                         , eva_arg_ty = Scaled mult arg_ty }) }

tcEValArg :: AppCtxt -> EValArg 'TcpInst -> TcSigmaType -> TcM (LHsExpr GhcTc)
-- Typecheck one value argument of a function call
tcEValArg ctxt (ValArg larg@(L arg_loc arg)) exp_arg_sigma
  = addArgCtxt ctxt larg $
    do { arg' <- tcPolyExpr arg (mkCheckExpType exp_arg_sigma)
       ; _concrete_ev <- hasFixedRuntimeRep (FRRApp arg) exp_arg_sigma
       ; return (L arg_loc arg') }

tcEValArg ctxt (ValArgQL { va_expr = larg@(L arg_loc arg)
                         , va_fun = (inner_fun, fun_ctxt)
                         , va_args = inner_args
                         , va_ty = app_res_rho }) exp_arg_sigma
  = addArgCtxt ctxt larg $
    do { traceTc "tcEValArgQL {" (vcat [ ppr inner_fun <+> ppr inner_args ])
       ; tc_args <- tcValArgs True inner_args
       ; co      <- unifyType Nothing app_res_rho exp_arg_sigma
       ; _concrete_ev <- hasFixedRuntimeRep (FRRApp arg) exp_arg_sigma
       ; traceTc "tcEValArg }" empty
       ; return (L arg_loc $ mkHsWrapCo co $
                 rebuildHsApps inner_fun fun_ctxt tc_args) }

{- *********************************************************************
*                                                                      *
              Instantiating the call
*                                                                      *
********************************************************************* -}

type Delta = TcTyVarSet   -- Set of instantiation variables,
                          --   written \kappa in the QL paper
                          -- Just a set of ordinary unification variables,
                          --   but ones that QL may fill in with polytypes

tcInstFun :: Bool   -- True  <=> Do quick-look
          -> Bool   -- False <=> Instantiate only /inferred/ variables at the end
                    --           so may return a sigma-typex
                    -- True  <=> Instantiate all type variables at the end:
                    --           return a rho-type
                    -- The /only/ call site that passes in False is the one
                    --    in tcInferSigma, which is used only to implement :type
                    -- Otherwise we do eager instantiation; in Fig 5 of the paper
                    --    |-inst returns a rho-type
          -> (HsExpr GhcRn, AppCtxt)        -- Error messages only
          -> TcSigmaType -> [HsExprArg 'TcpRn]
          -> TcM ( Delta
                 , [HsExprArg 'TcpInst]
                 , TcSigmaType )
-- This function implements the |-inst judgement in Fig 4, plus the
-- modification in Fig 5, of the QL paper:
-- "A quick look at impredicativity" (ICFP'20).
tcInstFun do_ql inst_final (rn_fun, fun_ctxt) fun_sigma rn_args
  = do { traceTc "tcInstFun" (vcat [ ppr rn_fun, ppr fun_sigma
                                   , text "args:" <+> ppr rn_args
                                   , text "do_ql" <+> ppr do_ql ])
       ; go emptyVarSet [] [] fun_sigma rn_args }
  where
    fun_loc  = appCtxtLoc fun_ctxt
    fun_orig = exprCtOrigin (case fun_ctxt of
                               VAExpansion e _ -> e
                               VACall e _ _    -> e)
    set_fun_ctxt thing_inside
      | not (isGoodSrcSpan fun_loc)   -- noSrcSpan => no arguments
      = thing_inside                  -- => context is already set
      | otherwise
      = setSrcSpan fun_loc $
        case fun_ctxt of
          VAExpansion orig _ -> addExprCtxt orig thing_inside
          VACall {}          -> thing_inside

    herald = sep [ text "The function" <+> quotes (ppr rn_fun)
                 , text "is applied to"]

    -- Count value args only when complaining about a function
    -- applied to too many value args
    -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify.
    n_val_args = count isHsValArg rn_args

    fun_is_out_of_scope  -- See Note [VTA for out-of-scope functions]
      = case rn_fun of
          HsUnboundVar {} -> True
          _               -> False

    inst_all :: ArgFlag -> Bool
    inst_all (Invisible {}) = True
    inst_all Required       = False

    inst_inferred :: ArgFlag -> Bool
    inst_inferred (Invisible InferredSpec)  = True
    inst_inferred (Invisible SpecifiedSpec) = False
    inst_inferred Required                  = False

    inst_fun :: [HsExprArg 'TcpRn] -> ArgFlag -> Bool
    inst_fun [] | inst_final  = inst_all
                | otherwise   = inst_inferred
    inst_fun (EValArg {} : _) = inst_all
    inst_fun _                = inst_inferred

    -----------
    go, go1 :: Delta
            -> [HsExprArg 'TcpInst]  -- Accumulator, reversed
            -> [Scaled TcSigmaType]  -- Value args to which applied so far
            -> TcSigmaType -> [HsExprArg 'TcpRn]
            -> TcM (Delta, [HsExprArg 'TcpInst], TcSigmaType)

    -- go: If fun_ty=kappa, look it up in Theta
    go delta acc so_far fun_ty args
      | Just kappa <- tcGetTyVar_maybe fun_ty
      , kappa `elemVarSet` delta
      = do { cts <- readMetaTyVar kappa
           ; case cts of
                Indirect fun_ty' -> go  delta acc so_far fun_ty' args
                Flexi            -> go1 delta acc so_far fun_ty  args }
     | otherwise
     = go1 delta acc so_far fun_ty args

    -- go1: fun_ty is not filled-in instantiation variable
    --      ('go' dealt with that case)

    -- Rule IALL from Fig 4 of the QL paper
    -- c.f. GHC.Tc.Utils.Instantiate.topInstantiate
    go1 delta acc so_far fun_ty args
      | (tvs,   body1) <- tcSplitSomeForAllTyVars (inst_fun args) fun_ty
      , (theta, body2) <- tcSplitPhiTy body1
      , not (null tvs && null theta)
      = do { (inst_tvs, wrap, fun_rho) <- set_fun_ctxt $
                                          instantiateSigma fun_orig tvs theta body2
                 -- set_fun_ctxt: important for the class constraints
                 -- that may be emitted from instantiating fun_sigma
           ; go (delta `extendVarSetList` inst_tvs)
                (addArgWrap wrap acc) so_far fun_rho args }
                -- Going around again means we deal easily with
                -- nested  forall a. Eq a => forall b. Show b => blah

    -- Rule IRESULT from Fig 4 of the QL paper
    go1 delta acc _ fun_ty []
       = do { traceTc "tcInstFun:ret" (ppr fun_ty)
            ; return (delta, reverse acc, fun_ty) }

    go1 delta acc so_far fun_ty (EWrap w : args)
      = go1 delta (EWrap w : acc) so_far fun_ty args

    go1 delta acc so_far fun_ty (EPrag sp prag : args)
      = go1 delta (EPrag sp prag : acc) so_far fun_ty args

    -- Rule ITYARG from Fig 4 of the QL paper
    go1 delta acc so_far fun_ty ( ETypeArg { eva_ctxt = ctxt, eva_hs_ty = hs_ty }
                                : rest_args )
      | fun_is_out_of_scope   -- See Note [VTA for out-of-scope functions]
      = go delta acc so_far fun_ty rest_args

      | otherwise
      = do { (ty_arg, inst_ty) <- tcVTA fun_ty hs_ty
           ; let arg' = ETypeArg { eva_ctxt = ctxt, eva_hs_ty = hs_ty, eva_ty = ty_arg }
           ; go delta (arg' : acc) so_far inst_ty rest_args }

    -- Rule IVAR from Fig 4 of the QL paper:
    go1 delta acc so_far fun_ty args@(EValArg {} : _)
      | Just kappa <- tcGetTyVar_maybe fun_ty
      , kappa `elemVarSet` delta
      = -- Function type was of form   f :: forall a b. t1 -> t2 -> b
        -- with 'b', one of the quantified type variables, in the corner
        -- but the call applies it to three or more value args.
        -- Suppose b is instantiated by kappa.  Then we want to make fresh
        -- instantiation variables nu1, nu2, and set kappa := nu1 -> nu2
        --
        -- In principle what is happening here is not unlike matchActualFunTysRho
        -- but there are many small differences:
        --   - We know that the function type in unfilled meta-tyvar
        --     matchActualFunTysRho is much more general, has a loop, etc.
        --   - We must be sure to actually update the variable right now,
        --     not defer in any way, because this is a QL instantiation variable.
        --   - We need the freshly allocated unification variables, to extend
        --     delta with.
        -- It's easier just to do the job directly here.
        do { let valArgsCount = countLeadingValArgs args
           ; arg_nus <- replicateM valArgsCount newOpenFlexiTyVar
             -- We need variables for multiplicity (#18731)
             -- Otherwise, 'undefined x' wouldn't be linear in x
           ; mults   <- replicateM valArgsCount (newFlexiTyVarTy multiplicityTy)
           ; res_nu  <- newOpenFlexiTyVar
           ; kind_co <- unifyKind Nothing liftedTypeKind (tyVarKind kappa)
           ; let delta'  = delta `extendVarSetList` (res_nu:arg_nus)
                 arg_tys = mkTyVarTys arg_nus
                 res_ty  = mkTyVarTy res_nu
                 fun_ty' = mkVisFunTys (zipWithEqual "tcInstFun" mkScaled mults arg_tys) res_ty
                 co_wrap = mkWpCastN (mkTcGReflLeftCo Nominal fun_ty' kind_co)
                 acc'    = addArgWrap co_wrap acc
                 -- Suppose kappa :: kk
                 -- Then fun_ty :: kk, fun_ty' :: Type, kind_co :: Type ~ kk
                 --      co_wrap :: (fun_ty' |> kind_co) ~ fun_ty'
           ; writeMetaTyVar kappa (mkCastTy fun_ty' kind_co)
                 -- kappa is uninstantiated ('go' already checked that)
           ; go delta' acc' so_far fun_ty' args }

    -- Rule IARG from Fig 4 of the QL paper:
    go1 delta acc so_far fun_ty
        (eva@(EValArg { eva_arg = ValArg arg, eva_ctxt = ctxt })  : rest_args)
      = do { (wrap, arg_ty, res_ty) <- matchActualFunTySigma herald
                                          (Just $ HsExprRnThing rn_fun)
                                          (n_val_args, so_far) fun_ty
          ; (delta', arg') <- if do_ql
                              then addArgCtxt ctxt arg $
                                   -- Context needed for constraints
                                   -- generated by calls in arg
                                   quickLookArg delta arg arg_ty
                              else return (delta, ValArg arg)
          ; let acc' = eva { eva_arg = arg', eva_arg_ty = arg_ty }
                       : addArgWrap wrap acc
          ; go delta' acc' (arg_ty:so_far) res_ty rest_args }


addArgCtxt :: AppCtxt -> LHsExpr GhcRn
           -> TcM a -> TcM a
-- Adds a "In the third argument of f, namely blah"
-- context, unless we are in generated code, in which case
-- use "In the expression: arg"
---See Note [Rebindable syntax and HsExpansion] in GHC.Hs.Expr
addArgCtxt (VACall fun arg_no _) (L arg_loc arg) thing_inside
  = setSrcSpanA arg_loc $
    addErrCtxt (funAppCtxt fun arg arg_no) $
    thing_inside

addArgCtxt (VAExpansion {}) (L arg_loc arg) thing_inside
  = setSrcSpanA arg_loc $
    addExprCtxt arg    $  -- Auto-suppressed if arg_loc is generated
    thing_inside

{- *********************************************************************
*                                                                      *
              Visible type application
*                                                                      *
********************************************************************* -}

tcVTA :: TcType            -- Function type
      -> LHsWcType GhcRn   -- Argument type
      -> TcM (TcType, TcType)
-- Deal with a visible type application
-- The function type has already had its Inferred binders instantiated
tcVTA fun_ty hs_ty
  | Just (tvb, inner_ty) <- tcSplitForAllTyVarBinder_maybe fun_ty
  , binderArgFlag tvb == Specified
    -- It really can't be Inferred, because we've just
    -- instantiated those. But, oddly, it might just be Required.
    -- See Note [Required quantifiers in the type of a term]
  = do { let tv   = binderVar tvb
             kind = tyVarKind tv
       ; ty_arg <- tcHsTypeApp hs_ty kind

       ; inner_ty <- zonkTcType inner_ty
             -- See Note [Visible type application zonk]

       ; let in_scope  = mkInScopeSet (tyCoVarsOfTypes [fun_ty, ty_arg])
             insted_ty = substTyWithInScope in_scope [tv] [ty_arg] inner_ty
                         -- NB: tv and ty_arg have the same kind, so this
                         --     substitution is kind-respecting
       ; traceTc "VTA" (vcat [ppr tv, debugPprType kind
                             , debugPprType ty_arg
                             , debugPprType (tcTypeKind ty_arg)
                             , debugPprType inner_ty
                             , debugPprType insted_ty ])
       ; return (ty_arg, insted_ty) }

  | otherwise
  = do { (_, fun_ty) <- zonkTidyTcType emptyTidyEnv fun_ty
       ; failWith $ TcRnInvalidTypeApplication fun_ty hs_ty }

{- Note [Required quantifiers in the type of a term]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#15859)

  data A k :: k -> Type      -- A      :: forall k -> k -> Type
  type KindOf (a :: k) = k   -- KindOf :: forall k. k -> Type
  a = (undefined :: KindOf A) @Int

With ImpredicativeTypes (thin ice, I know), we instantiate
KindOf at type (forall k -> k -> Type), so
  KindOf A = forall k -> k -> Type
whose first argument is Required

We want to reject this type application to Int, but in earlier
GHCs we had an ASSERT that Required could not occur here.

The ice is thin; c.f. Note [No Required TyCoBinder in terms]
in GHC.Core.TyCo.Rep.

Note [VTA for out-of-scope functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose 'wurble' is not in scope, and we have
   (wurble @Int @Bool True 'x')

Then the renamer will make (HsUnboundVar "wurble") for 'wurble',
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
can check if it is a HsUnboundVar.  We use this info to simply skip
over any visible type arguments.  We've already inferred the type of
the function (in tcInferAppHead), so we'll /already/ have emitted a
Hole constraint; failing preserves that constraint.

We do /not/ want to fail altogether in this case (via failM) because
that may abandon an entire instance decl, which (in the presence of
-fdefer-type-errors) leads to leading to #17792.

Downside: the typechecked term has lost its visible type arguments; we
don't even kind-check them.  But let's jump that bridge if we come to
it.  Meanwhile, let's not crash!


Note [Visible type application zonk]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Substitutions should be kind-preserving, so we need kind(tv) = kind(ty_arg).

* tcHsTypeApp only guarantees that
    - ty_arg is zonked
    - kind(zonk(tv)) = kind(ty_arg)
  (checkExpectedKind zonks as it goes).

So we must zonk inner_ty as well, to guarantee consistency between zonk(tv)
and inner_ty. Otherwise we can build an ill-kinded type. An example was #14158,
where we had:
   id :: forall k. forall (cat :: k -> k -> *). forall (a :: k). cat a a
and we had the visible type application
  id @(->)

* We instantiated k := kappa, yielding
    forall (cat :: kappa -> kappa -> *). forall (a :: kappa). cat a a
* Then we called tcHsTypeApp (->) with expected kind (kappa -> kappa -> *).
* That instantiated (->) as ((->) q1 q1), and unified kappa := q1,
  Here q1 :: RuntimeRep
* Now we substitute
     cat  :->  (->) q1 q1 :: TYPE q1 -> TYPE q1 -> *
  but we must first zonk the inner_ty to get
      forall (a :: TYPE q1). cat a a
  so that the result of substitution is well-kinded
  Failing to do so led to #14158.

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
rather directly.

Wrinkles:

* We avoid zonking, so quickLookArg thereby sees the argument type /before/
  the QL substitution Theta is applied to it. So we achieve argument-order
  independence for free (see 5.7 in the paper).

* When we quick-look at an argument, we save the work done, by returning
  an EValArg with a ValArgQL inside it.  (It started life with a ValArg
  inside.)  The ValArgQL remembers all the work that QL did (notably,
  decomposing the argument and instantiating) so that tcValArgs does
  not need to repeat it.  Rather neat, and remarkably easy.
-}

----------------
quickLookArg :: Delta
             -> LHsExpr GhcRn       -- Argument
             -> Scaled TcSigmaType  -- Type expected by the function
             -> TcM (Delta, EValArg 'TcpInst)
-- See Note [Quick Look at value arguments]
--
-- The returned Delta is a superset of the one passed in
-- with added instantiation variables from
--   (a) the call itself
--   (b) the arguments of the call
quickLookArg delta larg (Scaled _ arg_ty)
  | isEmptyVarSet delta  = skipQuickLook delta larg
  | otherwise            = go arg_ty
  where
    guarded = isGuardedTy arg_ty
      -- NB: guardedness is computed based on the original,
      -- unzonked arg_ty, so we deliberately do not exploit
      -- guardedness that emerges a result of QL on earlier args

    go arg_ty | not (isRhoTy arg_ty)
              = skipQuickLook delta larg

              -- This top-level zonk step, which is the reason
              -- we need a local 'go' loop, is subtle
              -- See Section 9 of the QL paper
              | Just kappa <- tcGetTyVar_maybe arg_ty
              , kappa `elemVarSet` delta
              = do { info <- readMetaTyVar kappa
                   ; case info of
                       Indirect arg_ty' -> go arg_ty'
                       Flexi            -> quickLookArg1 guarded delta larg arg_ty }

              | otherwise
              = quickLookArg1 guarded delta larg arg_ty

isGuardedTy :: TcType -> Bool
isGuardedTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | otherwise                               = False

quickLookArg1 :: Bool -> Delta -> LHsExpr GhcRn -> TcSigmaType
              -> TcM (Delta, EValArg 'TcpInst)
quickLookArg1 guarded delta larg@(L _ arg) arg_ty
  = do { let (fun@(rn_fun, fun_ctxt), rn_args) = splitHsApps arg
       ; mb_fun_ty <- tcInferAppHead_maybe rn_fun rn_args
       ; traceTc "quickLookArg 1" $
         vcat [ text "arg:" <+> ppr arg
              , text "head:" <+> ppr rn_fun <+> dcolon <+> ppr mb_fun_ty
              , text "args:" <+> ppr rn_args ]

       ; case mb_fun_ty of {
           Nothing     -> -- fun is too complicated
                          skipQuickLook delta larg ;
           Just (tc_fun, fun_sigma) ->

    do { let no_free_kappas = findNoQuantVars fun_sigma rn_args
       ; traceTc "quickLookArg 2" $
         vcat [ text "no_free_kappas:" <+> ppr no_free_kappas
              , text "guarded:" <+> ppr guarded
              , text "tc_fun:" <+> ppr tc_fun
              , text "fun_sigma:" <+> ppr fun_sigma ]
       ; if not (guarded || no_free_kappas)
         then skipQuickLook delta larg
         else
    do { do_ql <- wantQuickLook rn_fun
       ; (delta_app, inst_args, app_res_rho) <- tcInstFun do_ql True fun fun_sigma rn_args
       ; traceTc "quickLookArg 3" $
         vcat [ text "arg:" <+> ppr arg
              , text "delta:" <+> ppr delta
              , text "delta_app:" <+> ppr delta_app
              , text "arg_ty:" <+> ppr arg_ty
              , text "app_res_rho:" <+> ppr app_res_rho ]

       -- Do quick-look unification
       -- NB: arg_ty may not be zonked, but that's ok
       ; let delta' = delta `unionVarSet` delta_app
       ; qlUnify delta' arg_ty app_res_rho

       ; let ql_arg = ValArgQL { va_expr  = larg
                               , va_fun   = (tc_fun, fun_ctxt)
                               , va_args  = inst_args
                               , va_ty    = app_res_rho }
       ; return (delta', ql_arg) } } } }

skipQuickLook :: Delta -> LHsExpr GhcRn -> TcM (Delta, EValArg 'TcpInst)
skipQuickLook delta larg = return (delta, ValArg larg)

----------------
quickLookResultType :: Delta -> TcRhoType -> ExpRhoType -> TcM TcRhoType
-- This function implements the shaded bit of rule APP-Downarrow in
-- Fig 5 of the QL paper: "A quick look at impredicativity" (ICFP'20).
-- It returns its second argument, but with any variables in Delta
-- substituted out, so no variables in Delta escape

quickLookResultType delta app_res_rho (Check exp_rho)
  = -- In checking mode only, do qlUnify with the expected result type
    do { unless (isEmptyVarSet delta)  $ -- Optimisation only
         qlUnify delta app_res_rho exp_rho
       ; return app_res_rho }

quickLookResultType _ app_res_rho (Infer {})
  = zonkTcType app_res_rho
    -- Zonk the result type, to ensure that we substitute out any
    -- filled-in instantiation variable before calling
    -- unifyExpectedType. In the Check case, this isn't necessary,
    -- because unifyExpectedType just drops to tcUnify; but in the
    -- Infer case a filled-in instantiation variable (filled in by
    -- tcInstFun) might perhaps escape into the constraint
    -- generator. The safe thing to do is to zonk any instantiation
    -- variables away.  See Note [Instantiation variables are short lived]

---------------------
qlUnify :: Delta -> TcType -> TcType -> TcM ()
-- Unify ty1 with ty2, unifying only variables in delta
qlUnify delta ty1 ty2
  = do { traceTc "qlUnify" (ppr delta $$ ppr ty1 $$ ppr ty2)
       ; go (emptyVarSet,emptyVarSet) ty1 ty2 }
  where
    go :: (TyVarSet, TcTyVarSet)
       -> TcType -> TcType
       -> TcM ()
    -- The TyVarSets give the variables bound by enclosing foralls
    -- for the corresponding type. Don't unify with these.
    go bvs (TyVarTy tv) ty2
      | tv `elemVarSet` delta = go_kappa bvs tv ty2

    go (bvs1, bvs2) ty1 (TyVarTy tv)
      | tv `elemVarSet` delta = go_kappa (bvs2,bvs1) tv ty1

    go bvs (CastTy ty1 _) ty2 = go bvs ty1 ty2
    go bvs ty1 (CastTy ty2 _) = go bvs ty1 ty2

    go _ (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2 -- See GHC.Tc.Utils.Unify
      = return ()  -- Note [Expanding synonyms during unification]

    -- Now, and only now, expand synonyms
    go bvs rho1 rho2
      | Just rho1 <- tcView rho1 = go bvs rho1 rho2
      | Just rho2 <- tcView rho2 = go bvs rho1 rho2

    go bvs (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , not (isTypeFamilyTyCon tc1)
      , tys1 `equalLength` tys2
      = zipWithM_ (go bvs) tys1 tys2

    -- Decompose (arg1 -> res1) ~ (arg2 -> res2)
    -- and         (c1 => res1) ~   (c2 => res2)
    -- But for the latter we only learn instantiation info from t1~t2
    -- We look at the multiplicity too, although the chances of getting
    -- impredicative instantiation info from there seems...remote.
    go bvs (FunTy { ft_af = af1, ft_arg = arg1, ft_res = res1, ft_mult = mult1 })
           (FunTy { ft_af = af2, ft_arg = arg2, ft_res = res2, ft_mult = mult2 })
      | af1 == af2
      = do { when (af1 == VisArg) $
             do { go bvs arg1 arg2; go bvs mult1 mult2 }
           ; go bvs res1 res2 }

    -- ToDo: c.f. Tc.Utils.unify.uType,
    -- which does not split FunTy here
    -- Also NB tcRepSplitAppTy here, which does not split (c => t)
    go bvs (AppTy t1a t1b) ty2
      | Just (t2a, t2b) <- tcRepSplitAppTy_maybe ty2
      = do { go bvs t1a t2a; go bvs t1b t2b }

    go bvs ty1 (AppTy t2a t2b)
      | Just (t1a, t1b) <- tcRepSplitAppTy_maybe ty1
      = do { go bvs t1a t2a; go bvs t1b t2b }

    go (bvs1, bvs2) (ForAllTy bv1 ty1) (ForAllTy bv2 ty2)
      = go (bvs1',bvs2') ty1 ty2
      where
       bvs1' = bvs1 `extendVarSet` binderVar bv1
       bvs2' = bvs2 `extendVarSet` binderVar bv2

    go _ _ _ = return ()


    ----------------
    go_kappa bvs kappa ty2
      = assertPpr (isMetaTyVar kappa) (ppr kappa) $
        do { info <- readMetaTyVar kappa
           ; case info of
               Indirect ty1 -> go bvs ty1 ty2
               Flexi        -> do { ty2 <- zonkTcType ty2
                                  ; go_flexi bvs kappa ty2 } }

    ----------------
    go_flexi (_,bvs2) kappa ty2  -- ty2 is zonked
      | -- See Note [Actual unification in qlUnify]
        let ty2_tvs = shallowTyCoVarsOfType ty2
      , not (ty2_tvs `intersectsVarSet` bvs2)
          -- Can't instantiate a delta-varto a forall-bound variable
      , Just ty2 <- occCheckExpand [kappa] ty2
          -- Passes the occurs check
      = do { let ty2_kind   = typeKind ty2
                 kappa_kind = tyVarKind kappa
           ; co <- unifyKind (Just (TypeThing ty2)) ty2_kind kappa_kind
                   -- unifyKind: see Note [Actual unification in qlUnify]

           ; traceTc "qlUnify:update" $
             vcat [ hang (ppr kappa <+> dcolon <+> ppr kappa_kind)
                       2 (text ":=" <+> ppr ty2 <+> dcolon <+> ppr ty2_kind)
                 , text "co:" <+> ppr co ]
           ; writeMetaTyVar kappa (mkCastTy ty2 co) }

      | otherwise
      = return ()   -- Occurs-check or forall-bound varialbe


{- Note [Actual unification in qlUnify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In qlUnify, if we find (kappa ~ ty), we are going to update kappa := ty.
That is the entire point of qlUnify!   Wrinkles:

* We must not unify with anything bound by an enclosing forall; e.g.
    (forall a. kappa -> Int) ~ forall a. a -> Int)
  That's tracked by the 'bvs' arg of 'go'.

* We must not make an occurs-check; we use occCheckExpand for that.

* checkTypeEq also checks for various other things, including
  - foralls, and predicate types (which we want to allow here)
  - type families (relates to a very specific and exotic performance
    question, that is unlikely to bite here)
  - blocking coercion holes
  After some thought we believe that none of these are relevant
  here

* What if kappa and ty have different kinds?  We solve that problem by
  calling unifyKind, producing a coercion perhaps emitting some deferred
  equality constraints.  That is /different/ from the approach we use in
  the main constraint solver for herterogeneous equalities; see Note
  [Equalities with incompatible kinds] in Solver.Canonical

  Why different? Because:
  - We can't use qlUnify to solve the kind constraint because qlUnify
    won't unify ordinary (non-instantiation) unification variables.
    (It would have to worry about lots of things like untouchability
    if it did.)
  - qlUnify can't give up if the kinds look un-equal because that would
    mean that it might succeed some times (when the eager unifier
    has already unified those kinds) but not others -- order
    dependence.
  - We can't use the ordinary unifier/constraint solver instead,
    because it doesn't unify polykinds, and has all kinds of other
    magic.  qlUnify is very focused.

  TL;DR Calling unifyKind seems like the lesser evil.
  -}

{- *********************************************************************
*                                                                      *
              Guardedness
*                                                                      *
********************************************************************* -}

findNoQuantVars :: TcSigmaType -> [HsExprArg 'TcpRn] -> Bool
-- True <=> there are no free quantified variables
--          in the result of the call
-- E.g. in the call (f e1 e2), if
--   f :: forall a b. a -> b -> Int   return True
--   f :: forall a b. a -> b -> b     return False (b is free)
findNoQuantVars fun_ty args
  = go emptyVarSet fun_ty args
  where
    need_instantiation []               = True
    need_instantiation (EValArg {} : _) = True
    need_instantiation _                = False

    go :: TyVarSet -> TcSigmaType -> [HsExprArg 'TcpRn] -> Bool
    go bvs fun_ty args
      | need_instantiation args
      , (tvs, theta, rho) <- tcSplitSigmaTy fun_ty
      , not (null tvs && null theta)
      = go (bvs `extendVarSetList` tvs) rho args

    go bvs fun_ty [] =  tyCoVarsOfType fun_ty `disjointVarSet` bvs

    go bvs fun_ty (EWrap {} : args) = go bvs fun_ty args
    go bvs fun_ty (EPrag {} : args) = go bvs fun_ty args

    go bvs fun_ty args@(ETypeArg {} : rest_args)
      | (tvs,  body1) <- tcSplitSomeForAllTyVars (== Inferred) fun_ty
      , (theta, body2) <- tcSplitPhiTy body1
      , not (null tvs && null theta)
      = go (bvs `extendVarSetList` tvs) body2 args
      | Just (_tv, res_ty) <- tcSplitForAllTyVarBinder_maybe fun_ty
      = go bvs res_ty rest_args
      | otherwise
      = False  -- E.g. head ids @Int

    go bvs fun_ty (EValArg {} : rest_args)
      | Just (_, res_ty) <- tcSplitFunTy_maybe fun_ty
      = go bvs res_ty rest_args
      | otherwise
      = False  -- E.g. head id 'x'


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

isTagToEnum :: HsExpr GhcRn -> Bool
isTagToEnum (HsVar _ (L _ fun_id)) = fun_id `hasKey` tagToEnumKey
isTagToEnum _ = False

tcTagToEnum :: HsExpr GhcTc -> AppCtxt -> [HsExprArg 'TcpTc]
            -> TcRhoType
            -> TcM (HsExpr GhcTc)
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum tc_fun fun_ctxt tc_args res_ty
  | [val_arg] <- dropWhile (not . isHsValArg) tc_args
  = do { res_ty <- zonkTcType res_ty

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
             tc_expr = rebuildHsApps tc_fun' fun_ctxt [val_arg]
             df_wrap = mkWpCastR (mkTcSymCo coi)
       ; return (mkHsWrap df_wrap tc_expr) }}}}}

  | otherwise
  = failWithTc TcRnTagToEnumMissingValArg

  where
    vanilla_result = return (rebuildHsApps tc_fun fun_ctxt tc_args)

    check_enumeration ty' tc
      | isEnumerationTyCon tc = return ()
      | otherwise             = addErrTc (TcRnTagToEnumResTyNotAnEnum ty')


{- *********************************************************************
*                                                                      *
             Pragmas on expressions
*                                                                      *
********************************************************************* -}

tcExprPrag :: HsPragE GhcRn -> HsPragE GhcTc
tcExprPrag (HsPragSCC x1 src ann) = HsPragSCC x1 src ann
