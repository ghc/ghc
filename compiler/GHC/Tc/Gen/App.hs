
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
{-# LANGUAGE TypeApplications #-} -- Wrinkle in Note [Trees That Grow]

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

import GHC.Types.Var
import GHC.Builtin.Types ( multiplicityTy )
import GHC.Tc.Gen.Head
import Language.Haskell.Syntax.Basic
import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst_maybe )
import GHC.Tc.Gen.HsType
import GHC.Tc.Utils.Concrete  ( unifyConcrete, idConcreteTvs )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Tc.Zonk.TcType
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon (dataConConcreteTyVars)
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Subst (substTyWithInScope)
import GHC.Core.TyCo.FVs
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Types.Var.Set
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.Names
import GHC.Driver.DynFlags
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
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
  already /are/ unification variables!  See also
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
  = addExprCtxt rn_expr $
    setSrcSpanA loc     $
    do { (fun@(rn_fun,fun_ctxt), rn_args) <- splitHsApps rn_expr
       ; do_ql <- wantQuickLook rn_fun
       ; (tc_fun, fun_sigma) <- tcInferAppHead fun
       ; (_delta, inst_args, app_res_sigma) <- tcInstFun do_ql inst (tc_fun, fun_ctxt) fun_sigma rn_args
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

There is no special treatment for HsUnboundVarTc, HsOverLit etc, because
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

tcApp works like this:

1. Use splitHsApps, which peels off
     HsApp, HsTypeApp, HsPrag, HsPar
   returning the function in the corner and the arguments

   splitHsApps can deal with infix as well as prefix application,
   and returns a Rebuilder to re-assemble the application after
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
  See Note [Left and right sections] in GHC.Rename.Expr.

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
  = do { (fun@(rn_fun, fun_ctxt), rn_args) <- splitHsApps rn_expr
       ; traceTc "tcApp {" $
           vcat [ text "rn_expr:" <+> ppr rn_expr
                , text "rn_fun:" <+> ppr rn_fun
                , text "fun_ctxt:" <+> ppr fun_ctxt
                , text "rn_args:" <+> ppr rn_args ]

       ; (tc_fun, fun_sigma) <- tcInferAppHead fun

       -- Instantiate
       ; do_ql <- wantQuickLook rn_fun
       ; (delta, inst_args, app_res_rho) <- tcInstFun do_ql True (tc_fun, fun_ctxt) fun_sigma rn_args

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
                 = addHeadCtxt fun_ctxt thing_inside
                 | otherwise
                 = addFunResCtxt rn_fun rn_args app_res_rho exp_res_ty $
                   thing_inside

       -- Match up app_res_rho: the result type of rn_expr
       --     with exp_res_ty:  the expected result type
       ; do_ds <- xoptM LangExt.DeepSubsumption
       ; res_wrap <- perhaps_add_res_ty_ctxt $
            if not do_ds
            then -- No deep subsumption
                 -- app_res_rho and exp_res_ty are both rho-types,
                 -- so with simple subsumption we can just unify them
                 -- No need to zonk; the unifier does that
                 do { co <- unifyExpectedType rn_expr app_res_rho exp_res_ty
                    ; return (mkWpCastN co) }

            else -- Deep subsumption
                 -- Even though both app_res_rho and exp_res_ty are rho-types,
                 -- they may have nested polymorphism, so if deep subsumption
                 -- is on we must call tcSubType.
                 -- Zonk app_res_rho first, because QL may have instantiated some
                 -- delta variables to polytypes, and tcSubType doesn't expect that
                 do { app_res_rho <- liftZonkM $ zonkQuickLook do_ql app_res_rho
                    ; tcSubTypeDS rn_expr app_res_rho exp_res_ty }

       -- Typecheck the value arguments
       ; tc_args <- tcValArgs do_ql inst_args

       -- Reconstruct, with a special case for tagToEnum#.
       ; tc_expr <-
          if isTagToEnum rn_fun
          then tcTagToEnum tc_fun fun_ctxt tc_args app_res_rho
          else do rebuildHsApps tc_fun fun_ctxt tc_args app_res_rho

       ; whenDOptM Opt_D_dump_tc_trace $
         do { inst_args <- liftZonkM $ mapM zonkArg inst_args  -- Only when tracing
            ; traceTc "tcApp }" (vcat [ text "rn_fun:"      <+> ppr rn_fun
                                      , text "rn_args:"     <+> ppr rn_args
                                      , text "inst_args"    <+> brackets (pprWithCommas pprHsExprArgTc inst_args)
                                      , text "do_ql:  "     <+> ppr do_ql
                                      , text "fun_sigma:  " <+> ppr fun_sigma
                                      , text "delta:      " <+> ppr delta
                                      , text "app_res_rho:" <+> ppr app_res_rho
                                      , text "exp_res_ty:"  <+> ppr exp_res_ty
                                      , text "rn_expr:"     <+> ppr rn_expr
                                      , text "tc_fun:"      <+> ppr tc_fun
                                      , text "tc_args:"     <+> ppr tc_args
                                      , text "tc_expr:"     <+> ppr tc_expr ]) }

       -- Wrap the result
       ; return (mkHsWrap res_wrap tc_expr) }

--------------------
wantQuickLook :: HsExpr GhcRn -> TcM Bool
wantQuickLook (HsVar _ (L _ f))
  | getUnique f `elem` quickLookKeys = return True
wantQuickLook _                      = xoptM LangExt.ImpredicativeTypes

quickLookKeys :: [Unique]
-- See Note [Quick Look for particular Ids]
quickLookKeys = [dollarIdKey, leftSectionKey, rightSectionKey]

zonkQuickLook :: Bool -> TcType -> ZonkM TcType
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
zonkArg :: HsExprArg 'TcpInst -> ZonkM (HsExprArg 'TcpInst)
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
    tc_arg (EPrag l p) = return (EPrag l (tcExprPrag p))
    tc_arg (EWrap w)   = return (EWrap w)
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
             arg_ty <- liftZonkM $ zonkQuickLook do_ql arg_ty

             -- Now check the argument
           ; arg' <- tcScalingUsage mult $
                     do { traceTc "tcEValArg" $
                          vcat [ ppr ctxt
                               , text "arg type:" <+> ppr arg_ty
                               , text "arg:" <+> ppr arg ]
                        ; tcEValArg ctxt arg arg_ty }

           ; return (eva { eva_arg    = ValArg arg'
                         , eva_arg_ty = Scaled mult arg_ty }) }

tcEValArg :: AppCtxt -> EValArg 'TcpInst -> TcSigmaTypeFRR -> TcM (LHsExpr GhcTc)
-- Typecheck one value argument of a function call
tcEValArg ctxt (ValArg larg@(L arg_loc arg)) exp_arg_sigma
  = addArgCtxt ctxt larg $
    do { arg' <- tcPolyExpr arg (mkCheckExpType exp_arg_sigma)
       ; return (L arg_loc arg') }

tcEValArg ctxt (ValArgQL { va_expr = larg@(L arg_loc _)
                         , va_fun = (inner_fun, fun_ctxt)
                         , va_args = inner_args
                         , va_ty = app_res_rho }) exp_arg_sigma
  = addArgCtxt ctxt larg $
    do { traceTc "tcEValArgQL {" (vcat [ ppr inner_fun <+> ppr inner_args ])
       ; tc_args <- tcValArgs True inner_args

       ; co   <- unifyType Nothing app_res_rho exp_arg_sigma
       ; arg' <- mkHsWrapCo co <$> rebuildHsApps inner_fun fun_ctxt tc_args app_res_rho
       ; traceTc "tcEValArgQL }" $
           vcat [ text "inner_fun:" <+> ppr inner_fun
                , text "app_res_rho:" <+> ppr app_res_rho
                , text "exp_arg_sigma:" <+> ppr exp_arg_sigma ]
       ; return (L arg_loc arg') }

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
                    --           so may return a sigma-type
                    -- True  <=> Instantiate all type variables at the end:
                    --           return a rho-type
                    -- The /only/ call site that passes in False is the one
                    --    in tcInferSigma, which is used only to implement :type
                    -- Otherwise we do eager instantiation; in Fig 5 of the paper
                    --    |-inst returns a rho-type
          -> (HsExpr GhcTc, AppCtxt)
                -- ^ For error messages and to retrieve concreteness information
                -- of the function
          -> TcSigmaType -> [HsExprArg 'TcpRn]
          -> TcM ( Delta
                 , [HsExprArg 'TcpInst]
                 , TcSigmaType )
-- This function implements the |-inst judgement in Fig 4, plus the
-- modification in Fig 5, of the QL paper:
-- "A quick look at impredicativity" (ICFP'20).
tcInstFun do_ql inst_final (tc_fun, fun_ctxt) fun_sigma rn_args
  = do { traceTc "tcInstFun" (vcat [ text "tc_fun" <+> ppr tc_fun
                                   , text "fun_sigma" <+> ppr fun_sigma
                                   , text "fun_ctxt" <+> ppr fun_ctxt
                                   , text "args:" <+> ppr rn_args
                                   , text "do_ql" <+> ppr do_ql ])
       ; go emptyVarSet [] [] fun_sigma rn_args }
  where
    fun_orig
      | VAExpansion (OrigStmt{}) _ _ <- fun_ctxt
      = DoOrigin
      | VAExpansion (OrigPat pat) _ _ <- fun_ctxt
      = DoPatOrigin pat
      | VAExpansion (OrigExpr e) _ _ <- fun_ctxt
      = exprCtOrigin e
      | VACall e _ _ <- fun_ctxt
      = exprCtOrigin e

    -- These are the type variables which must be instantiated to concrete
    -- types. See Note [Representation-polymorphic Ids with no binding]
    -- in GHC.Tc.Gen.Head.
    fun_conc_tvs
      | HsVar _ (L _ fun_id) <- tc_fun
      = idConcreteTvs fun_id
      -- Recall that DataCons are represented using ConLikeTc at GhcTc stage,
      -- see Note [Typechecking data constructors] in GHC.Tc.Gen.Head.
      | XExpr (ConLikeTc (RealDataCon dc) _ _) <- tc_fun
      = dataConConcreteTyVars dc
      | otherwise
      = noConcreteTyVars

    -- Count value args only when complaining about a function
    -- applied to too many value args
    -- See Note [Herald for matchExpectedFunTys] in GHC.Tc.Utils.Unify.
    n_val_args = count isHsValArg rn_args

    fun_is_out_of_scope  -- See Note [VTA for out-of-scope functions]
      = case tc_fun of
          XExpr (HsUnboundVarTc {}) -> True
          _               -> False

    inst_fun :: [HsExprArg 'TcpRn] -> ForAllTyFlag -> Bool
    -- True <=> instantiate a tyvar with this ForAllTyFlag
    inst_fun [] | inst_final  = isInvisibleForAllTyFlag
                | otherwise   = const False
                -- Using `const False` for `:type` avoids
                -- `forall {r1} (a :: TYPE r1) {r2} (b :: TYPE r2). a -> b`
                -- turning into `forall a {r2} (b :: TYPE r2). a -> b`.
                -- See #21088.
    inst_fun (EValArg {} : _) = isInvisibleForAllTyFlag
    inst_fun _                = isInferredForAllTyFlag

    -----------
    go, go1 :: Delta
            -> [HsExprArg 'TcpInst]     -- Accumulator, reversed
            -> [Scaled TcSigmaTypeFRR]  -- Value args to which applied so far
            -> TcSigmaType -> [HsExprArg 'TcpRn]
            -> TcM (Delta, [HsExprArg 'TcpInst], TcSigmaType)

    -- go: If fun_ty=kappa, look it up in Theta
    go delta acc so_far fun_ty args
      | Just kappa <- getTyVar_maybe fun_ty
      , kappa `elemVarSet` delta
      = do { cts <- readMetaTyVar kappa
           ; case cts of
                Indirect fun_ty' -> go  delta acc so_far fun_ty' args
                Flexi            -> go1 delta acc so_far fun_ty  args }
     | otherwise
     = go1 delta acc so_far fun_ty args

    -- go1: fun_ty is not filled-in instantiation variable
    --      ('go' dealt with that case)

    -- Handle out-of-scope functions gracefully
    go1 delta acc so_far fun_ty (arg : rest_args)
      | fun_is_out_of_scope, looks_like_type_arg arg   -- See Note [VTA for out-of-scope functions]
      = go delta acc so_far fun_ty rest_args

    -- Rule IALL from Fig 4 of the QL paper
    -- Instantiate invisible foralls and dictionaries.
    -- c.f. GHC.Tc.Utils.Instantiate.topInstantiate
    go1 delta acc so_far fun_ty args
      | (tvs,   body1) <- tcSplitSomeForAllTyVars (inst_fun args) fun_ty
      , (theta, body2) <- if inst_fun args Inferred
                          then tcSplitPhiTy body1
                          else ([], body1)
        -- inst_fun args Inferred: dictionary parameters are like Inferred foralls
        -- E.g. #22908: f :: Foo => blah
        -- No foralls!  But if inst_final=False, don't instantiate
      , not (null tvs && null theta)
      = do { (inst_tvs, wrap, fun_rho) <-
                -- addHeadCtxt: important for the class constraints
                -- that may be emitted from instantiating fun_sigma
                addHeadCtxt fun_ctxt $
                instantiateSigma fun_orig fun_conc_tvs tvs theta body2
                  -- See Note [Representation-polymorphism checking built-ins]
                  -- in GHC.Tc.Gen.Head.
                  -- NB: we are doing this even when "acc" is not empty,
                  -- to handle e.g.
                  --
                  --   badTup :: forall r (a :: TYPE r). a -> (# Int, a #)
                  --   badTup = (# , #) @LiftedRep
                  --
                  -- in which we already have instantiated the first RuntimeRep
                  -- argument of (#,#) to @LiftedRep, but want to rule out the
                  -- second instantiation @r.

           ; go (delta `extendVarSetList` inst_tvs)
                (addArgWrap wrap acc) so_far fun_rho args }
                -- Going around again means we deal easily with
                -- nested  forall a. Eq a => forall b. Show b => blah

    -- Rule ITVDQ from the GHC Proposal #281
    go1 delta acc so_far fun_ty ((EValArg { eva_arg = ValArg arg }) : rest_args)
      | Just (tvb, body) <- tcSplitForAllTyVarBinder_maybe fun_ty
      = assertPpr (binderFlag tvb == Required) (ppr fun_ty $$ ppr arg) $
        -- Any invisible binders have been instantiated by IALL above,
        -- so this forall must be visible (i.e. Required)
        do { (ty_arg, inst_body) <- tcVDQ fun_conc_tvs (tvb, body) arg
           ; let wrap = mkWpTyApps [ty_arg]
           ; go delta (addArgWrap wrap acc) so_far inst_body rest_args }

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
      = do { (ty_arg, inst_ty) <- tcVTA fun_conc_tvs fun_ty hs_ty
           ; let arg' = ETypeArg { eva_ctxt = ctxt, eva_hs_ty = hs_ty, eva_ty = ty_arg }
           ; go delta (arg' : acc) so_far inst_ty rest_args }

    -- Rule IVAR from Fig 4 of the QL paper:
    go1 delta acc so_far fun_ty args@(EValArg {} : _)
      | Just kappa <- getTyVar_maybe fun_ty
      , kappa `elemVarSet` delta
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
        --   - We need the freshly allocated unification variables, to extend
        --     delta with.
        -- It's easier just to do the job directly here.
        do { let val_args       = leadingValArgs args
                 val_args_count = length val_args

            -- Create metavariables for the arguments. Following matchActualFunTy,
            -- we create nu_i :: TYPE kappa_i[conc], ensuring that the arguments
            -- have concrete runtime representations.
            -- When we come to unify the nus (in qlUnify), we will call
            -- unifyKind on the kinds. This will do the right thing, even though
            -- we are manually filling in the nu metavariables.
                 new_arg_tv (ValArg (L _ arg)) i =
                   newOpenFlexiFRRTyVar $
                   FRRExpectedFunTy (ExpectedFunTyArg (HsExprTcThing tc_fun) arg) i
           ; arg_nus <- zipWithM new_arg_tv
                          val_args
                          [length so_far + 1 ..]
             -- We need variables for multiplicity (#18731)
             -- Otherwise, 'undefined x' wouldn't be linear in x
           ; mults   <- replicateM val_args_count (newFlexiTyVarTy multiplicityTy)
           ; res_nu  <- newOpenFlexiTyVar
           ; kind_co <- unifyKind Nothing liftedTypeKind (tyVarKind kappa)
           ; let delta'  = delta `extendVarSetList` (res_nu:arg_nus)
                 arg_tys = mkTyVarTys arg_nus
                 res_ty  = mkTyVarTy res_nu
                 fun_ty' = mkScaledFunTys (zipWithEqual "tcInstFun" mkScaled mults arg_tys) res_ty
                 co_wrap = mkWpCastN (mkGReflLeftCo Nominal fun_ty' kind_co)
                 acc'    = addArgWrap co_wrap acc
                 -- Suppose kappa :: kk
                 -- Then fun_ty :: kk, fun_ty' :: Type, kind_co :: Type ~ kk
                 --      co_wrap :: (fun_ty' |> kind_co) ~ fun_ty'
           ; liftZonkM $ writeMetaTyVar kappa (mkCastTy fun_ty' kind_co)
                 -- kappa is uninstantiated ('go' already checked that)
           ; go delta' acc' so_far fun_ty' args }

    -- Rule IARG from Fig 4 of the QL paper:
    go1 delta acc so_far fun_ty
        (eva@(EValArg { eva_arg = ValArg arg, eva_ctxt = ctxt }) : rest_args)
      = do { let herald = case fun_ctxt of
                             VAExpansion (OrigStmt{}) _ _ -> ExpectedFunTySyntaxOp DoOrigin tc_fun
                             _ ->  ExpectedFunTyArg (HsExprTcThing tc_fun) (unLoc arg)
           ; (wrap, arg_ty, res_ty) <-
                -- NB: matchActualFunTy does the rep-poly check.
                -- For example, suppose we have f :: forall r (a::TYPE r). a -> Int
                -- In an application (f x), we need 'x' to have a fixed runtime
                -- representation; matchActualFunTy checks that when
                -- taking apart the arrow type (a -> Int).
                matchActualFunTy herald
                  (Just $ HsExprTcThing tc_fun)
                  (n_val_args, fun_sigma) fun_ty

           ; (delta', arg') <- if do_ql
                               then addArgCtxt ctxt arg $
                                    -- Context needed for constraints
                                    -- generated by calls in arg
                                    quickLookArg delta arg arg_ty
                               else return (delta, ValArg arg)
           ; let acc' = eva { eva_arg = arg', eva_arg_ty = arg_ty }
                       : addArgWrap wrap acc
           ; go delta' acc' (arg_ty:so_far) res_ty rest_args }

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
looks_like_type_arg EValArg{ eva_arg = ValArg (L _ e) } =
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

addArgCtxt :: AppCtxt -> LHsExpr GhcRn
           -> TcM a -> TcM a
-- There are four cases:
-- 1. In the normal case, we add an informative context
--          "In the third argument of f, namely blah"
-- 2. If we are deep inside generated code (`isGeneratedCode` is `True`)
--    or if all or part of this particular application is an expansion
--    `VAExpansion`, just use the less-informative context
--          "In the expression: arg"
--   Unless the arg is also a generated thing, in which case do nothing.
--   See Note [Rebindable syntax and XXExprGhcRn] in GHC.Hs.Expr
-- 3. We are in an expanded `do`-block's non-bind statement
--    we simply add the statement context
--       "In the statement of the `do`-block .."
-- 4. We are in an expanded do block's bind statement
--    a. Then either we are typechecking the first argument of the bind which is user located
--       so we set the location to be that of the argument
--    b. Or, we are typechecking the second argument which would be a generated lambda
--       so we set the location to be whatever the location in the context is
--  See Note [Expanding HsDo with XXExprGhcRn] in GHC.Tc.Gen.Do
-- For future: we need a cleaner way of doing this bit of adding the right error context.
-- There is a delicate dance of looking at source locations and reconstructing
-- whether the piece of code is a `do`-expanded code or some other expanded code.
addArgCtxt ctxt (L arg_loc arg) thing_inside
  = do { in_generated_code <- inGeneratedCode
       ; case ctxt of
           VACall fun arg_no _ | not in_generated_code
             -> do setSrcSpanA arg_loc                    $
                     addErrCtxt (funAppCtxt fun arg arg_no) $
                     thing_inside

           VAExpansion (OrigStmt (L _ stmt@(BindStmt {}))) _ loc
             | isGeneratedSrcSpan (locA arg_loc) -- This arg is the second argument to generated (>>=)
             -> setSrcSpan loc $
                  addStmtCtxt stmt $
                  thing_inside
             | otherwise                        -- This arg is the first argument to generated (>>=)
             -> setSrcSpanA arg_loc $
                  addStmtCtxt stmt $
                  thing_inside
           VAExpansion (OrigStmt (L loc stmt)) _ _
             -> setSrcSpanA loc $
                  addStmtCtxt stmt $
                  thing_inside

           _ -> setSrcSpanA arg_loc $
                  addExprCtxt arg     $  -- Auto-suppressed if arg_loc is generated
                  thing_inside }

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
         -- in GHC.Tc.Gen.Head.
      -> TcType            -- ^ Function type
      -> LHsWcType GhcRn   -- ^ Argument type
      -> TcM (TcType, TcType)
-- Deal with a visible type application
-- The function type has already had its Inferred binders instantiated
tcVTA conc_tvs fun_ty hs_ty
  | Just (tvb, inner_ty) <- tcSplitForAllTyVarBinder_maybe fun_ty
  , binderFlag tvb == Specified
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
  where
    go :: LHsExpr GhcRn -> TcM (LHsType GhcRn)
    go (L _ (HsEmbTy _ t)) =
      -- HsEmbTy means there is an explicit `type` herald, e.g. vfun :: forall a -> blah
      -- and the call   vfun (type Int)
      --           or   vfun (Int -> type Int)
      -- The T2T transformation can simply discard the herald and use the embedded type.
      unwrap_wc t
    go (L l (HsVar _ lname)) =
      -- as per #281: variables and constructors (regardless of their namespace)
      -- are mapped directly, without modification.
      return (L l (HsTyVar noAnn NotPromoted lname))
    go (L l (HsApp _ lhs rhs)) =
      do { lhs' <- go lhs
         ; rhs' <- go rhs
         ; return (L l (HsAppTy noExtField lhs' rhs')) }
    go (L l (HsAppType _ lhs rhs)) =
      do { lhs' <- go lhs
         ; rhs' <- unwrap_wc rhs
         ; return (L l (HsAppKindTy noExtField lhs' rhs')) }
    go (L l e@(OpApp _ lhs op rhs)) =
      do { lhs' <- go lhs
         ; op'  <- go op
         ; rhs' <- go rhs
         ; op_id <- unwrap_op_tv op'
         ; return (L l (HsOpTy noAnn NotPromoted lhs' op_id rhs')) }
      where
        unwrap_op_tv (L _ (HsTyVar _ _ op_id)) = return op_id
        unwrap_op_tv _ = failWith $ TcRnIllformedTypeArgument (L l e)
    go (L l (HsOverLit _ lit))
      | Just tylit <- tyLitFromOverloadedLit (ol_val lit)
      = return (L l (HsTyLit noExtField tylit))
    go (L l (HsLit _ lit))
      | Just tylit <- tyLitFromLit lit
      = return (L l (HsTyLit noExtField tylit))
    go (L l (ExplicitTuple _ tup_args boxity))
      -- Neither unboxed tuples (#e1,e2#) nor tuple sections (e1,,e2,) can be promoted
      | isBoxed boxity
      , Just es <- tupArgsPresent_maybe tup_args
      = do { ts <- traverse go es
           ; return (L l (HsExplicitTupleTy noExtField ts)) }
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
    go (L _ (XExpr (HsUnboundVarRn rdr)))
      | startsWithUnderscore occ =
          -- See Note [Wildcards in the T2T translation]
          do { wildcards_enabled <- xoptM LangExt.NamedWildCards
             ; if wildcards_enabled
               then illegal_wc rdr
               else not_in_scope }
      | otherwise = not_in_scope
      where occ = occName rdr
            not_in_scope = failWith $ mkTcRnNotInScope rdr NotInScope
    go (L l (XExpr (ExpandedThingRn (OrigExpr orig) _))) =
      -- Use the original, user-written expression (before expansion).
      -- Example. Say we have   vfun :: forall a -> blah
      --          and the call  vfun (Maybe [1,2,3])
      --          expanded to   vfun (Maybe (fromListN 3 [1,2,3]))
      -- (This happens when OverloadedLists is enabled).
      -- The expanded expression can't be promoted, as there is no type-level
      -- equivalent of fromListN, so we must use the original.
      go (L l orig)
    go e = failWith $ TcRnIllformedTypeArgument e

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
-- TODO(aidylns): Fix this note re HsUnboundVar
* It needs to check for LangExt.NamedWildCards to generate an appropriate
  error message for HsUnboundVar.
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
       -- in GHC.Tc.Gen.Head.
       ; th_stage <- getStage
       ; ty_arg <- case mb_conc of
           Nothing   -> return ty_arg0
           Just conc
             -- See [Wrinkle: Typed Template Haskell]
             -- in Note [hasFixedRuntimeRep] in GHC.Tc.Utils.Concrete.
             | Brack _ (TcPending {}) <- th_stage
             -> return ty_arg0
             | otherwise
             ->
             -- Example: user wrote e.g. (#,#) @(F Bool) for a type family F.
             -- Emit [W] F Bool ~ kappa[conc] and pretend the user wrote (#,#) @kappa.
             do { mco <- unifyConcrete (occNameFS $ getOccName $ tv_nm) conc ty_arg0
                ; return $ case mco of { MRefl -> ty_arg0; MCo co -> coercionRKind co } }

       ; let fun_ty    = mkForAllTy tvb inner_ty
             in_scope  = mkInScopeSet (tyCoVarsOfTypes [fun_ty, ty_arg])
             insted_ty = substTyWithInScope in_scope [tv] [ty_arg] inner_ty
               -- This substitution is well-kinded even when inner_ty
               -- is not fully zonked, because ty_arg is fully zonked.
               -- See Note [Type application substitution].

       ; traceTc "tc_inst_forall_arg (VTA/VDQ)" (
                  vcat [ text "fun_ty" <+> ppr fun_ty
                       , text "tv" <+> ppr tv <+> dcolon <+> debugPprType kind
                       , text "ty_arg" <+> debugPprType ty_arg <+> dcolon
                                       <+> debugPprType (typeKind ty_arg)
                       , text "inner_ty" <+> debugPprType inner_ty
                       , text "insted_ty" <+> debugPprType insted_ty ])
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
     data Pat = ConPat   ConLike [HsTyPat] [Pat]  -- (Con @tp1 @tp2 p1 p2)  (constructor pattern)
              | EmbTyPat HsTyPat                  -- (type tp)              (embed a type into a pattern with `type`)
              | ...
     data HsTyPat = HsTP LHsType
  (In ConPat, the type and term arguments are actually inside HsConPatDetails.)

  * Similar to HsAppType in HsExpr, the [HsTyPat] in ConPat is used just for @ty arguments
  * Similar to HsEmbTy   in HsExpr, EmbTyPat lets you embed a type in a pattern

* Examples:
      \ (MkT @a  (x :: a)) -> rhs    -- ConPat (c.o. Pat) and HsConPatTyArg (c.o. HsConPatTyArg)
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

Then the renamer will make (HsUnboundVarRn "wurble") for 'wurble',
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
can check if it is a HsUnboundVarRn.  We use this info to simply skip -- TODO(aidylns): HsUnboundVarRn or HsUnboundVarTc?
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
             -> LHsExpr GhcRn          -- ^ Argument
             -> Scaled TcSigmaTypeFRR  -- ^ Type expected by the function
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
              | Just kappa <- getTyVar_maybe arg_ty
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

quickLookArg1 :: Bool -> Delta -> LHsExpr GhcRn -> TcSigmaTypeFRR
              -> TcM (Delta, EValArg 'TcpInst)
quickLookArg1 guarded delta larg@(L _ arg) arg_ty
  = do { ((rn_fun, fun_ctxt), rn_args) <- splitHsApps arg
       ; mb_fun_ty <- tcInferAppHead_maybe rn_fun
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
       ; (delta_app, inst_args, app_res_rho) <- tcInstFun do_ql True (tc_fun, fun_ctxt) fun_sigma rn_args
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
  = liftZonkM $ zonkTcType app_res_rho
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
      | Just rho1 <- coreView rho1 = go bvs rho1 rho2
      | Just rho2 <- coreView rho2 = go bvs rho1 rho2

    go bvs (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , not (isTypeFamilyTyCon tc1)
      , tys1 `equalLength` tys2
      = zipWithM_ (go bvs) tys1 tys2

    -- Decompose (arg1 -> res1) ~ (arg2 -> res2)
    -- and         (c1 => res1) ~   (c2 => res2)
    -- But for the latter we only learn instantiation info from res1~res2
    -- We look at the multiplicity too, although the chances of getting
    -- impredicative instantiation info from there seems...remote.
    go bvs (FunTy { ft_af = af1, ft_arg = arg1, ft_res = res1, ft_mult = mult1 })
           (FunTy { ft_af = af2, ft_arg = arg2, ft_res = res2, ft_mult = mult2 })
      | af1 == af2 -- Match the arrow TyCon
      = do { when (isVisibleFunArg af1) (go bvs arg1 arg2)
           ; when (isFUNArg af1)        (go bvs mult1 mult2)
           ; go bvs res1 res2 }

    -- ToDo: c.f. Tc.Utils.unify.uType,
    -- which does not split FunTy here
    -- Also NB tcSplitAppTyNoView here, which does not split (c => t)
    go bvs (AppTy t1a t1b) ty2
      | Just (t2a, t2b) <- tcSplitAppTyNoView_maybe ty2
      = do { go bvs t1a t2a; go bvs t1b t2b }

    go bvs ty1 (AppTy t2a t2b)
      | Just (t1a, t1b) <- tcSplitAppTyNoView_maybe ty1
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
               Flexi        -> do { ty2 <- liftZonkM $ zonkTcType ty2
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
           ; liftZonkM $ writeMetaTyVar kappa (mkCastTy ty2 co) }

      | otherwise
      = return ()   -- Occurs-check or forall-bound variable


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
  the main constraint solver for heterogeneous equalities; see Note
  [Equalities with incompatible kinds] in GHC.Tc.Solver.Equality

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
       ; tc_expr <- rebuildHsApps tc_fun' fun_ctxt [val_arg] res_ty
       ; return (mkHsWrap df_wrap tc_expr) }}}}}

  | otherwise
  = failWithTc TcRnTagToEnumMissingValArg

  where
    vanilla_result = rebuildHsApps tc_fun fun_ctxt tc_args res_ty

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
tcExprPrag (HsPragSCC x1 ann) = HsPragSCC x1 ann
