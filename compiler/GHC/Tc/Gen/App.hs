
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

import GHC.Hs

import GHC.Tc.Gen.Head
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
import GHC.Tc.Utils.Concrete( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Zonk.TcType

import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon ( dataConConcreteTyVars, isNewDataCon, dataConTyCon )
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Subst ( substTyWithInScope )
import GHC.Core.Type
import GHC.Core.Coercion

import GHC.Builtin.Types ( multiplicityTy )
import GHC.Builtin.PrimOps( tagToEnumKey )
import GHC.Builtin.Names

import GHC.Types.Var
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
import Language.Haskell.Syntax.Basic( isBoxed )

import Control.Monad
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

(QL2) When we learn what an instantiation variable must be, we simply unify
  it with that type; this is done in qlUnify, which is the function mgu_ql(t1,t2)
  of the paper.  This may fill in a (mutable) instantiation variable with
  a polytype.

(QL3) When QL is done, we turn the instantiation variables into ordinary unification
  variables, using qlZonkTcType.  This function fully zonks the type (thereby
  revealing all the polytypes), and updates any instantiation variables with
  ordinary unification variables.
  See Note [Instantiation variables are short lived].

(QL4) We cleverly avoid the quadratic cost of QL, alluded to in the paper.
  See Note [Quick Look at value arguments]

Note [Instantiation variables are short lived]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* An instantation variable is a mutable meta-type-variable, whose level number
  is QLInstVar.

* Ordinary unification variables always stand for monotypes; only instantiation
  variables can be unified with a polytype (by `qlUnify`).

* When we start typechecking the argments of the call, in tcValArgs, we will
  (a) monomorphise any un-filled-in instantiation variables
      (see Note [Monomorphise instantiation variables])
  (b) zonk the argument type to reveal any polytypes before typechecking that
      argument (see calls to `zonkTcType` and "Crucial step" in tcValArg)..
  See Section 4.3 "Applications and instantiation" of the paper.

* The constraint solver never sees an instantiation variable [not quite true;
  see below]

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
       ; (inst_args, app_res_sigma) <- tcInstFun do_ql inst (tc_fun, fun_ctxt) fun_sigma rn_args
       ; _ <- tcValArgs do_ql inst_args
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

   This isn't perfect. Consider this (which uses visible type application):
    (let { f :: forall a. a -> a; f x = x } in f) @Int
   Since 'let' is not among the special cases for tcInferAppHead,
   we'll delegate back to tcExpr, which will instantiate f's type
   and the type application to @Int will fail.  Too bad!

3. Use tcInstFun to instantiate the function, Quick-Looking as we go.  This
   implements the |-inst judgement in Fig 4, plus the modification in Fig 5, of
   the QL paper: "A quick look at impredicativity" (ICFP'20).

   In tcInstFun we take a quick look at value arguments, using quickLookArg.
   See Note [Quick Look at value arguments].

   (TCAPP1) Crucially, just before `tcApp` calls `tcInstFun`, it sets the
       ambient TcLevel to QLInstVar, so all unification variables allocated by
       tcInstFun, and in the quick-looks it does at the arguments, will be
       instantiation variables.

   Consider (f (g (h x))).`tcApp` instantiates the call to `f`, and in doing
   so quick-looks at the argument(s), in this case (g (h x)).  But
   `quickLookArg` on (g (h x)) in turn instantiates `g` and quick-looks at
   /its/ argument(s), in this case (h x).  And so on recursively.  Key
   point: all these instantiations make instantiation variables.

Now we split into two cases:

4. Case NoQL: no Quick Look

   4.1 Use checkResultTy to connect the the result type.
       Do this /before/ checking the arguments; see
       Note [Unify with expected type before typechecking arguments]

   4.2 Check the arguments with `tcValArgs`.

   4.3 Use `finishApp` to wrap up.

5. Case DoQL: use Quick Look

   5.1 Use `quickLookResultType` to take a quick look at the result type,
       when in checking mode.  This is the shaded part of APP-Downarrow
       in Fig 5.  It also implements the key part of
       Note [Unify with expected type before typechecking arguments]

   5.2 Check the arguments with `tcValArgs`. Importantly, this will monomorphise
       all the instantiation variables of the call.
       See Note [Monomorphise instantiation variables].

   5.3 Use `zonkTcType` to expose the polymophism hidden under instantiation
       variables in `app_res_rho`, and the monomorphic versions of any
       un-unified instantiation variables.

   5.4 Use `checkResTy` to do the subsumption check as usual

   5.4 Use `finishApp` to wrap up

The funcion `finishApp` mainly calls `rebuildHsApps` to rebuild the
application; but it also does a couple of gruesome final checks:
  * Horrible newtype check
  * Special case for tagToEnum

(TCAPP2) There is a lurking difficulty in the above plan:
  * Before calling tcInstFun, we set the ambient level in the monad
    to QLInstVar (Step 2 above).
  * Then, when kind-checking the visible type args of the application,
    we may perhaps build an implication constraint.
  * That means we'll try to add 1 to the ambient level; which is a no-op.
  * So skolem escape checks won't work right.
  This is pretty exotic, so I'm just deferring it for now, leaving
  this note to alert you to the possiblity.

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

The latter is much better. That is why we call checkResultType before tcValArgs.
-}

tcApp :: HsExpr GhcRn
      -> ExpRhoType   -- When checking, -XDeepSubsumption <=> deeply skolemised
      -> TcM (HsExpr GhcTc)
-- See Note [tcApp: typechecking applications]
tcApp rn_expr exp_res_ty
  = do { -- Step 1: Split the application chain
         (fun@(rn_fun, fun_ctxt), rn_args) <- splitHsApps rn_expr
       ; traceTc "tcApp {" $
           vcat [ text "rn_expr:" <+> ppr rn_expr
                , text "rn_fun:" <+> ppr rn_fun
                , text "fun_ctxt:" <+> ppr fun_ctxt
                , text "rn_args:" <+> ppr rn_args ]

       -- Step 2: Infer the type of `fun`, the head of the application
       ; (tc_fun, fun_sigma) <- tcInferAppHead fun
       ; let tc_head = (tc_fun, fun_ctxt)

       -- Step 3: Instantiate the function type (taking a quick look at args)
       ; do_ql <- wantQuickLook rn_fun
       ; (inst_args, app_res_rho)
              <- setQLInstLevel do_ql $  -- See (TCAPP1) and (TCAPP2) in
                                         -- Note [tcApp: typechecking applications]
                 tcInstFun do_ql True tc_head fun_sigma rn_args

       ; case do_ql of
            NoQL -> do { traceTc "tcApp:NoQL" (ppr rn_fun $$ ppr app_res_rho)

                         -- Step 4.1: subsumption check against expected result type
                         -- See Note [Unify with expected type before typechecking arguments]
                       ; res_wrap <- checkResultTy rn_expr tc_head inst_args
                                                   app_res_rho exp_res_ty
                         -- Step 4.2: typecheck the  arguments
                       ; tc_args <- tcValArgs NoQL inst_args
                         -- Step 4.3: wrap up
                       ; finishApp tc_head tc_args app_res_rho res_wrap }

            DoQL -> do { traceTc "tcApp:DoQL" (ppr rn_fun $$ ppr app_res_rho)

                         -- Step 5.1: Take a quick look at the result type
                       ; quickLookResultType app_res_rho exp_res_ty
                         -- Step 5.2: typecheck the arguments, and monomorphise
                         --           any un-unified instantiation variables
                       ; tc_args <- tcValArgs DoQL inst_args
                         -- Step 5.3: typecheck the arguments
                       ; app_res_rho <- liftZonkM $ zonkTcType app_res_rho
                         -- Step 5.4: subsumption check against the expected type
                       ; res_wrap <- checkResultTy rn_expr tc_head inst_args
                                                   app_res_rho exp_res_ty
                         -- Step 5.5: wrap up
                       ; finishApp tc_head tc_args app_res_rho res_wrap } }

setQLInstLevel :: QLFlag -> TcM a -> TcM a
setQLInstLevel DoQL thing_inside = setTcLevel QLInstVar thing_inside
setQLInstLevel NoQL thing_inside = thing_inside

quickLookResultType :: TcRhoType -> ExpRhoType -> TcM ()
-- This function implements the shaded bit of rule APP-Downarrow in
-- Fig 5 of the QL paper: "A quick look at impredicativity" (ICFP'20).
quickLookResultType app_res_rho (Check exp_rho) = qlUnify app_res_rho exp_rho
quickLookResultType  _           _              = return ()

finishApp :: (HsExpr GhcTc, AppCtxt) -> [HsExprArg 'TcpTc]
          -> TcRhoType -> HsWrapper
          -> TcM (HsExpr GhcTc)
-- Do final checks and wrap up the result
finishApp tc_head@(tc_fun,_) tc_args app_res_rho res_wrap
  = do { -- Horrible newtype check
       ; rejectRepPolyNewtypes tc_head app_res_rho

       -- Reconstruct, with a horrible special case for tagToEnum#.
       ; res_expr <- if isTagToEnum tc_fun
                     then tcTagToEnum tc_head tc_args app_res_rho
                     else return (rebuildHsApps tc_head tc_args)
       ; return (mkHsWrap res_wrap res_expr) }

checkResultTy :: HsExpr GhcRn
              -> (HsExpr GhcTc, AppCtxt)  -- Head
              -> [HsExprArg p]            -- Arguments, just error messages
              -> TcRhoType  -- Inferred type of the application; zonked to
                            --   expose foralls, but maybe not deeply instantiated
              -> ExpRhoType -- Expected type; this is deeply skolemised
              -> TcM HsWrapper
-- Connect up the inferred type of the application with the expected type
-- This is usually just a unification, but with deep subsumption there is more to do
checkResultTy _ _ _ app_res_rho (Infer inf_res)
  = do { co <- fillInferResult app_res_rho inf_res
       ; return (mkWpCastN co) }

checkResultTy rn_expr (tc_fun, fun_ctxt) inst_args app_res_rho (Check res_ty)
-- Unify with expected type from the context
-- See Note [Unify with expected type before typechecking arguments]
--
-- Match up app_res_rho: the result type of rn_expr
--     with res_ty:  the expected result type
 = perhaps_add_res_ty_ctxt $
   do { ds_flag <- getDeepSubsumptionFlag
      ; traceTc "checkResultTy {" $
          vcat [ text "tc_fun:" <+> ppr tc_fun
               , text "app_res_rho:" <+> ppr app_res_rho
               , text "res_ty:"  <+> ppr res_ty
               , text "ds_flag:" <+> ppr ds_flag ]
      ; case ds_flag of
          Shallow -> -- No deep subsumption
             -- app_res_rho and res_ty are both rho-types,
             -- so with simple subsumption we can just unify them
             -- No need to zonk; the unifier does that
             do { co <- unifyExprType rn_expr app_res_rho res_ty
                ; traceTc "checkResultTy 1 }" (ppr co)
                ; return (mkWpCastN co) }

          Deep ->   -- Deep subsumption
             -- Even though both app_res_rho and res_ty are rho-types,
             -- they may have nested polymorphism, so if deep subsumption
             -- is on we must call tcSubType.
             -- Zonk app_res_rho first, because QL may have instantiated some
             -- delta variables to polytypes, and tcSubType doesn't expect that
             do { wrap <- tcSubTypeDS rn_expr app_res_rho res_ty
                ; traceTc "checkResultTy 2 }" (ppr app_res_rho $$ ppr res_ty)
                ; return wrap } }
  where
    -- perhaps_add_res_ty_ctxt: Inside an expansion, the addFunResCtxt stuff is
    -- more confusing than helpful because the function at the head isn't in
    -- the source program; it was added by the renamer.  See
    -- Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr
    perhaps_add_res_ty_ctxt thing_inside
      | insideExpansion fun_ctxt
      = addHeadCtxt fun_ctxt thing_inside
      | otherwise
      = addFunResCtxt tc_fun inst_args app_res_rho (mkCheckExpType res_ty) $
        thing_inside

----------------
tcValArgs :: QLFlag -> [HsExprArg 'TcpInst] -> TcM [HsExprArg 'TcpTc]
-- Importantly, tcValArgs works left-to-right, so that by the time we
-- encounter an argument, we have monomorphised all the instantiation
-- variables that its type contains.  All that is left to do is an ordinary
-- zonkTcType.  See Note [Monomorphise instantiation variables].
tcValArgs do_ql args = mapM (tcValArg do_ql) args

tcValArg :: QLFlag -> HsExprArg 'TcpInst    -- Actual argument
         -> TcM (HsExprArg 'TcpTc)          -- Resulting argument
tcValArg _     (EPrag l p)         = return (EPrag l (tcExprPrag p))
tcValArg _     (ETypeArg l hty ty) = return (ETypeArg l hty ty)
tcValArg do_ql (EWrap (EHsWrap w)) = do { whenQL do_ql $ qlMonoHsWrapper w
                                        ; return (EWrap (EHsWrap w)) }
  -- qlMonoHsWrapper: see Note [Monomorphise instantiation variables]
tcValArg _     (EWrap ew)          = return (EWrap ew)

tcValArg do_ql (EValArg { ea_ctxt   = ctxt
                        , ea_arg    = larg@(L arg_loc arg)
                        , ea_arg_ty = sc_arg_ty })
  = addArgCtxt ctxt larg $
    do { traceTc "tcValArg" $
         vcat [ ppr ctxt
              , text "arg type:" <+> ppr sc_arg_ty
              , text "arg:" <+> ppr arg ]

         -- Crucial step: expose QL results before checking exp_arg_ty
         -- So far as the paper is concerned, this step applies
         -- the poly-substitution Theta, learned by QL, so that we
         -- "see" the polymorphism in that argument type. E.g.
         --    (:) e ids, where ids :: [forall a. a->a]
         --                     (:) :: forall p. p->[p]->[p]
         -- Then Theta = [p :-> forall a. a->a], and we want
         -- to check 'e' with expected type (forall a. a->a)
         -- See Note [Instantiation variables are short lived]
       ; Scaled mult exp_arg_ty <- case do_ql of
              DoQL -> liftZonkM $ zonkScaledTcType sc_arg_ty
              NoQL -> return sc_arg_ty

         -- Now check the argument
       ; arg' <- tcScalingUsage mult $
                 tcPolyExpr arg (mkCheckExpType exp_arg_ty)

       ; return (EValArg { ea_ctxt = ctxt
                         , ea_arg = L arg_loc arg'
                         , ea_arg_ty = noExtField }) }

tcValArg _ (EValArgQL { eaql_wanted  = wanted
                      , eaql_ctxt    = ctxt
                      , eaql_arg_ty  = sc_arg_ty
                      , eaql_larg    = larg@(L arg_loc rn_expr)
                      , eaql_tc_fun  = tc_head
                      , eaql_args    = inst_args
                      , eaql_encl    = arg_influences_enclosing_call
                      , eaql_res_rho = app_res_rho })
  = addArgCtxt ctxt larg $
    do { -- Expose QL results to tcSkolemise, as in EValArg case
         Scaled mult exp_arg_ty <- liftZonkM $ zonkScaledTcType sc_arg_ty

       ; traceTc "tcEValArgQL {" (vcat [ text "app_res_rho:" <+> ppr app_res_rho
                                       , text "exp_arg_ty:" <+> ppr exp_arg_ty
                                       , text "args:" <+> ppr inst_args ])

       ; ds_flag <- getDeepSubsumptionFlag
       ; (wrap, arg')
            <- tcScalingUsage mult  $
               tcSkolemise ds_flag GenSigCtxt exp_arg_ty $ \ exp_arg_rho ->
               do { -- Emit saved-up constraints, /under/ the tcSkolemise
                    -- See (QLA4) in Note [Quick Look at value arguments]
                    emitConstraints wanted

                    -- Unify with context if we have not already done so
                    -- See (QLA4) in Note [Quick Look at value arguments]
                  ; unless arg_influences_enclosing_call $  -- Don't repeat
                    qlUnify app_res_rho exp_arg_rho         -- the qlUnify

                  ; tc_args <- tcValArgs DoQL inst_args
                  ; app_res_rho <- liftZonkM $ zonkTcType app_res_rho
                  ; res_wrap <- checkResultTy rn_expr tc_head inst_args
                                              app_res_rho (mkCheckExpType exp_arg_rho)
                  ; finishApp tc_head tc_args app_res_rho res_wrap }

       ; traceTc "tcEValArgQL }" $
           vcat [ text "app_res_rho:" <+> ppr app_res_rho ]

       ; return (EValArg { ea_ctxt   = ctxt
                         , ea_arg    = L arg_loc (mkHsWrap wrap arg')
                         , ea_arg_ty = noExtField }) }


--------------------
wantQuickLook :: HsExpr GhcRn -> TcM QLFlag
wantQuickLook (HsVar _ (L _ f))
  | getUnique f `elem` quickLookKeys = return DoQL
wantQuickLook _                      = do { impred <- xoptM LangExt.ImpredicativeTypes
                                          ; if impred then return DoQL else return NoQL }

quickLookKeys :: [Unique]
-- See Note [Quick Look for particular Ids]
quickLookKeys = [dollarIdKey, leftSectionKey, rightSectionKey]

{- *********************************************************************
*                                                                      *
              Instantiating the call
*                                                                      *
********************************************************************* -}

tcInstFun :: QLFlag
          -> Bool   -- False <=> Instantiate only /inferred/ variables at the end
                    --           so may return a sigma-type
                    -- True  <=> Instantiate all type variables at the end:
                    --           return a rho-type
                    -- The /only/ call site that passes in False is the one
                    --    in tcInferSigma, which is used only to implement :type
                    -- Otherwise we do eager instantiation; in Fig 5 of the paper
                    --    |-inst returns a rho-type
          -> (HsExpr GhcTc, AppCtxt)
          -> TcSigmaType -> [HsExprArg 'TcpRn]
          -> TcM ( [HsExprArg 'TcpInst]
                 , TcSigmaType )
-- This crucial function implements the |-inst judgement in Fig 4, plus the
-- modification in Fig 5, of the QL paper:
-- "A quick look at impredicativity" (ICFP'20).
tcInstFun do_ql inst_final (tc_fun, fun_ctxt) fun_sigma rn_args
  = do { traceTc "tcInstFun" (vcat [ text "tc_fun" <+> ppr tc_fun
                                   , text "fun_sigma" <+> ppr fun_sigma
                                   , text "fun_ctxt" <+> ppr fun_ctxt
                                   , text "args:" <+> ppr rn_args
                                   , text "do_ql" <+> ppr do_ql ])
       ; go 1 [] fun_sigma rn_args }
  where
    fun_orig = case fun_ctxt of
      VAExpansion (OrigStmt{}) _ _  -> DoOrigin
      VAExpansion (OrigPat pat) _ _ -> DoPatOrigin pat
      VAExpansion (OrigExpr e) _ _  -> exprCtOrigin e
      VACall e _ _                  -> exprCtOrigin e

    -- These are the type variables which must be instantiated to concrete
    -- types. See Note [Representation-polymorphic Ids with no binding]
    -- in GHC.Tc.Utils.Concrete
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
          HsUnboundVar {} -> True
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
    go, go1 :: Int                      -- Value-argument position of next arg
            -> [HsExprArg 'TcpInst]     -- Accumulator, reversed
            -> TcSigmaType -> [HsExprArg 'TcpRn]
            -> TcM ([HsExprArg 'TcpInst], TcSigmaType)

    -- go: If fun_ty=kappa, look it up in Theta
    go pos acc fun_ty args
      | Just kappa <- getTyVar_maybe fun_ty
      , isQLInstTyVar kappa
      = do { cts <- readMetaTyVar kappa
           ; case cts of
                Indirect fun_ty' -> go  pos acc fun_ty' args
                Flexi            -> go1 pos acc fun_ty  args }
     | otherwise
     = go1 pos acc fun_ty args

    -- go1: fun_ty is not filled-in instantiation variable
    --      ('go' dealt with that case)

    -- Handle out-of-scope functions gracefully
    go1 pos acc fun_ty (arg : rest_args)
      | fun_is_out_of_scope, looks_like_type_arg arg   -- See Note [VTA for out-of-scope functions]
      = go pos acc fun_ty rest_args

    -- Rule IALL from Fig 4 of the QL paper; applies even if args = []
    -- Instantiate invisible foralls and dictionaries.
    -- c.f. GHC.Tc.Utils.Instantiate.topInstantiate
    go1 pos acc fun_ty args
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
      = do { (_inst_tvs, wrap, fun_rho) <-
                -- addHeadCtxt: important for the class constraints
                -- that may be emitted from instantiating fun_sigma
                addHeadCtxt fun_ctxt $
                instantiateSigma fun_orig fun_conc_tvs tvs theta body2
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

           ; go pos (addArgWrap wrap acc) fun_rho args }
                -- Going around again means we deal easily with
                -- nested  forall a. Eq a => forall b. Show b => blah

    -- Rule IRESULT from Fig 4 of the QL paper; no more arguments
    go1 _pos acc fun_ty []
       = do { traceTc "tcInstFun:ret" (ppr fun_ty)
            ; return (reverse acc, fun_ty) }

    -- Rule ITVDQ from the GHC Proposal #281
    go1 pos acc fun_ty ((EValArg { ea_arg = arg }) : rest_args)
      | Just (tvb, body) <- tcSplitForAllTyVarBinder_maybe fun_ty
      = assertPpr (binderFlag tvb == Required) (ppr fun_ty $$ ppr arg) $
        -- Any invisible binders have been instantiated by IALL above,
        -- so this forall must be visible (i.e. Required)
        do { (ty_arg, inst_body) <- tcVDQ fun_conc_tvs (tvb, body) arg
           ; let wrap = mkWpTyApps [ty_arg]
           ; go (pos+1) (addArgWrap wrap acc) inst_body rest_args }

    go1 pos acc fun_ty (EWrap w : args)
      = go1 pos (EWrap w : acc) fun_ty args

    go1 pos acc fun_ty (EPrag sp prag : args)
      = go1 pos (EPrag sp prag : acc) fun_ty args

    -- Rule ITYARG from Fig 4 of the QL paper
    go1 pos acc fun_ty ( ETypeArg { ea_ctxt = ctxt, ea_hs_ty = hs_ty }
                             : rest_args )
      = do { (ty_arg, inst_ty) <- tcVTA fun_conc_tvs fun_ty hs_ty
           ; let arg' = ETypeArg { ea_ctxt = ctxt, ea_hs_ty = hs_ty, ea_ty_arg = ty_arg }
           ; go pos (arg' : acc) inst_ty rest_args }

    -- Rule IVAR from Fig 4 of the QL paper:
    go1 pos acc fun_ty args@(EValArg {} : _)
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
        do { arg_tys <- zipWithM new_arg_ty (leadingValArgs args) [pos..]
           ; res_ty  <- newOpenFlexiTyVarTy
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

           ; go pos acc' fun_ty' args }

    -- Rule IARG from Fig 4 of the QL paper:
    go1 pos acc fun_ty
        (EValArg { ea_arg = arg, ea_ctxt = ctxt } : rest_args)
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

           ; arg' <- quickLookArg do_ql ctxt arg arg_ty
           ; let acc' = arg' : addArgWrap wrap acc
           ; go (pos+1) acc' res_ty rest_args }

    new_arg_ty :: LHsExpr GhcRn -> Int -> TcM (Scaled TcType)
    -- Make a fresh nus for each argument in rule IVAR
    new_arg_ty (L _ arg) i
      = do { arg_nu <- newOpenFlexiFRRTyVarTy $
                       FRRExpectedFunTy (ExpectedFunTyArg (HsExprTcThing tc_fun) arg) i
               -- Following matchActualFunTy, we create nu_i :: TYPE kappa_i[conc],
               -- thereby ensuring that the arguments have concrete runtime representations

           ; mult_ty <- newFlexiTyVarTy multiplicityTy
               -- mult_ty: e need variables for argument multiplicities (#18731)
               -- Otherwise, 'undefined x' wouldn't be linear in x

           ; return (mkScaled mult_ty arg_nu) }

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
         -- in GHC.Tc.Utils.Concrete.
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
    go (L l (HsUnboundVar _ rdr))
      | isUnderscore occ = return (L l (HsWildCardTy noExtField))
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
       -- in GHC.Tc.Utils.Concrete.
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
-}

quickLookArg :: QLFlag -> AppCtxt
             -> LHsExpr GhcRn          -- ^ Argument
             -> Scaled TcSigmaTypeFRR  -- ^ Type expected by the function
             -> TcM (HsExprArg 'TcpInst)
-- See Note [Quick Look at value arguments]
quickLookArg NoQL ctxt larg orig_arg_ty
  = skipQuickLook ctxt larg orig_arg_ty
quickLookArg DoQL ctxt larg orig_arg_ty
  = do { is_rho <- tcIsDeepRho (scaledThing orig_arg_ty)
       ; traceTc "qla" (ppr orig_arg_ty $$ ppr is_rho)
       ; if not is_rho
         then skipQuickLook ctxt larg orig_arg_ty
         else quickLookArg1 ctxt larg orig_arg_ty }

skipQuickLook :: AppCtxt -> LHsExpr GhcRn -> Scaled TcRhoType
              -> TcM (HsExprArg 'TcpInst)
skipQuickLook ctxt larg arg_ty
  = return (EValArg { ea_ctxt   = ctxt
                    , ea_arg    = larg
                    , ea_arg_ty = arg_ty })

whenQL :: QLFlag -> ZonkM () -> TcM ()
whenQL DoQL thing_inside = liftZonkM thing_inside
whenQL NoQL _            = return ()

tcIsDeepRho :: TcType -> TcM Bool
-- This top-level zonk step, which is the reason we need a local 'go' loop,
-- is subtle. See Section 9 of the QL paper

tcIsDeepRho ty
  = do { ds_flag <- getDeepSubsumptionFlag
       ; go ds_flag ty }
  where
    go ds_flag ty
      | isSigmaTy ty = return False

      | Just kappa <- getTyVar_maybe ty
      , isQLInstTyVar kappa
      = do { info <- readMetaTyVar kappa
           ; case info of
               Indirect arg_ty' -> go ds_flag arg_ty'
               Flexi            -> return True }

      | Deep <- ds_flag
      , Just (_, res_ty) <- tcSplitFunTy_maybe ty
      = go ds_flag res_ty

      | otherwise = return True

isGuardedTy :: TcType -> Bool
isGuardedTy ty
  | Just (tc,_) <- tcSplitTyConApp_maybe ty = isGenerativeTyCon tc Nominal
  | Just {} <- tcSplitAppTy_maybe ty        = True
  | otherwise                               = False

quickLookArg1 :: AppCtxt -> LHsExpr GhcRn
              -> Scaled TcRhoType  -- Deeply skolemised
              -> TcM (HsExprArg 'TcpInst)
-- quickLookArg1 implements the "QL Argument" judgement in Fig 5 of the paper
quickLookArg1 ctxt larg@(L _ arg) sc_arg_ty@(Scaled _ orig_arg_rho)
  = addArgCtxt ctxt larg $ -- Context needed for constraints
                           -- generated by calls in arg
    do { ((rn_fun, fun_ctxt), rn_args) <- splitHsApps arg

       -- Step 1: get the type of the head of the argument
       ; mb_fun_ty <- tcInferAppHead_maybe rn_fun
       ; traceTc "quickLookArg {" $
         vcat [ text "arg:" <+> ppr arg
              , text "orig_arg_rho:" <+> ppr orig_arg_rho
              , text "head:" <+> ppr rn_fun <+> dcolon <+> ppr mb_fun_ty
              , text "args:" <+> ppr rn_args ]

       ; case mb_fun_ty of {
           Nothing -> skipQuickLook ctxt larg sc_arg_ty ;    -- fun is too complicated
           Just (tc_fun, fun_sigma) ->

       -- step 2: use |-inst to instantiate the head applied to the arguments
    do { let tc_head = (tc_fun, fun_ctxt)
       ; do_ql <- wantQuickLook rn_fun
       ; ((inst_args, app_res_rho), wanted)
             <- captureConstraints $
                tcInstFun do_ql True tc_head fun_sigma rn_args

       ; traceTc "quickLookArg 2" $
         vcat [ text "arg:" <+> ppr arg
              , text "orig_arg_rho:" <+> ppr orig_arg_rho
              , text "app_res_rho:" <+> ppr app_res_rho ]

       -- Step 3: Check the two other premises of APP-lightning-bolt (Fig 5 in the paper)
       --         Namely: (A) is orig_arg_rho is guarded
         --           or: (B) fiv(app_res_rho) = emptyset
       -- This tells us if the quick look at the argument yields information that
       -- influences the enclosing function call
       -- NB: guardedness is computed based on the original,
       -- unzonked orig_arg_rho, so that we deliberately do
       -- not exploit guardedness that emerges a result of QL on earlier args
       -- We must do the anyFreeKappa test /after/ tcInstFun; see (QLA3).
       ; arg_influences_enclosing_call
            <- if isGuardedTy orig_arg_rho
               then return True
               else not <$> anyFreeKappa app_res_rho  -- (B)
                    -- For (B) see Note [The fiv test in quickLookArg]

       -- Step 4: do quick-look unification if either (A) or (B) hold
       -- NB: orig_arg_rho may not be zonked, but that's ok
       ; when arg_influences_enclosing_call $
         qlUnify app_res_rho orig_arg_rho

       ; traceTc "quickLookArg done }" (ppr rn_fun)

       ; return (EValArgQL { eaql_ctxt    = ctxt
                           , eaql_arg_ty  = sc_arg_ty
                           , eaql_larg    = larg
                           , eaql_tc_fun  = tc_head
                           , eaql_args    = inst_args
                           , eaql_wanted  = wanted
                           , eaql_encl    = arg_influences_enclosing_call
                           , eaql_res_rho = app_res_rho }) }}}

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

We so this by look at the arguments, left to right, monomorphising the free
instantiation variables of the /type/ arguments of the call.  Those type
arguments appear (only) in
  * the `WpTyApp` components of
  * the `HsWrapper` of
  * a `EWrap` argument
See `qlMonoHsWrapper`.

By going left to right, we are sure to monomorphise instantiation variables
before we encounter them in an argument type (in `tcValArg`).

All instantiation variables for a call will be reachable from the type(s)
at which the function is instantiated -- i.e. those WpTyApps.  Even instantiation
variables allocoated by tcInstFun itself, such as in the IRESULT rule, end up
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

qlMonoHsWrapper :: HsWrapper -> ZonkM ()
-- See Note [Monomorphise instantiation variables]
qlMonoHsWrapper (WpCompose w1 w2) = qlMonoHsWrapper w1 >> qlMonoHsWrapper w2
qlMonoHsWrapper (WpTyApp ty)      = qlMonoTcType ty
qlMonoHsWrapper _                 = return ()

qlMonoTcType :: TcType -> ZonkM ()
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
  kappa ~ [Forall a. a->a]
-}

anyFreeKappa :: TcType -> TcM Bool
-- True if there is a free instantiation variable
-- in the argument type, after zonking
-- See Note [The fiv test in quickLookArg]
anyFreeKappa ty = unTcMBool (foldQLInstVars go_tv ty)
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
  = do_ty ty
  where
    (do_ty, _, _, _) = foldTyCo folder ()

    folder :: TyCoFolder () a
    folder = TyCoFolder { tcf_view = noView  -- See Note [Free vars and synonyms]
                                             -- in GHC.Core.TyCo.FVs
                        , tcf_tyvar = do_tv, tcf_covar = mempty
                        , tcf_hole = do_hole, tcf_tycobinder = do_bndr }

    do_bndr _ _ _ = ()

    do_hole _ hole = do_ty (coVarKind (coHoleCoVar hole))
                     -- See (MIV2) in Note [Monomorphise instantiation variables]

    do_tv :: () -> TcTyVar -> a
    do_tv _ tv | isQLInstTyVar tv = check_tv tv
               | otherwise        = mempty

{- *********************************************************************
*                                                                      *
                 QuickLook unification
*                                                                      *
********************************************************************* -}

qlUnify :: TcType -> TcType -> TcM ()
-- Unify ty1 with ty2:
--   * It can unify both instantiation variables (possibly with polytypes),
--     and ordinary unification variables (but only with monotypes)
--   * It does not return a coercion (unlike unifyType); it is called
--     for the sole purpose of unifying instantiation variables, although it
--     may also (opportunistically) unify regular unification variables.
--   * It never produces errors, even for mis-matched types
--   * It may return without having made the argument types equal, of course;
--     it just makes best efforts.
qlUnify ty1 ty2
  = do { traceTc "qlUnify" (ppr ty1 $$ ppr ty2)
       ; go ty1 ty2 }
  where
    go :: TcType -> TcType
       -> TcM ()
    go (TyVarTy tv) ty2
      | isMetaTyVar tv = go_kappa tv ty2
    go ty1 (TyVarTy tv)
      | isMetaTyVar tv = go_kappa tv ty1

    go (CastTy ty1 _) ty2 = go ty1 ty2
    go ty1 (CastTy ty2 _) = go ty1 ty2

    go (TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2 -- See GHC.Tc.Utils.Unify
      = return ()  -- Note [Expanding synonyms during unification]

    -- Now, and only now, expand synonyms
    go rho1 rho2
      | Just rho1 <- coreView rho1 = go rho1 rho2
      | Just rho2 <- coreView rho2 = go rho1 rho2

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      | tc1 == tc2
      , not (isTypeFamilyTyCon tc1)
      , tys1 `equalLength` tys2
      = zipWithM_ go tys1 tys2

    -- Decompose (arg1 -> res1) ~ (arg2 -> res2)
    -- and         (c1 => res1) ~   (c2 => res2)
    -- But for the latter we only learn instantiation info from res1~res2
    -- We look at the multiplicity too, although the chances of getting
    -- impredicative instantiation info from there seems...remote.
    go (FunTy { ft_af = af1, ft_arg = arg1, ft_res = res1, ft_mult = mult1 })
       (FunTy { ft_af = af2, ft_arg = arg2, ft_res = res2, ft_mult = mult2 })
      | af1 == af2 -- Match the arrow TyCon
      = do { when (isVisibleFunArg af1) (go arg1 arg2)
           ; when (isFUNArg af1)        (go mult1 mult2)
           ; go res1 res2 }

    -- ToDo: c.f. Tc.Utils.unify.uType,
    -- which does not split FunTy here
    -- Also NB tcSplitAppTyNoView here, which does not split (c => t)
    go  (AppTy t1a t1b) ty2
      | Just (t2a, t2b) <- tcSplitAppTyNoView_maybe ty2
      = do { go t1a t2a; go t1b t2b }

    go ty1 (AppTy t2a t2b)
      | Just (t1a, t1b) <- tcSplitAppTyNoView_maybe ty1
      = do { go t1a t2a; go t1b t2b }

    go _ _ = return ()
       -- Don't look under foralls; see (UQL4) of Note [QuickLook unification]

    ----------------
    go_kappa kappa ty2
      = assertPpr (isMetaTyVar kappa) (ppr kappa) $
        do { info <- readMetaTyVar kappa
           ; case info of
               Indirect ty1 -> go ty1 ty2
               Flexi        -> do { ty2 <- liftZonkM $ zonkTcType ty2
                                  ; go_flexi kappa ty2 } }

    ----------------
    -- Swap (kappa1[conc] ~ kappa2[tau])
    -- otherwise we'll fail to unify and emit a coercion.
    -- Just an optimisation: emitting a coercion is fine
    go_flexi kappa (TyVarTy tv2)
      | lhsPriority tv2 > lhsPriority kappa
      = go_flexi1 tv2 (TyVarTy kappa)
    go_flexi kappa ty2
      = go_flexi1 kappa ty2

    go_flexi1 kappa ty2  -- ty2 is zonked
      | -- See Note [QuickLook unification] (UQL1)
        simpleUnifyCheck UC_QuickLook kappa ty2
      = do { co <- unifyKind (Just (TypeThing ty2)) ty2_kind kappa_kind
                   -- unifyKind: see (UQL2) in Note [QuickLook unification]
                   --            and (MIV2) in Note [Monomorphise instantiation variables]
           ; let ty2' = mkCastTy ty2 co
           ; traceTc "qlUnify:update" $
             ppr kappa <+> text ":=" <+> ppr ty2
           ; liftZonkM $ writeMetaTyVar kappa ty2' }

      | otherwise
      = return ()   -- Occurs-check or forall-bound variable
      where
        kappa_kind = tyVarKind kappa
        ty2_kind   = typeKind ty2

{- Note [QuickLook unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In qlUnify, if we find (kappa ~ ty), we are going to update kappa := ty.
That is the entire point of qlUnify!   Wrinkles:

(UQL1) Before unifying an instantiation variable in `go_flexi`, we must check
  the usual unification conditions, by calling `GHC.Tc.Utils.Unify.simpleUnifyCheck`.
  For example that checks for
    * An occurs-check
    * Level mis-match
    * An attempt to unify a concrete type variable with a non-concrete type.

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

  See also (TCAPP2) in Note [tcApp: typechecking applications].

(UQL3) Instantiation variables don't really have a settled level yet;
  they have level QLInstVar (see Note [The QLInstVar TcLevel] in GHC.Tc.Utils.TcType.
  You might worry that we might unify
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

tcTagToEnum :: (HsExpr GhcTc, AppCtxt) -> [HsExprArg 'TcpTc]
            -> TcRhoType
            -> TcM (HsExpr GhcTc)
-- tagToEnum# :: forall a. Int# -> a
-- See Note [tagToEnum#]   Urgh!
tcTagToEnum (tc_fun, fun_ctxt) tc_args res_ty
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
             tc_expr = rebuildHsApps (tc_fun', fun_ctxt) [val_arg]
       ; return (mkHsWrap df_wrap tc_expr) }}}}}

  | otherwise
  = failWithTc TcRnTagToEnumMissingValArg

  where
    vanilla_result = return (rebuildHsApps (tc_fun, fun_ctxt) tc_args)

    check_enumeration ty' tc
      | -- isTypeDataTyCon: see wrinkle (W1) in
        -- Note [Type data declarations] in GHC.Rename.Module
        isTypeDataTyCon tc    = addErrTc (TcRnTagToEnumResTyTypeData ty')
      | isEnumerationTyCon tc = return ()
      | otherwise             = addErrTc (TcRnTagToEnumResTyNotAnEnum ty')


{- *********************************************************************
*                                                                      *
           Horrible hack for rep-poly unlifted newtypes
*                                                                      *
********************************************************************* -}

{- Note [Eta-expanding rep-poly unlifted newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Any occurrence of a newtype constructor must appear at a known representation.
If the newtype is applied to an argument, then we are done: by (I2) in
Note [Representation polymorphism invariants], the argument has a known
representation, and we are done. So we are left with the situation of an
unapplied newtype constructor. For example:

  type N :: TYPE r -> TYPE r
  newtype N a = MkN a

  ok :: N Int# -> N Int#
  ok = MkN

  bad :: forall r (a :: TYPE r). N (# Int, r #) -> N (# Int, r #)
  bad = MkN

The difficulty is that, unlike the situation described in
Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete,
it is not necessarily the case that we simply need to check the instantiation
of a single variable. Consider for example:

  type RR :: Type -> Type -> RuntimeRep
  type family RR a b where ...

  type T :: forall a -> forall b -> TYPE (RR a b)
  type family T a b where ...

  type M :: forall a -> forall b -> TYPE (RR a b)
  newtype M a b = MkM (T a b)

Now, suppose we instantiate MkM, say with two types X, Y from the environment:

  foo :: T X Y -> M X Y
  foo = MkM @X @Y

we need to check that we can eta-expand MkM, for which we need to know the
representation of its argument, which is "RR X Y".

To do this, in "rejectRepPolyNewtypes", we perform a syntactic representation-
polymorphism check on the instantiated argument of the newtype, and reject
the definition if the representation isn't concrete (in the sense of Note [Concrete types]
in GHC.Tc.Utils.Concrete).

For example, we would accept "ok" above, as "IntRep" is a concrete RuntimeRep.
However, we would reject "foo", because "RR X Y" is not a concrete RuntimeRep.
If we wanted to accept "foo" (performing a PHASE 2 check (in the sense of
Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete), we would have to
significantly re-engineer unlifted newtypes in GHC. Currently, "MkM" has type:

  MkM :: forall a b. T a b %1 -> M a b

However, we should only be able to use MkM when we know the representation of
T a b (which is RR a b). This means that MkM should instead have type:

  MkM :: forall {must_be_conc} a b (co :: RR a b ~# must_be_conc)
      .  T a b |> GRefl Nominal (TYPE co) %1 -> M a b

where "must_be_conc" is a skolem type variable that must be instantiated to a
concrete type, just as in Note [Representation-polymorphism checking built-ins]
in GHC.Tc.Utils.Concrete. This means that any instantiation of "MkM", such as
"MkM @X @Y" from "foo", would create a fresh concrete metavariable "gamma[conc]"
and emit a Wanted constraint

  [W] co :: RR X Y ~# gamma[conc]

However, this all seems like a lot of work for a feature that no one is asking for,
so we decided to keep the much simpler syntactic check. Note that one possible
advantage of this approach is that we should be able to stop skipping
representation-polymorphism checks in the output of the desugarer; see (C) in
Wrinkle [Representation-polymorphic lambdas] in Note [Typechecking data constructors].
-}

-- | Reject any unsaturated use of an unlifted newtype constructor
-- if the representation of its argument isn't known.
--
-- See Note [Eta-expanding rep-poly unlifted newtypes].
rejectRepPolyNewtypes :: (HsExpr GhcTc, AppCtxt)
                      -> TcRhoType
                      -> TcM ()
rejectRepPolyNewtypes (fun,_) app_res_rho = case fun of

  XExpr (ConLikeTc (RealDataCon con) _ _)
    -- Check that this is an unsaturated occurrence of a
    -- representation-polymorphic newtype constructor.
    | isNewDataCon con
    , not $ tcHasFixedRuntimeRep $ dataConTyCon con
    , Just (_rem_arg_af, _rem_arg_mult, rem_arg_ty, _nt_res_ty)
        <- splitFunTy_maybe app_res_rho
    -> do { let frr_ctxt = FRRRepPolyUnliftedNewtype con
          ; hasFixedRuntimeRep_syntactic frr_ctxt rem_arg_ty }

  _ -> return ()


{- *********************************************************************
*                                                                      *
             Pragmas on expressions
*                                                                      *
********************************************************************* -}

tcExprPrag :: HsPragE GhcRn -> HsPragE GhcTc
tcExprPrag (HsPragSCC x1 ann) = HsPragSCC x1 ann

