{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeApplications    #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Type subsumption and unification
module GHC.Tc.Utils.Unify (
  -- Full-blown subsumption
  tcWrapResult, tcWrapResultO, tcWrapResultMono,
  tcSubType, tcSubTypeSigma, tcSubTypePat, tcSubTypeDS,
  tcSubTypeAmbiguity, tcSubMult,
  checkConstraints, checkTvConstraints,
  buildImplicationFor, buildTvImplication, emitResidualTvConstraint,

  -- Skolemisation
  DeepSubsumptionFlag(..), getDeepSubsumptionFlag, isRhoTyDS,
  tcSkolemise, tcSkolemiseCompleteSig, tcSkolemiseExpectedType,

  -- Various unifications
  unifyType, unifyKind, unifyInvisibleType, unifyExpectedType,
  unifyExprType, unifyTypeAndEmit, promoteTcType,
  swapOverTyVars, touchabilityAndShapeTest, checkTopShape, lhsPriority,
  UnifyEnv(..), updUEnvLoc, setUEnvRole,
  uType,
  mightEqualLater,
  makeTypeConcrete,

  --------------------------------
  -- Holes
  tcInfer,
  matchExpectedListTy,
  matchExpectedTyConApp,
  matchExpectedAppTy,
  matchExpectedFunTys,
  matchExpectedFunKind,
  matchActualFunTy, matchActualFunTys,

  checkTyEqRhs, recurseIntoTyConApp, recurseIntoFamTyConApp,
  PuResult(..), okCheckRefl, mapCheck,
  TyEqFlags(..),
  notUnifying_TEFTask, unifyingLHSMetaTyVar_TEFTask, defaulting_TEFTask,
  pureTyEqFlags_LHSMetaTyVar, mkTEFA_Break,

  OccursCheck(..), LevelCheck(..), ConcreteCheck(..),
  TyEqFamApp(..), FamAppBreaker(..),
  checkPromoteFreeVars,

  simpleUnifyCheck, UnifyCheckCaller(..),

  fillInferResult,
  ) where

import GHC.Prelude

import GHC.Hs

import GHC.Tc.Errors.Types ( ErrCtxtMsg(..) )
import GHC.Tc.Errors.Ppr   ( pprErrCtxtMsg )
import GHC.Tc.Utils.Concrete
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( CtLoc, mkKindEqLoc, adjustCtLoc, updateCtLocOrigin, ctLocOrigin )
import GHC.Tc.Types.Origin
import GHC.Tc.Zonk.TcType
import GHC.Tc.Utils.TcMType qualified as TcM

import GHC.Tc.Solver.InertSet

import GHC.Core.Type
import GHC.Core.TyCo.Rep hiding (Refl)
import GHC.Core.TyCo.FVs( isInjectiveInType )
import GHC.Core.TyCo.Ppr( debugPprType {- pprTyVar -} )
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Unify
import GHC.Core.Predicate( EqRel(..), mkEqPredRole, mkNomEqPred )
import GHC.Core.Multiplicity
import GHC.Core.Reduction

import qualified GHC.LanguageExtensions as LangExt

import GHC.Builtin.Types
import GHC.Types.Name
import GHC.Types.Id( idType )
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.Unique.Set (nonDetEltsUniqSet)

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Driver.DynFlags

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe (firstJusts)
import GHC.Data.Pair

import Control.Monad
import Data.Functor.Identity (Identity(..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid as DM ( Any(..) )
import qualified Data.Semigroup as S ( (<>) )
import Data.Traversable (for)

{- *********************************************************************
*                                                                      *
              matchActualFunTys
*                                                                      *
********************************************************************* -}

-- | 'matchActualFunTy' looks for just one function arrow,
-- returning an uninstantiated sigma-type.
--
-- Invariant: the returned argument type has a syntactically fixed
-- RuntimeRep in the sense of Note [Fixed RuntimeRep]
-- in GHC.Tc.Utils.Concrete.
--
-- See Note [Return arguments with a fixed RuntimeRep].
matchActualFunTy
  :: ExpectedFunTyOrigin
      -- ^ See Note [Herald for matchExpectedFunTys]
  -> Maybe TypedThing
      -- ^ The thing with type TcSigmaType
  -> (Arity, TcType)
      -- ^ Total number of value args in the call, and
      --   the original function type
      -- (Both are used only for error messages)
  -> TcRhoType
      -- ^ Type to analyse: a TcRhoType
  -> TcM (HsWrapper, Scaled TcSigmaTypeFRR, TcSigmaType)
-- This function takes in a type to analyse (a RhoType) and returns
-- an argument type and a result type (splitting apart a function arrow).
-- The returned argument type is a SigmaType with a fixed RuntimeRep;
-- as explained in Note [Return arguments with a fixed RuntimeRep].
--
-- See Note [matchActualFunTy error handling] for the first three arguments

-- If   (wrap, arg_ty, res_ty) = matchActualFunTy ... fun_ty
-- then wrap :: fun_ty ~> (arg_ty -> res_ty)
-- and NB: res_ty is an (uninstantiated) SigmaType

matchActualFunTy herald mb_thing err_info fun_ty
  = assertPpr (isRhoTy fun_ty) (ppr fun_ty) $
    go fun_ty
  where
    -- Does not allocate unnecessary meta variables: if the input already is
    -- a function, we just take it apart.  Not only is this efficient,
    -- it's important for higher rank: the argument might be of form
    --              (forall a. ty) -> other
    -- If allocated (fresh-meta-var1 -> fresh-meta-var2) and unified, we'd
    -- hide the forall inside a meta-variable
    go :: TcRhoType   -- The type we're processing, perhaps after
                      -- expanding type synonyms
       -> TcM (HsWrapper, Scaled TcSigmaTypeFRR, TcSigmaType)
    go ty | Just ty' <- coreView ty = go ty'

    go (FunTy { ft_af = af, ft_mult = w, ft_arg = arg_ty, ft_res = res_ty })
      = assert (isVisibleFunArg af) $
      do { hasFixedRuntimeRep_syntactic (FRRExpectedFunTy herald 1) arg_ty
         ; return (idHsWrapper, Scaled w arg_ty, res_ty) }

    go ty@(TyVarTy tv)
      | isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty' -> go ty'
               Flexi        -> defer ty }

       -- In all other cases we bale out into ordinary unification
       -- However unlike the meta-tyvar case, we are sure that the
       -- number of arguments doesn't match arity of the original
       -- type, so we can add a bit more context to the error message
       -- (cf #7869).
       --
       -- It is not always an error, because specialized type may have
       -- different arity, for example:
       --
       -- > f1 = f2 'a'
       -- > f2 :: Monad m => m Bool
       -- > f2 = undefined
       --
       -- But in that case we add specialized type into error context
       -- anyway, because it may be useful. See also #9605.
    go ty = addErrCtxtM (mk_ctxt ty) (defer ty)

    ------------
    defer fun_ty
      = do { arg_ty <- new_check_arg_ty herald 1
           ; res_ty <- newOpenFlexiTyVarTy
           ; let unif_fun_ty = mkScaledFunTys [arg_ty] res_ty
           ; co <- unifyType mb_thing fun_ty unif_fun_ty
           ; return (mkWpCastN co, arg_ty, res_ty) }

    ------------
    mk_ctxt :: TcType -> TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg)
    mk_ctxt _res_ty = mkFunTysMsg herald err_info

{- Note [matchActualFunTy error handling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
matchActualFunTy is made much more complicated by the
desire to produce good error messages. Consider the application
    f @Int x y
In GHC.Tc.Gen.Head.tcInstFun we instantiate the function type, one
argument at a time.  It must instantiate any type/dictionary args,
before looking for an arrow type.

But if it doesn't find an arrow type, it wants to generate a message
like "f is applied to two arguments but its type only has one".
To do that, it needs to know about the args that tcArgs has already
munched up -- hence passing in n_val_args_in_call and arg_tys_so_far;
and hence also the accumulating so_far arg to 'go'.

This allows us (in mk_ctxt) to construct f's /instantiated/ type,
with just the values-arg arrows, which is what we really want
in the error message.

Ugh!
-}

-- | Like 'matchExpectedFunTys', but used when you have an "actual" type,
-- for example in function application.
--
-- INVARIANT: the returned argument types all have a syntactically fixed RuntimeRep
-- in the sense of Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.
-- See Note [Return arguments with a fixed RuntimeRep].
matchActualFunTys :: ExpectedFunTyOrigin -- ^ See Note [Herald for matchExpectedFunTys]
                  -> CtOrigin
                  -> Arity
                  -> TcSigmaType
                  -> TcM (HsWrapper, [Scaled TcSigmaTypeFRR], TcRhoType)
-- If    matchActualFunTys n ty = (wrap, [t1,..,tn], res_ty)
-- then  wrap : ty ~> (t1 -> ... -> tn -> res_ty)
--       and res_ty is a RhoType
-- NB: the returned type is top-instantiated; it's a RhoType
matchActualFunTys herald ct_orig n_val_args_wanted top_ty
  = go n_val_args_wanted [] top_ty
  where
    go n so_far fun_ty
      | not (isRhoTy fun_ty)
      = do { (wrap1, rho) <- topInstantiate ct_orig fun_ty
           ; (wrap2, arg_tys, res_ty) <- go n so_far rho
           ; return (wrap2 <.> wrap1, arg_tys, res_ty) }

    go 0 _ fun_ty = return (idHsWrapper, [], fun_ty)

    go n so_far fun_ty
      = do { (wrap_fun1, arg_ty1, res_ty1) <- matchActualFunTy
                                                 herald Nothing
                                                 (n_val_args_wanted, top_ty)
                                                 fun_ty
           ; (wrap_res, arg_tys, res_ty)   <- go (n-1) (arg_ty1:so_far) res_ty1
           ; let wrap_fun2 = mkWpFun idHsWrapper wrap_res arg_ty1 res_ty
           -- NB: arg_ty1 comes from matchActualFunTy, so it has
           -- a syntactically fixed RuntimeRep as needed to call mkWpFun.
           ; return (wrap_fun2 <.> wrap_fun1, arg_ty1:arg_tys, res_ty) }

{-
************************************************************************
*                                                                      *
          Skolemisation and matchExpectedFunTys
*                                                                      *
************************************************************************

Note [Skolemisation overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose f :: (forall a. a->a) -> blah, and we have the application (f e)
Then we want to typecheck `e` pushing in the type `forall a. a->a`. But we
need to be careful:

* Roughly speaking, in (tcPolyExpr e (forall a b. rho)), we skolemise `a` and `b`,
  and then call (tcExpr e rho)

* But not quite!  We must be careful if `e` is a type lambda (\ @p @q -> blah).
  Then we want to line up the skolemised variables `a`,`b`
  with `p`,`q`, so we can't just call (tcExpr (\ @p @q -> blah) rho)

* A very similar situation arises with
     (\ @p @q -> blah) :: forall a b. rho
  Again, we must line up `p`, `q` with the skolemised `a` and `b`.

* Another similar situation arises with
    g :: forall a b. rho
    g @p @q x y = ....
  Here again when skolemising `a` and `b` we must be careful to match them up
  with `p` and `q`.

OK, so how exactly do we check @p binders in lambdas?  First note that we only
we only attempt to deal with @p binders when /checking/. We don't do inference for
(\ @a -> blah), not yet anyway.

For checking, there are two cases to consider:
  * Function LHS, where the function has a type signature
                  f :: forall a. a -> forall b. [b] -> blah
                  f @p x @q y = ...

  * Lambda        \ @p x @q y -> ...
                  \cases { @p x @q y -> ... }
    (\case p behaves like \cases { p -> ... }, and p is always a term pattern.)

Both ultimately handled by matchExpectedFunTys.

* Function LHS case is handled by `GHC.Tc.Gen.Bind.tcPolyCheck`:
  * It calls `tcSkolemiseCompleteSig`
  * Passes the skolemised variables into `tcFunBindMatches`
  * Which uses `matchExpectedFunTys` to decompose the function type to
    match the arguments
  * And then passes the (skolemised-variables ++ arg tys) on to `tcMatches`

* For the Lambda case there are two sub-cases:
   * An expression with a type signature: (\ @a x y -> blah) :: hs_ty
     This is handled by `GHC.Tc.Gen.Head.tcExprWithSig`, which kind-checks
     the signature and hands off to `tcExprPolyCheck` via `tcPolyLExprSig`.
     Note that the foralls at the top of hs_ty scope over the expression.

   * A higher order call: h e, where h :: poly_ty -> blah
     This is handlded by `GHC.Tc.Gen.Expr.tcPolyExpr`, which (in the
     checking case) again hands off to `tcExprPolyCheck`.  Here there is
     no type-variable scoping to worry about.

  So both sub-cases end up in `GHC.Tc.Gen.Expr.tcPolyExprCheck`
  * This skolemises the /top-level/ invisible binders, but remembers
    the binders as [ExpPatType]
  * Then it looks for a lambda, and if so, calls `tcLambdaMatches` passing in
    the skolemised binders so they can be matched up with the lambda binders.
  * Otherwise it does deep-skolemisation if DeepSubsumption is on,
    and then calls tcExpr to typecheck `e`

  The outer skolemisation in tcPolyExprCheck is done using
    * tcSkolemiseCompleteSig when there is a user-written signature
    * tcSkolemiseGeneral when the polytype just comes from the context e.g. (f e)
  The former just calls the latter, so the two cases differ only slightly:
    * Both do shallow skolemisation
    * Both go via checkConstraints, which uses implicationNeeded to decide whether
      to build an implication constraint even if there /are/ no skolems.
      See Note [When to build an implication] below.

  The difference between the two cases is that `tcSkolemiseCompleteSig`
  also brings the outer type variables into scope.  It would do no
  harm to do so in both cases, but I found that (to my surprise) doing
  so caused a non-trivial (1%-ish) perf hit on the compiler.

* `tcFunBindMatches` and `tcLambdaMatches` both use `matchExpectedFunTys`, which
  ensures that any trailing invisible binders are skolemised; and does so deeply
  if DeepSubsumption is on.

  This corresponds to the plan: "skolemise at the '=' of a function binding or
  at the '->' of a lambda binding".  (See #17594 and "Plan B2".)

Some wrinkles

(SK1) tcSkolemiseGeneral and tcSkolemiseCompleteSig make fresh type variables
      See Note [Instantiate sig with fresh variables]

(SK2) All skolemisation (even without DeepSubsumption) builds just one implication
      constraint for a nested forall like:
          forall a. Eq a => forall b. Ord b => blah
      The implication constraint will look like
          forall a b. (Eq a, Ord b) => <constraints>
      See the loop in GHC.Tc.Utils.Instantiate.topSkolemise.
      and Note [Skolemisation en-bloc] in that module


Some examples:

*     f :: forall a b. blah
      f @p x = rhs
  `tcPolyCheck` calls `tcSkolemiseCompleteSig` to skolemise the signature, and
  then calls `tcFunBindMatches` passing in [a_sk, b_sk], the skolemsed
  variables. The latter ultimately calls `tcMatches`, and thence `tcMatchPats`.
  The latter matches up the `a_sk` with `@p`, and discards the `b_sk`.

*     f :: forall (a::Type) (b::a). blah
      f @(p::b) x = rhs
  `tcSkolemiseCompleteSig` brings `a` and `b` into scope, bound to `a_sk` and `b_sk` resp.
  When `tcMatchPats` typechecks the pattern `@(p::b)` it'll find that `b` is in
  scope (as a result of tcSkolemiseCompleteSig) which is a bit strange.  But
  it'll then unify the kinds `Type ~ b`, which will fail as it should.

*     f :: Int -> forall (a::Type) (b::a). blah
      f x  @p = rhs
  `matchExpectedFunTys` does shallow skolemisation eagerly, so we'll skolemise the
  forall a b.  Then `tcMatchPats` will bind [p :-> a_sk], and discard `b_sk`.
  Discarding the `b_sk` means that
      f x @p = \ @q -> blah
  or  f x @p = let .. in \ @q -> blah
  will both be rejected: this is Plan B2: skolemise at the "=".

* Suppose DeepSubsumption is on
    f :: forall a. a -> forall b. b -> b -> forall z. z
    f @p x @q y = rhs
  The `tcSkolemiseCompleteSig` uses shallow skolemisation, so it only skolemises
  and brings into scope [a :-> a_sk]. Then `matchExpectedFunTys` skolemises the
  forall b, because it needs to expose two value arguments.  Finally
  `matchExpectedFunTys` concludes with deeply skolemising the remaining type.

  So we end up with `[p :-> a_sk, q :-> b_sk]`.  Notice that we must not
  deeply-skolemise /first/ or we'd get the tyvars [a_sk, b_sk, c_sk] which would
  not line up with the patterns [@p, x, @q, y]
-}

tcSkolemiseGeneral
  :: DeepSubsumptionFlag
  -> UserTypeCtxt
  -> TcType -> TcType   -- top_ty and expected_ty
        -- Here, top_ty      is the type we started to skolemise; used only in SigSkol
        -- -     expected_ty is the type we are actually skolemising
        -- matchExpectedFunTys walks down the type, skolemising as it goes,
        -- keeping the same top_ty, but successively smaller expected_tys
  -> ([(Name, TcInvisTVBinder)] -> TcType -> TcM result)
  -> TcM (HsWrapper, result)
tcSkolemiseGeneral ds_flag ctxt top_ty expected_ty thing_inside
  | isRhoTyDS ds_flag expected_ty
    -- Fast path for a very very common case: no skolemisation to do
    -- But still call checkConstraints in case we need an implication regardless
  = do { let sig_skol = SigSkol ctxt top_ty []
       ; (ev_binds, result) <- checkConstraints sig_skol [] [] $
                               thing_inside [] expected_ty
       ; return (mkWpLet ev_binds, result) }

  | otherwise
  = do { -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
       ; rec { (wrap, tv_prs, given, rho_ty) <- case ds_flag of
                    Deep    -> deeplySkolemise skol_info expected_ty
                    Shallow -> topSkolemise skol_info expected_ty
             ; let sig_skol = SigSkol ctxt top_ty (map (fmap binderVar) tv_prs)
             ; skol_info <- mkSkolemInfo sig_skol }

       ; let skol_tvs = map (binderVar . snd) tv_prs
       ; traceTc "tcSkolemiseGeneral" (pprUserTypeCtxt ctxt <+> ppr skol_tvs <+> ppr given)
       ; (ev_binds, result) <- checkConstraints sig_skol skol_tvs given $
                               thing_inside tv_prs rho_ty

       ; return (wrap <.> mkWpLet ev_binds, result) }
         -- The ev_binds returned by checkConstraints is very
         -- often empty, in which case mkWpLet is a no-op

tcSkolemiseCompleteSig :: TcCompleteSig
                       -> ([ExpPatType] -> TcRhoType -> TcM result)
                       -> TcM (HsWrapper, result)
-- ^ The wrapper has type: spec_ty ~> expected_ty
-- See Note [Skolemisation] for the differences between
-- tcSkolemiseCompleteSig and tcTopSkolemise

tcSkolemiseCompleteSig (CSig { sig_bndr = poly_id, sig_ctxt = ctxt, sig_loc = loc })
                       thing_inside
  = do { cur_loc <- getSrcSpanM
       ; let poly_ty = idType poly_id
       ; setSrcSpan loc $   -- Sets the location for the implication constraint
         tcSkolemiseGeneral Shallow ctxt poly_ty poly_ty $ \tv_prs rho_ty ->
         setSrcSpan cur_loc $ -- Revert to the original location
         tcExtendNameTyVarEnv (map (fmap binderVar) tv_prs) $
         thing_inside (map (mkInvisExpPatType . snd) tv_prs) rho_ty }

tcSkolemiseExpectedType :: TcSigmaType
                        -> ([ExpPatType] -> TcRhoType -> TcM result)
                        -> TcM (HsWrapper, result)
-- Just like tcSkolemiseCompleteSig, except that we don't have a user-written
-- type signature, we only have a type comimg from the context.
-- Eg. f :: (forall a. blah) -> blah
--     In the call (f e) we will call tcSkolemiseExpectedType on (forall a.blah)
--     before typececking `e`
tcSkolemiseExpectedType exp_ty thing_inside
  = tcSkolemiseGeneral Shallow GenSigCtxt exp_ty exp_ty $ \tv_prs rho_ty ->
    thing_inside (map (mkInvisExpPatType . snd) tv_prs) rho_ty

tcSkolemise :: DeepSubsumptionFlag -> UserTypeCtxt -> TcSigmaType
            -> (TcRhoType -> TcM result)
            -> TcM (HsWrapper, result)
tcSkolemise ds_flag ctxt expected_ty thing_inside
  = tcSkolemiseGeneral ds_flag ctxt expected_ty expected_ty $ \_ rho_ty ->
    thing_inside rho_ty

checkConstraints :: SkolemInfoAnon
                 -> [TcTyVar]           -- Skolems
                 -> [EvVar]             -- Given
                 -> TcM result
                 -> TcM (TcEvBinds, result)
-- checkConstraints is careful to build an implication even if
-- `skol_tvs` and `given` are both empty, under certain circumstances
-- See Note [When to build an implication]
checkConstraints skol_info skol_tvs given thing_inside
  = do { implication_needed <- implicationNeeded skol_info skol_tvs given

       ; if implication_needed
         then do { (tclvl, wanted, result) <- pushLevelAndCaptureConstraints thing_inside
                 ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info skol_tvs given wanted
                 ; traceTc "checkConstraints" (ppr tclvl $$ ppr skol_tvs)
                 ; emitImplications implics
                 ; return (ev_binds, result) }

         else -- Fast path.  We check every function argument with tcCheckPolyExpr,
              -- which uses tcTopSkolemise and hence checkConstraints.
              -- So this fast path is well-exercised
              do { res <- thing_inside
                 ; return (emptyTcEvBinds, res) } }

checkTvConstraints :: SkolemInfo
                   -> [TcTyVar]          -- Skolem tyvars
                   -> TcM result
                   -> TcM result

checkTvConstraints skol_info skol_tvs thing_inside
  = do { (tclvl, wanted, result) <- pushLevelAndCaptureConstraints thing_inside
       ; emitResidualTvConstraint skol_info skol_tvs tclvl wanted
       ; return result }

emitResidualTvConstraint :: SkolemInfo -> [TcTyVar]
                         -> TcLevel -> WantedConstraints -> TcM ()
emitResidualTvConstraint skol_info skol_tvs tclvl wanted
  | not (isEmptyWC wanted) ||
    checkTelescopeSkol skol_info_anon
  = -- checkTelescopeSkol: in this case, /always/ emit this implication
    -- even if 'wanted' is empty. We need the implication so that we check
    -- for a bad telescope. See Note [Skolem escape and forall-types] in
    -- GHC.Tc.Gen.HsType
    do { implic <- buildTvImplication skol_info_anon skol_tvs tclvl wanted
       ; emitImplication implic }

  | otherwise  -- Empty 'wanted', emit nothing
  = return ()
  where
     skol_info_anon = getSkolemInfo skol_info

buildTvImplication :: SkolemInfoAnon -> [TcTyVar]
                   -> TcLevel -> WantedConstraints -> TcM Implication
buildTvImplication skol_info skol_tvs tclvl wanted
  = assertPpr (all (isSkolemTyVar <||> isTyVarTyVar) skol_tvs) (ppr skol_tvs) $
    do { ev_binds <- newNoTcEvBinds  -- Used for equalities only, so all the constraints
                                     -- are solved by filling in coercion holes, not
                                     -- by creating a value-level evidence binding
       ; implic   <- newImplication

       ; let implic' = implic { ic_tclvl     = tclvl
                              , ic_skols     = skol_tvs
                              , ic_given_eqs = NoGivenEqs
                              , ic_wanted    = wanted
                              , ic_binds     = ev_binds
                              , ic_info      = skol_info }

       ; checkImplicationInvariants implic'
       ; return implic' }

implicationNeeded :: SkolemInfoAnon -> [TcTyVar] -> [EvVar] -> TcM Bool
-- See Note [When to build an implication]
implicationNeeded skol_info skol_tvs given
  | null skol_tvs
  , null given
  , not (alwaysBuildImplication skol_info)
  = -- Empty skolems and givens
    do { tc_lvl <- getTcLevel
       ; if not (isTopTcLevel tc_lvl)  -- No implication needed if we are
         then return False             -- already inside an implication
         else
    do { dflags <- getDynFlags       -- If any deferral can happen,
                                     -- we must build an implication
       ; return (gopt Opt_DeferTypeErrors dflags ||
                 gopt Opt_DeferTypedHoles dflags ||
                 gopt Opt_DeferOutOfScopeVariables dflags) } }

  | otherwise     -- Non-empty skolems or givens
  = return True   -- Definitely need an implication

alwaysBuildImplication :: SkolemInfoAnon -> Bool
-- See Note [When to build an implication]
alwaysBuildImplication _ = False

{-  Commmented out for now while I figure out about error messages.
    See #14185

alwaysBuildImplication (SigSkol ctxt _ _)
  = case ctxt of
      FunSigCtxt {} -> True  -- RHS of a binding with a signature
      _             -> False
alwaysBuildImplication (RuleSkol {})      = True
alwaysBuildImplication (InstSkol {})      = True
alwaysBuildImplication (FamInstSkol {})   = True
alwaysBuildImplication _                  = False
-}

buildImplicationFor :: TcLevel -> SkolemInfoAnon -> [TcTyVar]
                   -> [EvVar] -> WantedConstraints
                   -> TcM (Bag Implication, TcEvBinds)
buildImplicationFor tclvl skol_info skol_tvs given wanted
  | isEmptyWC wanted && null given
             -- Optimisation : if there are no wanteds, and no givens
             -- don't generate an implication at all.
             -- Reason for the (null given): we don't want to lose
             -- the "inaccessible alternative" error check
  = return (emptyBag, emptyTcEvBinds)

  | otherwise
  = assertPpr (all (isSkolemTyVar <||> isTyVarTyVar) skol_tvs) (ppr skol_tvs) $
      -- Why allow TyVarTvs? Because implicitly declared kind variables in
      -- non-CUSK type declarations are TyVarTvs, and we need to bring them
      -- into scope as a skolem in an implication. This is OK, though,
      -- because TyVarTvs will always remain tyvars, even after unification.
    do { ev_binds_var <- newTcEvBinds
       ; implic <- newImplication
       ; let implic' = implic { ic_tclvl  = tclvl
                              , ic_skols  = skol_tvs
                              , ic_given  = given
                              , ic_wanted = wanted
                              , ic_binds  = ev_binds_var
                              , ic_info   = skol_info }
       ; checkImplicationInvariants implic'

       ; return (unitBag implic', TcEvBinds ev_binds_var) }

{- Note [When to build an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have some 'skolems' and some 'givens', and we are
considering whether to wrap the constraints in their scope into an
implication.  We must /always/ do so if either 'skolems' or 'givens' are
non-empty.  But what if both are empty?  You might think we could
always drop the implication.  Other things being equal, the fewer
implications the better.  Less clutter and overhead.  But we must
take care:

* If we have an unsolved [W] g :: a ~# b, and -fdefer-type-errors,
  we'll make a /term-level/ evidence binding for 'g = error "blah"'.
  We must have an EvBindsVar those bindings!, otherwise they end up as
  top-level unlifted bindings, which are verboten. This only matters
  at top level, so we check for that
  See also Note [Deferred errors for coercion holes] in GHC.Tc.Errors.
  cf #14149 for an example of what goes wrong.

* This is /necessary/ for top level but may be /desirable/ even for
  nested bindings, because if the deferred coercion is bound too far
  out it will be reported even if that thunk (say) is not evaluated.

* If you have
     f :: Int;  f = f_blah
     g :: Bool; g = g_blah
  If we don't build an implication for f or g (no tyvars, no givens),
  the constraints for f_blah and g_blah are solved together.  And that
  can yield /very/ confusing error messages, because we can get
      [W] C Int b1    -- from f_blah
      [W] C Int b2    -- from g_blan
  and fundeps can yield [W] b1 ~ b2, even though the two functions have
  literally nothing to do with each other.  #14185 is an example.
  Building an implication keeps them separate.

Note [Herald for matchExpectedFunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'herald' always looks like:
   "The equation(s) for 'f' have"
   "The abstraction (\x.e) takes"
   "The section (+ x) expects"
   "The function 'f' is applied to"

This is used to construct a message of form

   The abstraction `\Just 1 -> ...' takes two arguments
   but its type `Maybe a -> a' has only one

   The equation(s) for `f' have two arguments
   but its type `Maybe a -> a' has only one

   The section `(f 3)' requires 'f' to take two arguments
   but its type `Int -> Int' has only one

   The function 'f' is applied to two arguments
   but its type `Int -> Int' has only one

When visible type applications (e.g., `f @Int 1 2`, as in #13902) enter the
picture, we have a choice in deciding whether to count the type applications as
proper arguments:

   The function 'f' is applied to one visible type argument
     and two value arguments
   but its type `forall a. a -> a` has only one visible type argument
     and one value argument

Or whether to include the type applications as part of the herald itself:

   The expression 'f @Int' is applied to two arguments
   but its type `Int -> Int` has only one

The latter is easier to implement and is arguably easier to understand, so we
choose to implement that option.

Note [matchExpectedFunTys]
~~~~~~~~~~~~~~~~~~~~~~~~~~
matchExpectedFunTys checks that a sigma has the form
of an n-ary function.  It passes the decomposed type to the
thing_inside, and returns a wrapper to coerce between the two types

It's used wherever a language construct must have a functional type,
namely:
        A lambda expression
        A function definition
     An operator section

This function must be written CPS'd because it needs to fill in the
ExpTypes produced for arguments before it can fill in the ExpType
passed in.

Note [Return arguments with a fixed RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The functions

  - matchExpectedFunTys,
  - matchActualFunTy,
  - matchActualFunTys,

peel off argument types, as explained in Note [matchExpectedFunTys].
It's important that these functions return argument types that have
a fixed runtime representation, otherwise we would be in violation
of the representation-polymorphism invariants of
Note [Representation polymorphism invariants] in GHC.Core.

This is why all these functions have an additional invariant,
that the argument types they return all have a syntactically fixed RuntimeRep,
in the sense of Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.

Example:

  Suppose we have

    type F :: Type -> RuntimeRep
    type family F a where { F Int = LiftedRep }

    type Dual :: Type -> Type
    type family Dual a where
      Dual a = a -> ()

    f :: forall (a :: TYPE (F Int)). Dual a
    f = \ x -> ()

  The body of `f` is a lambda abstraction, so we must be able to split off
  one argument type from its type. This is handled by `matchExpectedFunTys`
  (see 'GHC.Tc.Gen.Match.tcLambdaMatches'). We end up with desugared Core that
  looks like this:

    f :: forall (a :: TYPE (F Int)). Dual (a |> (TYPE F[0]))
    f = \ @(a :: TYPE (F Int)) ->
          (\ (x :: (a |> (TYPE F[0]))) -> ())
          `cast`
          (Sub (Sym (Dual[0] <(a |> (TYPE F[0]))>)))

  Two important transformations took place:

    1. We inserted casts around the argument type to ensure that it has
       a fixed runtime representation, as required by invariant (I1) from
       Note [Representation polymorphism invariants] in GHC.Core.
    2. We inserted a cast around the whole lambda to make everything line up
       with the type signature.
-}

-- | Use this function to split off arguments types when you have an
-- \"expected\" type.
--
-- This function skolemises at each polytype.
--
-- Invariant: this function only applies the provided function
-- to a list of argument types which all have a syntactically fixed RuntimeRep
-- in the sense of Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.
-- See Note [Return arguments with a fixed RuntimeRep].
matchExpectedFunTys :: forall a.
                       ExpectedFunTyOrigin  -- See Note [Herald for matchExpectedFunTys]
                    -> UserTypeCtxt
                    -> VisArity
                    -> ExpSigmaType
                    -> ([ExpPatType] -> ExpRhoType -> TcM a)
                    -> TcM (HsWrapper, a)
-- If    matchExpectedFunTys n ty = (wrap, _)
-- then  wrap : (t1 -> ... -> tn -> ty_r) ~> ty,
--   where [t1, ..., tn], ty_r are passed to the thing_inside
--
-- Unconditionally concludes by skolemising any trailing invisible
-- binders and, if DeepSubsumption is on, it does so deeply.
--
-- Postcondition:
--   If exp_ty is Check {}, then [ExpPatType] and ExpRhoType results are all Check{}
--   If exp_ty is Infer {}, then [ExpPatType] and ExpRhoType results are all Infer{}
matchExpectedFunTys herald _ arity (Infer inf_res) thing_inside
  = do { arg_tys <- mapM (new_infer_arg_ty herald) [1 .. arity]
       ; res_ty  <- newInferExpType
       ; result  <- thing_inside (map ExpFunPatTy arg_tys) res_ty
       ; arg_tys <- mapM (\(Scaled m t) -> Scaled m <$> readExpType t) arg_tys
       ; res_ty  <- readExpType res_ty
       ; co <- fillInferResult (mkScaledFunTys arg_tys res_ty) inf_res
       ; return (mkWpCastN co, result) }

matchExpectedFunTys herald ctx arity (Check top_ty) thing_inside
  = check arity [] top_ty
  where
    check :: VisArity -> [ExpPatType] -> TcSigmaType -> TcM (HsWrapper, a)
    -- `check` is called only in the Check{} case
    -- It collects rev_pat_tys in reversed order
    -- n_req is the number of /visible/ arguments still needed

    ----------------------------
    -- Skolemise quantifiers, both visible (up to n_req) and invisible
    -- See Note [Visible type application and abstraction] in GHC.Tc.Gen.App
    check n_req rev_pat_tys ty
      | isSigmaTy ty                     -- An invisible quantifier at the top
        || (n_req > 0 && isForAllTy ty)  -- A visible quantifier at top, and we need it
      = do { rec { (n_req', wrap_gen, tv_nms, bndrs, given, inner_ty) <- skolemiseRequired skol_info n_req ty
                 ; let sig_skol = SigSkol ctx top_ty (tv_nms `zip` skol_tvs)
                       skol_tvs = binderVars bndrs
                 ; skol_info <- mkSkolemInfo sig_skol }
             -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
             --           in GHC.Tc.Utils.TcType
           ; (ev_binds, (wrap_res, result))
                  <- checkConstraints (getSkolemInfo skol_info) skol_tvs given $
                     check n_req'
                           (reverse (map ExpForAllPatTy bndrs) ++ rev_pat_tys)
                           inner_ty
           ; assertPpr (not (null bndrs && null given)) (ppr ty) $
                       -- The guard ensures that we made some progress
             return (wrap_gen <.> mkWpLet ev_binds <.> wrap_res, result) }

    ----------------------------
    -- Base case: (n_req == 0): no more args
    --    The earlier skolemisation ensurs that rho_ty has no top-level invisible quantifiers
    --    If there is deep subsumption, do deep skolemisation now
    check n_req rev_pat_tys rho_ty
      | n_req == 0
      = do { let pat_tys = reverse rev_pat_tys
           ; ds_flag <- getDeepSubsumptionFlag
           ; case ds_flag of
               Shallow -> do { res <- thing_inside pat_tys (mkCheckExpType rho_ty)
                             ; return (idHsWrapper, res) }
               Deep    -> tcSkolemiseGeneral Deep ctx top_ty rho_ty $ \_ rho_ty ->
                          -- "_" drop the /deeply/-skolemise binders
                          -- They do not line up with binders in the Match
                          thing_inside pat_tys (mkCheckExpType rho_ty) }

    ----------------------------
    -- Function types
    check n_req rev_pat_tys (FunTy { ft_af = af, ft_mult = mult
                                   , ft_arg = arg_ty, ft_res = res_ty })
      = assert (isVisibleFunArg af) $
        do { let arg_pos = arity - n_req + 1   -- 1 for the first argument etc
           ; (arg_co, arg_ty) <- hasFixedRuntimeRep (FRRExpectedFunTy herald arg_pos) arg_ty
           ; (wrap_res, result) <- check (n_req - 1)
                                         (mkCheckExpFunPatTy (Scaled mult arg_ty) : rev_pat_tys)
                                         res_ty
           ; let wrap_arg = mkWpCastN arg_co
                 fun_wrap = mkWpFun wrap_arg wrap_res (Scaled mult arg_ty) res_ty
           ; return (fun_wrap, result) }

    ----------------------------
    -- Type variables
    check n_req rev_pat_tys ty@(TyVarTy tv)
      | isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty' -> check n_req rev_pat_tys ty'
               Flexi        -> defer n_req rev_pat_tys ty }

    ----------------------------
    -- NOW do coreView.  We didn't do it before, so that we do not unnecessarily
    -- unwrap a synonym in the returned rho_ty
    check n_req rev_pat_tys ty
      | Just ty' <- coreView ty = check n_req rev_pat_tys ty'

       -- In all other cases we bale out into ordinary unification
       -- However unlike the meta-tyvar case, we are sure that the
       -- number of arguments doesn't match arity of the original
       -- type, so we can add a bit more context to the error message
       -- (cf #7869).
       --
       -- It is not always an error, because specialized type may have
       -- different arity, for example:
       --
       -- > f1 = f2 'a'
       -- > f2 :: Monad m => m Bool
       -- > f2 = undefined
       --
       -- But in that case we add specialized type into error context
       -- anyway, because it may be useful. See also #9605.
    check n_req rev_pat_tys res_ty
      = addErrCtxtM (mkFunTysMsg herald (arity, top_ty))  $
        defer n_req rev_pat_tys res_ty

    ------------
    defer :: VisArity -> [ExpPatType] -> TcRhoType -> TcM (HsWrapper, a)
    defer n_req rev_pat_tys fun_ty
      = do { more_arg_tys <- mapM (new_check_arg_ty herald) [arity - n_req + 1 .. arity]
           ; let all_pats = reverse rev_pat_tys ++ map mkCheckExpFunPatTy more_arg_tys
           ; res_ty <- newOpenFlexiTyVarTy
           ; result <- thing_inside all_pats (mkCheckExpType res_ty)

           ; co <- unifyType Nothing (mkScaledFunTys more_arg_tys res_ty) fun_ty
           ; return (mkWpCastN co, result) }

new_infer_arg_ty :: ExpectedFunTyOrigin -> Int -> TcM (Scaled ExpSigmaTypeFRR)
new_infer_arg_ty herald arg_pos -- position for error messages only
  = do { mult     <- newFlexiTyVarTy multiplicityTy
       ; inf_hole <- newInferExpTypeFRR (FRRExpectedFunTy herald arg_pos)
       ; return (mkScaled mult inf_hole) }

new_check_arg_ty :: ExpectedFunTyOrigin -> Int -> TcM (Scaled TcType)
new_check_arg_ty herald arg_pos -- Position for error messages only, 1 for first arg
  = do { mult   <- newFlexiTyVarTy multiplicityTy
       ; arg_ty <- newOpenFlexiFRRTyVarTy (FRRExpectedFunTy herald arg_pos)
       ; return (mkScaled mult arg_ty) }

mkFunTysMsg :: ExpectedFunTyOrigin
            -> (VisArity, TcType)
            -> TidyEnv -> ZonkM (TidyEnv, ErrCtxtMsg)
-- See Note [Reporting application arity errors]
mkFunTysMsg herald (n_vis_args_in_call, fun_ty) env
  = do { (env', fun_ty) <- zonkTidyTcType env fun_ty

       ; let (pi_ty_bndrs, _) = splitPiTys fun_ty
             n_fun_args = count isVisiblePiTyBinder pi_ty_bndrs

       ; return (env', FunTysCtxt herald fun_ty n_vis_args_in_call n_fun_args) }


{- Note [Reporting application arity errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider      f :: Int -> Int -> Int
and the call  foo = f 3 4 5
We'd like to get an error like:

    • Couldn't match expected type ‘t0 -> t’ with actual type ‘Int’
    • The function ‘f’ is applied to three visible arguments,           -- What are "visible" arguments?
        but its type ‘Int -> Int -> Int’ has only two                   -- See Note [Visibility and arity] in GHC.Types.Basic

That is what `mkFunTysMsg` tries to do.  But what is the "type of the function".
Most obviously, we can report its full, polymorphic type; that is simple and
explicable.  But sometimes a bit odd.  Consider
    f :: Bool -> t Int Int
    foo = f True 5 10
We get this error:
    • Couldn't match type ‘Int’ with ‘t0 -> t’
      Expected: Int -> t0 -> t
        Actual: Int -> Int
    • The function ‘f’ is applied to three visible arguments,
        but its type ‘Bool -> t Int Int’ has only one

That's not /quite/ right beause we can instantiate `t` to an arrow and get
two arrows (but not three!).  With that in mind, one could consider reporting
the /instantiated/ type, and GHC used to do so.  But it's more work, and in
some ways more confusing, especially when nested quantifiers are concerned, e.g.
    f :: Bool -> forall t. t Int Int

So we just keep it simple and report the original function type.


************************************************************************
*                                                                      *
                    Other matchExpected functions
*                                                                      *
********************************************************************* -}

matchExpectedListTy :: TcRhoType -> TcM (TcCoercionN, TcRhoType)
-- Special case for lists
matchExpectedListTy exp_ty
 = do { (co, [elt_ty]) <- matchExpectedTyConApp listTyCon exp_ty
      ; return (co, elt_ty) }

---------------------
matchExpectedTyConApp :: TyCon                -- T :: forall kv1 ... kvm. k1 -> ... -> kn -> *
                      -> TcRhoType            -- orig_ty
                      -> TcM (TcCoercionN,    -- T k1 k2 k3 a b c ~N orig_ty
                              [TcSigmaType])  -- Element types, k1 k2 k3 a b c

-- It's used for wired-in tycons, so we call checkWiredInTyCon
-- Precondition: never called with FunTyCon
-- Precondition: input type :: *
-- Postcondition: (T k1 k2 k3 a b c) is well-kinded

matchExpectedTyConApp tc orig_ty
  = assertPpr (isAlgTyCon tc) (ppr tc) $
    go orig_ty
  where
    go ty
       | Just ty' <- coreView ty
       = go ty'

    go ty@(TyConApp tycon args)
       | tc == tycon  -- Common case
       = return (mkNomReflCo ty, args)

    go (TyVarTy tv)
       | isMetaTyVar tv
       = do { cts <- readMetaTyVar tv
            ; case cts of
                Indirect ty -> go ty
                Flexi       -> defer }

    go _ = defer

    -- If the common case does not occur, instantiate a template
    -- T k1 .. kn t1 .. tm, and unify with the original type
    -- Doing it this way ensures that the types we return are
    -- kind-compatible with T.  For example, suppose we have
    --       matchExpectedTyConApp T (f Maybe)
    -- where data T a = MkT a
    -- Then we don't want to instantiate T's data constructors with
    --    (a::*) ~ Maybe
    -- because that'll make types that are utterly ill-kinded.
    -- This happened in #7368
    defer
      = do { (_, arg_tvs) <- newMetaTyVars (tyConTyVars tc)
           ; traceTc "matchExpectedTyConApp" (ppr tc $$ ppr (tyConTyVars tc) $$ ppr arg_tvs)
           ; let args = mkTyVarTys arg_tvs
                 tc_template = mkTyConApp tc args
           ; co <- unifyType Nothing tc_template orig_ty
           ; return (co, args) }

----------------------
matchExpectedAppTy :: TcRhoType                         -- orig_ty
                   -> TcM (TcCoercion,                   -- m a ~N orig_ty
                           (TcSigmaType, TcSigmaType))  -- Returns m, a
-- If the incoming type is a mutable type variable of kind k, then
-- matchExpectedAppTy returns a new type variable (m: * -> k); note the *.

matchExpectedAppTy orig_ty
  = go orig_ty
  where
    go ty
      | Just ty' <- coreView ty = go ty'

      | Just (fun_ty, arg_ty) <- tcSplitAppTy_maybe ty
      = return (mkNomReflCo orig_ty, (fun_ty, arg_ty))

    go (TyVarTy tv)
      | isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty -> go ty
               Flexi       -> defer }

    go _ = defer

    -- Defer splitting by generating an equality constraint
    defer
      = do { ty1 <- newFlexiTyVarTy kind1
           ; ty2 <- newFlexiTyVarTy kind2
           ; co <- unifyType Nothing (mkAppTy ty1 ty2) orig_ty
           ; return (co, (ty1, ty2)) }

    orig_kind = typeKind orig_ty
    kind1 = mkVisFunTyMany liftedTypeKind orig_kind
    kind2 = liftedTypeKind    -- m :: * -> k
                              -- arg type :: *

{- **********************************************************************
*
                      fillInferResult
*
********************************************************************** -}

{- Note [inferResultToType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
expTypeToType and inferResultType convert an InferResult to a monotype.
It must be a monotype because if the InferResult isn't already filled in,
we fill it in with a unification variable (hence monotype).  So to preserve
order-independence we check for mono-type-ness even if it *is* filled in
already.

See also Note [TcLevel of ExpType] in GHC.Tc.Utils.TcType, and
Note [fillInferResult].
-}

-- | Fill an 'InferResult' with the given type.
--
-- If @co = fillInferResult t1 infer_res@, then @co :: t1 ~# t2@,
-- where @t2@ is the type stored in the 'ir_ref' field of @infer_res@.
--
-- This function enforces the following invariants:
--
--  - Level invariant.
--    The stored type @t2@ is at the same level as given by the
--    'ir_lvl' field.
--  - FRR invariant.
--    Whenever the 'ir_frr' field is not @Nothing@, @t2@ is guaranteed
--    to have a syntactically fixed RuntimeRep, in the sense of
--    Note [Fixed RuntimeRep] in GHC.Tc.Utils.Concrete.
fillInferResult :: TcType -> InferResult -> TcM TcCoercionN
fillInferResult act_res_ty (IR { ir_uniq = u
                               , ir_lvl  = res_lvl
                               , ir_frr  = mb_frr
                               , ir_ref  = ref })
  = do { mb_exp_res_ty <- readTcRef ref
       ; case mb_exp_res_ty of
            Just exp_res_ty
               -- We progressively refine the type stored in 'ref',
               -- for example when inferring types across multiple equations.
               --
               -- Example:
               --
               --  \ x -> case y of { True -> x ; False -> 3 :: Int }
               --
               -- When inferring the return type of this function, we will create
               -- an 'Infer' 'ExpType', which will first be filled by the type of 'x'
               -- after typechecking the first equation, and then filled again with
               -- the type 'Int', at which point we want to ensure that we unify
               -- the type of 'x' with 'Int'. This is what is happening below when
               -- we are "joining" several inferred 'ExpType's.
               -> do { traceTc "Joining inferred ExpType" $
                       ppr u <> colon <+> ppr act_res_ty <+> char '~' <+> ppr exp_res_ty
                     ; cur_lvl <- getTcLevel
                     ; unless (cur_lvl `sameDepthAs` res_lvl) $
                       ensureMonoType act_res_ty
                     ; unifyType Nothing act_res_ty exp_res_ty }
            Nothing
               -> do { traceTc "Filling inferred ExpType" $
                       ppr u <+> text ":=" <+> ppr act_res_ty

                     -- Enforce the level invariant: ensure the TcLevel of
                     -- the type we are writing to 'ref' matches 'ir_lvl'.
                     ; (prom_co, act_res_ty) <- promoteTcType res_lvl act_res_ty

                     -- Enforce the FRR invariant: ensure the type has a syntactically
                     -- fixed RuntimeRep (if necessary, i.e. 'mb_frr' is not 'Nothing').
                     ; (frr_co, act_res_ty) <-
                         case mb_frr of
                           Nothing       -> return (mkNomReflCo act_res_ty, act_res_ty)
                           Just frr_orig -> hasFixedRuntimeRep frr_orig act_res_ty

                     -- Compose the two coercions.
                     ; let final_co = prom_co `mkTransCo` frr_co

                     ; writeTcRef ref (Just act_res_ty)

                     ; return final_co }
     }

{- Note [fillInferResult]
~~~~~~~~~~~~~~~~~~~~~~~~~
When inferring, we use fillInferResult to "fill in" the hole in InferResult
   data InferResult = IR { ir_uniq :: Unique
                         , ir_lvl  :: TcLevel
                         , ir_ref  :: IORef (Maybe TcType) }

There are two things to worry about:

1. What if it is under a GADT or existential pattern match?
   - GADTs: a unification variable (and Infer's hole is similar) is untouchable
   - Existentials: be careful about skolem-escape

2. What if it is filled in more than once?  E.g. multiple branches of a case
     case e of
        T1 -> e1
        T2 -> e2

Our typing rules are:

* The RHS of a existential or GADT alternative must always be a
  monotype, regardless of the number of alternatives.

* Multiple non-existential/GADT branches can have (the same)
  higher rank type (#18412).  E.g. this is OK:
      case e of
        True  -> hr
        False -> hr
  where hr:: (forall a. a->a) -> Int
  c.f. Section 7.1 of "Practical type inference for arbitrary-rank types"
       We use choice (2) in that Section.
       (GHC 8.10 and earlier used choice (1).)

  But note that
      case e of
        True  -> hr
        False -> \x -> hr x
  will fail, because we still /infer/ both branches, so the \x will get
  a (monotype) unification variable, which will fail to unify with
  (forall a. a->a)

For (1) we can detect the GADT/existential situation by seeing that
the current TcLevel is greater than that stored in ir_lvl of the Infer
ExpType.  We bump the level whenever we go past a GADT/existential match.

Then, before filling the hole use promoteTcType to promote the type
to the outer ir_lvl.  promoteTcType does this
  - create a fresh unification variable alpha at level ir_lvl
  - emits an equality alpha[ir_lvl] ~ ty
  - fills the hole with alpha
That forces the type to be a monotype (since unification variables can
only unify with monotypes); and catches skolem-escapes because the
alpha is untouchable until the equality floats out.

For (2), we simply look to see if the hole is filled already.
  - if not, we promote (as above) and fill the hole
  - if it is filled, we simply unify with the type that is
    already there

There is one wrinkle.  Suppose we have
   case e of
      T1 -> e1 :: (forall a. a->a) -> Int
      G2 -> e2
where T1 is not GADT or existential, but G2 is a GADT.  Then suppose the
T1 alternative fills the hole with (forall a. a->a) -> Int, which is fine.
But now the G2 alternative must not *just* unify with that else we'd risk
allowing through (e2 :: (forall a. a->a) -> Int).  If we'd checked G2 first
we'd have filled the hole with a unification variable, which enforces a
monotype.

So if we check G2 second, we still want to emit a constraint that restricts
the RHS to be a monotype. This is done by ensureMonoType, and it works
by simply generating a constraint (alpha ~ ty), where alpha is a fresh
unification variable.  We discard the evidence.

-}



{-
************************************************************************
*                                                                      *
                Subsumption checking
*                                                                      *
************************************************************************

Note [Subsumption checking: tcSubType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All the tcSubType calls have the form
                tcSubType actual_ty expected_ty
which checks
                actual_ty <= expected_ty

That is, that a value of type actual_ty is acceptable in
a place expecting a value of type expected_ty.  I.e. that

    actual ty   is more polymorphic than   expected_ty

It returns a wrapper function
        co_fn :: actual_ty ~ expected_ty
which takes an HsExpr of type actual_ty into one of type
expected_ty.

Note [Ambiguity check and deep subsumption]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: (forall b. Eq b => a -> a) -> Int

Does `f` have an ambiguous type?   The ambiguity check usually checks
that this definition of f' would typecheck, where f' has the exact same
type as f:
   f' :: (forall b. Eq b => a -> a) -> Intp
   f' = f

This will be /rejected/ with DeepSubsumption but /accepted/ with
ShallowSubsumption.  On the other hand, this eta-expanded version f''
would be rejected both ways:
   f'' :: (forall b. Eq b => a -> a) -> Intp
   f'' x = f x

This is squishy in the same way as other examples in GHC.Tc.Validity
Note [The squishiness of the ambiguity check]

The situation in June 2022.  Since we have SimpleSubsumption at the moment,
we don't want introduce new breakage if you add -XDeepSubsumption, by
rejecting types as ambiguous that weren't ambiguous before.  So, as a
holding decision, we /always/ use SimpleSubsumption for the ambiguity check
(erring on the side accepting more programs). Hence tcSubTypeAmbiguity.
-}



-----------------
-- tcWrapResult needs both un-type-checked (for origins and error messages)
-- and type-checked (for wrapping) expressions
tcWrapResult :: HsExpr GhcRn -> HsExpr GhcTc -> TcSigmaType -> ExpRhoType
             -> TcM (HsExpr GhcTc)
tcWrapResult rn_expr = tcWrapResultO (exprCtOrigin rn_expr) rn_expr

tcWrapResultO :: CtOrigin -> HsExpr GhcRn -> HsExpr GhcTc -> TcSigmaType -> ExpRhoType
               -> TcM (HsExpr GhcTc)
tcWrapResultO orig rn_expr expr actual_ty res_ty
  = do { traceTc "tcWrapResult" (vcat [ text "Actual:  " <+> ppr actual_ty
                                      , text "Expected:" <+> ppr res_ty ])
       ; wrap <- tcSubTypeNC orig GenSigCtxt (Just $ HsExprRnThing rn_expr) actual_ty res_ty
       ; return (mkHsWrap wrap expr) }

tcWrapResultMono :: HsExpr GhcRn -> HsExpr GhcTc
                 -> TcRhoType   -- Actual -- a rho-type not a sigma-type
                 -> ExpRhoType  -- Expected
                 -> TcM (HsExpr GhcTc)
-- A version of tcWrapResult to use when the actual type is a
-- rho-type, so nothing to instantiate; just go straight to unify.
-- It means we don't need to pass in a CtOrigin
tcWrapResultMono rn_expr expr act_ty res_ty
  = assertPpr (isRhoTy act_ty) (ppr act_ty $$ ppr rn_expr) $
    do { co <- unifyExpectedType rn_expr act_ty res_ty
       ; return (mkHsWrapCo co expr) }

unifyExpectedType :: HsExpr GhcRn
                  -> TcRhoType   -- Actual -- a rho-type not a sigma-type
                  -> ExpRhoType  -- Expected
                  -> TcM TcCoercionN
unifyExpectedType rn_expr act_ty exp_ty
  = case exp_ty of
      Infer inf_res -> fillInferResult act_ty inf_res
      Check exp_ty  -> unifyType (Just $ HsExprRnThing rn_expr) act_ty exp_ty

------------------------
tcSubTypePat :: CtOrigin -> UserTypeCtxt
            -> ExpSigmaType -> TcSigmaType -> TcM HsWrapper
-- Used in patterns; polarity is backwards compared
--   to tcSubType
-- If wrap = tc_sub_type_et t1 t2
--    => wrap :: t1 ~> t2
tcSubTypePat inst_orig ctxt (Check ty_actual) ty_expected
  = tc_sub_type unifyTypeET inst_orig ctxt ty_actual ty_expected

tcSubTypePat _ _ (Infer inf_res) ty_expected
  = do { co <- fillInferResult ty_expected inf_res
               -- In patterns we do not instantatiate

       ; return (mkWpCastN (mkSymCo co)) }

---------------
tcSubType :: CtOrigin -> UserTypeCtxt
          -> TcSigmaType  -- ^ Actual
          -> ExpRhoType   -- ^ Expected
          -> TcM HsWrapper
-- Checks that 'actual' is more polymorphic than 'expected'
tcSubType orig ctxt ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubType" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tcSubTypeNC orig ctxt Nothing ty_actual ty_expected }

---------------
tcSubTypeDS :: HsExpr GhcRn
            -> TcRhoType   -- Actual type -- a rho-type not a sigma-type
            -> TcRhoType   -- Expected type
                           -- DeepSubsumption <=> when checking, this type
                           --                     is deeply skolemised
            -> TcM HsWrapper
-- Similar signature to unifyExpectedType; does deep subsumption
-- Only one call site, in GHC.Tc.Gen.App.tcApp
tcSubTypeDS rn_expr act_rho exp_rho
  = tc_sub_type_deep (unifyExprType rn_expr) orig GenSigCtxt act_rho exp_rho
  where
    orig = exprCtOrigin rn_expr

---------------
tcSubTypeNC :: CtOrigin          -- ^ Used when instantiating
            -> UserTypeCtxt      -- ^ Used when skolemising
            -> Maybe TypedThing -- ^ The expression that has type 'actual' (if known)
            -> TcSigmaType       -- ^ Actual type
            -> ExpRhoType        -- ^ Expected type
            -> TcM HsWrapper
tcSubTypeNC inst_orig ctxt m_thing ty_actual res_ty
  = case res_ty of
      Check ty_expected -> tc_sub_type (unifyType m_thing) inst_orig ctxt
                                       ty_actual ty_expected

      Infer inf_res -> do { (wrap, rho) <- topInstantiate inst_orig ty_actual
                                   -- See Note [Instantiation of InferResult]
                          ; co <- fillInferResult rho inf_res
                          ; return (mkWpCastN co <.> wrap) }

---------------
tcSubTypeSigma :: CtOrigin       -- where did the actual type arise / why are we
                                 -- doing this subtype check?
               -> UserTypeCtxt   -- where did the expected type arise?
               -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- External entry point, but no ExpTypes on either side
-- Checks that actual <= expected
-- Returns HsWrapper :: actual ~ expected
tcSubTypeSigma orig ctxt ty_actual ty_expected
  = tc_sub_type (unifyType Nothing) orig ctxt ty_actual ty_expected

---------------
tcSubTypeAmbiguity :: UserTypeCtxt   -- Where did this type arise
                   -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- See Note [Ambiguity check and deep subsumption]
tcSubTypeAmbiguity ctxt ty_actual ty_expected
  = tc_sub_type_ds Shallow (unifyType Nothing)
                           (AmbiguityCheckOrigin ctxt)
                           ctxt ty_actual ty_expected

---------------
addSubTypeCtxt :: TcType -> ExpType -> TcM a -> TcM a
addSubTypeCtxt ty_actual ty_expected thing_inside
 | isRhoTy ty_actual        -- If there is no polymorphism involved, the
 , isRhoExpTy ty_expected   -- TypeEqOrigin stuff (added by the _NC functions)
 = thing_inside             -- gives enough context by itself
 | otherwise
 = addErrCtxtM mk_msg thing_inside
  where
    mk_msg tidy_env
      = do { (tidy_env, ty_actual)   <- zonkTidyTcType tidy_env ty_actual
           ; ty_expected             <- readExpType ty_expected
                   -- A worry: might not be filled if we're debugging. Ugh.
           ; (tidy_env, ty_expected) <- zonkTidyTcType tidy_env ty_expected
           ; return (tidy_env, SubTypeCtxt ty_expected ty_actual) }


{- Note [Instantiation of InferResult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We now always instantiate before filling in InferResult, so that
the result is a TcRhoType: see #17173 for discussion.

For example:

1. Consider
    f x = (*)
   We want to instantiate the type of (*) before returning, else we
   will infer the type
     f :: forall {a}. a -> forall b. Num b => b -> b -> b
   This is surely confusing for users.

   And worse, the monomorphism restriction won't work properly. The MR is
   dealt with in simplifyInfer, and simplifyInfer has no way of
   instantiating. This could perhaps be worked around, but it may be
   hard to know even when instantiation should happen.

2. Another reason.  Consider
       f :: (?x :: Int) => a -> a
       g y = let ?x = 3::Int in f
   Here want to instantiate f's type so that the ?x::Int constraint
  gets discharged by the enclosing implicit-parameter binding.

3. Suppose one defines plus = (+). If we instantiate lazily, we will
   infer plus :: forall a. Num a => a -> a -> a. However, the monomorphism
   restriction compels us to infer
      plus :: Integer -> Integer -> Integer
   (or similar monotype). Indeed, the only way to know whether to apply
   the monomorphism restriction at all is to instantiate

There is one place where we don't want to instantiate eagerly,
namely in GHC.Tc.Module.tcRnExpr, which implements GHCi's :type
command. See Note [Implementing :type] in GHC.Tc.Module.
-}

---------------
tc_sub_type :: (TcType -> TcType -> TcM TcCoercionN)  -- How to unify
            -> CtOrigin       -- Used when instantiating
            -> UserTypeCtxt   -- Used when skolemising
            -> TcSigmaType    -- Actual; a sigma-type
            -> TcSigmaType    -- Expected; also a sigma-type
            -> TcM HsWrapper
-- Checks that actual_ty is more polymorphic than expected_ty
-- If wrap = tc_sub_type t1 t2
--    => wrap :: t1 ~> t2
--
-- The "how to unify argument" is always a call to `uType TypeLevel orig`,
-- but with different ways of constructing the CtOrigin `orig` from
-- the argument types and context.

----------------------
tc_sub_type unify inst_orig ctxt ty_actual ty_expected
  = do { ds_flag <- getDeepSubsumptionFlag
       ; tc_sub_type_ds ds_flag unify inst_orig ctxt ty_actual ty_expected }

----------------------
tc_sub_type_ds :: DeepSubsumptionFlag
               -> (TcType -> TcType -> TcM TcCoercionN)
               -> CtOrigin -> UserTypeCtxt -> TcSigmaType
               -> TcSigmaType -> TcM HsWrapper
-- tc_sub_type_ds is the main subsumption worker function
-- It takes an explicit DeepSubsumptionFlag
tc_sub_type_ds ds_flag unify inst_orig ctxt ty_actual ty_expected
  | definitely_poly ty_expected   -- See Note [Don't skolemise unnecessarily]
  , isRhoTyDS ds_flag ty_actual
  = do { traceTc "tc_sub_type (drop to equality)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; mkWpCastN <$>
         unify ty_actual ty_expected }

  | otherwise   -- This is the general case
  = do { traceTc "tc_sub_type (general case)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]

       ; (sk_wrap, inner_wrap)
            <- tcSkolemise ds_flag ctxt ty_expected $ \sk_rho ->
               case ds_flag of
                 Deep    -> tc_sub_type_deep unify inst_orig ctxt ty_actual sk_rho
                 Shallow -> tc_sub_type_shallow unify inst_orig ty_actual sk_rho

       ; return (sk_wrap <.> inner_wrap) }

----------------------
tc_sub_type_shallow :: (TcType -> TcType -> TcM TcCoercionN)
                    -> CtOrigin
                    -> TcSigmaType
                    -> TcRhoType   -- Skolemised (shallow-ly)
                    -> TcM HsWrapper
tc_sub_type_shallow unify inst_orig ty_actual sk_rho
  = do { (wrap, rho_a) <- topInstantiate inst_orig ty_actual
       ; cow           <- unify rho_a sk_rho
       ; return (mkWpCastN cow <.> wrap) }

----------------------
definitely_poly :: TcType -> Bool
-- A very conservative test:
-- see Note [Don't skolemise unnecessarily]
definitely_poly ty
  | (tvs, theta, tau) <- tcSplitSigmaTy ty
  , (tv:_) <- tvs   -- At least one tyvar
  , null theta      -- No constraints; see (DP1)
  , tv `isInjectiveInType` tau
       -- The tyvar actually occurs (DP2),
       -- and occurs in an injective position (DP3).
  = True
  | otherwise
  = False

{- Note [Don't skolemise unnecessarily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are trying to solve
     ty_actual   <= ty_expected
    (Char->Char) <= (forall a. a->a)
We could skolemise the 'forall a', and then complain
that (Char ~ a) is insoluble; but that's a pretty obscure
error.  It's better to say that
    (Char->Char) ~ (forall a. a->a)
fails.

If we prematurely go to equality we'll reject a program we should
accept (e.g. #13752).  So the test (which is only to improve error
message) is very conservative:

 * ty_actual   is /definitely/ monomorphic: see `definitely_mono`
   This definitely_mono test comes in "shallow" and "deep" variants

 * ty_expected is /definitely/ polymorphic: see `definitely_poly`
   This definitely_poly test is more subtle than you might think.
   Here are three cases where expected_ty looks polymorphic, but
   isn't, and where it would be /wrong/ to switch to equality:

   (DP1)  (Char->Char) <= (forall a. (a~Char) => a -> a)

   (DP2)  (Char->Char) <= (forall a. Char -> Char)

   (DP3)  (Char->Char) <= (forall a. F [a] Char -> Char)
                          where type instance F [x] t = t


Note [Coercion errors in tcSubMult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the moment, we insist that all sub-multiplicity tests turn out
(once the typechecker has finished its work) to be equalities,
i.e. implementable by ReflCo.  Why?  Because our type system has
no way to express non-Refl sub-multiplicities.

How can we check that every call to `tcSubMult` returns `Refl`?
It might not be `Refl` *yet*.

[TODO: add counterexample #25130]

So we take the following approach

* In `tcEqMult`:
  - Emit a perfectly ordinary Wanted equality constraint for the equality,
    returning a coercion.

  - Wrap that coercion with `DE_Multiplicity` to make a `DelayedError`, and put
    that delayed error into `wc_errors` of the current WantedConstraints.  This
    is done by `ensureReflMultiplicityCo`.

* When solving constraints, discard any `DE_Multiplicity` errors that wrap a
  reflective coercion, of kind `ty ~ ty`.  This is done in
  `GHC.Tc.Solver.simplifyDelayedErrors`

* After constraint solving is complete report an error if there are any
  remaining `DE_Multiplicity` errors.  See
  `GHC.Tc.Errors.reportMultiplicityCoercionErrs`

Wrinkles

(DME1) If the multiplicity constraint is /solved/, but with a non-reflective
   coercion, we'll have just a `DE_Multiplicity` error left over.

   But if the multiplicity constraint is /unsolved/ (e.g. ManyTy ~ OneTy), we
   will have /both/ an unsolved Wanted in `wc_simple`, /and/ a `DE_Multiplicity`
   in `wc_errors`.  We don't want to report both.  Solution: suppress all
   `DE_Multiplicity` constraints if there are any unsolved wanted.

   This way, the delayed error is indeed only reported when the constraint is
   solved with a non-reflexivity coercion.

An alternative would be to have a kind of constraint which can only produce
trivial evidence. This would allow such checks to happen in the constraint
solver (#18756).  This would be similar to the existing setup for Concrete, see
Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete (PHASE 1 in particular).

-}

tcSubMult :: CtOrigin -> Mult -> Mult -> TcM ()
tcSubMult origin w_actual w_expected
  | Just (w1, w2) <- isMultMul w_actual =
  do { tcSubMult origin w1 w_expected
     ; tcSubMult origin w2 w_expected }
  -- Currently, we consider p*q and sup p q to be equal.  Therefore, p*q <= r is
  -- equivalent to p <= r and q <= r.  For other cases, we approximate p <= q by p
  -- ~ q.  This is not complete, but it's sound. See also Note [Overapproximating
  -- multiplicities] in Multiplicity.
tcSubMult origin w_actual w_expected =
  case submult w_actual w_expected of
    Submult -> return ()
    Unknown -> tcEqMult origin w_actual w_expected

tcEqMult :: CtOrigin -> Mult -> Mult -> TcM ()
tcEqMult origin w_actual w_expected = do
  {
  -- Note that here we do not call to `submult`, so we check
  -- for strict equality.
  ; coercion <- unifyTypeAndEmit TypeLevel origin w_actual w_expected
  -- See Note [Coercion errors in tcSubMult].
  ; ensureReflMultiplicityCo coercion origin }


{- *********************************************************************
*                                                                      *
                    Deep subsumption
*                                                                      *
********************************************************************* -}

{- Note [Deep subsumption]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The DeepSubsumption extension, documented here

    https://github.com/ghc-proposals/ghc-proposals/pull/511.

makes a best-efforts attempt implement deep subsumption as it was
prior to the Simplify Subsumption proposal:

    https://github.com/ghc-proposals/ghc-proposals/pull/287

The effects are in these main places:

1. In the subsumption check, tcSubType, we must do deep skolemisation:
   see the call to tcSkolemise Deep in tc_sub_type_deep

2. In tcPolyExpr we must do deep skolemisation:
   see the call to tcSkolemise in tcSkolemiseExpType

3. for expression type signatures (e :: ty), and functions with type
   signatures (e.g. f :: ty; f = e), we must deeply skolemise the type;
   see the call to tcDeeplySkolemise in tcSkolemiseScoped.

4. In GHC.Tc.Gen.App.tcApp we call tcSubTypeDS to match the result
   type. Without deep subsumption, unifyExpectedType would be sufficent.

In all these cases note that the deep skolemisation must be done /first/.
Consider (1)
     (forall a. Int -> a -> a)  <=  Int -> (forall b. b -> b)
We must skolemise the `forall b` before instantiating the `forall a`.
See also Note [Deep skolemisation].

Wrinkles:

(DS1) Note that we /always/ use shallow subsumption in the ambiguity check.
      See Note [Ambiguity check and deep subsumption].

(DS2) Deep subsumption requires deep instantiation too.
      See Note [The need for deep instantiation]

(DS3) The interaction between deep subsumption and required foralls
      (forall a -> ty) is a bit subtle.  See #24696 and
      Note [Deep subsumption and required foralls]

Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~
deeplySkolemise decomposes and skolemises a type, returning a type
with all its arrows visible (ie not buried under foralls)

Examples:

  deeplySkolemise (Int -> forall a. Ord a => blah)
    =  ( wp, [a], [d:Ord a], Int -> blah )
    where wp = \x:Int. /\a. \(d:Ord a). <hole> x

  deeplySkolemise  (forall a. Ord a => Maybe a -> forall b. Eq b => blah)
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], Maybe a -> blah )
    where wp = /\a.\(d1:Ord a).\(x:Maybe a)./\b.\(d2:Ord b). <hole> x

In general,
  if      deeplySkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds tvs, evs

ToDo: this eta-abstraction plays fast and loose with termination,
      because it can introduce extra lambdas.  Maybe add a `seq` to
      fix this

Note [Setting the argument context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider we are doing the ambiguity check for the (bogus)
  f :: (forall a b. C b => a -> a) -> Int

We'll call
   tcSubType ((forall a b. C b => a->a) -> Int )
             ((forall a b. C b => a->a) -> Int )

with a UserTypeCtxt of (FunSigCtxt "f").  Then we'll do the co/contra thing
on the argument type of the (->) -- and at that point we want to switch
to a UserTypeCtxt of GenSigCtxt.  Why?

* Error messages.  If we stick with FunSigCtxt we get errors like
     * Could not deduce: C b
       from the context: C b0
        bound by the type signature for:
            f :: forall a b. C b => a->a
  But of course f does not have that type signature!
  Example tests: T10508, T7220a, Simple14

* Implications. We may decide to build an implication for the whole
  ambiguity check, but we don't need one for each level within it,
  and TcUnify.alwaysBuildImplication checks the UserTypeCtxt.
  See Note [When to build an implication]

Note [Multiplicity in deep subsumption]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   t1 ->{mt} t2  <=   s1 ->{ms} s2

At the moment we /unify/ ms~mt, via tcEqMult.

Arguably we should use `tcSubMult`. But then if mt=m0 (a unification
variable) and ms=Many, `tcSubMult` is a no-op (since anything is a
sub-multiplicty of Many).  But then `m0` may never get unified with
anything.  It is then skolemised by the zonker; see GHC.HsToCore.Binds
Note [Free tyvars on rule LHS].  So we in RULE foldr/app in GHC.Base
we get this

 "foldr/app"     [1] forall ys m1 m2. foldr (\x{m1} \xs{m2}. (:) x xs) ys
                                       = \xs -> xs ++ ys

where we eta-expanded that (:).  But now foldr expects an argument
with ->{Many} and gets an argument with ->{m1} or ->{m2}, and Lint
complains.

The easiest solution was to use tcEqMult in tc_sub_type_deep, and
insist on equality. This is only in the DeepSubsumption code anyway.

Note [The need for deep instantiation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this, without Quick Look, but with Deep Subsumption:
   f :: ∀a b c. a b c -> Int
   g :: Bool -> ∀d. d -> d
Consider the application (f g).  We need to do the subsumption test

  (Bool -> ∀ d. d->d)   <=   (alpha beta gamma)

where alpha, beta, gamma are the unification variables that instantiate a,b,c,
respectively.  We must not drop down to unification, or we will reject the call.
Rather we must deeply instantiate the LHS to get

  (Bool -> delta -> delta)   <=   (alpha beta gamma)

and now we can unify to get

   alpha = (->)
   beta = Bool
   gamma = delta -> delta

Hence the call to `deeplyInstantiate` in `tc_sub_type_deep`.

See typecheck/should_compile/T11305 for an example of when this is important.

Note [Deep subsumption and required foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A required forall, (forall a -> ty) behaves like a "rho-type", one with no
top-level quantification.  In particular, it is neither implicitly instantiated nor
skolemised.  So

  rid1 :: forall a -> a -> a
  rid1 = id

  rid2 :: forall a -> a -> a
  rid2 a = id

Here `rid2` wll typecheck, but `rid1` will not, because we don't implicitly skolemise
the  type.

This "no implicit subsumption nor skolemisation" applies during subsumption.
For example
   (forall a. a->a)  <=  (forall a -> a -> a)  -- NOT!
does /not/ hold, because that would require implicitly skoleming the (forall a->).

Note also that, in Core, `eqType` distinguishes between
   (forall a. blah) and forall a -> blah)
See discussion on #22762 and these Notes in GHC.Core.TyCo.Compare
  * Note [ForAllTy and type equality]
  * Note [Comparing visibility]

So during deep subsumption we simply stop (and drop down to equality) when we encounter
a (forall a->).  This is a little odd:
* Deep subsumption looks inside invisible foralls (forall a. ty)
* Deep subsumption looks inside arrows (t1 -> t2)
* But it does not look inside required foralls (forall a -> ty)

There is discussion on #24696.  How is this implemented?

* In `tc_sub_type_deep`, the calls to `topInstantiate` and `deeplyInstantiate`
  instantiate only /invisible/ binders.
* In `tc_sub_type_ds`, the call to `tcSkolemise` skolemises only /invisible/
  binders.

Here is a slightly more powerful alternative
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 In the story above, if we have
    (forall a -> Eq a => a -> a)  <=  (forall a -> Ord a => a -> a)
we'll reject it, because both are rho-types but they aren't equal.  But in the
"drop to equality" stage we could instead see if both rho-types are headed with
(forall a ->) and if so strip that off and go back into deep subsumption.

This is a bit more powerful, but also a bit more complicated, so GHC
doesn't do it yet, awaiting credible user demand.  See #24696.
-}

data DeepSubsumptionFlag = Deep | Shallow

instance Outputable DeepSubsumptionFlag where
    ppr Deep    = text "Deep"
    ppr Shallow = text "Shallow"

getDeepSubsumptionFlag :: TcM DeepSubsumptionFlag
getDeepSubsumptionFlag = do { ds <- xoptM LangExt.DeepSubsumption
                            ; if ds then return Deep else return Shallow }

tc_sub_type_deep :: HasDebugCallStack
                 => (TcType -> TcType -> TcM TcCoercionN)  -- How to unify
                 -> CtOrigin       -- Used when instantiating
                 -> UserTypeCtxt   -- Used when skolemising
                 -> TcSigmaType    -- Actual; a sigma-type
                 -> TcRhoType      -- Expected; deeply skolemised
                 -> TcM HsWrapper

-- If wrap = tc_sub_type_deep t1 t2
--    => wrap :: t1 ~> t2
-- Here is where the work actually happens!
-- Precondition: ty_expected is deeply skolemised

tc_sub_type_deep unify inst_orig ctxt ty_actual ty_expected
  = assertPpr (isDeepRhoTy ty_expected) (ppr ty_expected) $
    do { traceTc "tc_sub_type_deep" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; go ty_actual ty_expected }
  where
    -- NB: 'go' is not recursive, except for doing coreView
    go ty_a ty_e | Just ty_a' <- coreView ty_a = go ty_a' ty_e
                 | Just ty_e' <- coreView ty_e = go ty_a  ty_e'

    go (TyVarTy tv_a) ty_e
      = do { lookup_res <- isFilledMetaTyVar_maybe tv_a
           ; case lookup_res of
               Just ty_a' ->
                 do { traceTc "tc_sub_type_deep following filled meta-tyvar:"
                        (ppr tv_a <+> text "-->" <+> ppr ty_a')
                    ; tc_sub_type_deep unify inst_orig ctxt ty_a' ty_e }
               Nothing -> just_unify ty_actual ty_expected }

    go ty_a@(FunTy { ft_af = af1, ft_mult = act_mult, ft_arg = act_arg, ft_res = act_res })
       ty_e@(FunTy { ft_af = af2, ft_mult = exp_mult, ft_arg = exp_arg, ft_res = exp_res })
      | isVisibleFunArg af1, isVisibleFunArg af2
      = if (isTauTy ty_a && isTauTy ty_e)       -- Short cut common case to avoid
        then just_unify ty_actual ty_expected   -- unnecessary eta expansion
        else
        -- This is where we do the co/contra thing, and generate a WpFun, which in turn
        -- causes eta-expansion, which we don't like; hence encouraging NoDeepSubsumption
        do { arg_wrap  <- tc_sub_type_ds Deep unify given_orig GenSigCtxt exp_arg act_arg
                          -- GenSigCtxt: See Note [Setting the argument context]
           ; res_wrap  <- tc_sub_type_deep unify inst_orig ctxt act_res exp_res
           ; tcEqMult inst_orig act_mult exp_mult
             -- See Note [Multiplicity in deep subsumption]
           ; return (mkWpFun arg_wrap res_wrap (Scaled exp_mult exp_arg) exp_res) }
                     -- arg_wrap :: exp_arg ~> act_arg
                     -- res_wrap :: act-res ~> exp_res
      where
        given_orig = GivenOrigin (SigSkol GenSigCtxt exp_arg [])

    go ty_a ty_e
      | let (tvs, theta, _) = tcSplitSigmaTy ty_a
      , not (null tvs && null theta)
      = do { (in_wrap, in_rho) <- topInstantiate inst_orig ty_a
           ; body_wrap <- tc_sub_type_deep unify inst_orig ctxt in_rho ty_e
           ; return (body_wrap <.> in_wrap) }

      | otherwise   -- Revert to unification
      = do { -- It's still possible that ty_actual has nested foralls. Instantiate
             -- these, as there's no way unification will succeed with them in.
             -- See Note [The need for deep instantiation]
             (inst_wrap, rho_a) <- deeplyInstantiate inst_orig ty_actual
           ; unify_wrap         <- just_unify rho_a ty_expected
           ; return (unify_wrap <.> inst_wrap) }

    just_unify ty_a ty_e = do { cow <- unify ty_a ty_e
                              ; return (mkWpCastN cow) }

-----------------------
deeplySkolemise :: SkolemInfo -> TcSigmaType
                -> TcM ( HsWrapper
                       , [(Name,TcInvisTVBinder)]     -- All skolemised variables
                       , [EvVar]                      -- All "given"s
                       , TcRhoType )
-- See Note [Deep skolemisation]
deeplySkolemise skol_info ty
  = go init_subst ty
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType ty))

    go subst ty
      | Just (arg_tys, bndrs, theta, ty') <- tcDeepSplitSigmaTy_maybe ty
      = do { let arg_tys' = substScaledTys subst arg_tys
           ; ids1             <- newSysLocalIds (fsLit "dk") arg_tys'
           ; (subst', bndrs1) <- tcInstSkolTyVarBndrsX skol_info subst bndrs
           ; ev_vars1         <- newEvVars (substTheta subst' theta)
           ; (wrap, tvs_prs2, ev_vars2, rho) <- go subst' ty'
           ; let tvs     = binderVars bndrs
                 tvs1    = binderVars bndrs1
                 tv_prs1 = map tyVarName tvs `zip` bndrs1
           ; return ( mkWpEta ids1 (mkWpTyLams tvs1
                                    <.> mkWpEvLams ev_vars1
                                    <.> wrap)
                    , tv_prs1  ++ tvs_prs2
                    , ev_vars1 ++ ev_vars2
                    , mkScaledFunTys arg_tys' rho ) }

      | otherwise
      = return (idHsWrapper, [], [], substTy subst ty)
        -- substTy is a quick no-op on an empty substitution

deeplyInstantiate :: CtOrigin -> TcType -> TcM (HsWrapper, Type)
deeplyInstantiate orig ty
  = go init_subst ty
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType ty))

    go subst ty
      | Just (arg_tys, bndrs, theta, rho) <- tcDeepSplitSigmaTy_maybe ty
      = do { let tvs = binderVars bndrs
           ; (subst', tvs') <- newMetaTyVarsX subst tvs
           ; let arg_tys' = substScaledTys   subst' arg_tys
                 theta'   = substTheta subst' theta
           ; ids1  <- newSysLocalIds (fsLit "di") arg_tys'
           ; wrap1 <- instCall orig (mkTyVarTys tvs') theta'
           ; (wrap2, rho2) <- go subst' rho
           ; return (mkWpEta ids1 (wrap2 <.> wrap1),
                     mkScaledFunTys arg_tys' rho2) }

      | otherwise
      = do { let ty' = substTy subst ty
           ; return (idHsWrapper, ty') }

tcDeepSplitSigmaTy_maybe
  :: TcSigmaType -> Maybe ([Scaled TcType], [TcInvisTVBinder], ThetaType, TcSigmaType)
-- Looks for a *non-trivial* quantified type, under zero or more function arrows
-- By "non-trivial" we mean either tyvars or constraints are non-empty
tcDeepSplitSigmaTy_maybe ty
  = go ty
  where
  go ty | Just (arg_ty, res_ty)           <- tcSplitFunTy_maybe ty
        , Just (arg_tys, tvs, theta, rho) <- go res_ty
        = Just (arg_ty:arg_tys, tvs, theta, rho)

        | (tvs, theta, rho) <- tcSplitSigmaTyBndrs ty
        , not (null tvs && null theta)
        = Just ([], tvs, theta, rho)

        | otherwise = Nothing

isDeepRhoTy :: TcType -> Bool
-- True if there are no foralls or (=>) at the top, or nested under
-- arrows to the right.  e.g
--    forall a. a                  False
--    Int -> forall a. a           False
--    (forall a. a) -> Int         True
-- Returns True iff tcDeepSplitSigmaTy_maybe returns Nothing
isDeepRhoTy ty
  | not (isRhoTy ty)                       = False  -- Foralls or (=>) at top
  | Just (_, res) <- tcSplitFunTy_maybe ty = isDeepRhoTy res
  | otherwise                              = True   -- No forall, (=>), or (->) at top

isRhoTyDS :: DeepSubsumptionFlag -> TcType -> Bool
isRhoTyDS ds_flag ty
  = case ds_flag of
      Shallow -> isRhoTy ty      -- isRhoTy: no top level forall or (=>)
      Deep    -> isDeepRhoTy ty  -- "deep" version: no nested forall or (=>)

{-
************************************************************************
*                                                                      *
                Boxy unification
*                                                                      *
************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.
-}

unifyExprType :: HsExpr GhcRn -> TcType -> TcType -> TcM TcCoercionN
unifyExprType rn_expr ty1 ty2
  = unifyType (Just (HsExprRnThing rn_expr)) ty1 ty2

unifyType :: Maybe TypedThing  -- ^ If present, the thing that has type ty1
          -> TcTauType -> TcTauType    -- ty1 (actual), ty2 (expected)
          -> TcM TcCoercionN           -- :: ty1 ~# ty2
-- Actual and expected types
-- Returns a coercion : ty1 ~ ty2
unifyType thing ty1 ty2
  = unifyTypeAndEmit TypeLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty1
                          , uo_expected = ty2
                          , uo_thing    = thing
                          , uo_visible  = True }

unifyInvisibleType :: TcTauType -> TcTauType    -- ty1 (actual), ty2 (expected)
                   -> TcM TcCoercionN           -- :: ty1 ~# ty2
-- Actual and expected types
-- Returns a coercion : ty1 ~ ty2
unifyInvisibleType ty1 ty2
  = unifyTypeAndEmit TypeLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty1
                          , uo_expected = ty2
                          , uo_thing    = Nothing
                          , uo_visible  = False }  -- This is the "invisible" bit

unifyTypeET :: TcTauType -> TcTauType -> TcM CoercionN
-- Like unifyType, but swap expected and actual in error messages
-- This is used when typechecking patterns
unifyTypeET ty1 ty2
  = unifyTypeAndEmit TypeLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty2   -- NB swapped
                          , uo_expected = ty1   -- NB swapped
                          , uo_thing    = Nothing
                          , uo_visible  = True }


unifyKind :: Maybe TypedThing -> TcKind -> TcKind -> TcM CoercionN
unifyKind mb_thing ty1 ty2
  = unifyTypeAndEmit KindLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty1
                          , uo_expected = ty2
                          , uo_thing    = mb_thing
                          , uo_visible  = True }

unifyTypeAndEmit :: TypeOrKind -> CtOrigin -> TcType -> TcType -> TcM CoercionN
-- Make a ref-cell, unify, emit the collected constraints
unifyTypeAndEmit t_or_k orig ty1 ty2
  = do { ref <- newTcRef emptyBag
       ; loc <- getCtLocM orig (Just t_or_k)
       ; let env = UE { u_loc = loc, u_role = Nominal
                      , u_rewriters = emptyRewriterSet  -- ToDo: check this
                      , u_defer = ref, u_unified = Nothing }

       -- The hard work happens here
       ; co <- uType env ty1 ty2

       ; cts <- readTcRef ref
       ; unless (null cts) (emitSimples cts)
       ; return co }

{-
%************************************************************************
%*                                                                      *
                 uType and friends
%*                                                                      *
%************************************************************************

Note [The eager unifier]
~~~~~~~~~~~~~~~~~~~~~~~~
The eager unifier, `uType`, is called by

  * The constraint generator (e.g. in GHC.Tc.Gen.Expr),
    via the wrappers `unifyType`, `unifyKind` etc

  * The constraint solver (e.g. in GHC.Tc.Solver.Equality),
    via `GHC.Tc.Solver.Monad.wrapUnifierTcS`.

`uType` runs in the TcM monad, but it carries a UnifyEnv that tells it
what to do when unifying a variable or deferring a constraint. Specifically,
  * it collects deferred constraints in `u_defer`, and
  * it records which unification variables it has unified in `u_unified`
Then it is up to the wrappers (one for the constraint generator, one for
the constraint solver) to deal with these collected sets.

Although `uType` runs in the TcM monad for convenience, really it could
operate just with the ability to
  * write to the accumulators of deferred constraints
    and unification variables in UnifyEnv.
  * read and update existing unification variables
  * zonk types befire unifying (`zonkTcType` in `uUnfilledVar`, and
    `zonkTyCoVarKind` in `uUnfilledVar1`
  * create fresh coercion holes (`newCoercionHole`)
  * emit tracing info for debugging
  * look at the ambient TcLevel: `getTcLevel`
A job for the future.
-}

data UnifyEnv
  = UE { u_role      :: Role
       , u_loc       :: CtLoc
       , u_rewriters :: RewriterSet

         -- Deferred constraints
       , u_defer     :: TcRef (Bag Ct)

         -- Which variables are unified;
         -- if Nothing, we don't care
       , u_unified :: Maybe (TcRef [TcTyVar])
    }

setUEnvRole :: UnifyEnv -> Role -> UnifyEnv
setUEnvRole uenv role = uenv { u_role = role }

updUEnvLoc :: UnifyEnv -> (CtLoc -> CtLoc) -> UnifyEnv
updUEnvLoc uenv@(UE { u_loc = loc }) upd = uenv { u_loc = upd loc }

mkKindEnv :: UnifyEnv -> TcType -> TcType -> UnifyEnv
-- Modify the UnifyEnv to be right for unifing
-- the kinds of these two types
mkKindEnv env@(UE { u_loc = ctloc }) ty1 ty2
  = env { u_role = Nominal, u_loc = mkKindEqLoc ty1 ty2 ctloc }

uType, uType_defer
  :: UnifyEnv
  -> TcType    -- ty1 is the *actual* type
  -> TcType    -- ty2 is the *expected* type
  -> TcM CoercionN

-- It is always safe to defer unification to the main constraint solver
-- See Note [Deferred unification]
uType_defer (UE { u_loc = loc, u_defer = ref
                , u_role = role, u_rewriters = rewriters })
            ty1 ty2  -- ty1 is "actual", ty2 is "expected"
  = do { let pred_ty = mkEqPredRole role ty1 ty2
       ; hole <- newCoercionHole loc pred_ty
       ; let ct = mkNonCanonical $
                  CtWanted { ctev_pred      = pred_ty
                           , ctev_dest      = HoleDest hole
                           , ctev_loc       = loc
                           , ctev_rewriters = rewriters }
             co = HoleCo hole
       ; updTcRef ref (`snocBag` ct)
         -- snocBag: see Note [Work-list ordering] in GHC.Tc.Solver.Equality

       -- Error trace only
       -- NB. do *not* call mkErrCtxt unless tracing is on,
       --     because it is hugely expensive (#5631)
       ; whenDOptM Opt_D_dump_tc_trace $
         do { ctxt     <- getErrCtxt
            ; err_ctxt <- mkErrCtxt emptyTidyEnv ctxt
            ; traceTc "utype_defer" $
                vcat ( ppr role
                     : debugPprType ty1
                     : debugPprType ty2
                     : map pprErrCtxtMsg err_ctxt )
            ; traceTc "utype_defer2" (ppr co) }

       ; return co }


--------------
uType env@(UE { u_role = role }) orig_ty1 orig_ty2
  | Phantom <- role
  = do { kind_co <- uType (mkKindEnv env orig_ty1 orig_ty2)
                          (typeKind orig_ty1) (typeKind orig_ty2)
       ; return (mkPhantomCo kind_co orig_ty1 orig_ty2) }

  | otherwise
  = do { tclvl <- getTcLevel
       ; traceTc "u_tys" $ vcat
              [ text "tclvl" <+> ppr tclvl
              , sep [ ppr orig_ty1, text "~" <> ppr role, ppr orig_ty2] ]
       ; co <- go orig_ty1 orig_ty2
       ; if isReflCo co
            then traceTc "u_tys yields no coercion" Outputable.empty
            else traceTc "u_tys yields coercion:" (ppr co)
       ; return co }
  where
    go :: TcType -> TcType -> TcM CoercionN
        -- The arguments to 'go' are always semantically identical
        -- to orig_ty{1,2} except for looking through type synonyms

     -- Unwrap casts before looking for variables. This way, we can easily
     -- recognize (t |> co) ~ (t |> co), which is nice. Previously, we
     -- didn't do it this way, and then the unification above was deferred.
    go (CastTy t1 co1) t2
      = do { co_tys <- uType env t1 t2
           ; return (mkCoherenceLeftCo role t1 co1 co_tys) }

    go t1 (CastTy t2 co2)
      = do { co_tys <- uType env t1 t2
           ; return (mkCoherenceRightCo role t2 co2 co_tys) }

        -- Variables; go for uUnfilledVar
        -- Note that we pass in *original* (before synonym expansion),
        -- so that type variables tend to get filled in with
        -- the most informative version of the type
    go (TyVarTy tv1) ty2
      = do { lookup_res <- isFilledMetaTyVar_maybe tv1
           ; case lookup_res of
               Just ty1 -> do { traceTc "found filled tyvar" (ppr tv1 <+> text ":->" <+> ppr ty1)
                              ; uType env ty1 orig_ty2 }
               Nothing -> uUnfilledVar env NotSwapped tv1 ty2 }

    go ty1 (TyVarTy tv2)
      = do { lookup_res <- isFilledMetaTyVar_maybe tv2
           ; case lookup_res of
               Just ty2 -> do { traceTc "found filled tyvar" (ppr tv2 <+> text ":->" <+> ppr ty2)
                              ; uType env orig_ty1 ty2 }
               Nothing -> uUnfilledVar env IsSwapped tv2 ty1 }

      -- See Note [Unifying type synonyms] in GHC.Core.Unify
    go ty1@(TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = return $ mkReflCo role ty1

        -- Now expand synonyms
        -- See Note [Expanding synonyms during unification]
        --
        -- Also NB that we recurse to 'go' so that we don't push a
        -- new item on the origin stack. As a result if we have
        --   type Foo = Int
        -- and we try to unify  Foo ~ Bool
        -- we'll end up saying "can't match Foo with Bool"
        -- rather than "can't match "Int with Bool".  See #4535.
    go ty1 ty2
      | Just ty1' <- coreView ty1 = go ty1' ty2
      | Just ty2' <- coreView ty2 = go ty1  ty2'

    -- Functions (t1 -> t2) just check the two parts
    go (FunTy { ft_af = af1, ft_mult = w1, ft_arg = arg1, ft_res = res1 })
       (FunTy { ft_af = af2, ft_mult = w2, ft_arg = arg2, ft_res = res2 })
      | isVisibleFunArg af1  -- Do not attempt (c => t); just defer
      , af1 == af2           -- Important!  See #21530
      = do { co_w <- uType (env { u_role = funRole role SelMult }) w1   w2
           ; co_l <- uType (env { u_role = funRole role SelArg })  arg1 arg2
           ; co_r <- uType (env { u_role = funRole role SelRes })  res1 res2
           ; return $ mkNakedFunCo role af1 co_w co_l co_r }

        -- Always defer if a type synonym family (type function)
        -- is involved.  (Data families behave rigidly.)
    go ty1@(TyConApp tc1 _) ty2
      | isTypeFamilyTyCon tc1 = defer ty1 ty2
    go ty1 ty2@(TyConApp tc2 _)
      | isTypeFamilyTyCon tc2 = defer ty1 ty2

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      -- See Note [Mismatched type lists and application decomposition]
      | tc1 == tc2, equalLength tys1 tys2
      , isInjectiveTyCon tc1 role -- don't look under newtypes at Rep equality
      = assertPpr (isGenerativeTyCon tc1 role) (ppr tc1) $
        do { traceTc "go-tycon" (ppr tc1 $$ ppr tys1 $$ ppr tys2 $$ ppr (take 10 (tyConRoleListX role tc1)))
           ; cos <- zipWith4M u_tc_arg (tyConVisibilities tc1)   -- Infinite
                                       (tyConRoleListX role tc1) -- Infinite
                                       tys1 tys2
           ; return $ mkTyConAppCo role tc1 cos }

    go (LitTy m) ty@(LitTy n)
      | m == n
      = return $ mkReflCo role ty

        -- See Note [Care with type applications]
        -- Do not decompose FunTy against App;
        -- it's often a type error, so leave it for the constraint solver
    go ty1@(AppTy s1 t1) ty2@(AppTy s2 t2)
      = go_app (isNextArgVisible s1) ty1 s1 t1 ty2 s2 t2

    go ty1@(AppTy s1 t1) ty2@(TyConApp tc2 ts2)
      | Just (ts2', t2') <- snocView ts2
      = assert (not (tyConMustBeSaturated tc2)) $
        go_app (isNextTyConArgVisible tc2 ts2')
               ty1 s1 t1 ty2 (TyConApp tc2 ts2') t2'

    go ty1@(TyConApp tc1 ts1) ty2@(AppTy s2 t2)
      | Just (ts1', t1') <- snocView ts1
      = assert (not (tyConMustBeSaturated tc1)) $
        go_app (isNextTyConArgVisible tc1 ts1')
               ty1 (TyConApp tc1 ts1') t1' ty2 s2 t2

    go ty1@(CoercionTy co1) ty2@(CoercionTy co2)
      = do { kco <- uType (mkKindEnv env ty1 ty2)
                          (coercionType co1) (coercionType co2)
           ; return $ mkProofIrrelCo role kco co1 co2 }

        -- Anything else fails
        -- E.g. unifying for-all types, which is relative unusual
    go ty1 ty2 = defer ty1 ty2

    ------------------
    defer ty1 ty2   -- See Note [Check for equality before deferring]
      | ty1 `tcEqType` ty2 = return (mkReflCo role ty1)
      | otherwise          = uType_defer env orig_ty1 orig_ty2


    ------------------
    u_tc_arg is_vis role ty1 ty2
      = do { traceTc "u_tc_arg" (ppr role $$ ppr ty1 $$ ppr ty2)
           ; uType env_arg ty1 ty2 }
      where
        env_arg = env { u_loc = adjustCtLoc is_vis False (u_loc env)
                      , u_role = role }

    ------------------
    -- For AppTy, decompose only nominal equalities
    -- See Note [Decomposing AppTy equalities] in GHC.Tc.Solver.Equality
    go_app vis ty1 s1 t1 ty2 s2 t2
      | Nominal <- role
      = -- Unify arguments t1/t2 before function s1/s2, because
        -- the former have smaller kinds, and hence simpler error messages
        -- c.f. GHC.Tc.Solver.Equality.can_eq_app
        -- Example: test T8603
        do { let env_arg = env { u_loc = adjustCtLoc vis False (u_loc env) }
           ; co_t <- uType env_arg t1 t2
           ; co_s <- uType env s1 s2
           ; return $ mkAppCo co_s co_t }
      | otherwise
      = defer ty1 ty2

{- Note [Check for equality before deferring]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Particularly in ambiguity checks we can get equalities like (ty ~ ty).
If ty involves a type function we may defer, which isn't very sensible.
An egregious example of this was in test T9872a, which has a type signature
       Proxy :: Proxy (Solutions Cubes)
Doing the ambiguity check on this signature generates the equality
   Solutions Cubes ~ Solutions Cubes
and currently the constraint solver normalises both sides at vast cost.
This little short-cut in 'defer' helps quite a bit.

Note [Care with type applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note: type applications need a bit of care!
They can match FunTy and TyConApp, so use splitAppTy_maybe
NB: we've already dealt with type variables and Notes,
so if one type is an App the other one jolly well better be too

Note [Mismatched type lists and application decomposition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we find two TyConApps, you might think that the argument lists
are guaranteed equal length.  But they aren't. Consider matching
        w (T x) ~ Foo (T x y)
We do match (w ~ Foo) first, but in some circumstances we simply create
a deferred constraint; and then go ahead and match (T x ~ T x y).
This came up in #3950.

So either
   (a) either we must check for identical argument kinds
       when decomposing applications,

   (b) or we must be prepared for ill-kinded unification sub-problems

Currently we adopt (b) since it seems more robust -- no need to maintain
a global invariant.

Note [Expanding synonyms during unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We expand synonyms during unification, but:
 * We expand *after* the variable case so that we tend to unify
   variables with un-expanded type synonym. This just makes it
   more likely that the inferred types will mention type synonyms
   understandable to the user

 * Similarly, we expand *after* the CastTy case, just in case the
   CastTy wraps a variable.

 * We expand *before* the TyConApp case.  For example, if we have
      type Phantom a = Int
   and are unifying
      Phantom Int ~ Phantom Char
   it is *wrong* to unify Int and Char.

 * The problem case immediately above can happen only with arguments
   to the tycon. So we check for nullary tycons *before* expanding.
   This is particularly helpful when checking (* ~ *), because * is
   now a type synonym.  See Note [Unifying type synonyms] in GHC.Core.Unify.

Note [Deferred unification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We may encounter a unification ty1 ~ ty2 that cannot be performed syntactically,
and yet its consistency is undetermined. Previously, there was no way to still
make it consistent. So a mismatch error was issued.

Now these unifications are deferred until constraint simplification, where type
family instances and given equations may (or may not) establish the consistency.
Deferred unifications are of the form
                F ... ~ ...
or              x ~ ...
where F is a type function and x is a type variable.
E.g.
        id :: x ~ y => x -> y
        id e = e

involves the unification x = y. It is deferred until we bring into account the
context x ~ y to establish that it holds.

If available, we defer original types (rather than those where closed type
synonyms have already been expanded via tcCoreView).  This is, as usual, to
improve error messages.

************************************************************************
*                                                                      *
                 uUnfilledVar and friends
*                                                                      *
************************************************************************

@uunfilledVar@ is called when at least one of the types being unified is a
variable.  It does {\em not} assume that the variable is a fixed point
of the substitution; rather, notice that @uVar@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.
-}

----------
uUnfilledVar, uUnfilledVar1
    :: UnifyEnv
    -> SwapFlag
    -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                      --    definitely not a /filled/ meta-tyvar
    -> TcTauType      -- Type 2
    -> TcM CoercionN
-- "Unfilled" means that the variable is definitely not a filled-in meta tyvar
--            It might be a skolem, or untouchable, or meta
uUnfilledVar env swapped tv1 ty2
  | Nominal <- u_role env
  = do { ty2 <- liftZonkM $ zonkTcType ty2
                  -- Zonk to expose things to the occurs check, and so
                  -- that if ty2 looks like a type variable then it
                  -- /is/ a type variable
       ; uUnfilledVar1 env swapped tv1 ty2 }

  | otherwise  -- See Note [Do not unify representational equalities]
               -- in GHC.Tc.Solver.Equality
  = unSwap swapped (uType_defer env) (mkTyVarTy tv1) ty2

uUnfilledVar1 env       -- Precondition: u_role==Nominal
              swapped
              tv1
              ty2       -- ty2 is zonked
  | Just tv2 <- getTyVar_maybe ty2
  = go tv2

  | otherwise
  = uUnfilledVar2 env swapped tv1 ty2

  where
    -- 'go' handles the case where both are
    -- tyvars so we might want to swap
    -- E.g. maybe tv2 is a meta-tyvar and tv1 is not
    go tv2 | tv1 == tv2  -- Same type variable => no-op
           = return (mkNomReflCo (mkTyVarTy tv1))

           | swapOverTyVars False tv1 tv2   -- Distinct type variables
               -- Swap meta tyvar to the left if poss
           = do { tv1 <- liftZonkM $ zonkTyCoVarKind tv1
                     -- We must zonk tv1's kind because that might
                     -- not have happened yet, and it's an invariant of
                     -- uUnfilledTyVar2 that ty2 is fully zonked
                     -- Omitting this caused #16902
                ; uUnfilledVar2 env (flipSwap swapped) tv2 (mkTyVarTy tv1) }

           | otherwise
           = uUnfilledVar2 env swapped tv1 ty2

----------
uUnfilledVar2 :: UnifyEnv       -- Precondition: u_role==Nominal
              -> SwapFlag
              -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                                --    definitely not a /filled/ meta-tyvar
              -> TcTauType      -- Type 2, zonked
              -> TcM CoercionN
uUnfilledVar2 env@(UE { u_defer = def_eq_ref }) swapped tv1 ty2
  = do { cur_lvl <- getTcLevel
           -- See Note [Unification preconditions], (UNTOUCHABLE) wrinkles
           -- Here we don't know about given equalities here; so we treat
           -- /any/ level outside this one as untouchable.  Hence cur_lvl.
       ; if not (touchabilityAndShapeTest cur_lvl tv1 ty2
                 && simpleUnifyCheck UC_OnTheFly tv1 ty2)
         then not_ok_so_defer cur_lvl
         else
    do { def_eqs <- readTcRef def_eq_ref  -- Capture current state of def_eqs

       -- Attempt to unify kinds
       -- When doing so, be careful to preserve orientation;
       --    see Note [Kind Equality Orientation] in GHC.Tc.Solver.Equality
       --    and wrinkle (W2) in Note [Fundeps with instances, and equality orientation]
       --        in GHC.Tc.Solver.Dict
       -- Failing to preserve orientation led to #25597.
       ; let kind_env = unSwap swapped (mkKindEnv env) ty1 ty2
       ; co_k <- unSwap swapped (uType kind_env) (tyVarKind tv1) (typeKind ty2)

       ; traceTc "uUnfilledVar2 ok" $
         vcat [ ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1)
              , ppr ty2 <+> dcolon <+> ppr (typeKind  ty2)
              , ppr (isReflCo co_k), ppr co_k ]

       ; if isReflCo co_k
           -- Only proceed if the kinds match
           -- NB: tv1 should still be unfilled, despite the kind unification
           --     because tv1 is not free in ty2' (or, hence, in its kind)
         then do { liftZonkM $ writeMetaTyVar tv1 ty2
                 ; case u_unified env of
                     Nothing -> return ()
                     Just uref -> updTcRef uref (tv1 :)
                 ; return (mkNomReflCo ty2) }  -- Unification is always Nominal

         else -- The kinds don't match yet, so defer instead.
              do { writeTcRef def_eq_ref def_eqs
                     -- Since we are discarding co_k, also discard any constraints
                     -- emitted by kind unification; they are just useless clutter.
                     -- Do this dicarding by simply restoring the previous state
                     -- of def_eqs; a bit imperative/yukky but works fine.
                 ; defer }
         }}
  where
    ty1 = mkTyVarTy tv1
    defer = unSwap swapped (uType_defer env) ty1 ty2

    not_ok_so_defer cur_lvl =
      do { traceTc "uUnfilledVar2 not ok" $
             vcat [ text "tv1:" <+> ppr tv1
                  , text "ty2:" <+> ppr ty2
                  , text "simple-unify-chk:" <+> ppr (simpleUnifyCheck UC_OnTheFly tv1 ty2)
                  , text "touchability:" <+> ppr (touchabilityAndShapeTest cur_lvl tv1 ty2)]
               -- Occurs check or an untouchable: just defer
               -- NB: occurs check isn't necessarily fatal:
               --     eg tv1 occurred in type family parameter
          ; defer }

swapOverTyVars :: Bool -> TcTyVar -> TcTyVar -> Bool
swapOverTyVars is_given tv1 tv2
  -- See Note [Unification variables on the left]
  | not is_given, pri1 == 0, pri2 > 0 = True
  | not is_given, pri2 == 0, pri1 > 0 = False

  -- Level comparison: see Note [TyVar/TyVar orientation]
  | lvl1 `strictlyDeeperThan` lvl2 = False
  | lvl2 `strictlyDeeperThan` lvl1 = True

  -- Priority: see Note [TyVar/TyVar orientation]
  | pri1 > pri2 = False
  | pri2 > pri1 = True

  -- Names: see Note [TyVar/TyVar orientation]
  | isSystemName tv2_name, not (isSystemName tv1_name) = True

  | otherwise = False

  where
    lvl1 = tcTyVarLevel tv1
    lvl2 = tcTyVarLevel tv2
    pri1 = lhsPriority tv1
    pri2 = lhsPriority tv2
    tv1_name = Var.varName tv1
    tv2_name = Var.varName tv2


lhsPriority :: TcTyVar -> Int
-- Higher => more important to be on the LHS
--        => more likely to be eliminated
-- Only used when the levels are identical
-- See Note [TyVar/TyVar orientation]
lhsPriority tv
  = assertPpr (isTyVar tv) (ppr tv) $
    case tcTyVarDetails tv of
      RuntimeUnk  -> 0
      SkolemTv {} -> 0
      MetaTv { mtv_info = info, mtv_tclvl = lvl }
        | QLInstVar <- lvl
        -> 5  -- Eliminate instantiation variables first
        | otherwise
        -> case info of
             CycleBreakerTv -> 0
             TyVarTv        -> 1
             ConcreteTv {}  -> 2
             TauTv          -> 3
             RuntimeUnkTv   -> 4

{- Note [Unification preconditions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Question: given a homogeneous equality (alpha ~# ty), when is it OK to
unify alpha := ty?

This note only applied to /homogeneous/ equalities, in which both
sides have the same kind.

There are five reasons not to unify:

1. (SKOL-ESC) Skolem-escape
   Consider the constraint
        forall[2] a[2]. alpha[1] ~ Maybe a[2]
   If we unify alpha := Maybe a, the skolem 'a' may escape its scope.
   The level alpha[1] says that alpha may be used outside this constraint,
   where 'a' is not in scope at all.  So we must not unify.

   Bottom line: when looking at a constraint alpha[n] := ty, do not unify
   if any free variable of 'ty' has level deeper (greater) than n

2. (UNTOUCHABLE) Untouchable unification variables
   Consider the constraint
        forall[2] a[2]. b[1] ~ Int => alpha[1] ~ Int
   There is no (SKOL-ESC) problem with unifying alpha := Int, but it might
   not be the principal solution. Perhaps the "right" solution is alpha := b.
   We simply can't tell.  See "OutsideIn(X): modular type inference with local
   assumptions", section 2.2.  We say that alpha[1] is "untouchable" inside
   this implication.

   Bottom line: at ambient level 'l', when looking at a constraint
   alpha[n] ~ ty, do not unify alpha := ty if there are any given equalities
   between levels 'n' and 'l'.

   Exactly what is a "given equality" for the purpose of (UNTOUCHABLE)?
   Answer: see Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet

3. (TYVAR-TV) Unifying TyVarTvs and CycleBreakerTvs
   This precondition looks at the MetaInfo of the unification variable:

   * TyVarTv: When considering alpha{tyv} ~ ty, if alpha{tyv} is a
     TyVarTv it can only unify with a type variable, not with a
     structured type.  So if 'ty' is a structured type, such as (Maybe x),
     don't unify.

   * CycleBreakerTv: never unified, except by restoreTyVarCycles.

4. (CONCRETE) A ConcreteTv can only unify with a concrete type,
    by definition.

    That is, if we have `rr[conc] ~ F Int`, we can't unify
    `rr` with `F Int`, so we hold off on unifying.
    Note however that the equality might get rewritten; for instance
    if we can rewrite `F Int` to a concrete type, say `FloatRep`,
    then we will have `rr[conc] ~ FloatRep` and we can unify `rr ~ FloatRep`.

    Note that we can still make progress on unification even if
    we can't fully solve an equality, e.g.

      alpha[conc] ~# TupleRep '[ beta[tau], F gamma[tau] ]

    we can fill beta[tau] := beta[conc]. This is why we call
    'makeTypeConcrete' in startSolvingByUnification.

5. (COERCION-HOLE) Confusing coercion holes
   Suppose our equality is
     (alpha :: k) ~ (Int |> {co})
   where co :: Type ~ k is an unsolved wanted. Note that this equality
   is homogeneous; both sides have kind k. We refrain from unifying here, because
   of the coercion hole in the RHS -- see Wrinkle (EIK2) in
   Note [Equalities with incompatible kinds] in GHC.Solver.Equality.

Needless to say, all there are wrinkles:

* (SKOL-ESC) Promotion.  Given alpha[n] ~ ty, what if beta[k] is free
  in 'ty', where beta is a unification variable, and k>n?  'beta'
  stands for a monotype, and since it is part of a level-n type
  (equal to alpha[n]), we must /promote/ beta to level n.  Just make
  up a fresh gamma[n], and unify beta[k] := gamma[n].

* (TYVAR-TV) Unification variables.  Suppose alpha[tyv,n] is a level-n
  TyVarTv (see Note [TyVarTv] in GHC.Tc.Types.TcMType)? Now
  consider alpha[tyv,n] ~ Bool.  We don't want to unify because that
  would break the TyVarTv invariant.

  What about alpha[tyv,n] ~ beta[tau,n], where beta is an ordinary
  TauTv?  Again, don't unify, because beta might later be unified
  with, say Bool.  (If levels permit, we reverse the orientation here;
  see Note [TyVar/TyVar orientation].)

* (UNTOUCHABLE) Untouchability.  When considering (alpha[n] ~ ty), how
  do we know whether there are any given equalities between level n
  and the ambient level?  We answer in two ways:

  * In the eager unifier, we only unify if l=n.  If not, alpha may be
    untouchable, and defer to the constraint solver.  This check is
    made in GHC.Tc.Utils.uUnifilledVar2, in the guard
    isTouchableMetaTyVar.

  * In the constraint solver, we track where Given equalities occur
    and use that to guard unification in
    GHC.Tc.Utils.Unify.touchabilityAndShapeTest. More details in
    Note [Tracking Given equalities] in GHC.Tc.Solver.InertSet

    Historical note: in the olden days (pre 2021) the constraint solver
    also used to unify only if l=n.  Equalities were "floated" out of the
    implication in a separate step, so that they would become touchable.
    But the float/don't-float question turned out to be very delicate,
    as you can see if you look at the long series of Notes associated with
    GHC.Tc.Solver.floatEqualities, around Nov 2020.  It's much easier
    to unify in-place, with no floating.

Note [TyVar/TyVar orientation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [Fundeps with instances, and equality orientation]
where the kind equality orientation is important

Given (a ~ b), should we orient the equality as (a~b) or (b~a)?
This is a surprisingly tricky question!

The question is answered by swapOverTyVars, which is used
  - in the eager unifier, in GHC.Tc.Utils.Unify.uUnfilledVar1
  - in the constraint solver, in GHC.Tc.Solver.Equality.canEqCanLHS2

First note: only swap if you have to!
   See Note [Avoid unnecessary swaps]

So we look for a positive reason to swap, using a three-step test:

* Level comparison. If 'a' has deeper level than 'b',
  put 'a' on the left.  See Note [Deeper level on the left]

* Priority.  If the levels are the same, look at what kind of
  type variable it is, using 'lhsPriority'.

  Generally speaking we always try to put a MetaTv on the left in
  preference to SkolemTv or RuntimeUnkTv, because the MetaTv may be
  touchable and can be unified.

  Tie-breaking rules for MetaTvs:
  - CycleBreakerTv: This is essentially a stand-in for another type;
       it's untouchable and should have the same priority as a skolem: 0.

  - TyVarTv: These can unify only with another tyvar, but we can't unify
       a TyVarTv with a TauTv, because then the TyVarTv could (transitively)
       get a non-tyvar type. So give these a low priority: 1.

  - ConcreteTv: These are like TauTv, except they can only unify with
    a concrete type. So we want to be able to write to them, but not quite
    as much as TauTvs: 2.

  - TauTv: This is the common case; we want these on the left so that they
       can be written to: 3.

  - RuntimeUnkTv: These aren't really meta-variables used in type inference,
       but just a convenience in the implementation of the GHCi debugger.
       Eagerly write to these: 4. See Note [RuntimeUnkTv] in
       GHC.Runtime.Heap.Inspect.

* Names. If the level and priority comparisons are all
  equal, try to eliminate a TyVar with a System Name in
  favour of ones with a Name derived from a user type signature

* Age.  At one point in the past we tried to break any remaining
  ties by eliminating the younger type variable, based on their
  Uniques.  See Note [Eliminate younger unification variables]
  (which also explains why we don't do this any more)

Note [Unification variables on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For wanteds, but not givens, swap (skolem ~ meta-tv) regardless of
level, so that the unification variable is on the left.

* We /don't/ want this for Givens because if we ave
    [G] a[2] ~ alpha[1]
    [W] Bool ~ a[2]
  we want to rewrite the wanted to Bool ~ alpha[1],
  so we can float the constraint and solve it.

* But for Wanteds putting the unification variable on
  the left means an easier job when floating, and when
  reporting errors -- just fewer cases to consider.

  In particular, we get better skolem-escape messages:
  see #18114

Note [Deeper level on the left]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The most important thing is that we want to put tyvars with
the deepest level on the left.  The reason to do so differs for
Wanteds and Givens, but either way, deepest wins!  Simple.

* Wanteds.  Putting the deepest variable on the left maximise the
  chances that it's a touchable meta-tyvar which can be solved.

* Givens. Suppose we have something like
     forall a[2]. b[1] ~ a[2] => beta[1] ~ a[2]

  If we orient the Given a[2] on the left, we'll rewrite the Wanted to
  (beta[1] ~ b[1]), and that can float out of the implication.
  Otherwise it can't.  By putting the deepest variable on the left
  we maximise our changes of eliminating skolem capture.

  See also GHC.Tc.Solver.InertSet Note [Let-bound skolems] for another reason
  to orient with the deepest skolem on the left.

  IMPORTANT NOTE: this test does a level-number comparison on
  skolems, so it's important that skolems have (accurate) level
  numbers.

See #15009 for an further analysis of why "deepest on the left"
is a good plan.

Note [Avoid unnecessary swaps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we swap without actually improving matters, we can get an infinite loop.
Consider
    work item:  a ~ b
   inert item:  b ~ c
We canonicalise the work-item to (a ~ c).  If we then swap it before
adding to the inert set, we'll add (c ~ a), and therefore kick out the
inert guy, so we get
   new work item:  b ~ c
   inert item:     c ~ a
And now the cycle just repeats

Historical Note [Eliminate younger unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a choice of unifying
     alpha := beta   or   beta := alpha
we try, if possible, to eliminate the "younger" one, as determined
by `ltUnique`.  Reason: the younger one is less likely to appear free in
an existing inert constraint, and hence we are less likely to be forced
into kicking out and rewriting inert constraints.

This is a performance optimisation only.  It turns out to fix
#14723 all by itself, but clearly not reliably so!

It's simple to implement (see nicer_to_update_tv2 in swapOverTyVars).
But, to my surprise, it didn't seem to make any significant difference
to the compiler's performance, so I didn't take it any further.  Still
it seemed too nice to discard altogether, so I'm leaving these
notes.  SLPJ Jan 18.

Note [Prevent unification with type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prevent unification with type families because of an uneasy compromise.
It's perfectly sound to unify with type families, and it even improves the
error messages in the testsuite. It also modestly improves performance, at
least in some cases. But it's disastrous for test case perf/compiler/T3064.
Here is the problem: Suppose we have (F ty) where we also have [G] F ty ~ a.
What do we do? Do we reduce F? Or do we use the given? Hard to know what's
best. GHC reduces. This is a disaster for T3064, where the type's size
spirals out of control during reduction. If we prevent
unification with type families, then the solver happens to use the equality
before expanding the type family.

It would be lovely in the future to revisit this problem and remove this
extra, unnecessary check. But we retain it for now as it seems to work
better in practice.

Revisited in Nov '20, along with removing flattening variables. Problem
is still present, and the solution is still the same.

Note [Non-TcTyVars in GHC.Tc.Utils.Unify]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Because the same code is now shared between unifying types and unifying
kinds, we sometimes will see proper TyVars floating around the unifier.
Example (from test case polykinds/PolyKinds12):

    type family Apply (f :: k1 -> k2) (x :: k1) :: k2
    type instance Apply g y = g y

When checking the instance declaration, we first *kind-check* the LHS
and RHS, discovering that the instance really should be

    type instance Apply k3 k4 (g :: k3 -> k4) (y :: k3) = g y

During this kind-checking, all the tyvars will be TcTyVars. Then, however,
as a second pass, we desugar the RHS (which is done in functions prefixed
with "tc" in GHC.Tc.TyCl"). By this time, all the kind-vars are proper
TyVars, not TcTyVars, get some kind unification must happen.

Thus, we always check if a TyVar is a TcTyVar before asking if it's a
meta-tyvar.

This used to not be necessary for type-checking (that is, before * :: *)
because expressions get desugared via an algorithm separate from
type-checking (with wrappers, etc.). Types get desugared very differently,
causing this wibble in behavior seen here.
-}

-- | Breaks apart a function kind into its pieces.
matchExpectedFunKind
  :: TypedThing     -- ^ type, only for errors
  -> Arity           -- ^ n: number of desired arrows
  -> TcKind          -- ^ fun_kind
  -> TcM Coercion    -- ^ co :: fun_kind ~ (arg1 -> ... -> argn -> res)

matchExpectedFunKind hs_ty n k = go n k
  where
    go 0 k = return (mkNomReflCo k)

    go n k | Just k' <- coreView k = go n k'

    go n k@(TyVarTy kvar)
      | isMetaTyVar kvar
      = do { maybe_kind <- readMetaTyVar kvar
           ; case maybe_kind of
                Indirect fun_kind -> go n fun_kind
                Flexi ->             defer n k }

    go n (FunTy { ft_af = af, ft_mult = w, ft_arg = arg, ft_res = res })
      | isVisibleFunArg af
      = do { co <- go (n-1) res
           ; return (mkNakedFunCo Nominal af (mkNomReflCo w) (mkNomReflCo arg) co) }

    go n other
     = defer n other

    defer n k
      = do { arg_kinds <- newMetaKindVars n
           ; res_kind  <- newMetaKindVar
           ; let new_fun = mkVisFunTysMany arg_kinds res_kind
                 origin  = TypeEqOrigin { uo_actual   = k
                                        , uo_expected = new_fun
                                        , uo_thing    = Just hs_ty
                                        , uo_visible  = True
                                        }
           ; unifyTypeAndEmit KindLevel origin k new_fun }

{- *********************************************************************
*                                                                      *
                 Checking alpha ~ ty
              for the on-the-fly unifier
*                                                                      *
********************************************************************* -}

data UnifyCheckCaller
  = UC_OnTheFly   -- Called from the on-the-fly unifier
  | UC_QuickLook  -- Called from Quick Look
  | UC_Solver     -- Called from constraint solver
  | UC_Defaulting -- Called when doing top-level defaulting

simpleUnifyCheck :: UnifyCheckCaller -> TcTyVar -> TcType -> Bool
-- simpleUnifyCheck does a fast check: True <=> unification is OK
-- If it says 'False' then unification might still be OK, but
-- it'll take more work to do -- use the full checkTypeEq
--
-- * Rejects if lhs_tv occurs in rhs_ty (occurs check)
-- * Rejects foralls unless
--      lhs_tv is RuntimeUnk (used by GHCi debugger)
--          or is a QL instantiation variable
-- * Rejects a non-concrete type if lhs_tv is concrete
-- * Rejects type families unless fam_ok=True
-- * Does a level-check for type variables, to avoid skolem escape
--
-- This function is pretty heavily used, so it's optimised not to allocate
simpleUnifyCheck caller lhs_tv rhs
  = go rhs
  where

    !(occ_in_ty, occ_in_co) = mkOccFolders (tyVarName lhs_tv)

    lhs_tv_lvl         = tcTyVarLevel lhs_tv
    lhs_tv_is_concrete = isConcreteTyVar lhs_tv

    forall_ok = case caller of
                   UC_QuickLook -> isQLInstTyVar lhs_tv
                   _            -> isRuntimeUnkTyVar lhs_tv

    -- This fam_ok thing relates to a very specific perf problem
    -- See Note [Prevent unification with type families]
    -- A couple of QuickLook regression tests rely on unifying with type
    --   families, so we let it through there (not very principled, but let's
    --   see if it bites us)
    fam_ok = case caller of
               UC_Solver     -> True
               UC_QuickLook  -> True
               UC_OnTheFly   -> False
               UC_Defaulting -> True

    go (TyVarTy tv)
      | lhs_tv == tv                                    = False
      | tcTyVarLevel tv `strictlyDeeperThan` lhs_tv_lvl = False
      | lhs_tv_is_concrete, not (isConcreteTyVar tv)    = False
      | occ_in_ty $! (tyVarKind tv)                     = False
      | otherwise                                       = True

    go (FunTy {ft_af = af, ft_mult = w, ft_arg = a, ft_res = r})
      | not forall_ok, isInvisibleFunArg af = False
      | otherwise                           = go w && go a && go r

    go (TyConApp tc tys)
      | lhs_tv_is_concrete, not (isConcreteTyCon tc) = False
      | not forall_ok, not (isTauTyCon tc)           = False
      | not fam_ok,    not (isFamFreeTyCon tc)       = False
      | otherwise                                    = all go tys

    go (ForAllTy (Bndr tv _) ty)
      | forall_ok = go (tyVarKind tv) && (tv == lhs_tv || go ty)
      | otherwise = False

    go (AppTy t1 t2)    = go t1 && go t2
    go (CastTy ty co)   = not (occ_in_co co) && go ty
    go (CoercionTy co)  = not (occ_in_co co)
    go (LitTy {})       = True


mkOccFolders :: Name -> (TcType -> Bool, TcCoercion -> Bool)
-- These functions return True
--   * if lhs_tv occurs (incl deeply, in the kind of variable)
--   * if there is a coercion hole
-- No expansion of type synonyms
mkOccFolders lhs_tv = (getAny . check_ty, getAny . check_co)
  where
    !(check_ty, _, check_co, _) = foldTyCo occ_folder emptyVarSet
    occ_folder = TyCoFolder { tcf_view  = noView  -- Don't expand synonyms
                            , tcf_tyvar = do_tcv, tcf_covar = do_tcv
                            , tcf_hole  = do_hole
                            , tcf_tycobinder = do_bndr }

    do_tcv is v = Any (not (v `elemVarSet` is) && tyVarName v == lhs_tv)
                  `mappend` check_ty (varType v)

    do_bndr is tcv _faf = extendVarSet is tcv
    do_hole _is _hole = DM.Any True  -- Reject coercion holes

{- *********************************************************************
*                                                                      *
                 Equality invariant checking
*                                                                      *
********************************************************************* -}


{-  Note [Checking for foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never want to unify
    alpha ~ (forall a. a->a) -> Int
So we look for foralls hidden inside the type, and it's convenient
to do that at the same time as the occurs check (which looks for
occurrences of alpha).

However, it's not just a question of looking for foralls /anywhere/!
Consider
   (alpha :: forall k. k->*)  ~  (beta :: forall k. k->*)
This is legal; e.g. dependent/should_compile/T11635.

We don't want to reject it because of the forall in beta's kind, but
(see Note [Occurrence checking: look inside kinds] in GHC.Core.Type)
we do need to look in beta's kind.  So we carry a flag saying if a
'forall' is OK, and switch the flag on when stepping inside a kind.

Why is it OK?  Why does it not count as impredicative polymorphism?
The reason foralls are bad is because we reply on "seeing" foralls
when doing implicit instantiation.  But the forall inside the kind is
fine.  We'll generate a kind equality constraint
  (forall k. k->*) ~ (forall k. k->*)
to check that the kinds of lhs and rhs are compatible.  If alpha's
kind had instead been
  (alpha :: kappa)
then this kind equality would rightly complain about unifying kappa
with (forall k. k->*)

Note [Forgetful synonyms in checkTyConApp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   type S a b = b   -- Forgets 'a'

   [W] alpha[2] ~ Maybe (S beta[4] gamma[2])

We don't want to promote beta to level 2; rather, we should
expand the synonym. (Currently, in checkTypeEqRhs promotion
is irrevocable, by side effect.)

To avoid this risk we eagerly expand forgetful synonyms.
This also means we won't get an occurs check in
   a ~ Maybe (S a b)

The annoyance is that we might expand the synonym unnecessarily,
something we generally try to avoid.  But for now, this seems
simple.

In a forgetful case like a ~ Maybe (S a b), `checkTyEqRhs` returns
a Reduction that looks
    Reduction { reductionCoercion    = Refl
              , reductionReducedType = Maybe b }
We must jolly well use that reductionReduced type, even though the
reductionCoercion is Refl.  See `canEqCanLHSFinish_no_unification`.
-}

data PuResult a b = PuFail CheckTyEqResult
                  | PuOK (Bag a) b
  deriving stock (Functor, Foldable, Traversable)

instance Applicative (PuResult a) where
  pure x = PuOK emptyBag x
  PuFail p1 <*> PuFail p2 = PuFail (p1 S.<> p2)
  PuFail p1 <*> PuOK {}   = PuFail p1
  PuOK {}   <*> PuFail p2 = PuFail p2
  PuOK c1 f <*> PuOK c2 x = PuOK (c1 `unionBags` c2) (f x)

instance (Outputable a, Outputable b) => Outputable (PuResult a b) where
  ppr (PuFail prob) = text "PuFail" <+> (ppr prob)
  ppr (PuOK cts x)  = text "PuOK" <> braces
                        (vcat [ text "redn:" <+> ppr x
                              , text "cts:" <+> ppr cts ])

okCheckRefl :: TcType -> PuResult a Reduction
okCheckRefl ty = PuOK emptyBag (mkReflRedn Nominal ty)

mapCheck :: Monad m
         => (x -> m (PuResult a Reduction))
         -> [x]
         -> m (PuResult a Reductions)
mapCheck f xs
  = do { (ress :: [PuResult a Reduction]) <- mapM f xs
       ; return (unzipRedns <$> sequenceA ress) }
         -- sequenceA :: [PuResult a Reduction] -> PuResult a [Reduction]
         -- unzipRedns :: [Reduction] -> Reductions
{-# INLINEABLE mapCheck #-}

-----------------------------
-- | Options describing how to deal with a type equality
-- in the eager unifier. See 'checkTyEqRhs'
data TyEqFlags m a
  -- | LHS is a type family application; we are not unifying.
  = TEFTyFam
    { tefTyFam_occursCheck :: CheckTyEqProblem
       -- ^ The 'CheckTyEqProblem' to report for occurs-check failures
       -- (soluble or insoluble)
    , tefTyFam_tyCon :: TyCon
    , tefTyFam_args  :: [Type]
    , tef_fam_app :: TyEqFamApp m a
        -- ^ How to deal with type family applications
    }
  -- | LHS is a 'TyVar'.
  | TEFTyVar
    -- NB: this constructor does not actually store a 'TyVar', in order to
    -- support being called from 'makeTypeConcrete' (which works as if we
    -- created a fresh 'ConcreteTv' metavariable; see (3) in Note [TyEqFlags])
    { tefTyVar_occursCheck    :: OccursCheck
        -- ^ Occurs check
    , tefTyVar_levelCheck     :: LevelCheck m
        -- ^ Level check
    , tefTyVar_concreteCheck  :: ConcreteCheck m
        -- ^ Concreteness check
    , tef_fam_app :: TyEqFamApp m a
        -- ^ How to deal with type family applications
    }

-- | What to do when encountering a type-family application while processing
-- a type equality in the pure unifier.
--
-- See Note [Family applications in canonical constraints]
data TyEqFamApp m a where
  -- | Always fail
  TEFA_Fail        :: TyEqFamApp m a
  -- | Just recurse
  TEFA_Recurse     :: TyEqFamApp m a
  -- | Recurse, but replace with cycle breaker if fails,
  -- using the specified 'FamAppBreaker'
  TEFA_Break       :: FamAppBreaker a -> TyEqFamApp TcM a

-- | A defunctionalisation of family application breaker functions.
--     Interpreter: `famAppBreaker`
data FamAppBreaker a where
  BreakGiven  :: FamAppBreaker (TcTyVar, TcType)
  BreakWanted :: CtEvidence -> TcTyVar -> FamAppBreaker Ct

data OccursCheck where
  -- | No occurs check.
  OC_None :: OccursCheck
  -- | Do an occurs check between the LHS tyvar and the RHS.
  OC_Check ::
    { occurs_tv_name :: Name
      -- ^ The 'Name' to perform an occurs-check on
    , occurs_problem :: CheckTyEqProblem
      -- ^ Which 'CheckTyEqProblem' to report for occurs-check failures
      -- (soluble or insoluble)
    } -> OccursCheck

-- | What level check to perform, in a call to the pure unifier?
-- A defunctionalisation of the possible level-check functions
--   Interpreter: `tyVarLevelCheck`
data LevelCheck m where
  -- | No level check.
  LC_None :: LevelCheck m
  -- | Do a level check between the LHS tyvar and the occurrence tyvar.
  --
  -- Fail if the level check fails.
  --
  -- True <=> lenient check, e.g. the levels have a chance of working out
  -- after promotion.
  LC_Check ::
    { lc_lvlc    :: TcLevel
    , lc_lenient :: Bool
    } -> LevelCheck m

  -- | Do a level check between the LHS tyvar and the occurrence tyvar.
  --
  -- If the level check fails, and the occurrence is a unification
  -- variable, promote it.
  --
  --   - False <=> don't promote under type families (the common case)
  --   -  True <=> promote even under type families
  --             (see Note [Defaulting equalities] in GHC.Tc.Solver)
  LC_Promote
    :: { lc_lvlp :: TcLevel
       , lc_deep :: Bool
       } -> LevelCheck TcM

data ConcreteCheck m where
  -- | No concreteness check.
  CC_None :: ConcreteCheck m
  -- | Simple check for concreteness, leniently returning 'OK'
  -- if concreteness could be achieved after promotion.
  CC_Check :: ConcreteCheck m
  -- | Proper concreteness check: promote non-concrete metavariables,
  -- failing if there are any problems.
  CC_Promote :: ConcreteTvOrigin -> ConcreteCheck TcM

{- Note [TyEqFlags]
~~~~~~~~~~~~~~~~~~~
When we call the eager unifier, e.g. through 'checkTyEqRhs', we specify what
kind of checks the unifier performs via the 'TyEqFlags' argument. In particular,
when the LHS type in a unification is a type variable, we might want to perform
different checks; this is achieved using the 'TEFTyVar' constructor to 'TyEqFlags':

  1. `notUnifying_TEFTask`
     LHS is a skolem tyvar, or an untouchable meta-tyvar.
     We are not unifying; we only want to perform occurs-checks.

      TEFTyVar
        { tefTyVar_occursCheck   = OC_Check ...
        , tefTyVar_levelCheck    = LC_None
        , tefTyVar_concreteCheck = CC_None
        , tef_fam_app            = TEFA_Recurse
        }

  2a. `unifyingLHSMetaTyVar_TEFTask`
     We are unifying; we want to perform an occurs check, a level check,
     and a concreteness check (when the meta-tyvar is a ConcreteTv).

      TEFTyVar
        { tefTyVar_occursCheck   = OC_Check ...
        , tefTyVar_levelCheck    = LC_Promote ...
        , tefTyVar_concreteCheck = CC_Promote or CC_None
            -- depending on whether or not the lhs tv is concrete
        , tef_fam_app            = TEFA_Recurse
        }

  2b. `defaulting_TEFTask`
     We are in the top-level defaulting code, considering unifying a
     touchable meta-tyvar with a type.

      TEFTyVar
        { tefTyVar_occursCheck   = OC_None
        , tefTyVar_levelCheck    = LC_None
        , tefTyVar_concreteCheck = CC_Promote or CC_None
            -- depending on whether or not the lhs tv is concrete
        , tef_fam_app            = TEFA_Recurse
        }

  3. `makeTypeConcrete`
     LHS is a fresh ConcreteTv meta-tyvar (see call to 'checkTyEqRhs' in
     `makeTypeConcrete`). We are unifying; we only want to perform
     a concreteness check.

      TEFTyVar
        { tefTyVar_occursCheck   = OC_None
        , tefTyVar_levelCheck    = LC_None
        , tefTyVar_concreteCheck = CC_Promote conc_orig
        , tef_fam_app            = TEFA_Recurse
        }

  4. `pureTyEqFlags_LHSMetaTyVar`
     We want to perform a non-monadic check, i.e. we want to know whether we are able
     to unify 'lhs_tv' with 'rhs_ty', but don't want to actually perform
     a side-effecting unification. This is used in 'mightEqualLater'.

      TEFTyVar
        { tefTyVar_occursCheck   = OC_Check ...
        , tefTyVar_levelCheck    = LC_Check ...
        , tefTyVar_concreteCheck = CC_Check or CC_None
            -- depending on whether or not the lhs tv is concrete
        , tef_fam_app            = TEFA_Recurse
        }

     Here 'LC_Check True' and 'ConcreteSimpleCheck' make 'checkTyEqRhs' perform
     conservative pure checks, without actually carrying out any promotion.
-}

-- | Create a "not unifying" 'TyEqFlags' from a 'CanEqLHS'.
--
-- See use-case (1) in Note [TyEqFlags].
notUnifying_TEFTask :: CheckTyEqProblem -> CanEqLHS -> TyEqFlags m a
notUnifying_TEFTask occ_prob = \case
  TyFamLHS tc tys ->
    TEFTyFam
      { tefTyFam_tyCon = tc
      , tefTyFam_args  = tys
      , tefTyFam_occursCheck = occ_prob
      , tef_fam_app = TEFA_Recurse
      }
  TyVarLHS tv ->
    TEFTyVar
      { tefTyVar_occursCheck   = OC_Check (tyVarName tv) occ_prob
      , tefTyVar_levelCheck    = LC_None
      , tefTyVar_concreteCheck = CC_None
      , tef_fam_app            = TEFA_Recurse
     }
    -- We need an occurs-check here, but no level check.
    -- See Note [Promotion and level-checking] wrinkle (W1)
    -- TEFA_Recurse: see Note [Don't cycle-break Wanteds when not unifying]

-- | Create "unifying" 'TyEqFlags' from a 'TyVarLHS'.
--
-- Invariant: the argument 'TcTyVar' is a 'MetaTv'.
unifyingLHSMetaTyVar_TEFTask :: CtEvidence -> TcTyVar -> TyEqFlags TcM Ct
unifyingLHSMetaTyVar_TEFTask ev lhs_tv =
  TEFTyVar
    { tefTyVar_occursCheck   = OC_Check (tyVarName lhs_tv) cteInsolubleOccurs
    , tefTyVar_levelCheck    = LC_Promote { lc_lvlp = tcTyVarLevel lhs_tv
                                          , lc_deep = False }
    , tefTyVar_concreteCheck = mkConcreteCheck lhs_tv
    , tef_fam_app            = mkTEFA_Break ev NomEq (BreakWanted ev lhs_tv)
    }

defaulting_TEFTask :: TcTyVar -> TyEqFlags TcM a
-- Used during top-level defauting in GHC.Tc.Solver.Default.defaultEquality
-- Invariant: the argument 'TcTyVar' is a 'MetaTv'.
defaulting_TEFTask lhs_tv =
  TEFTyVar
    { tefTyVar_occursCheck   = OC_Check (tyVarName lhs_tv) cteInsolubleOccurs
    , tefTyVar_levelCheck    = LC_Promote { lc_lvlp = tcTyVarLevel lhs_tv
                                          , lc_deep = True }
              -- LC_Promote: promote deeper unification variables (DE4)
              -- lc_deep =  True: ...including under type families (DE5)
    , tefTyVar_concreteCheck = mkConcreteCheck lhs_tv
    , tef_fam_app = TEFA_Recurse
    }

mkConcreteCheck :: TcTyVar -> ConcreteCheck TcM
mkConcreteCheck lhs_tv = case isConcreteTyVar_maybe lhs_tv of
                           Nothing        -> CC_None
                           Just conc_orig -> CC_Promote conc_orig

-- | 'TyEqFlags' for a pure call of 'checkTyEqRhs'.
--
-- A call to 'checkTyEqRhs' with these 'TyEqFlags' will perform an optimistic
-- check: it will return 'PuFail' if there is definitely a problem, but it
-- might return 'PuOK' if it isn't entirely sure.
pureTyEqFlags_LHSMetaTyVar :: TyVar -> TyEqFlags Identity ()
pureTyEqFlags_LHSMetaTyVar lhs_tv =
  TEFTyVar
    { tefTyVar_occursCheck   = OC_Check (tyVarName lhs_tv) cteInsolubleOccurs
    , tefTyVar_levelCheck    = LC_Check { lc_lvlc = tcTyVarLevel lhs_tv
                                        , lc_lenient = True }
    , tefTyVar_concreteCheck = case isConcreteTyVar_maybe lhs_tv of
                                  Nothing -> CC_None
                                  Just {} -> CC_Check   -- No promotion
    , tef_fam_app = TEFA_Recurse }


mkTEFA_Break :: CtEvidence -> EqRel -> FamAppBreaker a -> TyEqFamApp TcM a
mkTEFA_Break ev eq_rel breaker
  | NomEq <- eq_rel
  , not cycle_breaker_origin
  = TEFA_Break breaker
  | otherwise
  = TEFA_Recurse
  where
    -- cycle_breaker_origin: see Detail (7) of Note [Type equality cycles]
    -- in GHC.Tc.Solver.Equality
    cycle_breaker_origin = case ctLocOrigin (ctEvLoc ev) of
                              CycleBreakerOrigin {} -> True
                              _                     -> False

-- | Do we want to perform a concreteness check in 'checkTyEqRhs'?
tefConcrete :: TyEqFlags m a -> Bool
tefConcrete (TEFTyFam {}) = False
tefConcrete (TEFTyVar { tefTyVar_concreteCheck = conc }) =
  case conc of
    CC_None -> False
    CC_Check -> True
    CC_Promote {} -> True

-- | Given a family-application ty, return a @'Reduction' :: ty ~ cvb@
-- where @cbv@ is a fresh loop-breaker tyvar (for Given), or
-- just a fresh 'TauTv' (for Wanted)
famAppBreaker :: FamAppBreaker a -> TcType -> TcM (PuResult a Reduction)
famAppBreaker BreakGiven fam_app
   = do { new_tv <- TcM.newCycleBreakerTyVar (typeKind fam_app)
        ; return (PuOK (unitBag (new_tv, fam_app))
                       (mkReflRedn Nominal (mkTyVarTy new_tv))) }
                 -- Why reflexive? See Detail (4) of the Note
famAppBreaker (BreakWanted ev lhs_tv) fam_app
  -- Occurs check or skolem escape; so flatten
  = do { reason <- checkPromoteFreeVars cteInsolubleOccurs
                     (tyVarName lhs_tv) lhs_tv_lvl
                     (tyCoVarsOfType fam_app_kind)
       ; if not (cterHasNoProblem reason)  -- Failed to promote free vars
         then return $ PuFail reason
         else
    do { new_tv_ty <-
          case lhs_tv_info of
            ConcreteTv conc_info ->
              -- Make a concrete tyvar if lhs_tv is concrete
              -- e.g.  alpha[2,conc] ~ Maybe (F beta[4])
              --       We want to flatten to
              --       alpha[2,conc] ~ Maybe gamma[2,conc]
              --       gamma[2,conc] ~ F beta[4]
              TcM.newConcreteTyVarTyAtLevel conc_info lhs_tv_lvl fam_app_kind
            _ -> TcM.newMetaTyVarTyAtLevel lhs_tv_lvl fam_app_kind

       ; let pty = mkNomEqPred fam_app new_tv_ty
       ; hole <- TcM.newVanillaCoercionHole pty
       ; let new_ev = CtWanted { ctev_pred      = pty
                               , ctev_dest      = HoleDest hole
                               , ctev_loc       = cb_loc
                               , ctev_rewriters = ctEvRewriters ev }
       ; return (PuOK (singleCt (mkNonCanonical new_ev))
                      (mkReduction (HoleCo hole) new_tv_ty)) } }
  where
    (lhs_tv_info, lhs_tv_lvl) =
      case tcTyVarDetails lhs_tv of
         MetaTv { mtv_info = info, mtv_tclvl = lvl } -> (info,lvl)
         -- lhs_tv should be a meta-tyvar
         _ -> pprPanic "checkTouchableTyVarEq" (ppr lhs_tv)
    fam_app_kind = typeKind fam_app
    -- See Detail (7) of the Note
    cb_loc = updateCtLocOrigin (ctEvLoc ev) CycleBreakerOrigin


instance Outputable (TyEqFlags m a) where
  ppr = \case
    TEFTyFam occ tc tys fam_app ->
      text "TEFTyFam" <> braces (
        hcat [ ppr occ
             , ppr (mkTyConApp tc tys)
             , ppr fam_app ] )
    TEFTyVar occ lc conc fam_app ->
      text "TEFTyVar" <> braces (hcat (punctuate comma fields))
      where
        fields = [ text "OccursCheck:" <+> ppr occ ]
                   ++
                 [ text "LevelCheck:" <+> ppr lc ]
                   ++
                 [ text "ConcreteCheck:" <+> ppr conc ]
                   ++
                 [ text "FamApp:" <+> ppr fam_app ]

instance Outputable (TyEqFamApp m a) where
  ppr TEFA_Fail            = text "TEFA_Fail"
  ppr TEFA_Recurse         = text "TEFA_Recurse"
  ppr (TEFA_Break breaker) = text "TEFA_BreakGiven" <+> ppr breaker

instance Outputable (FamAppBreaker a) where
  ppr BreakGiven          = text "BreakGiven"
  ppr (BreakWanted ev tv) = parens $ text "BreakWanted" <+> ppr ev <+> ppr tv

instance Outputable OccursCheck where
  ppr OC_None = text "OC_None"
  ppr (OC_Check { occurs_tv_name = nm }) = text "OC_Check" <+> ppr nm
instance Outputable (ConcreteCheck m) where
  ppr CC_None = text "CC_None"
  ppr CC_Check = text "CC_Check"
  ppr (CC_Promote {}) = text "CC_Promote"
instance Outputable (LevelCheck m) where
  ppr (LC_None) = text "LC_None"
  ppr (LC_Check lvl lenient) = text "LC_Check" <+> ppr lvl <+> ppWhen lenient (text "(lenient)")
  ppr (LC_Promote lvl deep)  = text "LC_Promote" <+> ppr lvl <+> ppWhen deep (text "(deep)")

-- | Adjust the 'TyEqFlags' when going under a type family:
--
--  1. Only the outer family application gets the loop-breaker treatment
--  2. Weaken level checks for tyvar promotion. For example, in @[W] alpha[2] ~ Maybe (F beta[3])@,
--     do not promote @beta[3]@, instead promote @(F beta[3])@.
--  3. Occurs checks become potentially soluble (after additional type family
--     reductions).
famAppArgFlags :: TyEqFlags m a -> TyEqFlags m a
famAppArgFlags flags = case flags of
  TEFTyFam {} ->
    flags
      { tef_fam_app = TEFA_Recurse -- (1)
      , tefTyFam_occursCheck = cteSolubleOccurs -- (3)
      }
  TEFTyVar { tefTyVar_occursCheck = occ, tefTyVar_levelCheck = mb_lc } ->
    flags
      { tef_fam_app = TEFA_Recurse -- (1)
      , tefTyVar_levelCheck = zap_lc mb_lc -- (2)
      , tefTyVar_occursCheck = soluble_occ occ -- (3)
      }
  where
    soluble_occ = \case
      OC_Check tv _ -> OC_Check tv cteSolubleOccurs
      OC_None -> OC_None
    zap_lc = \case
      LC_Promote { lc_lvlp = lvl, lc_deep = deeply }
        | not deeply
        -> LC_Check { lc_lvlc = lvl, lc_lenient = False }
      lc -> lc

checkTyEqRhs :: forall m a
             .  Monad m
             => TyEqFlags m a
             -> TcType           -- Already zonked
             -> m (PuResult a Reduction)
checkTyEqRhs flags ty
  = case ty of
      LitTy {}        -> return $ okCheckRefl ty
      TyConApp tc tys -> checkTyConApp flags ty tc tys
      TyVarTy tv      -> checkTyVar flags tv
        -- Don't worry about foralls inside the kind; see Note [Checking for foralls]
        -- Nor can we expand synonyms; see Note [Occurrence checking: look inside kinds]
        --                             in GHC.Core.FVs

      FunTy {ft_af = af, ft_mult = w, ft_arg = a, ft_res = r}
       | isInvisibleFunArg af  -- e.g.  Num a => blah
       -> return $ PuFail impredicativeProblem -- Not allowed (TyEq:F)
       | otherwise
       -> do { w_res <- checkTyEqRhs flags w
             ; a_res <- checkTyEqRhs flags a
             ; r_res <- checkTyEqRhs flags r
             ; return (mkFunRedn Nominal af <$> w_res <*> a_res <*> r_res) }

      AppTy fun arg -> do { fun_res <- checkTyEqRhs flags fun
                          ; arg_res <- checkTyEqRhs flags arg
                          ; return (mkAppRedn <$> fun_res <*> arg_res) }

      CastTy ty co  -> do { ty_res <- checkTyEqRhs flags ty
                          ; co_res <- checkCo flags co
                          ; return (mkCastRedn1 Nominal ty <$> co_res <*> ty_res) }

      CoercionTy co -> do { co_res <- checkCo flags co
                          ; return (mkReflCoRedn Nominal <$> co_res) }

      ForAllTy {}   -> return $ PuFail impredicativeProblem -- Not allowed (TyEq:F)
{-# INLINEABLE checkTyEqRhs #-}

-------------------
checkCo :: Monad m => TyEqFlags m a -> Coercion -> m (PuResult a Coercion)
-- See Note [checkCo]
checkCo flags co =
  case flags of
    TEFTyFam {} ->
      -- NB: 'TEFTyFam' case means we are not unifying.
      return (pure co)
    TEFTyVar
      { tefTyVar_concreteCheck = conc
      , tefTyVar_levelCheck    = lc
      , tefTyVar_occursCheck   = occ
      }
        -- Coercions cannot appear in concrete types.
        --
        -- See Note [Concrete types] in GHC.Tc.Utils.Concrete.
        | case conc of { CC_None -> False; _ -> True }
        -> return $ PuFail (cteProblem cteConcrete)

        -- Check for coercion holes, if unifying.
        -- See (COERCION-HOLE) in Note [Unification preconditions]
        | case lc of { LC_None {} -> False; _ -> True } -- equivalent to "we are unifying"; see Note [TyEqFlags]
        , hasCoercionHoleCo co
        -> return $ PuFail (cteProblem cteCoercionHole)

        -- Occurs check (can promote)
        | OC_Check lhs_tv occ_prob <- occ
        , LC_Promote { lc_lvlp = lhs_tv_lvl } <- lc
        -> do { reason <- checkPromoteFreeVars occ_prob lhs_tv lhs_tv_lvl (tyCoVarsOfCo co)
              ; return $
                if cterHasNoProblem reason
                then pure co
                else PuFail reason }

        -- Occurs check (no promotion)
        | OC_Check lhs_tv occ_prob <- occ
        , nameUnique lhs_tv `elemVarSetByKey` tyCoVarsOfCo co
        -> return $ PuFail (cteProblem occ_prob)

        | otherwise
        -> return (pure co)

{- Note [checkCo]
~~~~~~~~~~~~~~~~~
We don't often care about the contents of coercions, so checking
coercions before making an equality constraint may be surprising.
But there are several cases we need to be wary of:

(1) When we're unifying a variable, we must make sure that the variable
    appears nowhere on the RHS -- even in a coercion. Otherwise, we'll
    create a loop.

(2) We must still make sure that no variable in a coercion is at too
    high a level. But, when unifying, we can promote any variables we encounter.

(3) We do not unify variables with a type with a free coercion hole.
    See (COERCION-HOLE) in Note [Unification preconditions].


Note [Promotion and level-checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"Promotion" happens when we have this:

  [W] w1: alpha[2] ~ Maybe beta[4]

Here we must NOT unify alpha := Maybe beta, because beta may turn out
to stand for a type involving some inner skolem.  Yikes!
Skolem-escape.  So instead we /promote/ beta, like this:

  beta[4] := beta'[2]
  [W] w1: alpha[2] ~ Maybe beta'[2]

Now we can unify alpha := Maybe beta', which might unlock other
constraints.  But if some other constraint wants to unify beta with a
nested skolem, it'll get stuck with a skolem-escape error.

Now consider `w2` where a type family is involved (#22194):

  [W] w2: alpha[2] ~ Maybe (F gamma beta[4])

In `w2`, it may or may not be the case that `beta` is level 2; suppose
we later discover gamma := Int, and type instance F Int _ = Int.
So, instead, we promote the entire funcion call:

  [W] w2': alpha[2] ~ Maybe gamma[2]
  [W] w3:  gamma[2] ~ F gamma beta[4]

Now we can unify alpha := Maybe gamma, which is a Good Thng.

Wrinkle (W1)

There is an important wrinkle: /all this only applies when unifying/.
For example, suppose we have
 [G] a[2] ~ Maybe b[4]
where 'a' is a skolem.  This Given might arise from a GADT match, and
we can absolutely use it to rewrite locally. In fact we must do so:
that is how we exploit local knowledge about the outer skolem a[2].
This applies equally for a Wanted [W] a[2] ~ Maybe b[4]. Using it for
local rewriting is fine. (It's not clear to me that it is /useful/,
but it's fine anyway.)

So we only do the level-check in checkTyVar when /unifying/ not for
skolems (or untouchable unification variables).

Note [Family applications in canonical constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constraint with a type family application in the RHS needs special care.

* First, occurs checks.  If we have
     [G] a ~ Maybe (F (Maybe a))
     [W] alpha ~ Maybe (F (Maybe alpha))
  it looks as if we have an occurs check.  But go read
  Note [Type equality cycles] in GHC.Tc.Solver.Equality

  The same considerations apply when the LHS is a type family:
     [G] G a ~ Maybe (F (Maybe (G a)))
     [W] G alpha ~ Maybe (F (Maybe (G alpha)))

* Second, promotion. If we have (#22194)
     [W] alpha[2] ~ Maybe (F beta[4])
  it is wrong to promote beta.  Instead we want to split to
     [W] alpha[2] ~ Maybe gamma[2]
     [W] gamma[2] ~ F beta[4]
  See Note [Promotion and level-checking] above.

* Third, concrete type variables.  If we have
     [W] alpha[conc] ~ Maybe (F tys)
  we want to add an extra variable thus:
     [W] alpha[conc] ~ Maybe gamma[conc]
     [W] gamma[conc] ~ F tys
  Now we can unify alpha, and that might unlock something else.

In all these cases we want to create a fresh type variable, and
emit a new equality connecting it to the type family application.

The `tef_fam_app` field of `TypeEqFlags` says what to do at a type
family application in the RHS of the constraint.  `TEFA_Fail` and
`TEFA_Recurse` are straightforward.  `TEFA_Break` is the clever
one. As you can see in `checkFamApp`, it
  * Checks the arguments, but using `famAppArgFlags` to record that
    we are now "under" a type-family application. It `tef_fam_app` to
    `TEFA_Recurse`.
  * If any of the arguments fail (level-check error, occurs check)
    use the `FamAppBreaker` to create the extra binding.

Note that this always cycle-breaks the /outermost/ family application.
If we have  [W] alpha ~ Maybe (F (G alpha))
* We'll use checkFamApp on `(F (G alpha))`
* It will recurse into `(G alpha)` with TEFA_Recurse, but not cycle-break it
* The occurs check will fire when we hit `alpha`
* `checkFamApp` on `(F (G alpha))` will see the failure and invoke
  the `FamAppBreaker`.
-}

-------------------
checkTyConApp :: Monad m
              => TyEqFlags m a
              -> TcType -> TyCon -> [TcType]
              -> m (PuResult a Reduction)
checkTyConApp flags tc_app tc tys
  | isTypeFamilyTyCon tc
  , let arity = tyConArity tc
  = if tys `lengthIs` arity
    then checkFamApp flags tc_app tc tys  -- Common case
    else do { let (fun_args, extra_args) = splitAt (tyConArity tc) tys
                  fun_app                = mkTyConApp tc fun_args
            ; fun_res   <- checkFamApp flags fun_app tc fun_args
            ; extra_res <- mapCheck (checkTyEqRhs flags) extra_args
            ; return (mkAppRedns <$> fun_res <*> extra_res) }

  | Just ty' <- rewriterView tc_app
       -- e.g. S a  where  type S a = F [a]
       --             or   type S a = Int
       -- See Note [Forgetful synonyms in checkTyConApp]
  = checkTyEqRhs flags ty'

  | not (isTauTyCon tc)
  = return $ PuFail impredicativeProblem

  | tefConcrete flags
  , not (isConcreteTyCon tc)
  = return $ PuFail (cteProblem cteConcrete)

  | otherwise  -- Recurse on arguments
  = recurseIntoTyConApp flags tc tys

-------------------
recurseIntoTyConApp :: Monad m
                    => TyEqFlags m a
                    -> TyCon -> [TcType]
                    -> m (PuResult a Reduction)
recurseIntoTyConApp flags tc tys
  = do { tys_res <- mapCheck (checkTyEqRhs flags) tys
       ; return (mkTyConAppRedn Nominal tc <$> tys_res) }

recurseIntoFamTyConApp :: Monad m
                       => TyEqFlags m a
                       -> TyCon -> [TcType]
                       -> m (PuResult a Reduction)
recurseIntoFamTyConApp flags tc tys
  = recurseIntoTyConApp (famAppArgFlags flags) tc tys
    -- famAppArgFlags: adjust flags when going under fam app

-------------------
checkFamApp :: forall m a.
               Monad m
            => TyEqFlags m a
            -> TcType -> TyCon -> [TcType]  -- Saturated family application
            -> m (PuResult a Reduction)
-- See Note [Family applications in canonical constraints]
checkFamApp flags fam_app tc tys
  = case flags of
      TEFTyVar { tef_fam_app = fam_app_flag, tefTyVar_concreteCheck = conc }
        -> case conc of
             CC_None -> case fam_app_flag of
                          TEFA_Fail      -> fail_with cteTypeFamily
                          TEFA_Recurse   -> recurseIntoFamTyConApp flags tc tys
                          TEFA_Break brk -> famAppBreaker brk fam_app
             _       -> fail_with cteConcrete

      TEFTyFam { tefTyFam_tyCon = lhs_tc, tefTyFam_args = lhs_tys
               , tefTyFam_occursCheck = occ_prob, tef_fam_app = fam_app_flag }
        | tcEqTyConApps lhs_tc lhs_tys tc tys
          -- This occurrence is the same as the LHS
        -> case fam_app_flag of
              TEFA_Fail      -> fail_with cteTypeFamily
              TEFA_Recurse   -> fail_with occ_prob
              TEFA_Break brk -> famAppBreaker brk fam_app

        | otherwise
        -> case fam_app_flag of
              TEFA_Fail      -> fail_with cteTypeFamily
              TEFA_Recurse   -> recurseIntoFamTyConApp flags tc tys
              TEFA_Break brk -> do { rec_res <- recurseIntoFamTyConApp flags tc tys
                                   ; case rec_res of
                                       PuOK {}   -> return rec_res
                                       PuFail {} -> famAppBreaker brk fam_app }
                  -- For TEFA_Break, try recursion; and break if there is a problem
                  -- e.g.  alpha[2] ~ Maybe (F beta[2])    No problem: just unify
                  --       alpha[2] ~ Maybe (F beta[4])    Level-check problem: break
                  -- NB: in the latter case, don't promote beta[4]; hence arg_flags!
  where
    fail_with prob = return $ PuFail (cteProblem prob)

-------------------

-- | The result of a single check in 'checkTyVar', such as a concreteness check
-- or a level check.
data TyVarCheckResult m where
  -- | Check succeded; nothing else to do.
  TyVarCheck_Success :: TyVarCheckResult m
  -- | Check succeeded, but requires an additional promotion.
  --
  -- Invariant: at least one of the fields is not 'Nothing'.
  TyVarCheck_Promote
    :: Maybe TcLevel
         -- ^ @Just lvl@ <=> 'TyVar' needs to be promoted to @lvl@
    -> Maybe ConcreteTvOrigin
         -- ^ @Just conc_orig@ <=> 'TyVar' needs to be make concrete
    -> TyVarCheckResult TcM

  -- | Check failed with some 'CheckTyEqProblem's.
  --
  -- Invariant: the 'CheckTyEqResult' is not 'cteOK'.
  TyVarCheck_Error
    :: CheckTyEqResult -> TyVarCheckResult m

instance Semigroup (TyVarCheckResult m) where
  TyVarCheck_Success <> r = r
  r <> TyVarCheck_Success = r
  TyVarCheck_Error e1 <> TyVarCheck_Error e2 =
    TyVarCheck_Error (e1 S.<> e2)
  e@(TyVarCheck_Error {}) <> _ = e
  _ <> e@(TyVarCheck_Error {}) = e
  TyVarCheck_Promote l1 c1 <> TyVarCheck_Promote l2 c2 =
    TyVarCheck_Promote
      (combineMaybe minTcLevel l1 l2)
      (combineMaybe const c1 c2) -- pick one 'ConcreteTvOrigin' arbitrarily

combineMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
combineMaybe _ Nothing r = r
combineMaybe _ r Nothing = r
combineMaybe f (Just a) (Just b) = Just (f a b)
instance Monoid (TyVarCheckResult m) where
  mempty = TyVarCheck_Success

checkTyVar :: forall m a. Monad m => TyEqFlags m a -> TcTyVar -> m (PuResult a Reduction)
checkTyVar flags occ_tv
  = case flags of
      TEFTyFam {}
        -> success   -- Nothing to do if the LHS is a type-family
      TEFTyVar { tefTyVar_occursCheck = occ, tefTyVar_levelCheck = lc
               , tefTyVar_concreteCheck = conc }
        -> do { already_promoted <- promotionDone occ_tv lc conc

              ; if already_promoted
                then success
                  -- Already promoted; job done
                  -- Example alpha[2] ~ Maybe (beta[4], beta[4])
                  -- We promote the first occurrence, and then encounter it
                  -- a second time; we don't want to re-promote it!
                  -- Remember, the entire process started with a fully zonked type
                  -- so the only reason for a filled meta-tyvar is promotion

                else do_rhs_checks
                     [ simpleOccursCheck      occ  occ_tv
                     , tyVarLevelCheck        lc   occ_tv
                     , tyVarConcretenessCheck conc occ_tv ]
              }
  where
    success = return $ okCheckRefl (mkTyVarTy occ_tv)

    -- Combine the results of individual checks. See 'TyVarCheckResult'.
    do_rhs_checks :: [TyVarCheckResult m] -> m (PuResult a Reduction)
    do_rhs_checks checks =
      case mconcat checks of
        TyVarCheck_Success                -> success
        TyVarCheck_Promote mb_lvl mb_conc -> promote flags mb_lvl mb_conc
        TyVarCheck_Error cte_prob         -> return $ PuFail cte_prob

    ---------------------
    -- occ_tv is definitely a MetaTyVar; we need to promote it/make it concrete
    promote :: TyEqFlags TcM a -> Maybe TcLevel -> Maybe ConcreteTvOrigin
            -> TcM (PuResult a Reduction)
    promote flags mb_lhs_tv_lvl mb_conc
      | MetaTv { mtv_info = info_occ, mtv_tclvl = lvl_occ } <- tcTyVarDetails occ_tv
      = do { let new_info | Just conc <- mb_conc = ConcreteTv conc
                          | otherwise            = info_occ
                 new_lvl =
                    case mb_lhs_tv_lvl of
                      Nothing         -> lvl_occ
                      Just lhs_tv_lvl -> lhs_tv_lvl `minTcLevel` lvl_occ
                        -- c[conc,3] ~ p[tau,2]: want to clone p:=p'[conc,2]
                        -- c[tau,2]  ~ p[tau,3]: want to clone p:=p'[tau,2]

           -- Check the kind of occ_tv
           --
           -- This is important for several reasons:
           --
           --  1. To ensure there is no occurs check or skolem-escape
           --     in the kind of occ_tv.
           --  2. If the LHS is a concrete type variable and the RHS is an
           --     unfilled meta-tyvar, we need to ensure that the kind of
           --     'occ_tv' is concrete.   Test cases: T23051, T23176.
           ; let occ_kind = tyVarKind occ_tv
           ; kind_result <- checkTyEqRhs flags occ_kind
           ; for kind_result $ \ kind_redn ->
        do { let kind_co  = reductionCoercion kind_redn
                 new_kind = reductionReducedType kind_redn
                 occ_tv'  = setTyVarKind occ_tv new_kind
           ; new_tv_ty <- promote_meta_tyvar new_info new_lvl occ_tv'
           ; return $ mkGReflLeftRedn Nominal new_tv_ty (mkSymCo kind_co)
           } }

      | otherwise = pprPanic "promote" (ppr occ_tv)

---------------------
promotionDone :: Monad m => TcTyVar -> LevelCheck m -> ConcreteCheck m -> m Bool
promotionDone occ_tv (LC_Promote {}) _ = isFilledMetaTyVar occ_tv
promotionDone occ_tv _ (CC_Promote {}) = isFilledMetaTyVar occ_tv
promotionDone _      _               _ = return False

---------------------
simpleOccursCheck :: OccursCheck -> TcTyVar -> TyVarCheckResult m
-- ^ The "interpreter" for 'OccursCheck'
simpleOccursCheck OC_None _
  = TyVarCheck_Success
simpleOccursCheck (OC_Check lhs_tv occ_prob) occ_tv
  | lhs_tv == tyVarName occ_tv || check_kind (tyVarKind occ_tv)
  = TyVarCheck_Error (cteProblem occ_prob)
  | otherwise
  = TyVarCheck_Success
  where
    (check_kind, _) = mkOccFolders lhs_tv

-------------------------
tyVarLevelCheck :: LevelCheck m -> TcTyVar -> TyVarCheckResult m
-- ^ The "interpreter" for 'LevelCheck'
tyVarLevelCheck LC_None _ = TyVarCheck_Success
tyVarLevelCheck lc occ_tv
  | not occ_is_deeper
  = TyVarCheck_Success
  | isSkolemTyVar occ_tv
  = TyVarCheck_Error (cteProblem cteSkolemEscape)
  | otherwise
  = case lc of
      LC_Check { lc_lenient = lenient } ->
        if lenient
        then TyVarCheck_Success
        else TyVarCheck_Error (cteProblem cteSkolemEscape)
      LC_Promote { lc_lvlp = lhs_tv_lvl } ->
        TyVarCheck_Promote (Just lhs_tv_lvl) Nothing
  where
    occ_is_deeper =
      case lc of
        LC_Check { lc_lvlc = lhs_tv_lvl } ->
          tcTyVarLevel occ_tv `strictlyDeeperThan` lhs_tv_lvl
        LC_Promote { lc_lvlp = lhs_tv_lvl } ->
          tcTyVarLevel occ_tv `strictlyDeeperThan` lhs_tv_lvl

-------------------------
tyVarConcretenessCheck :: ConcreteCheck m -> TcTyVar -> TyVarCheckResult m
-- ^ The "interpreter" for 'ConcreteCheck'
tyVarConcretenessCheck cc occ_tv
  | CC_None <- cc             -- No concreteness checks => ok
  = TyVarCheck_Success
  | isConcreteTyVar occ_tv    -- It's concrete already  => ok
  = TyVarCheck_Success
  | not (isMetaTyVar occ_tv)  -- Not a meta-tyvar => error
  = TyVarCheck_Error (cteProblem cteConcrete)

  | otherwise  -- It's a non-concrete meta-tyvar
  = case cc of
       CC_Check -> TyVarCheck_Success
       CC_Promote conc_orig -> TyVarCheck_Promote Nothing (Just conc_orig)
       -- CC_None is dealt with at the top

-------------------------
checkPromoteFreeVars :: CheckTyEqProblem    -- What occurs check problem to report
                     -> Name -> TcLevel
                     -> TyCoVarSet -> TcM CheckTyEqResult
-- Check this set of TyCoVars for
--   (a) occurs check
--   (b) promote if necessary, or report skolem escape
checkPromoteFreeVars occ_prob lhs_tv lhs_tv_lvl vs
  = do { oks <- mapM do_one (nonDetEltsUniqSet vs)
       ; return (mconcat oks) }
  where
    do_one :: TyCoVar -> TcM CheckTyEqResult
    do_one v | isCoVar v             = return cteOK
             | tyVarName v == lhs_tv = return (cteProblem occ_prob)
             | no_promotion          = return cteOK
             | not (isMetaTyVar v)   = return (cteProblem cteSkolemEscape)
             | otherwise             = promote_one v
      where
        no_promotion = not (tcTyVarLevel v `strictlyDeeperThan` lhs_tv_lvl)

    -- isCoVar case: coercion variables are not an escape risk
    -- If an implication binds a coercion variable, it'll have equalities,
    -- so the "intervening given equalities" test above will catch it
    -- Coercion holes get filled with coercions, so again no problem.

    promote_one :: TyVar -> TcM CheckTyEqResult
    promote_one tv =
      do { _ <- promote_meta_tyvar TauTv lhs_tv_lvl tv
         ; return cteOK }

promote_meta_tyvar :: MetaInfo -> TcLevel -> TcTyVar -> TcM TcType
promote_meta_tyvar info dest_lvl occ_tv
  = do { -- Check whether occ_tv is already unified. The rhs-type
         -- started zonked, but we may have promoted one of its type
         -- variables, and we then encounter it for the second time.
         -- But if so, it'll definitely be another already-checked TyVar
         mb_filled <- isFilledMetaTyVar_maybe occ_tv
       ; case mb_filled of {
           Just ty -> return ty ;
           Nothing ->

    -- OK, not done already, so clone/promote it
    do { new_tv <- cloneMetaTyVarWithInfo info dest_lvl occ_tv
       ; liftZonkM $ writeMetaTyVar occ_tv (mkTyVarTy new_tv)
       ; traceTc "promoteTyVar" (ppr occ_tv <+> text "-->" <+> ppr new_tv)
       ; return (mkTyVarTy new_tv) } } }



-------------------------
touchabilityAndShapeTest :: TcLevel -> TcTyVar -> TcType -> Bool
-- This is the key test for untouchability:
-- See Note [Unification preconditions] in GHC.Tc.Utils.Unify
-- and Note [Solve by unification] in GHC.Tc.Solver.Equality
-- True <=> touchability and shape are OK
touchabilityAndShapeTest given_eq_lvl tv rhs
  | MetaTv { mtv_info = info, mtv_tclvl = tv_lvl } <- tcTyVarDetails tv
  , tv_lvl `deeperThanOrSame` given_eq_lvl
  , checkTopShape info rhs
  = True
  | otherwise
  = False

-------------------------
-- | checkTopShape checks (TYVAR-TV)
-- Note [Unification preconditions]; returns True if these conditions
-- are satisfied. But see the Note for other preconditions, too.
checkTopShape :: MetaInfo -> TcType -> Bool
checkTopShape info xi
  = case info of
      TyVarTv ->
        case getTyVar_maybe xi of   -- Looks through type synonyms
           Nothing -> False
           Just tv -> case tcTyVarDetails tv of -- (TYVAR-TV) wrinkle
                        SkolemTv {} -> True
                        RuntimeUnk  -> True
                        MetaTv { mtv_info = TyVarTv } -> True
                        _                             -> False
      CycleBreakerTv -> False  -- We never unify these
      _ -> True

--------------------------------------------------------------------------------
-- Making a type concrete.

-- | Try to turn the provided type into a concrete type, by ensuring
-- unfilled metavariables are appropriately marked as concrete.
--
-- Returns a coercion whose RHS is a zonked type which is "as concrete as possible",
-- and a collection of Wanted equality constraints that are necessary to make
-- the type concrete.
--
-- For example, for an input @TYPE a[sk]@ we will return a coercion with RHS
-- @TYPE gamma[conc]@ together with the Wanted equality constraint @a ~# gamma@.
--
-- INVARIANT: the RHS type of the returned coercion is equal to the input type,
-- up to zonking and the returned Wanted equality constraints.
--
-- INVARIANT: if this function returns an empty list of constraints
-- then the RHS type of the returned coercion is concrete,
-- in the sense of Note [Concrete types].
makeTypeConcrete :: FastString -> ConcreteTvOrigin
                 -> TcType -> TcM (TcCoercion, Cts)
makeTypeConcrete occ_fs conc_orig ty =
  do { traceTc "makeTypeConcrete {" $
        vcat [ text "ty:" <+> ppr ty ]

     -- To make a type 'ty' concrete, we query what would happen were we
     -- to try unifying
     --
     --   alpha[conc] ~# ty
     --
     -- for a fresh concrete metavariable 'alpha'.
     --
     -- We do this by calling 'checkTyEqRhs' with suitable 'TyEqFlags'.
     -- NB: we don't actually need to create a fresh concrete metavariable
     -- in order to call 'checkTyEqRhs'.
     ; let ty_eq_flags =
            TEFTyVar
              { tefTyVar_occursCheck   = OC_None
                  -- LHS is a fresh meta-tyvar: no occurs check needed
              , tefTyVar_levelCheck    = LC_None
              , tefTyVar_concreteCheck = CC_Promote conc_orig
              , tef_fam_app            = TEFA_Recurse
              }

     -- NB: 'checkTyEqRhs' expects a fully zonked type as input.
     ; ty' <- liftZonkM $ zonkTcType ty
     ; pu_res <- checkTyEqRhs @TcM @() ty_eq_flags ty'
     -- NB: 'checkTyEqRhs' will also check the kind, thus upholding the
     -- invariant that the kind of a concrete type must also be a concrete type.

     ; (cts, final_co) <-
         case pu_res of
           PuOK _ redn ->
            do { traceTc "makeTypeConcrete: unifier success" $
                   vcat [ text "ty:" <+> ppr ty <+> dcolon <+> ppr (typeKind ty)
                        , text "redn:" <+> ppr redn
                        ]
               ; return (emptyBag, mkSymCo $ reductionCoercion redn)
                  -- NB: the unifier returns a 'Reduction' with the concrete
                  -- type on the left, but we want a coercion with it on the
                  -- right; so we use 'mkSymCo'.
               }
           PuFail _prob ->
             do { traceTc "makeTypeConcrete: unifier failure" $
                   vcat [ text "ty:" <+> ppr ty <+> dcolon <+> ppr (typeKind ty)
                        , text "problem:" <+> ppr _prob
                        ]
                  -- We failed to make 'ty' concrete. In order to continue
                  -- typechecking, we proceed as follows:
                  --
                  --   - create a new concrete metavariable alpha[conc]
                  --   - emit the equality @ty ~# alpha[conc]@.
                  --
                  -- This equality will eventually get reported as insoluble
                  -- to the user.

                -- The kind of a concrete metavariable must itself be concrete,
                -- so we need to do a concreteness check on the kind first.
                ; let ki = typeKind ty
                ; (kind_co, kind_cts) <-
                    if isConcreteType ki
                    then return (mkNomReflCo ki, emptyBag)
                    else makeTypeConcrete occ_fs conc_orig ki

                -- Now create the new concrete metavariable.
                ; conc_tv <- newConcreteTyVar conc_orig occ_fs (coercionRKind kind_co)
                ; let conc_ty = mkTyVarTy conc_tv
                      pty = mkEqPredRole Nominal ty' conc_ty
                ; hole <- newCoercionHoleO orig pty
                ; loc <- getCtLocM orig (Just KindLevel)
                ; let ct = mkNonCanonical
                         $ CtWanted { ctev_pred      = pty
                                    , ctev_dest      = HoleDest hole
                                    , ctev_loc       = loc
                                    , ctev_rewriters = emptyRewriterSet }
                ; return (kind_cts S.<> unitBag ct, HoleCo hole)
                }

     ; traceTc "makeTypeConcrete }" $
        vcat [ text "ty :" <+> _ppr_ty ty
             , text "ty':" <+> _ppr_ty ty'
             , text "final_co:" <+> _ppr_co final_co ]

     ; return (final_co, cts)
     }
  where

    _ppr_ty ty = ppr ty <+> dcolon <+> ppr (typeKind ty)
    _ppr_co co = ppr co <+> dcolon <+> parens (_ppr_ty (coercionLKind co)) <+> text "~#" <+> parens (_ppr_ty (coercionRKind co))

    orig :: CtOrigin
    orig = case conc_orig of
      ConcreteFRR frr_orig -> FRROrigin frr_orig

--------------------------------------------------------------------------------
-- mightEqualLater

mightEqualLater :: InertSet -> TcPredType -> CtLoc -> TcPredType -> CtLoc -> Maybe Subst
-- See Note [What might equal later?]
-- Used to implement logic in Note [Instance and Given overlap] in GHC.Tc.Solver.Dict
mightEqualLater inert_set given_pred given_loc wanted_pred wanted_loc
  | prohibitedSuperClassSolve given_loc wanted_loc
  = Nothing

  | otherwise
  = case tcUnifyTysFG bind_fun [flattened_given] [flattened_wanted] of
      Unifiable subst
        -> Just subst
      MaybeApart reason subst
        | MARInfinite <- reason -- see Example 7 in the Note.
        -> Nothing
        | otherwise
        -> Just subst
      SurelyApart -> Nothing

  where
    in_scope  = mkInScopeSet $ tyCoVarsOfTypes [given_pred, wanted_pred]

    -- NB: flatten both at the same time, so that we can share mappings
    -- from type family applications to variables, and also to guarantee
    -- that the fresh variables are really fresh between the given and
    -- the wanted. Flattening both at the same time is needed to get
    -- Example 10 from the Note.
    (Pair flattened_given flattened_wanted, var_mapping)
      = flattenTysX in_scope (Pair given_pred wanted_pred)

    bind_fun :: BindFun
    bind_fun tv rhs_ty
      | MetaTv { mtv_info = info } <- tcTyVarDetails tv
      = if ok_shape tv info rhs_ty && can_unify tv rhs_ty
        then BindMe
        else Apart

         -- See Examples 4, 5, and 6 from the Note
      | Just (_fam_tc, fam_args) <- lookupVarEnv var_mapping tv
      , anyFreeVarsOfTypes mentions_meta_ty_var fam_args
      = BindMe

      | otherwise
      = Apart
      where

    can_unify :: TcTyVar -> TcType -> Bool
    can_unify tv rhs_ty
      -- See Note [Use checkTyEqRhs in mightEqualLater]
      | PuOK {} <- runIdentity $ checkTyEqRhs (pureTyEqFlags_LHSMetaTyVar tv) rhs_ty
      = True
      | otherwise
      = False

    -- True for TauTv and TyVarTv (and RuntimeUnkTv) meta-tyvars
    -- (as they can be unified)
    -- and also for CycleBreakerTvs that mentions meta-tyvars
    mentions_meta_ty_var :: TyVar -> Bool
    mentions_meta_ty_var tv
      | isMetaTyVar tv
      = case metaTyVarInfo tv of
          -- See Examples 8 and 9 in the Note
          CycleBreakerTv
            -> anyFreeVarsOfType mentions_meta_ty_var
                 (lookupCycleBreakerVar tv inert_set)
          _ -> True
      | otherwise
      = False

    -- Like checkTopShape, but allows cbv variables to unify
    ok_shape :: TcTyVar -> MetaInfo -> Type -> Bool
    ok_shape _lhs_tv TyVarTv rhs_ty  -- see Example 3 from the Note
      | Just rhs_tv <- getTyVar_maybe rhs_ty
      = case tcTyVarDetails rhs_tv of
          MetaTv { mtv_info = TyVarTv } -> True
          MetaTv {}                     -> False  -- Could unify with anything
          SkolemTv {}                   -> True
          RuntimeUnk                    -> True
      | otherwise  -- not a var on the RHS
      = False
    ok_shape lhs_tv _other _rhs_ty = mentions_meta_ty_var lhs_tv

{- Note [Use checkTyEqRhs in mightEqualLater]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In 'mightEqualLater', we are checking whether two types ty1 and ty2 might become
equal after further unifications. To do this, we call the pure unifier, via
the function 'tcUnifyTysFG'. However, it is important that we don't return
false positives:

  1. Level check:  ty1 = alpha[tau:1] /~ ty2 = k[sk:3]
  2. Occurs check: ty1 = beta[tau]    /~ ty2 = Maybe beta[tau]
  3. Concreteness: ty1 = kappa[conc]  /~ ty2 = k[sk].

In these examples, ty1 and ty2 cannot unify; to inform the pure unifier of this
fact, we use 'checkTyEqRhs' to provide the 'BindFun'.

Failing to account for this caused #25744:

  work item = [W] w1: Eq (g[sk:1] x[sk:3])
  inerts = { [G] g1: Eq x[sk:3]
           , [G] g2: forall y. Eq y => Eq (g[sk:1] y)
           , [G] g3: Eq (f[tau:1] v[tau:1]) }

We want to solve w1 using g1 and g2. The presence of g3 is irrelevant, because
we cannot unify 'f[tau:1] v[tau:1]' and 'g[sk:1] x[sk:3]' due to skolem escape.
-}

-- | Return the type family application a CycleBreakerTv maps to.
lookupCycleBreakerVar :: TcTyVar    -- ^ cbv, must be a CycleBreakerTv
                      -> InertSet
                      -> TcType     -- ^ type family application the cbv maps to
lookupCycleBreakerVar cbv (IS { inert_cycle_breakers = cbvs_stack })
-- This function looks at every environment in the stack. This is necessary
-- to avoid #20231. This function (and its one usage site) is the only reason
-- that we store a stack instead of just the top environment.
  | Just tyfam_app <- assert (isCycleBreakerTyVar cbv) $
                      firstJusts (NE.map (lookupBag cbv) cbvs_stack)
  = tyfam_app
  | otherwise
  = pprPanic "lookupCycleBreakerVar found an unbound cycle breaker" (ppr cbv $$ ppr cbvs_stack)

--------------------------------------------------------------------------------
