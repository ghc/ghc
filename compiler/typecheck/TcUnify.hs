{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Type subsumption and unification
-}

{-# LANGUAGE CPP, MultiWayIf, TupleSections, ScopedTypeVariables #-}

module TcUnify (
  -- Full-blown subsumption
  tcWrapResult, tcWrapResultO, tcSkolemise, tcSkolemiseET,
  tcSubTypeHR, tcSubTypeO, tcSubType_NC, tcSubTypeDS,
  tcSubTypeDS_NC_O, tcSubTypeET,
  checkConstraints, buildImplicationFor,

  -- Various unifications
  unifyType, unifyTheta, unifyKind,
  uType, promoteTcType,
  swapOverTyVars, canSolveByUnification,

  --------------------------------
  -- Holes
  tcInferInst, tcInferNoInst,
  matchExpectedListTy,
  matchExpectedPArrTy,
  matchExpectedTyConApp,
  matchExpectedAppTy,
  matchExpectedFunTys,
  matchActualFunTys, matchActualFunTysPart,
  matchExpectedFunKind,

  wrapFunResCoercion,

  occCheckExpand, metaTyVarUpdateOK,
  occCheckForErrors, OccCheckResult(..)

  ) where

#include "HsVersions.h"

import HsSyn
import TyCoRep
import TcMType
import TcRnMonad
import TcType
import Type
import Coercion
import TcEvidence
import Name ( isSystemName )
import Inst
import TyCon
import TysWiredIn
import TysPrim( tYPE )
import Var
import VarSet
import VarEnv
import ErrUtils
import DynFlags
import BasicTypes
import Bag
import Util
import Pair( pFst )
import qualified GHC.LanguageExtensions as LangExt
import Outputable
import FastString

import Control.Monad
import Control.Arrow ( second )

{-
************************************************************************
*                                                                      *
             matchExpected functions
*                                                                      *
************************************************************************

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

-}

-- Use this one when you have an "expected" type.
matchExpectedFunTys :: forall a.
                       SDoc   -- See Note [Herald for matchExpectedFunTys]
                    -> Arity
                    -> ExpRhoType  -- deeply skolemised
                    -> ([ExpSigmaType] -> ExpRhoType -> TcM a)
                          -- must fill in these ExpTypes here
                    -> TcM (a, HsWrapper)
-- If    matchExpectedFunTys n ty = (_, wrap)
-- then  wrap : (t1 -> ... -> tn -> ty_r) ~> ty,
--   where [t1, ..., tn], ty_r are passed to the thing_inside
matchExpectedFunTys herald arity orig_ty thing_inside
  = case orig_ty of
      Check ty -> go [] arity ty
      _        -> defer [] arity orig_ty
  where
    go acc_arg_tys 0 ty
      = do { result <- thing_inside (reverse acc_arg_tys) (mkCheckExpType ty)
           ; return (result, idHsWrapper) }

    go acc_arg_tys n ty
      | Just ty' <- tcView ty = go acc_arg_tys n ty'

    go acc_arg_tys n (FunTy arg_ty res_ty)
      = ASSERT( not (isPredTy arg_ty) )
        do { (result, wrap_res) <- go (mkCheckExpType arg_ty : acc_arg_tys)
                                      (n-1) res_ty
           ; return ( result
                    , mkWpFun idHsWrapper wrap_res arg_ty res_ty doc ) }
      where
        doc = text "When inferring the argument type of a function with type" <+>
              quotes (ppr orig_ty)

    go acc_arg_tys n ty@(TyVarTy tv)
      | isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty' -> go acc_arg_tys n ty'
               Flexi        -> defer acc_arg_tys n (mkCheckExpType ty) }

       -- In all other cases we bale out into ordinary unification
       -- However unlike the meta-tyvar case, we are sure that the
       -- number of arguments doesn't match arity of the original
       -- type, so we can add a bit more context to the error message
       -- (cf Trac #7869).
       --
       -- It is not always an error, because specialized type may have
       -- different arity, for example:
       --
       -- > f1 = f2 'a'
       -- > f2 :: Monad m => m Bool
       -- > f2 = undefined
       --
       -- But in that case we add specialized type into error context
       -- anyway, because it may be useful. See also Trac #9605.
    go acc_arg_tys n ty = addErrCtxtM mk_ctxt $
                          defer acc_arg_tys n (mkCheckExpType ty)

    ------------
    defer :: [ExpSigmaType] -> Arity -> ExpRhoType -> TcM (a, HsWrapper)
    defer acc_arg_tys n fun_ty
      = do { more_arg_tys <- replicateM n newInferExpTypeNoInst
           ; res_ty       <- newInferExpTypeInst
           ; result       <- thing_inside (reverse acc_arg_tys ++ more_arg_tys) res_ty
           ; more_arg_tys <- mapM readExpType more_arg_tys
           ; res_ty       <- readExpType res_ty
           ; let unif_fun_ty = mkFunTys more_arg_tys res_ty
           ; wrap <- tcSubTypeDS AppOrigin GenSigCtxt unif_fun_ty fun_ty
                         -- Not a good origin at all :-(
           ; return (result, wrap) }

    ------------
    mk_ctxt :: TidyEnv -> TcM (TidyEnv, MsgDoc)
    mk_ctxt env = do { (env', ty) <- zonkTidyTcType env orig_tc_ty
                     ; let (args, _) = tcSplitFunTys ty
                           n_actual = length args
                           (env'', orig_ty') = tidyOpenType env' orig_tc_ty
                     ; return ( env''
                              , mk_fun_tys_msg orig_ty' ty n_actual arity herald) }
      where
        orig_tc_ty = checkingExpType "matchExpectedFunTys" orig_ty
            -- this is safe b/c we're called from "go"

-- Like 'matchExpectedFunTys', but used when you have an "actual" type,
-- for example in function application
matchActualFunTys :: SDoc   -- See Note [Herald for matchExpectedFunTys]
                  -> CtOrigin
                  -> Maybe (HsExpr GhcRn)   -- the thing with type TcSigmaType
                  -> Arity
                  -> TcSigmaType
                  -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
-- If    matchActualFunTys n ty = (wrap, [t1,..,tn], ty_r)
-- then  wrap : ty ~> (t1 -> ... -> tn -> ty_r)
matchActualFunTys herald ct_orig mb_thing arity ty
  = matchActualFunTysPart herald ct_orig mb_thing arity ty [] arity

-- | Variant of 'matchActualFunTys' that works when supplied only part
-- (that is, to the right of some arrows) of the full function type
matchActualFunTysPart :: SDoc -- See Note [Herald for matchExpectedFunTys]
                      -> CtOrigin
                      -> Maybe (HsExpr GhcRn)  -- the thing with type TcSigmaType
                      -> Arity
                      -> TcSigmaType
                      -> [TcSigmaType] -- reversed args. See (*) below.
                      -> Arity   -- overall arity of the function, for errs
                      -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
matchActualFunTysPart herald ct_orig mb_thing arity orig_ty
                      orig_old_args full_arity
  = go arity orig_old_args orig_ty
-- Does not allocate unnecessary meta variables: if the input already is
-- a function, we just take it apart.  Not only is this efficient,
-- it's important for higher rank: the argument might be of form
--              (forall a. ty) -> other
-- If allocated (fresh-meta-var1 -> fresh-meta-var2) and unified, we'd
-- hide the forall inside a meta-variable

-- (*) Sometimes it's necessary to call matchActualFunTys with only part
-- (that is, to the right of some arrows) of the type of the function in
-- question. (See TcExpr.tcArgs.) This argument is the reversed list of
-- arguments already seen (that is, not part of the TcSigmaType passed
-- in elsewhere).

  where
    -- This function has a bizarre mechanic: it accumulates arguments on
    -- the way down and also builds an argument list on the way up. Why:
    -- 1. The returns args list and the accumulated args list might be different.
    --    The accumulated args include all the arg types for the function,
    --    including those from before this function was called. The returned
    --    list should include only those arguments produced by this call of
    --    matchActualFunTys
    --
    -- 2. The HsWrapper can be built only on the way up. It seems (more)
    --    bizarre to build the HsWrapper but not the arg_tys.
    --
    -- Refactoring is welcome.
    go :: Arity
       -> [TcSigmaType] -- accumulator of arguments (reversed)
       -> TcSigmaType   -- the remainder of the type as we're processing
       -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
    go 0 _ ty = return (idHsWrapper, [], ty)

    go n acc_args ty
      | not (null tvs && null theta)
      = do { (wrap1, rho) <- topInstantiate ct_orig ty
           ; (wrap2, arg_tys, res_ty) <- go n acc_args rho
           ; return (wrap2 <.> wrap1, arg_tys, res_ty) }
      where
        (tvs, theta, _) = tcSplitSigmaTy ty

    go n acc_args ty
      | Just ty' <- tcView ty = go n acc_args ty'

    go n acc_args (FunTy arg_ty res_ty)
      = ASSERT( not (isPredTy arg_ty) )
        do { (wrap_res, tys, ty_r) <- go (n-1) (arg_ty : acc_args) res_ty
           ; return ( mkWpFun idHsWrapper wrap_res arg_ty ty_r doc
                    , arg_ty : tys, ty_r ) }
      where
        doc = text "When inferring the argument type of a function with type" <+>
              quotes (ppr orig_ty)

    go n acc_args ty@(TyVarTy tv)
      | isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty' -> go n acc_args ty'
               Flexi        -> defer n ty }

       -- In all other cases we bale out into ordinary unification
       -- However unlike the meta-tyvar case, we are sure that the
       -- number of arguments doesn't match arity of the original
       -- type, so we can add a bit more context to the error message
       -- (cf Trac #7869).
       --
       -- It is not always an error, because specialized type may have
       -- different arity, for example:
       --
       -- > f1 = f2 'a'
       -- > f2 :: Monad m => m Bool
       -- > f2 = undefined
       --
       -- But in that case we add specialized type into error context
       -- anyway, because it may be useful. See also Trac #9605.
    go n acc_args ty = addErrCtxtM (mk_ctxt (reverse acc_args) ty) $
                       defer n ty

    ------------
    defer n fun_ty
      = do { arg_tys <- replicateM n newOpenFlexiTyVarTy
           ; res_ty  <- newOpenFlexiTyVarTy
           ; let unif_fun_ty = mkFunTys arg_tys res_ty
           ; co <- unifyType mb_thing fun_ty unif_fun_ty
           ; return (mkWpCastN co, arg_tys, res_ty) }

    ------------
    mk_ctxt :: [TcSigmaType] -> TcSigmaType -> TidyEnv -> TcM (TidyEnv, MsgDoc)
    mk_ctxt arg_tys res_ty env
      = do { let ty = mkFunTys arg_tys res_ty
           ; (env1, zonked) <- zonkTidyTcType env ty
                   -- zonking might change # of args
           ; let (zonked_args, _) = tcSplitFunTys zonked
                 n_actual         = length zonked_args
                 (env2, unzonked) = tidyOpenType env1 ty
           ; return ( env2
                    , mk_fun_tys_msg unzonked zonked n_actual full_arity herald) }

mk_fun_tys_msg :: TcType  -- the full type passed in (unzonked)
               -> TcType  -- the full type passed in (zonked)
               -> Arity   -- the # of args found
               -> Arity   -- the # of args wanted
               -> SDoc    -- overall herald
               -> SDoc
mk_fun_tys_msg full_ty ty n_args full_arity herald
  = herald <+> speakNOf full_arity (text "argument") <> comma $$
    if n_args == full_arity
      then text "its type is" <+> quotes (pprType full_ty) <>
           comma $$
           text "it is specialized to" <+> quotes (pprType ty)
      else sep [text "but its type" <+> quotes (pprType ty),
                if n_args == 0 then text "has none"
                else text "has only" <+> speakN n_args]

----------------------
matchExpectedListTy :: TcRhoType -> TcM (TcCoercionN, TcRhoType)
-- Special case for lists
matchExpectedListTy exp_ty
 = do { (co, [elt_ty]) <- matchExpectedTyConApp listTyCon exp_ty
      ; return (co, elt_ty) }

----------------------
matchExpectedPArrTy :: TcRhoType -> TcM (TcCoercionN, TcRhoType)
-- Special case for parrs
matchExpectedPArrTy exp_ty
  = do { (co, [elt_ty]) <- matchExpectedTyConApp parrTyCon exp_ty
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
  = ASSERT(tc /= funTyCon) go orig_ty
  where
    go ty
       | Just ty' <- tcView ty
       = go ty'

    go ty@(TyConApp tycon args)
       | tc == tycon  -- Common case
       = return (mkTcNomReflCo ty, args)

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
    -- Then we don't want to instantate T's data constructors with
    --    (a::*) ~ Maybe
    -- because that'll make types that are utterly ill-kinded.
    -- This happened in Trac #7368
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
      | Just ty' <- tcView ty = go ty'

      | Just (fun_ty, arg_ty) <- tcSplitAppTy_maybe ty
      = return (mkTcNomReflCo orig_ty, (fun_ty, arg_ty))

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
    kind1 = mkFunTy liftedTypeKind orig_kind
    kind2 = liftedTypeKind    -- m :: * -> k
                              -- arg type :: *

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

It returns a coercion function
        co_fn :: actual_ty ~ expected_ty
which takes an HsExpr of type actual_ty into one of type
expected_ty.

These functions do not actually check for subsumption. They check if
expected_ty is an appropriate annotation to use for something of type
actual_ty. This difference matters when thinking about visible type
application. For example,

   forall a. a -> forall b. b -> b
      DOES NOT SUBSUME
   forall a b. a -> b -> b

because the type arguments appear in a different order. (Neither does
it work the other way around.) BUT, these types are appropriate annotations
for one another. Because the user directs annotations, it's OK if some
arguments shuffle around -- after all, it's what the user wants.
Bottom line: none of this changes with visible type application.

There are a number of wrinkles (below).

Notice that Wrinkle 1 and 2 both require eta-expansion, which technically
may increase termination.  We just put up with this, in exchange for getting
more predictable type inference.

Wrinkle 1: Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want   (forall a. Int -> a -> a)  <=  (Int -> forall a. a->a)
(see section 4.6 of "Practical type inference for higher rank types")
So we must deeply-skolemise the RHS before we instantiate the LHS.

That is why tc_sub_type starts with a call to tcSkolemise (which does the
deep skolemisation), and then calls the DS variant (which assumes
that expected_ty is deeply skolemised)

Wrinkle 2: Note [Co/contra-variance of subsumption checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider  g :: (Int -> Int) -> Int
  f1 :: (forall a. a -> a) -> Int
  f1 = g

  f2 :: (forall a. a -> a) -> Int
  f2 x = g x
f2 will typecheck, and it would be odd/fragile if f1 did not.
But f1 will only typecheck if we have that
    (Int->Int) -> Int  <=  (forall a. a->a) -> Int
And that is only true if we do the full co/contravariant thing
in the subsumption check.  That happens in the FunTy case of
tcSubTypeDS_NC_O, and is the sole reason for the WpFun form of
HsWrapper.

Another powerful reason for doing this co/contra stuff is visible
in Trac #9569, involving instantiation of constraint variables,
and again involving eta-expansion.

Wrinkle 3: Note [Higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider tc150:
  f y = \ (x::forall a. a->a). blah
The following happens:
* We will infer the type of the RHS, ie with a res_ty = alpha.
* Then the lambda will split  alpha := beta -> gamma.
* And then we'll check tcSubType IsSwapped beta (forall a. a->a)

So it's important that we unify beta := forall a. a->a, rather than
skolemising the type.
-}


-- | Call this variant when you are in a higher-rank situation and
-- you know the right-hand type is deeply skolemised.
tcSubTypeHR :: CtOrigin               -- ^ of the actual type
            -> Maybe (HsExpr GhcRn)   -- ^ If present, it has type ty_actual
            -> TcSigmaType -> ExpRhoType -> TcM HsWrapper
tcSubTypeHR orig = tcSubTypeDS_NC_O orig GenSigCtxt

------------------------
tcSubTypeET :: CtOrigin -> UserTypeCtxt
            -> ExpSigmaType -> TcSigmaType -> TcM HsWrapper
-- If wrap = tc_sub_type_et t1 t2
--    => wrap :: t1 ~> t2
tcSubTypeET orig ctxt (Check ty_actual) ty_expected
  = tc_sub_tc_type eq_orig orig ctxt ty_actual ty_expected
  where
    eq_orig = TypeEqOrigin { uo_actual   = ty_expected
                           , uo_expected = ty_actual
                           , uo_thing    = Nothing }

tcSubTypeET _ _ (Infer inf_res) ty_expected
  = ASSERT2( not (ir_inst inf_res), ppr inf_res $$ ppr ty_expected )
    do { co <- fillInferResult ty_expected inf_res
       ; return (mkWpCastN (mkTcSymCo co)) }

------------------------
tcSubTypeO :: CtOrigin      -- ^ of the actual type
           -> UserTypeCtxt  -- ^ of the expected type
           -> TcSigmaType
           -> ExpRhoType
           -> TcM HsWrapper
tcSubTypeO orig ctxt ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubTypeDS_O" (vcat [ pprCtOrigin orig
                                       , pprUserTypeCtxt ctxt
                                       , ppr ty_actual
                                       , ppr ty_expected ])
       ; tcSubTypeDS_NC_O orig ctxt Nothing ty_actual ty_expected }

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
                   -- might not be filled if we're debugging. ugh.
           ; mb_ty_expected          <- readExpType_maybe ty_expected
           ; (tidy_env, ty_expected) <- case mb_ty_expected of
                                          Just ty -> second mkCheckExpType <$>
                                                     zonkTidyTcType tidy_env ty
                                          Nothing -> return (tidy_env, ty_expected)
           ; ty_expected             <- readExpType ty_expected
           ; (tidy_env, ty_expected) <- zonkTidyTcType tidy_env ty_expected
           ; let msg = vcat [ hang (text "When checking that:")
                                 4 (ppr ty_actual)
                            , nest 2 (hang (text "is more polymorphic than:")
                                         2 (ppr ty_expected)) ]
           ; return (tidy_env, msg) }

---------------
-- The "_NC" variants do not add a typechecker-error context;
-- the caller is assumed to do that

tcSubType_NC :: UserTypeCtxt -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- Checks that actual <= expected
-- Returns HsWrapper :: actual ~ expected
tcSubType_NC ctxt ty_actual ty_expected
  = do { traceTc "tcSubType_NC" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tc_sub_tc_type origin origin ctxt ty_actual ty_expected }
  where
    origin = TypeEqOrigin { uo_actual   = ty_actual
                          , uo_expected = ty_expected
                          , uo_thing    = Nothing }

tcSubTypeDS :: CtOrigin -> UserTypeCtxt -> TcSigmaType -> ExpRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised (hence "DS")
tcSubTypeDS orig ctxt ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubTypeDS_NC" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tcSubTypeDS_NC_O orig ctxt Nothing ty_actual ty_expected }

tcSubTypeDS_NC_O :: CtOrigin   -- origin used for instantiation only
                 -> UserTypeCtxt
                 -> Maybe (HsExpr GhcRn)
                 -> TcSigmaType -> ExpRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised
tcSubTypeDS_NC_O inst_orig ctxt m_thing ty_actual ty_expected
  = case ty_expected of
      Infer inf_res -> fillInferResult_Inst inst_orig ty_actual inf_res
      Check ty      -> tc_sub_type_ds eq_orig inst_orig ctxt ty_actual ty
         where
           eq_orig = TypeEqOrigin { uo_actual = ty_actual, uo_expected = ty
                                  , uo_thing  = ppr <$> m_thing }

---------------
tc_sub_tc_type :: CtOrigin   -- used when calling uType
               -> CtOrigin   -- used when instantiating
               -> UserTypeCtxt -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- If wrap = tc_sub_type t1 t2
--    => wrap :: t1 ~> t2
tc_sub_tc_type eq_orig inst_orig ctxt ty_actual ty_expected
  | definitely_poly ty_expected      -- See Note [Don't skolemise unnecessarily]
  , not (possibly_poly ty_actual)
  = do { traceTc "tc_sub_tc_type (drop to equality)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; mkWpCastN <$>
         uType eq_orig TypeLevel ty_actual ty_expected }

  | otherwise   -- This is the general case
  = do { traceTc "tc_sub_tc_type (general case)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; (sk_wrap, inner_wrap) <- tcSkolemise ctxt ty_expected $
                                                   \ _ sk_rho ->
                                  tc_sub_type_ds eq_orig inst_orig ctxt
                                                 ty_actual sk_rho
       ; return (sk_wrap <.> inner_wrap) }
  where
    possibly_poly ty
      | isForAllTy ty                        = True
      | Just (_, res) <- splitFunTy_maybe ty = possibly_poly res
      | otherwise                            = False
      -- NB *not* tcSplitFunTy, because here we want
      -- to decompose type-class arguments too

    definitely_poly ty
      | (tvs, theta, tau) <- tcSplitSigmaTy ty
      , (tv:_) <- tvs
      , null theta
      , isInsolubleOccursCheck NomEq tv tau
      = True
      | otherwise
      = False

{- Note [Don't skolemise unnecessarily]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are trying to solve
    (Char->Char) <= (forall a. a->a)
We could skolemise the 'forall a', and then complain
that (Char ~ a) is insoluble; but that's a pretty obscure
error.  It's better to say that
    (Char->Char) ~ (forall a. a->a)
fails.

So roughly:
 * if the ty_expected has an outermost forall
      (i.e. skolemisation is the next thing we'd do)
 * and the ty_actual has no top-level polymorphism (but looking deeply)
then we can revert to simple equality.  But we need to be careful.
These examples are all fine:

 * (Char -> forall a. a->a) <= (forall a. Char -> a -> a)
      Polymorphism is buried in ty_actual

 * (Char->Char) <= (forall a. Char -> Char)
      ty_expected isn't really polymorphic

 * (Char->Char) <= (forall a. (a~Char) => a -> a)
      ty_expected isn't really polymorphic

 * (Char->Char) <= (forall a. F [a] Char -> Char)
                   where type instance F [x] t = t
     ty_expected isn't really polymorphic

If we prematurely go to equality we'll reject a program we should
accept (e.g. Trac #13752).  So the test (which is only to improve
error message) is very conservative:
 * ty_actual is /definitely/ monomorphic
 * ty_expected is /definitely/ polymorphic
-}

---------------
tc_sub_type_ds :: CtOrigin    -- used when calling uType
               -> CtOrigin    -- used when instantiating
               -> UserTypeCtxt -> TcSigmaType -> TcRhoType -> TcM HsWrapper
-- If wrap = tc_sub_type_ds t1 t2
--    => wrap :: t1 ~> t2
-- Here is where the work actually happens!
-- Precondition: ty_expected is deeply skolemised
tc_sub_type_ds eq_orig inst_orig ctxt ty_actual ty_expected
  = do { traceTc "tc_sub_type_ds" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; go ty_actual ty_expected }
  where
    go ty_a ty_e | Just ty_a' <- tcView ty_a = go ty_a' ty_e
                 | Just ty_e' <- tcView ty_e = go ty_a  ty_e'

    go (TyVarTy tv_a) ty_e
      = do { lookup_res <- lookupTcTyVar tv_a
           ; case lookup_res of
               Filled ty_a' ->
                 do { traceTc "tcSubTypeDS_NC_O following filled act meta-tyvar:"
                        (ppr tv_a <+> text "-->" <+> ppr ty_a')
                    ; tc_sub_type_ds eq_orig inst_orig ctxt ty_a' ty_e }
               Unfilled _   -> unify }

    -- Historical note (Sept 16): there was a case here for
    --    go ty_a (TyVarTy alpha)
    -- which, in the impredicative case unified  alpha := ty_a
    -- where th_a is a polytype.  Not only is this probably bogus (we
    -- simply do not have decent story for imprdicative types), but it
    -- caused Trac #12616 because (also bizarrely) 'deriving' code had
    -- -XImpredicativeTypes on.  I deleted the entire case.

    go (FunTy act_arg act_res) (FunTy exp_arg exp_res)
      | not (isPredTy act_arg)
      , not (isPredTy exp_arg)
      = -- See Note [Co/contra-variance of subsumption checking]
        do { res_wrap <- tc_sub_type_ds eq_orig inst_orig  ctxt act_res exp_res
           ; arg_wrap <- tc_sub_tc_type eq_orig given_orig ctxt exp_arg act_arg
           ; return (mkWpFun arg_wrap res_wrap exp_arg exp_res doc) }
               -- arg_wrap :: exp_arg ~> act_arg
               -- res_wrap :: act-res ~> exp_res
      where
        given_orig = GivenOrigin (SigSkol GenSigCtxt exp_arg [])
        doc = text "When checking that" <+> quotes (ppr ty_actual) <+>
              text "is more polymorphic than" <+> quotes (ppr ty_expected)

    go ty_a ty_e
      | let (tvs, theta, _) = tcSplitSigmaTy ty_a
      , not (null tvs && null theta)
      = do { (in_wrap, in_rho) <- topInstantiate inst_orig ty_a
           ; body_wrap <- tc_sub_type_ds
                            (eq_orig { uo_actual = in_rho
                                     , uo_expected = ty_expected })
                            inst_orig ctxt in_rho ty_e
           ; return (body_wrap <.> in_wrap) }

      | otherwise   -- Revert to unification
      = inst_and_unify
         -- It's still possible that ty_actual has nested foralls. Instantiate
         -- these, as there's no way unification will succeed with them in.
         -- See typecheck/should_compile/T11305 for an example of when this
         -- is important. The problem is that we're checking something like
         --  a -> forall b. b -> b     <=   alpha beta gamma
         -- where we end up with alpha := (->)

    inst_and_unify = do { (wrap, rho_a) <- deeplyInstantiate inst_orig ty_actual

                           -- if we haven't recurred through an arrow, then
                           -- the eq_orig will list ty_actual. In this case,
                           -- we want to update the origin to reflect the
                           -- instantiation. If we *have* recurred through
                           -- an arrow, it's better not to update.
                        ; let eq_orig' = case eq_orig of
                                TypeEqOrigin { uo_actual   = orig_ty_actual }
                                  |  orig_ty_actual `tcEqType` ty_actual
                                  ,  not (isIdHsWrapper wrap)
                                  -> eq_orig { uo_actual = rho_a }
                                _ -> eq_orig

                        ; cow <- uType eq_orig' TypeLevel rho_a ty_expected
                        ; return (mkWpCastN cow <.> wrap) }


     -- use versions without synonyms expanded
    unify = mkWpCastN <$> uType eq_orig TypeLevel ty_actual ty_expected

-----------------
-- needs both un-type-checked (for origins) and type-checked (for wrapping)
-- expressions
tcWrapResult :: HsExpr GhcRn -> HsExpr GhcTcId -> TcSigmaType -> ExpRhoType
             -> TcM (HsExpr GhcTcId)
tcWrapResult rn_expr = tcWrapResultO (exprCtOrigin rn_expr) rn_expr

-- | Sometimes we don't have a @HsExpr Name@ to hand, and this is more
-- convenient.
tcWrapResultO :: CtOrigin -> HsExpr GhcRn -> HsExpr GhcTcId -> TcSigmaType -> ExpRhoType
               -> TcM (HsExpr GhcTcId)
tcWrapResultO orig rn_expr expr actual_ty res_ty
  = do { traceTc "tcWrapResult" (vcat [ text "Actual:  " <+> ppr actual_ty
                                      , text "Expected:" <+> ppr res_ty ])
       ; cow <- tcSubTypeDS_NC_O orig GenSigCtxt
                                 (Just rn_expr) actual_ty res_ty
       ; return (mkHsWrap cow expr) }

-----------------------------------
wrapFunResCoercion
        :: [TcType]        -- Type of args
        -> HsWrapper       -- HsExpr a -> HsExpr b
        -> TcM HsWrapper   -- HsExpr (arg_tys -> a) -> HsExpr (arg_tys -> b)
wrapFunResCoercion arg_tys co_fn_res
  | isIdHsWrapper co_fn_res
  = return idHsWrapper
  | null arg_tys
  = return co_fn_res
  | otherwise
  = do  { arg_ids <- newSysLocalIds (fsLit "sub") arg_tys
        ; return (mkWpLams arg_ids <.> co_fn_res <.> mkWpEvVarApps arg_ids) }


{- **********************************************************************
%*                                                                      *
            ExpType functions: tcInfer, fillInferResult
%*                                                                      *
%********************************************************************* -}

-- | Infer a type using a fresh ExpType
-- See also Note [ExpType] in TcMType
-- Does not attempt to instantiate the inferred type
tcInferNoInst :: (ExpSigmaType -> TcM a) -> TcM (a, TcSigmaType)
tcInferNoInst = tcInfer False

tcInferInst :: (ExpRhoType -> TcM a) -> TcM (a, TcRhoType)
tcInferInst = tcInfer True

tcInfer :: Bool -> (ExpSigmaType -> TcM a) -> TcM (a, TcSigmaType)
tcInfer instantiate tc_check
  = do { res_ty <- newInferExpType instantiate
       ; result <- tc_check res_ty
       ; res_ty <- readExpType res_ty
       ; return (result, res_ty) }

fillInferResult_Inst :: CtOrigin -> TcType -> InferResult -> TcM HsWrapper
-- If wrap = fillInferResult_Inst t1 t2
--    => wrap :: t1 ~> t2
-- See Note [Deep instantiation of InferResult]
fillInferResult_Inst orig ty inf_res@(IR { ir_inst = instantiate_me })
  | instantiate_me
  = do { (wrap, rho) <- deeplyInstantiate orig ty
       ; co <- fillInferResult rho inf_res
       ; return (mkWpCastN co <.> wrap) }

  | otherwise
  = do { co <- fillInferResult ty inf_res
       ; return (mkWpCastN co) }

fillInferResult :: TcType -> InferResult -> TcM TcCoercionN
-- If wrap = fillInferResult t1 t2
--    => wrap :: t1 ~> t2
fillInferResult orig_ty (IR { ir_uniq = u, ir_lvl = res_lvl
                            , ir_ref = ref })
  = do { (ty_co, ty_to_fill_with) <- promoteTcType res_lvl orig_ty

       ; traceTc "Filling ExpType" $
         ppr u <+> text ":=" <+> ppr ty_to_fill_with

       ; when debugIsOn (check_hole ty_to_fill_with)

       ; writeTcRef ref (Just ty_to_fill_with)

       ; return ty_co }
  where
    check_hole ty   -- Debug check only
      = do { let ty_lvl = tcTypeLevel ty
           ; MASSERT2( not (ty_lvl `strictlyDeeperThan` res_lvl),
                       ppr u $$ ppr res_lvl $$ ppr ty_lvl $$
                       ppr ty <+> ppr (typeKind ty) $$ ppr orig_ty )
           ; cts <- readTcRef ref
           ; case cts of
               Just already_there -> pprPanic "writeExpType"
                                       (vcat [ ppr u
                                             , ppr ty
                                             , ppr already_there ])
               Nothing -> return () }

{- Note [Deep instantiation of InferResult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In some cases we want to deeply instantiate before filling in
an InferResult, and in some cases not.  That's why InferReult
has the ir_inst flag.

* ir_inst = True: deeply instantiate

  Consider
    f x = (*)
  We want to instantiate the type of (*) before returning, else we
  will infer the type
    f :: forall {a}. a -> forall b. Num b => b -> b -> b
  This is surely confusing for users.

  And worse, the the monomorphism restriction won't properly. The MR is
  dealt with in simplifyInfer, and simplifyInfer has no way of
  instantiating. This could perhaps be worked around, but it may be
  hard to know even when instantiation should happen.

  Another reason.  Consider
       f :: (?x :: Int) => a -> a
       g y = let ?x = 3::Int in f
  Here want to instantiate f's type so that the ?x::Int constraint
  gets discharged by the enclosing implicit-parameter binding.

* ir_inst = False: do not instantiate

  Consider this (which uses visible type application):

    (let { f :: forall a. a -> a; f x = x } in f) @Int

  We'll call TcExpr.tcInferFun to infer the type of the (let .. in f)
  And we don't want to instantite the type of 'f' when we reach it,
  else the outer visible type application won't work
-}

{- *********************************************************************
*                                                                      *
              Promoting types
*                                                                      *
********************************************************************* -}

promoteTcType :: TcLevel -> TcType -> TcM (TcCoercion, TcType)
-- See Note [Promoting a type]
-- promoteTcType level ty = (co, ty')
--   * Returns ty'  whose max level is just 'level'
--             and  whose kind is ~# to the kind of 'ty'
--             and  whose kind has form TYPE rr
--   * and co :: ty ~ ty'
--   * and emits constraints to justify the coercion
promoteTcType dest_lvl ty
  = do { cur_lvl <- getTcLevel
       ; if (cur_lvl `sameDepthAs` dest_lvl)
         then dont_promote_it
         else promote_it }
  where
    promote_it :: TcM (TcCoercion, TcType)
    promote_it  -- Emit a constraint  (alpha :: TYPE rr) ~ ty
                -- where alpha and rr are fresh and from level dest_lvl
      = do { rr      <- newMetaTyVarTyAtLevel dest_lvl runtimeRepTy
           ; prom_ty <- newMetaTyVarTyAtLevel dest_lvl (tYPE rr)
           ; let eq_orig = TypeEqOrigin { uo_actual   = ty
                                        , uo_expected = prom_ty
                                        , uo_thing    = Nothing }

           ; co <- emitWantedEq eq_orig TypeLevel Nominal ty prom_ty
           ; return (co, prom_ty) }

    dont_promote_it :: TcM (TcCoercion, TcType)
    dont_promote_it  -- Check that ty :: TYPE rr, for some (fresh) rr
      = do { res_kind <- newOpenTypeKind
           ; let ty_kind = typeKind ty
                 kind_orig = TypeEqOrigin { uo_actual   = ty_kind
                                          , uo_expected = res_kind
                                          , uo_thing    = Nothing }
           ; ki_co <- uType kind_orig KindLevel (typeKind ty) res_kind
           ; let co = mkTcNomReflCo ty `mkTcCoherenceRightCo` ki_co
           ; return (co, ty `mkCastTy` ki_co) }

{- Note [Promoting a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #12427)

  data T where
    MkT :: (Int -> Int) -> a -> T

  h y = case y of MkT v w -> v

We'll infer the RHS type with an expected type ExpType of
  (IR { ir_lvl = l, ir_ref = ref, ... )
where 'l' is the TcLevel of the RHS of 'h'.  Then the MkT pattern
match will increase the level, so we'll end up in tcSubType, trying to
unify the type of v,
  v :: Int -> Int
with the expected type.  But this attempt takes place at level (l+1),
rightly so, since v's type could have mentioned existential variables,
(like w's does) and we want to catch that.

So we
  - create a new meta-var alpha[l+1]
  - fill in the InferRes ref cell 'ref' with alpha
  - emit an equality constraint, thus
        [W] alpha[l+1] ~ (Int -> Int)

That constraint will float outwards, as it should, unless v's
type mentions a skolem-captured variable.

This approach fails if v has a higher rank type; see
Note [Promotion and higher rank types]


Note [Promotion and higher rank types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If v had a higher-rank type, say v :: (forall a. a->a) -> Int,
then we'd emit an equality
        [W] alpha[l+1] ~ ((forall a. a->a) -> Int)
which will sadly fail because we can't unify a unification variable
with a polytype.  But there is nothing really wrong with the program
here.

We could just about solve this by "promote the type" of v, to expose
its polymorphic "shape" while still leaving constraints that will
prevent existential escape.  But we must be careful!  Exposing
the "shape" of the type is precisely what we must NOT do under
a GADT pattern match!  So in this case we might promote the type
to
        (forall a. a->a) -> alpha[l+1]
and emit the constraint
        [W] alpha[l+1] ~ Int
Now the poromoted type can fill the ref cell, while the emitted
equality can float or not, according to the usual rules.

But that's not quite right!  We are exposing the arrow! We could
deal with that too:
        (forall a. mu[l+1] a a) -> alpha[l+1]
with constraints
        [W] alpha[l+1] ~ Int
        [W] mu[l+1] ~ (->)
Here we abstract over the '->' inside the forall, in case that
is subject to an equality constraint from a GADT match.

Note that we kept the outer (->) because that's part of
the polymorphic "shape".  And becauuse of impredicativity,
GADT matches can't give equalities that affect polymorphic
shape.

This reasoning just seems too complicated, so I decided not
to do it.  These higher-rank notes are just here to record
the thinking.
-}

{- *********************************************************************
*                                                                      *
                    Generalisation
*                                                                      *
********************************************************************* -}

-- | Take an "expected type" and strip off quantifiers to expose the
-- type underneath, binding the new skolems for the @thing_inside@.
-- The returned 'HsWrapper' has type @specific_ty -> expected_ty@.
tcSkolemise :: UserTypeCtxt -> TcSigmaType
            -> ([TcTyVar] -> TcType -> TcM result)
         -- ^ These are only ever used for scoped type variables.
            -> TcM (HsWrapper, result)
        -- ^ The expression has type: spec_ty -> expected_ty

tcSkolemise ctxt expected_ty thing_inside
   -- We expect expected_ty to be a forall-type
   -- If not, the call is a no-op
  = do  { traceTc "tcSkolemise" Outputable.empty
        ; (wrap, tv_prs, given, rho') <- deeplySkolemise expected_ty

        ; lvl <- getTcLevel
        ; when debugIsOn $
              traceTc "tcSkolemise" $ vcat [
                ppr lvl,
                text "expected_ty" <+> ppr expected_ty,
                text "inst tyvars" <+> ppr tv_prs,
                text "given"       <+> ppr given,
                text "inst type"   <+> ppr rho' ]

        -- Generally we must check that the "forall_tvs" havn't been constrained
        -- The interesting bit here is that we must include the free variables
        -- of the expected_ty.  Here's an example:
        --       runST (newVar True)
        -- Here, if we don't make a check, we'll get a type (ST s (MutVar s Bool))
        -- for (newVar True), with s fresh.  Then we unify with the runST's arg type
        -- forall s'. ST s' a. That unifies s' with s, and a with MutVar s Bool.
        -- So now s' isn't unconstrained because it's linked to a.
        --
        -- However [Oct 10] now that the untouchables are a range of
        -- TcTyVars, all this is handled automatically with no need for
        -- extra faffing around

        ; let tvs' = map snd tv_prs
              skol_info = SigSkol ctxt expected_ty tv_prs

        ; (ev_binds, result) <- checkConstraints skol_info tvs' given $
                                thing_inside tvs' rho'

        ; return (wrap <.> mkWpLet ev_binds, result) }
          -- The ev_binds returned by checkConstraints is very
          -- often empty, in which case mkWpLet is a no-op

-- | Variant of 'tcSkolemise' that takes an ExpType
tcSkolemiseET :: UserTypeCtxt -> ExpSigmaType
              -> (ExpRhoType -> TcM result)
              -> TcM (HsWrapper, result)
tcSkolemiseET _ et@(Infer {}) thing_inside
  = (idHsWrapper, ) <$> thing_inside et
tcSkolemiseET ctxt (Check ty) thing_inside
  = tcSkolemise ctxt ty $ \_ -> thing_inside . mkCheckExpType

checkConstraints :: SkolemInfo
                 -> [TcTyVar]           -- Skolems
                 -> [EvVar]             -- Given
                 -> TcM result
                 -> TcM (TcEvBinds, result)

checkConstraints skol_info skol_tvs given thing_inside
  = do { (implics, ev_binds, result)
            <- buildImplication skol_info skol_tvs given thing_inside
       ; emitImplications implics
       ; return (ev_binds, result) }

buildImplication :: SkolemInfo
                 -> [TcTyVar]           -- Skolems
                 -> [EvVar]             -- Given
                 -> TcM result
                 -> TcM (Bag Implication, TcEvBinds, result)
buildImplication skol_info skol_tvs given thing_inside
  = do { tc_lvl <- getTcLevel
       ; deferred_type_errors <- goptM Opt_DeferTypeErrors <||>
                                 goptM Opt_DeferTypedHoles
       ; if null skol_tvs && null given && (not deferred_type_errors ||
                                            not (isTopTcLevel tc_lvl))
         then do { res <- thing_inside
                 ; return (emptyBag, emptyTcEvBinds, res) }
      -- Fast path.  We check every function argument with
      -- tcPolyExpr, which uses tcSkolemise and hence checkConstraints.
      -- But with the solver producing unlifted equalities, we need
      -- to have an EvBindsVar for them when they might be deferred to
      -- runtime. Otherwise, they end up as top-level unlifted bindings,
      -- which are verboten. See also Note [Deferred errors for coercion holes]
      -- in TcErrors.
         else
    do { (tclvl, wanted, result) <- pushLevelAndCaptureConstraints thing_inside
       ; (implics, ev_binds) <- buildImplicationFor tclvl skol_info skol_tvs given wanted
       ; return (implics, ev_binds, result) }}

buildImplicationFor :: TcLevel -> SkolemInfo -> [TcTyVar]
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
  = ASSERT2( all isSkolemTyVar skol_tvs, ppr skol_tvs )
    do { ev_binds_var <- newTcEvBinds
       ; env <- getLclEnv
       ; let implic = Implic { ic_tclvl = tclvl
                             , ic_skols = skol_tvs
                             , ic_no_eqs = False
                             , ic_given = given
                             , ic_wanted = wanted
                             , ic_status  = IC_Unsolved
                             , ic_binds = ev_binds_var
                             , ic_env = env
                             , ic_needed = emptyVarSet
                             , ic_info = skol_info }

       ; return (unitBag implic, TcEvBinds ev_binds_var) }

{-
************************************************************************
*                                                                      *
                Boxy unification
*                                                                      *
************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.
-}

unifyType :: Maybe (HsExpr GhcRn)   -- ^ If present, has type 'ty1'
          -> TcTauType -> TcTauType -> TcM TcCoercionN
-- Actual and expected types
-- Returns a coercion : ty1 ~ ty2
unifyType thing ty1 ty2 = traceTc "utype" (ppr ty1 $$ ppr ty2 $$ ppr thing) >>
                          uType origin TypeLevel ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual = ty1, uo_expected = ty2
                          , uo_thing  = ppr <$> thing }

unifyKind :: Maybe (HsType GhcRn) -> TcKind -> TcKind -> TcM CoercionN
unifyKind thing ty1 ty2 = traceTc "ukind" (ppr ty1 $$ ppr ty2 $$ ppr thing) >>
                          uType origin KindLevel ty1 ty2
  where origin = TypeEqOrigin { uo_actual = ty1, uo_expected = ty2
                              , uo_thing  = ppr <$> thing }

---------------
unifyPred :: PredType -> PredType -> TcM TcCoercionN
-- Actual and expected types
unifyPred = unifyType Nothing

---------------
unifyTheta :: TcThetaType -> TcThetaType -> TcM [TcCoercionN]
-- Actual and expected types
unifyTheta theta1 theta2
  = do  { checkTc (equalLength theta1 theta2)
                  (vcat [text "Contexts differ in length",
                         nest 2 $ parens $ text "Use RelaxedPolyRec to allow this"])
        ; zipWithM unifyPred theta1 theta2 }

{-
%************************************************************************
%*                                                                      *
                 uType and friends
%*                                                                      *
%************************************************************************

uType is the heart of the unifier.
-}

uType, uType_defer
  :: CtOrigin
  -> TypeOrKind
  -> TcType    -- ty1 is the *actual* type
  -> TcType    -- ty2 is the *expected* type
  -> TcM Coercion

--------------
-- It is always safe to defer unification to the main constraint solver
-- See Note [Deferred unification]
uType_defer origin t_or_k ty1 ty2
  = do { co <- emitWantedEq origin t_or_k Nominal ty1 ty2

       -- Error trace only
       -- NB. do *not* call mkErrInfo unless tracing is on,
       --     because it is hugely expensive (#5631)
       ; whenDOptM Opt_D_dump_tc_trace $ do
            { ctxt <- getErrCtxt
            ; doc <- mkErrInfo emptyTidyEnv ctxt
            ; traceTc "utype_defer" (vcat [ppr co, ppr ty1,
                                           ppr ty2, pprCtOrigin origin, doc])
            }
       ; return co }

--------------
uType origin t_or_k orig_ty1 orig_ty2
  = do { tclvl <- getTcLevel
       ; traceTc "u_tys" $ vcat
              [ text "tclvl" <+> ppr tclvl
              , sep [ ppr orig_ty1, text "~", ppr orig_ty2]
              , pprCtOrigin origin]
       ; co <- go orig_ty1 orig_ty2
       ; if isReflCo co
            then traceTc "u_tys yields no coercion" Outputable.empty
            else traceTc "u_tys yields coercion:" (ppr co)
       ; return co }
  where
    go :: TcType -> TcType -> TcM Coercion
        -- The arguments to 'go' are always semantically identical
        -- to orig_ty{1,2} except for looking through type synonyms

        -- Variables; go for uVar
        -- Note that we pass in *original* (before synonym expansion),
        -- so that type variables tend to get filled in with
        -- the most informative version of the type
    go (TyVarTy tv1) ty2
      = do { lookup_res <- lookupTcTyVar tv1
           ; case lookup_res of
               Filled ty1   -> do { traceTc "found filled tyvar" (ppr tv1 <+> text ":->" <+> ppr ty1)
                                  ; go ty1 ty2 }
               Unfilled _ -> uUnfilledVar origin t_or_k NotSwapped tv1 ty2 }
    go ty1 (TyVarTy tv2)
      = do { lookup_res <- lookupTcTyVar tv2
           ; case lookup_res of
               Filled ty2   -> do { traceTc "found filled tyvar" (ppr tv2 <+> text ":->" <+> ppr ty2)
                                  ; go ty1 ty2 }
               Unfilled _ -> uUnfilledVar origin t_or_k IsSwapped tv2 ty1 }

      -- See Note [Expanding synonyms during unification]
    go ty1@(TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = return $ mkReflCo Nominal ty1

        -- See Note [Expanding synonyms during unification]
        --
        -- Also NB that we recurse to 'go' so that we don't push a
        -- new item on the origin stack. As a result if we have
        --   type Foo = Int
        -- and we try to unify  Foo ~ Bool
        -- we'll end up saying "can't match Foo with Bool"
        -- rather than "can't match "Int with Bool".  See Trac #4535.
    go ty1 ty2
      | Just ty1' <- tcView ty1 = go ty1' ty2
      | Just ty2' <- tcView ty2 = go ty1  ty2'

    go (CastTy t1 co1) t2
      = do { co_tys <- go t1 t2
           ; return (mkCoherenceLeftCo co_tys co1) }

    go t1 (CastTy t2 co2)
      = do { co_tys <- go t1 t2
           ; return (mkCoherenceRightCo co_tys co2) }

        -- Functions (or predicate functions) just check the two parts
    go (FunTy fun1 arg1) (FunTy fun2 arg2)
      = do { co_l <- uType origin t_or_k fun1 fun2
           ; co_r <- uType origin t_or_k arg1 arg2
           ; return $ mkFunCo Nominal co_l co_r }

        -- Always defer if a type synonym family (type function)
        -- is involved.  (Data families behave rigidly.)
    go ty1@(TyConApp tc1 _) ty2
      | isTypeFamilyTyCon tc1 = defer ty1 ty2
    go ty1 ty2@(TyConApp tc2 _)
      | isTypeFamilyTyCon tc2 = defer ty1 ty2

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      -- See Note [Mismatched type lists and application decomposition]
      | tc1 == tc2, equalLength tys1 tys2
      = ASSERT2( isGenerativeTyCon tc1 Nominal, ppr tc1 )
        do { cos <- zipWithM (uType origin t_or_k) tys1 tys2
           ; return $ mkTyConAppCo Nominal tc1 cos }

    go (LitTy m) ty@(LitTy n)
      | m == n
      = return $ mkNomReflCo ty

        -- See Note [Care with type applications]
        -- Do not decompose FunTy against App;
        -- it's often a type error, so leave it for the constraint solver
    go (AppTy s1 t1) (AppTy s2 t2)
      = go_app s1 t1 s2 t2

    go (AppTy s1 t1) (TyConApp tc2 ts2)
      | Just (ts2', t2') <- snocView ts2
      = ASSERT( mightBeUnsaturatedTyCon tc2 )
        go_app s1 t1 (TyConApp tc2 ts2') t2'

    go (TyConApp tc1 ts1) (AppTy s2 t2)
      | Just (ts1', t1') <- snocView ts1
      = ASSERT( mightBeUnsaturatedTyCon tc1 )
        go_app (TyConApp tc1 ts1') t1' s2 t2

    go (CoercionTy co1) (CoercionTy co2)
      = do { let ty1 = coercionType co1
                 ty2 = coercionType co2
           ; kco <- uType (KindEqOrigin orig_ty1 (Just orig_ty2) origin
                                        (Just t_or_k))
                          KindLevel
                          ty1 ty2
           ; return $ mkProofIrrelCo Nominal kco co1 co2 }

        -- Anything else fails
        -- E.g. unifying for-all types, which is relative unusual
    go ty1 ty2 = defer ty1 ty2

    ------------------
    defer ty1 ty2   -- See Note [Check for equality before deferring]
      | ty1 `tcEqType` ty2 = return (mkNomReflCo ty1)
      | otherwise          = uType_defer origin t_or_k ty1 ty2

    ------------------
    go_app s1 t1 s2 t2
      = do { co_s <- uType origin t_or_k s1 s2
           ; co_t <- uType origin t_or_k t1 t2
           ; return $ mkAppCo co_s co_t }

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
This came up in Trac #3950.

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

 * We expand *before* the TyConApp case.  For example, if we have
      type Phantom a = Int
   and are unifying
      Phantom Int ~ Phantom Char
   it is *wrong* to unify Int and Char.

 * The problem case immediately above can happen only with arguments
   to the tycon. So we check for nullary tycons *before* expanding.
   This is particularly helpful when checking (* ~ *), because * is
   now a type synonym.

Note [Deferred Unification]
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
                 uVar and friends
*                                                                      *
************************************************************************

@uVar@ is called when at least one of the types being unified is a
variable.  It does {\em not} assume that the variable is a fixed point
of the substitution; rather, notice that @uVar@ (defined below) nips
back into @uTys@ if it turns out that the variable is already bound.
-}

----------
uUnfilledVar :: CtOrigin
             -> TypeOrKind
             -> SwapFlag
             -> TcTyVar        -- Tyvar 1
             -> TcTauType      -- Type 2
             -> TcM Coercion
-- "Unfilled" means that the variable is definitely not a filled-in meta tyvar
--            It might be a skolem, or untouchable, or meta

uUnfilledVar origin t_or_k swapped tv1 ty2
  = do { ty2 <- zonkTcType ty2
             -- Zonk to expose things to the
             -- occurs check, and so that if ty2
             -- looks like a type variable then it
             -- /is/ a type variable
       ; uUnfilledVar1 origin t_or_k swapped tv1 ty2 }

----------
uUnfilledVar1 :: CtOrigin
              -> TypeOrKind
              -> SwapFlag
              -> TcTyVar        -- Tyvar 1
              -> TcTauType      -- Type 2, zonked
              -> TcM Coercion
uUnfilledVar1 origin t_or_k swapped tv1 ty2
  | Just tv2 <- tcGetTyVar_maybe ty2
  = go tv2

  | otherwise
  = uUnfilledVar2 origin t_or_k swapped tv1 ty2

  where
    -- 'go' handles the case where both are
    -- tyvars so we might want to swap
    go tv2 | tv1 == tv2  -- Same type variable => no-op
           = return (mkNomReflCo (mkTyVarTy tv1))

           | swapOverTyVars tv1 tv2   -- Distinct type variables
           = uUnfilledVar2 origin t_or_k (flipSwap swapped)
                           tv2 (mkTyVarTy tv1)

           | otherwise
           = uUnfilledVar2 origin t_or_k swapped tv1 ty2

----------
uUnfilledVar2 :: CtOrigin
              -> TypeOrKind
              -> SwapFlag
              -> TcTyVar        -- Tyvar 1
              -> TcTauType      -- Type 2, zonked
              -> TcM Coercion
uUnfilledVar2 origin t_or_k swapped tv1 ty2
  = do { dflags  <- getDynFlags
       ; cur_lvl <- getTcLevel
       ; go dflags cur_lvl }
  where
    go dflags cur_lvl
      | canSolveByUnification cur_lvl tv1 ty2
      , Just ty2' <- metaTyVarUpdateOK dflags tv1 ty2
      = do { co_k <- uType kind_origin KindLevel (typeKind ty2') (tyVarKind tv1)
           ; if isTcReflCo co_k  -- only proceed if the kinds matched.

             then do { writeMetaTyVar tv1 ty2'
                     ; return (mkTcNomReflCo ty2') }
             else defer } -- this cannot be solved now.
                          -- See Note [Equalities with incompatible kinds]
                          -- in TcCanonical

      | otherwise
      = defer
               -- Occurs check or an untouchable: just defer
               -- NB: occurs check isn't necessarily fatal:
               --     eg tv1 occured in type family parameter

    ty1 = mkTyVarTy tv1
    kind_origin = KindEqOrigin ty1 (Just ty2) origin (Just t_or_k)

    defer = unSwap swapped (uType_defer origin t_or_k) ty1 ty2

swapOverTyVars :: TcTyVar -> TcTyVar -> Bool
swapOverTyVars tv1 tv2
  | isFmvTyVar tv1 = False  -- See Note [Fmv Orientation Invariant]
  | isFmvTyVar tv2 = True

  | Just lvl1 <- metaTyVarTcLevel_maybe tv1
      -- If tv1 is touchable, swap only if tv2 is also
      -- touchable and it's strictly better to update the latter
      -- But see Note [Avoid unnecessary swaps]
  = case metaTyVarTcLevel_maybe tv2 of
      Nothing   -> False
      Just lvl2 | lvl2 `strictlyDeeperThan` lvl1 -> True
                | lvl1 `strictlyDeeperThan` lvl2 -> False
                | otherwise                      -> nicer_to_update tv2

  -- So tv1 is not a meta tyvar
  -- If only one is a meta tyvar, put it on the left
  -- This is not because it'll be solved; but because
  -- the floating step looks for meta tyvars on the left
  | isMetaTyVar tv2 = True

  -- So neither is a meta tyvar (including FlatMetaTv)

  -- If only one is a flatten skolem, put it on the left
  -- See Note [Eliminate flat-skols]
  | not (isFlattenTyVar tv1), isFlattenTyVar tv2 = True

  | otherwise = False

  where
    nicer_to_update tv2
      =  (isSigTyVar tv1                 && not (isSigTyVar tv2))
      || (isSystemName (Var.varName tv2) && not (isSystemName (Var.varName tv1)))

-- @trySpontaneousSolve wi@ solves equalities where one side is a
-- touchable unification variable.
-- Returns True <=> spontaneous solve happened
canSolveByUnification :: TcLevel -> TcTyVar -> TcType -> Bool
canSolveByUnification tclvl tv xi
  | isTouchableMetaTyVar tclvl tv
  = case metaTyVarInfo tv of
      SigTv -> is_tyvar xi
      _     -> True

  | otherwise    -- Untouchable
  = False
  where
    is_tyvar xi
      = case tcGetTyVar_maybe xi of
          Nothing -> False
          Just tv -> case tcTyVarDetails tv of
                       MetaTv { mtv_info = info }
                                   -> case info of
                                        SigTv -> True
                                        _     -> False
                       SkolemTv {} -> True
                       RuntimeUnk  -> True

{- Note [Fmv Orientation Invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * We always orient a constraint
        fmv ~ alpha
     with fmv on the left, even if alpha is
     a touchable unification variable

Reason: doing it the other way round would unify alpha:=fmv, but that
really doesn't add any info to alpha.  But a later constraint alpha ~
Int might unlock everything.  Comment:9 of #12526 gives a detailed
example.

WARNING: I've gone to and fro on this one several times.
I'm now pretty sure that unifying alpha:=fmv is a bad idea!
So orienting with fmvs on the left is a good thing.

This example comes from IndTypesPerfMerge. (Others include
T10226, T10009.)
    From the ambiguity check for
      f :: (F a ~ a) => a
    we get:
          [G] F a ~ a
          [WD] F alpha ~ alpha, alpha ~ a

    From Givens we get
          [G] F a ~ fsk, fsk ~ a

    Now if we flatten we get
          [WD] alpha ~ fmv, F alpha ~ fmv, alpha ~ a

    Now, if we unified alpha := fmv, we'd get
          [WD] F fmv ~ fmv, [WD] fmv ~ a
    And now we are stuck.

So instead the Fmv Orientation Invariant puts te fmv on the
left, giving
      [WD] fmv ~ alpha, [WD] F alpha ~ fmv, [WD] alpha ~ a

    Now we get alpha:=a, and everything works out

Note [Prevent unification with type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We prevent unification with type families because of an uneasy compromise.
It's perfectly sound to unify with type families, and it even improves the
error messages in the testsuite. It also modestly improves performance, at
least in some cases. But it's disastrous for test case perf/compiler/T3064.
Here is the problem: Suppose we have (F ty) where we also have [G] F ty ~ a.
What do we do? Do we reduce F? Or do we use the given? Hard to know what's
best. GHC reduces. This is a disaster for T3064, where the type's size
spirals out of control during reduction. (We're not helped by the fact that
the flattener re-flattens all the arguments every time around.) If we prevent
unification with type families, then the solver happens to use the equality
before expanding the type family.

It would be lovely in the future to revisit this problem and remove this
extra, unnecessary check. But we retain it for now as it seems to work
better in practice.

Note [Refactoring hazard: checkTauTvUpdate]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I (Richard E.) have a sad story about refactoring this code, retained here
to prevent others (or a future me!) from falling into the same traps.

It all started with #11407, which was caused by the fact that the TyVarTy
case of defer_me didn't look in the kind. But it seemed reasonable to
simply remove the defer_me check instead.

It referred to two Notes (since removed) that were out of date, and the
fast_check code in occurCheckExpand seemed to do just about the same thing as
defer_me. The one piece that defer_me did that wasn't repeated by
occurCheckExpand was the type-family check. (See Note [Prevent unification
with type families].) So I checked the result of occurCheckExpand for any
type family occurrences and deferred if there were any. This was done
in commit e9bf7bb5cc9fb3f87dd05111aa23da76b86a8967 .

This approach turned out not to be performant, because the expanded
type was bigger than the original type, and tyConsOfType (needed to
see if there are any type family occurrences) looks through type
synonyms. So it then struck me that we could dispense with the
defer_me check entirely. This simplified the code nicely, and it cut
the allocations in T5030 by half. But, as documented in Note [Prevent
unification with type families], this destroyed performance in
T3064. Regardless, I missed this regression and the change was
committed as 3f5d1a13f112f34d992f6b74656d64d95a3f506d .

Bottom lines:
 * defer_me is back, but now fixed w.r.t. #11407.
 * Tread carefully before you start to refactor here. There can be
   lots of hard-to-predict consequences.

Note [Type synonyms and the occur check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking we try to update a variable with type synonyms not
expanded, which improves later error messages, unless looking
inside a type synonym may help resolve a spurious occurs check
error. Consider:
          type A a = ()

          f :: (A a -> a -> ()) -> ()
          f = \ _ -> ()

          x :: ()
          x = f (\ x p -> p x)

We will eventually get a constraint of the form t ~ A t. The ok function above will
properly expand the type (A t) to just (), which is ok to be unified with t. If we had
unified with the original type A t, we would lead the type checker into an infinite loop.

Hence, if the occurs check fails for a type synonym application, then (and *only* then),
the ok function expands the synonym to detect opportunities for occurs check success using
the underlying definition of the type synonym.

The same applies later on in the constraint interaction code; see TcInteract,
function @occ_check_ok@.

Note [Non-TcTyVars in TcUnify]
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
with "tc" in TcTyClsDecls"). By this time, all the kind-vars are proper
TyVars, not TcTyVars, get some kind unification must happen.

Thus, we always check if a TyVar is a TcTyVar before asking if it's a
meta-tyvar.

This used to not be necessary for type-checking (that is, before * :: *)
because expressions get desugared via an algorithm separate from
type-checking (with wrappers, etc.). Types get desugared very differently,
causing this wibble in behavior seen here.
-}

data LookupTyVarResult  -- The result of a lookupTcTyVar call
  = Unfilled TcTyVarDetails     -- SkolemTv or virgin MetaTv
  | Filled   TcType

lookupTcTyVar :: TcTyVar -> TcM LookupTyVarResult
lookupTcTyVar tyvar
  | MetaTv { mtv_ref = ref } <- details
  = do { meta_details <- readMutVar ref
       ; case meta_details of
           Indirect ty -> return (Filled ty)
           Flexi -> do { is_touchable <- isTouchableTcM tyvar
                             -- Note [Unifying untouchables]
                       ; if is_touchable then
                            return (Unfilled details)
                         else
                            return (Unfilled vanillaSkolemTv) } }
  | otherwise
  = return (Unfilled details)
  where
    details = tcTyVarDetails tyvar

{-
Note [Unifying untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat an untouchable type variable as if it was a skolem.  That
ensures it won't unify with anything.  It's a slight had, because
we return a made-up TcTyVarDetails, but I think it works smoothly.
-}

-- | Breaks apart a function kind into its pieces.
matchExpectedFunKind :: Outputable fun
                     => fun             -- ^ type, only for errors
                     -> TcKind          -- ^ function kind
                     -> TcM (Coercion, TcKind, TcKind)
                                  -- ^ co :: old_kind ~ arg -> res
matchExpectedFunKind hs_ty = go
  where
    go k | Just k' <- tcView k = go k'

    go k@(TyVarTy kvar)
      | isTcTyVar kvar, isMetaTyVar kvar
      = do { maybe_kind <- readMetaTyVar kvar
           ; case maybe_kind of
                Indirect fun_kind -> go fun_kind
                Flexi ->             defer k }

    go k@(FunTy arg res) = return (mkNomReflCo k, arg, res)
    go other             = defer other

    defer k
      = do { arg_kind <- newMetaKindVar
           ; res_kind <- newMetaKindVar
           ; let new_fun = mkFunTy arg_kind res_kind
                 origin  = TypeEqOrigin { uo_actual   = k
                                        , uo_expected = new_fun
                                        , uo_thing    = Just (ppr hs_ty)
                                        }
           ; co <- uType origin KindLevel k new_fun
           ; return (co, arg_kind, res_kind) }


{- *********************************************************************
*                                                                      *
                 Occurrence checking
*                                                                      *
********************************************************************* -}


{- Note [Occurs check expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(occurCheckExpand tv xi) expands synonyms in xi just enough to get rid
of occurrences of tv outside type function arguments, if that is
possible; otherwise, it returns Nothing.

For example, suppose we have
  type F a b = [a]
Then
  occCheckExpand b (F Int b) = Just [Int]
but
  occCheckExpand a (F a Int) = Nothing

We don't promise to do the absolute minimum amount of expanding
necessary, but we try not to do expansions we don't need to.  We
prefer doing inner expansions first.  For example,
  type F a b = (a, Int, a, [a])
  type G b   = Char
We have
  occCheckExpand b (F (G b)) = Just (F Char)
even though we could also expand F to get rid of b.

The two variants of the function are to support TcUnify.checkTauTvUpdate,
which wants to prevent unification with type families. For more on this
point, see Note [Prevent unification with type families] in TcUnify.

Note [Occurrence checking: look inside kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are considering unifying
   (alpha :: *)  ~  Int -> (beta :: alpha -> alpha)
This may be an error (what is that alpha doing inside beta's kind?),
but we must not make the mistake of actuallyy unifying or we'll
build an infinite data structure.  So when looking for occurrences
of alpha in the rhs, we must look in the kinds of type variables
that occur there.

NB: we may be able to remove the problem via expansion; see
    Note [Occurs check expansion].  So we have to try that.

Note [Checking for foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unless we have -XImpredicativeTypes (which is a totally unsupported
feature), we do not want to unify
    alpha ~ (forall a. a->a) -> Int
So we look for foralls hidden inside the type, and it's convenient
to do that at the same time as the occurs check (which looks for
occurrences of alpha).

However, it's not just a question of looking for foralls /anywhere/!
Consider
   (alpha :: forall k. k->*)  ~  (beta :: forall k. k->*)
This is legal; e.g. dependent/should_compile/T11635.

We don't want to reject it because of the forall in beta's kind,
but (see Note [Occurrence checking: look inside kinds]) we do
need to look in beta's kind.  So we carry a flag saying if a 'forall'
is OK, and sitch the flag on when stepping inside a kind.

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

-}

data OccCheckResult a
  = OC_OK a
  | OC_Bad     -- Forall or type family
  | OC_Occurs

instance Functor OccCheckResult where
      fmap = liftM

instance Applicative OccCheckResult where
      pure = OC_OK
      (<*>) = ap

instance Monad OccCheckResult where
  OC_OK x    >>= k = k x
  OC_Bad     >>= _ = OC_Bad
  OC_Occurs  >>= _ = OC_Occurs

occCheckForErrors :: DynFlags -> TcTyVar -> Type -> OccCheckResult ()
-- Just for error-message generation; so we return OccCheckResult
-- so the caller can report the right kind of error
-- Check whether
--   a) the given variable occurs in the given type.
--   b) there is a forall in the type (unless we have -XImpredicativeTypes)
occCheckForErrors dflags tv ty
  = case preCheck dflags True tv ty of
      OC_OK _   -> OC_OK ()
      OC_Bad    -> OC_Bad
      OC_Occurs -> case occCheckExpand tv ty of
                     Nothing -> OC_Occurs
                     Just _  -> OC_OK ()

----------------
metaTyVarUpdateOK :: DynFlags
                  -> TcTyVar             -- tv :: k1
                  -> TcType              -- ty :: k2
                  -> Maybe TcType        -- possibly-expanded ty
-- (metaTyFVarUpdateOK tv ty)
-- We are about to update the meta-tyvar tv with ty
-- Check (a) that tv doesn't occur in ty (occurs check)
--       (b) that ty does not have any foralls
--           (in the impredicative case), or type functions
--
-- We have two possible outcomes:
-- (1) Return the type to update the type variable with,
--        [we know the update is ok]
-- (2) Return Nothing,
--        [the update might be dodgy]
--
-- Note that "Nothing" does not mean "definite error".  For example
--   type family F a
--   type instance F Int = Int
-- consider
--   a ~ F a
-- This is perfectly reasonable, if we later get a ~ Int.  For now, though,
-- we return Nothing, leaving it to the later constraint simplifier to
-- sort matters out.
--
-- See Note [Refactoring hazard: checkTauTvUpdate]

metaTyVarUpdateOK dflags tv ty
  = case preCheck dflags False tv ty of
         -- False <=> type families not ok
         -- See Note [Prevent unification with type families]
      OC_OK _   -> Just ty
      OC_Bad    -> Nothing  -- forall or type function
      OC_Occurs -> occCheckExpand tv ty

preCheck :: DynFlags -> Bool -> TcTyVar -> TcType -> OccCheckResult ()
-- A quick check for
--   (a) a forall type (unless -XImpredivativeTypes)
--   (b) a type family
--   (c) an occurrence of the type variable (occurs check)
--
-- For (a) and (b) we check only the top level of the type, NOT
-- inside the kinds of variables it mentions.  But for (c) we do
-- look in the kinds of course.

preCheck dflags ty_fam_ok tv ty
  = fast_check ty
  where
    details          = tcTyVarDetails tv
    impredicative_ok = canUnifyWithPolyType dflags details

    ok :: OccCheckResult ()
    ok = OC_OK ()

    fast_check :: TcType -> OccCheckResult ()
    fast_check (TyVarTy tv')
      | tv == tv' = OC_Occurs
      | otherwise = fast_check_occ (tyVarKind tv')
           -- See Note [Occurrence checking: look inside kinds]

    fast_check (TyConApp tc tys)
      | bad_tc tc              = OC_Bad
      | otherwise              = mapM fast_check tys >> ok
    fast_check (LitTy {})      = ok
    fast_check (FunTy a r)     = fast_check a   >> fast_check r
    fast_check (AppTy fun arg) = fast_check fun >> fast_check arg
    fast_check (CastTy ty co)  = fast_check ty  >> fast_check_co co
    fast_check (CoercionTy co) = fast_check_co co
    fast_check (ForAllTy (TvBndr tv' _) ty)
       | not impredicative_ok = OC_Bad
       | tv == tv'            = ok
       | otherwise = do { fast_check_occ (tyVarKind tv')
                        ; fast_check_occ ty }
       -- Under a forall we look only for occurrences of
       -- the type variable

     -- For kinds, we only do an occurs check; we do not worry
     -- about type families or foralls
     -- See Note [Checking for foralls]
    fast_check_occ k | tv `elemVarSet` tyCoVarsOfType k = OC_Occurs
                     | otherwise                        = ok

     -- For coercions, we are only doing an occurs check here;
     -- no bother about impredicativity in coercions, as they're
     -- inferred
    fast_check_co co | tv `elemVarSet` tyCoVarsOfCo co = OC_Occurs
                     | otherwise                       = ok

    bad_tc :: TyCon -> Bool
    bad_tc tc
      | not (impredicative_ok || isTauTyCon tc)     = True
      | not (ty_fam_ok        || isFamFreeTyCon tc) = True
      | otherwise                                   = False

occCheckExpand :: TcTyVar -> TcType -> Maybe TcType
-- See Note [Occurs check expansion]
-- We may have needed to do some type synonym unfolding in order to
-- get rid of the variable (or forall), so we also return the unfolded
-- version of the type, which is guaranteed to be syntactically free
-- of the given type variable.  If the type is already syntactically
-- free of the variable, then the same type is returned.
occCheckExpand tv ty
  = go emptyVarEnv ty
  where
    go :: VarEnv TyVar -> Type -> Maybe Type
          -- The VarEnv carries mappings necessary
          -- because of kind expansion
    go env (TyVarTy tv')
      | tv == tv'                         = Nothing
      | Just tv'' <- lookupVarEnv env tv' = return (mkTyVarTy tv'')
      | otherwise                         = do { k' <- go env (tyVarKind tv')
                                               ; return (mkTyVarTy $
                                                         setTyVarKind tv' k') }
           -- See Note [Occurrence checking: look inside kinds]

    go _   ty@(LitTy {}) = return ty
    go env (AppTy ty1 ty2) = do { ty1' <- go env ty1
                                ; ty2' <- go env ty2
                                ; return (mkAppTy ty1' ty2') }
    go env (FunTy ty1 ty2) = do { ty1' <- go env ty1
                                ; ty2' <- go env ty2
                                ; return (mkFunTy ty1' ty2') }
    go env ty@(ForAllTy (TvBndr tv' vis) body_ty)
       | tv == tv'         = return ty
       | otherwise         = do { ki' <- go env (tyVarKind tv')
                                ; let tv'' = setTyVarKind tv' ki'
                                      env' = extendVarEnv env tv' tv''
                                ; body' <- go env' body_ty
                                ; return (ForAllTy (TvBndr tv'' vis) body') }

    -- For a type constructor application, first try expanding away the
    -- offending variable from the arguments.  If that doesn't work, next
    -- see if the type constructor is a type synonym, and if so, expand
    -- it and try again.
    go env ty@(TyConApp tc tys)
      = case mapM (go env) tys of
          Just tys' -> return (mkTyConApp tc tys')
          Nothing | Just ty' <- tcView ty -> go env ty'
                  | otherwise             -> Nothing
                      -- Failing that, try to expand a synonym

    go env (CastTy ty co) =  do { ty' <- go env ty
                                ; co' <- go_co env co
                                ; return (mkCastTy ty' co') }
    go env (CoercionTy co) = do { co' <- go_co env co
                                ; return (mkCoercionTy co') }

    ------------------
    go_co env (Refl r ty)               = do { ty' <- go env ty
                                             ; return (mkReflCo r ty') }
      -- Note: Coercions do not contain type synonyms
    go_co env (TyConAppCo r tc args)    = do { args' <- mapM (go_co env) args
                                             ; return (mkTyConAppCo r tc args') }
    go_co env (AppCo co arg)            = do { co' <- go_co env co
                                             ; arg' <- go_co env arg
                                             ; return (mkAppCo co' arg') }
    go_co env co@(ForAllCo tv' kind_co body_co)
      | tv == tv'         = return co
      | otherwise         = do { kind_co' <- go_co env kind_co
                               ; let tv'' = setTyVarKind tv' $
                                            pFst (coercionKind kind_co')
                                     env' = extendVarEnv env tv' tv''
                               ; body' <- go_co env' body_co
                               ; return (ForAllCo tv'' kind_co' body') }
    go_co env (FunCo r co1 co2)         = do { co1' <- go_co env co1
                                             ; co2' <- go_co env co2
                                             ; return (mkFunCo r co1' co2') }
    go_co env (CoVarCo c)               = do { k' <- go env (varType c)
                                             ; return (mkCoVarCo (setVarType c k')) }
    go_co env (AxiomInstCo ax ind args) = do { args' <- mapM (go_co env) args
                                             ; return (mkAxiomInstCo ax ind args') }
    go_co env (UnivCo p r ty1 ty2)      = do { p' <- go_prov env p
                                             ; ty1' <- go env ty1
                                             ; ty2' <- go env ty2
                                             ; return (mkUnivCo p' r ty1' ty2') }
    go_co env (SymCo co)                = do { co' <- go_co env co
                                             ; return (mkSymCo co') }
    go_co env (TransCo co1 co2)         = do { co1' <- go_co env co1
                                             ; co2' <- go_co env co2
                                             ; return (mkTransCo co1' co2') }
    go_co env (NthCo n co)              = do { co' <- go_co env co
                                             ; return (mkNthCo n co') }
    go_co env (LRCo lr co)              = do { co' <- go_co env co
                                             ; return (mkLRCo lr co') }
    go_co env (InstCo co arg)           = do { co' <- go_co env co
                                             ; arg' <- go_co env arg
                                             ; return (mkInstCo co' arg') }
    go_co env (CoherenceCo co1 co2)     = do { co1' <- go_co env co1
                                             ; co2' <- go_co env co2
                                             ; return (mkCoherenceCo co1' co2') }
    go_co env (KindCo co)               = do { co' <- go_co env co
                                             ; return (mkKindCo co') }
    go_co env (SubCo co)                = do { co' <- go_co env co
                                             ; return (mkSubCo co') }
    go_co env (AxiomRuleCo ax cs)       = do { cs' <- mapM (go_co env) cs
                                             ; return (mkAxiomRuleCo ax cs') }

    ------------------
    go_prov _   UnsafeCoerceProv    = return UnsafeCoerceProv
    go_prov env (PhantomProv co)    = PhantomProv <$> go_co env co
    go_prov env (ProofIrrelProv co) = ProofIrrelProv <$> go_co env co
    go_prov _   p@(PluginProv _)    = return p
    go_prov _   p@(HoleProv _)      = return p

canUnifyWithPolyType :: DynFlags -> TcTyVarDetails -> Bool
canUnifyWithPolyType dflags details
  = case details of
      MetaTv { mtv_info = SigTv }    -> False
      MetaTv { mtv_info = TauTv }    -> xopt LangExt.ImpredicativeTypes dflags
      _other                         -> True
          -- We can have non-meta tyvars in given constraints
