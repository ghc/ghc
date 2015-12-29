{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Type subsumption and unification
-}

{-# LANGUAGE CPP, MultiWayIf, TupleSections #-}

module TcUnify (
  -- Full-blown subsumption
  tcWrapResult, tcWrapResultO, tcSkolemise,
  tcSubTypeHR, tcSubType, tcSubType_NC, tcSubTypeDS, tcSubTypeDS_O,
  tcSubTypeDS_NC, tcSubTypeDS_NC_O,
  checkConstraints, buildImplication, buildImplicationFor,

  -- Various unifications
  unifyType_, unifyType, unifyTheta, unifyKind, noThing,
  uType,

  --------------------------------
  -- Holes
  tcInfer,
  matchExpectedListTy,
  matchExpectedPArrTy,
  matchExpectedTyConApp,
  matchExpectedAppTy,
  matchExpectedFunTys,
  matchActualFunTys, matchActualFunTysPart,
  matchExpectedFunKind,

  wrapFunResCoercion

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
import Var
import VarEnv
import VarSet
import ErrUtils
import DynFlags
import BasicTypes
import Name   ( Name )
import Bag
import Util
import Outputable
import FastString

import Control.Monad

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

-}

-- Use this one when you have an "expected" type.
matchExpectedFunTys :: SDoc   -- See Note [Herald for matchExpectedFunTys]
                    -> Arity
                    -> TcSigmaType  -- deeply skolemised
                    -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
-- If    matchExpectedFunTys n ty = (wrap, [t1,..,tn], ty_r)
-- then  wrap : (t1 -> ... -> tn -> ty_r) "->" ty

-- This function is always called with a deeply skolemised expected result
-- type. This means that matchActualFunTys will never actually instantiate,
-- and the returned HsWrapper will be reversible (that is, just a coercion).
-- So we just piggyback on matchActualFunTys. This is just a bit dodgy, but
-- it's much better than duplicating all the logic in matchActualFunTys.
-- To keep expected/actual working out properly, we tell matchActualFunTys
-- to swap the arguments to unifyType.
matchExpectedFunTys herald arity ty
  = ASSERT( is_deeply_skolemised ty )
    do { (wrap, arg_tys, res_ty)
           <- match_fun_tys True herald
                            (Shouldn'tHappenOrigin "matchExpectedFunTys")
                            arity ty [] arity
       ; return $
         case symWrapper_maybe wrap of
           Just wrap' -> (wrap', arg_tys, res_ty)
           Nothing    -> pprPanic "matchExpectedFunTys" (ppr wrap $$ ppr ty) }
  where
    is_deeply_skolemised (TyVarTy {})    = True
    is_deeply_skolemised (AppTy {})      = True
    is_deeply_skolemised (TyConApp {})   = True
    is_deeply_skolemised (LitTy {})      = True
    is_deeply_skolemised (CastTy ty _)   = is_deeply_skolemised ty
    is_deeply_skolemised (CoercionTy {}) = True

    is_deeply_skolemised (ForAllTy (Anon _) res) = is_deeply_skolemised res
    is_deeply_skolemised (ForAllTy (Named {}) _) = False

matchActualFunTys :: SDoc   -- See Note [Herald for matchExpectedFunTys]
                  -> CtOrigin
                  -> Arity
                  -> TcSigmaType
                  -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
matchActualFunTys herald ct_orig arity ty
  = matchActualFunTysPart herald ct_orig arity ty [] arity

-- | Variant of 'matchActualFunTys' that works when supplied only part
-- (that is, to the right of some arrows) of the full function type
matchActualFunTysPart :: SDoc -- See Note [Herald for matchExpectedFunTys]
                      -> CtOrigin
                      -> Arity
                      -> TcSigmaType
                      -> [TcSigmaType] -- reversed args. See (*) below.
                      -> Arity   -- overall arity of the function, for errs
                      -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
matchActualFunTysPart = match_fun_tys False

match_fun_tys :: Bool -- True <=> swap the args when unifying,
                      -- for better expected/actual in error messages;
                      -- see comments with matchExpectedFunTys
              -> SDoc
              -> CtOrigin
              -> Arity
              -> TcSigmaType
              -> [TcSigmaType]
              -> Arity
              -> TcM (HsWrapper, [TcSigmaType], TcSigmaType)
match_fun_tys swap_tys herald ct_orig arity orig_ty orig_old_args full_arity
  = go arity orig_old_args orig_ty
-- If    matchActualFunTys n ty = (wrap, [t1,..,tn], ty_r)
-- then  wrap : ty "->" (t1 -> ... -> tn -> ty_r)
--
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
      | Just ty' <- coreView ty = go n acc_args ty'

    go n acc_args (ForAllTy (Anon arg_ty) res_ty)
      = ASSERT( not (isPredTy arg_ty) )
        do { (wrap_res, tys, ty_r) <- go (n-1) (arg_ty : acc_args) res_ty
           ; return ( mkWpFun idHsWrapper wrap_res arg_ty (mkFunTys tys ty_r)
                    , arg_ty:tys, ty_r ) }

    go n acc_args ty@(TyVarTy tv)
      | ASSERT( isTcTyVar tv) isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty' -> go n acc_args ty'
               Flexi        -> defer n ty (isReturnTyVar tv) }

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
                       defer n ty False

    ------------
    -- If we decide that a ReturnTv (see Note [ReturnTv] in TcType) should
    -- really be a function type, then we need to allow the
    -- result types also to be a ReturnTv.
    defer n fun_ty is_return
      = do { arg_tys <- replicateM n new_flexi
           ; res_ty  <- new_flexi
           ; let unif_fun_ty = mkFunTys arg_tys res_ty
           ; co <- if swap_tys
                   then mkTcSymCo <$> unifyType noThing unif_fun_ty fun_ty
                   else               unifyType noThing fun_ty unif_fun_ty
           ; return (mkWpCastN co, arg_tys, res_ty) }
      where
        -- preserve ReturnTv-ness
        new_flexi :: TcM TcType
        new_flexi | is_return = (mkTyVarTy . fst) <$> newOpenReturnTyVar
                  | otherwise = newOpenFlexiTyVarTy

    ------------
    mk_ctxt :: [TcSigmaType] -> TcSigmaType -> TidyEnv -> TcM (TidyEnv, MsgDoc)
    mk_ctxt arg_tys res_ty env
      = do { let ty = mkFunTys arg_tys res_ty
           ; (env1, zonked) <- zonkTidyTcType env ty
                   -- zonking might change # of args
           ; let (zonked_args, _) = tcSplitFunTys zonked
                 n_actual         = length zonked_args
                 (env2, unzonked) = tidyOpenType env1 ty
           ; return (env2, mk_msg unzonked zonked n_actual) }

    mk_msg full_ty ty n_args
      = herald <+> speakNOf full_arity (text "argument") <> comma $$
        if n_args == full_arity
          then ptext (sLit "its type is") <+> quotes (pprType full_ty) <>
               comma $$
               ptext (sLit "it is specialized to") <+> quotes (pprType ty)
          else sep [ptext (sLit "but its type") <+> quotes (pprType ty),
                    if n_args == 0 then ptext (sLit "has none")
                    else ptext (sLit "has only") <+> speakN n_args]

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
  = go orig_ty
  where
    go ty
       | Just ty' <- coreView ty
       = go ty'

    go ty@(TyConApp tycon args)
       | tc == tycon  -- Common case
       = return (mkTcNomReflCo ty, args)

    go (TyVarTy tv)
       | ASSERT( isTcTyVar tv) isMetaTyVar tv
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
      = ASSERT2( classifiesTypeWithValues res_kind, ppr tc )
        do { (k_subst, kvs') <- newMetaTyVars kvs
           ; let arg_kinds' = substTys k_subst arg_kinds
                 kappa_tys  = mkTyVarTys kvs'
           ; tau_tys <- mapM newFlexiTyVarTy arg_kinds'
           ; co <- unifyType noThing (mkTyConApp tc (kappa_tys ++ tau_tys)) orig_ty
           ; return (co, kappa_tys ++ tau_tys) }

    (bndrs, res_kind)     = splitPiTys (tyConKind tc)
    (kvs, arg_kinds)      = partitionBinders bndrs

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
      = return (mkTcNomReflCo orig_ty, (fun_ty, arg_ty))

    go (TyVarTy tv)
      | ASSERT( isTcTyVar tv) isMetaTyVar tv
      = do { cts <- readMetaTyVar tv
           ; case cts of
               Indirect ty -> go ty
               Flexi       -> defer }

    go _ = defer

    -- Defer splitting by generating an equality constraint
    defer
      = do { ty1 <- newFlexiTyVarTy kind1
           ; ty2 <- newFlexiTyVarTy kind2
           ; co <- unifyType noThing (mkAppTy ty1 ty2) orig_ty
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
tcSubTypeHR :: Outputable a
            => CtOrigin    -- ^ of the actual type
            -> Maybe a     -- ^ If present, it has type ty_actual
            -> TcSigmaType -> TcRhoType -> TcM HsWrapper
tcSubTypeHR orig = tcSubTypeDS_NC_O orig GenSigCtxt

tcSubType :: Outputable a
          => UserTypeCtxt -> Maybe a  -- ^ If present, it has type ty_actual
          -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
-- Checks that actual <= expected
-- Returns HsWrapper :: actual ~ expected
tcSubType ctxt maybe_thing ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubType" (vcat [ pprUserTypeCtxt ctxt
                                   , ppr maybe_thing
                                   , ppr ty_actual
                                   , ppr ty_expected ])
       ; tc_sub_type origin origin ctxt ty_actual ty_expected }
  where
    origin = TypeEqOrigin { uo_actual   = ty_actual
                          , uo_expected = ty_expected
                          , uo_thing    = mkErrorThing <$> maybe_thing }

tcSubTypeDS :: Outputable a => UserTypeCtxt -> Maybe a  -- ^ has type ty_actual
            -> TcSigmaType -> TcRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised (hence "DS")
tcSubTypeDS ctxt m_expr ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    tcSubTypeDS_NC ctxt m_expr ty_actual ty_expected

-- | Like 'tcSubTypeDS', but takes a 'CtOrigin' to use when instantiating
-- the "actual" type
tcSubTypeDS_O :: Outputable a
              => CtOrigin -> UserTypeCtxt
              -> Maybe a -> TcSigmaType -> TcRhoType
              -> TcM HsWrapper
tcSubTypeDS_O orig ctxt maybe_thing ty_actual ty_expected
  = addSubTypeCtxt ty_actual ty_expected $
    do { traceTc "tcSubTypeDS_O" (vcat [ pprCtOrigin orig
                                       , pprUserTypeCtxt ctxt
                                       , ppr ty_actual
                                       , ppr ty_expected ])
       ; tcSubTypeDS_NC_O orig ctxt maybe_thing ty_actual ty_expected }

addSubTypeCtxt :: TcType -> TcType -> TcM a -> TcM a
addSubTypeCtxt ty_actual ty_expected thing_inside
 | isRhoTy ty_actual     -- If there is no polymorphism involved, the
 , isRhoTy ty_expected   -- TypeEqOrigin stuff (added by the _NC functions)
 = thing_inside          -- gives enough context by itself
 | otherwise
 = addErrCtxtM mk_msg thing_inside
  where
    mk_msg tidy_env
      = do { (tidy_env, ty_actual)   <- zonkTidyTcType tidy_env ty_actual
           ; (tidy_env, ty_expected) <- zonkTidyTcType tidy_env ty_expected
           ; let msg = vcat [ hang (ptext (sLit "When checking that:"))
                                 4 (ppr ty_actual)
                            , nest 2 (hang (ptext (sLit "is more polymorphic than:"))
                                         2 (ppr ty_expected)) ]
           ; return (tidy_env, msg) }

---------------
-- The "_NC" variants do not add a typechecker-error context;
-- the caller is assumed to do that

tcSubType_NC :: UserTypeCtxt -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
tcSubType_NC ctxt ty_actual ty_expected
  = do { traceTc "tcSubType_NC" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tc_sub_type origin origin ctxt ty_actual ty_expected }
  where
    origin = TypeEqOrigin { uo_actual   = ty_actual
                          , uo_expected = ty_expected
                          , uo_thing    = Nothing }

tcSubTypeDS_NC :: Outputable a
               => UserTypeCtxt
               -> Maybe a  -- ^ If present, this has type ty_actual
               -> TcSigmaType -> TcRhoType -> TcM HsWrapper
tcSubTypeDS_NC ctxt maybe_thing ty_actual ty_expected
  = do { traceTc "tcSubTypeDS_NC" (vcat [pprUserTypeCtxt ctxt, ppr ty_actual, ppr ty_expected])
       ; tcSubTypeDS_NC_O origin ctxt maybe_thing ty_actual ty_expected }
  where
    origin = TypeEqOrigin { uo_actual   = ty_actual
                          , uo_expected = ty_expected
                          , uo_thing    = mkErrorThing <$> maybe_thing }

tcSubTypeDS_NC_O :: Outputable a
                 => CtOrigin   -- origin used for instantiation only
                 -> UserTypeCtxt
                 -> Maybe a
                 -> TcSigmaType -> TcRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised
tcSubTypeDS_NC_O inst_orig ctxt m_thing ty_actual ty_expected
  = tc_sub_type_ds eq_orig inst_orig ctxt ty_actual ty_expected
  where
    eq_orig = TypeEqOrigin { uo_actual = ty_actual, uo_expected = ty_expected
                           , uo_thing = mkErrorThing <$> m_thing}

---------------
tc_sub_type :: CtOrigin   -- origin used when calling uType
            -> CtOrigin   -- origin used when instantiating
            -> UserTypeCtxt -> TcSigmaType -> TcSigmaType -> TcM HsWrapper
tc_sub_type eq_orig inst_orig ctxt ty_actual ty_expected
  | Just tv_actual <- tcGetTyVar_maybe ty_actual -- See Note [Higher rank types]
  = do { lookup_res <- lookupTcTyVar tv_actual
       ; case lookup_res of
           Filled ty_actual' -> tc_sub_type eq_orig inst_orig
                                            ctxt ty_actual' ty_expected

             -- It's tempting to see if tv_actual can unify with a polytype
             -- and, if so, call uType; otherwise, skolemise first. But this
             -- is wrong, because skolemising will bump the TcLevel and the
             -- unification will fail anyway.
             -- It's also tempting to call uUnfilledVar directly, but calling
             -- uType seems safer in the presence of possible refactoring
             -- later.
           Unfilled _        -> mkWpCastN <$>
                                uType eq_orig TypeLevel ty_actual ty_expected }

  | otherwise  -- See Note [Deep skolemisation]
  = do { (sk_wrap, inner_wrap) <- tcSkolemise ctxt ty_expected $
                                  \ _ sk_rho ->
                                  tc_sub_type_ds eq_orig inst_orig ctxt
                                                 ty_actual sk_rho
       ; return (sk_wrap <.> inner_wrap) }

---------------
tc_sub_type_ds :: CtOrigin    -- used when calling uType
               -> CtOrigin    -- used when instantiating
               -> UserTypeCtxt -> TcSigmaType -> TcRhoType -> TcM HsWrapper
-- Just like tcSubType, but with the additional precondition that
-- ty_expected is deeply skolemised
tc_sub_type_ds eq_orig inst_orig ctxt ty_actual ty_expected
  = go ty_actual ty_expected
  where
    go ty_a ty_e | Just ty_a' <- coreView ty_a = go ty_a' ty_e
                 | Just ty_e' <- coreView ty_e = go ty_a  ty_e'

    go (TyVarTy tv_a) ty_e
      = do { lookup_res <- lookupTcTyVar tv_a
           ; case lookup_res of
               Filled ty_a' ->
                 do { traceTc "tcSubTypeDS_NC_O following filled act meta-tyvar:"
                        (ppr tv_a <+> text "-->" <+> ppr ty_a')
                    ; tc_sub_type_ds eq_orig inst_orig ctxt ty_a' ty_e }
               Unfilled _   -> unify }


    go ty_a (TyVarTy tv_e)
      = do { dflags <- getDynFlags
           ; tclvl  <- getTcLevel
           ; lookup_res <- lookupTcTyVar tv_e
           ; case lookup_res of
               Filled ty_e' ->
                 do { traceTc "tcSubTypeDS_NC_O following filled exp meta-tyvar:"
                        (ppr tv_e <+> text "-->" <+> ppr ty_e')
                    ; tc_sub_type eq_orig inst_orig ctxt ty_a ty_e' }
               Unfilled details
                 |  canUnifyWithPolyType dflags details
                    && isTouchableMetaTyVar tclvl tv_e  -- don't want skolems here
                 -> unify

     -- We've avoided instantiating ty_actual just in case ty_expected is
     -- polymorphic. But we've now assiduously determined that it is *not*
     -- polymorphic. So instantiate away. This is needed for e.g. test
     -- typecheck/should_compile/T4284.
                 |  otherwise
                 -> inst_and_unify }

    go (ForAllTy (Anon act_arg) act_res) (ForAllTy (Anon exp_arg) exp_res)
      | not (isPredTy act_arg)
      , not (isPredTy exp_arg)
      = -- See Note [Co/contra-variance of subsumption checking]
        do { res_wrap <- tc_sub_type_ds eq_orig inst_orig ctxt act_res exp_res
           ; arg_wrap
               <- tc_sub_type eq_orig (GivenOrigin (SigSkol GenSigCtxt exp_arg))
                              ctxt exp_arg act_arg
           ; return (mkWpFun arg_wrap res_wrap exp_arg exp_res) }
               -- arg_wrap :: exp_arg ~ act_arg
               -- res_wrap :: act-res ~ exp_res

    go ty_a ty_e
      | let (tvs, theta, _) = tcSplitSigmaTy ty_a
      , not (null tvs && null theta)
      = do { (in_wrap, in_rho) <- topInstantiate inst_orig ty_a
           ; body_wrap <- tcSubTypeDS_NC_O inst_orig ctxt noThing in_rho ty_e
           ; return (body_wrap <.> in_wrap) }

      | otherwise   -- Revert to unification
      = inst_and_unify
         -- It's still possible that ty_actual has nested foralls. Instantiate
         -- these, as there's no way unification will succeed with them in.
         -- See typecheck/should_compiler/T11350 for an example of when this
         -- is important.

    inst_and_unify = do { (wrap, rho_a) <- deeplyInstantiate inst_orig ty_actual

                           -- if we haven't recurred through an arrow, then
                           -- the eq_orig will list ty_actual. In this case,
                           -- we want to update the origin to reflect the
                           -- instantiation. If we *have* recurred through
                           -- an arrow, it's better not to update.
                        ; let eq_orig' = case eq_orig of
                                TypeEqOrigin { uo_actual   = orig_ty_actual
                                             , uo_expected = orig_ty_expected
                                             , uo_thing    = thing }
                                  |  orig_ty_actual `tcEqType` ty_actual
                                  ,  not (isIdHsWrapper wrap)
                                  -> TypeEqOrigin
                                       { uo_actual = rho_a
                                       , uo_expected = orig_ty_expected
                                       , uo_thing    = thing }
                                _ -> eq_orig

                        ; cow <- uType eq_orig' TypeLevel rho_a ty_expected
                        ; return (mkWpCastN cow <.> wrap) }


     -- use versions without synonyms expanded
    unify = mkWpCastN <$> uType eq_orig TypeLevel ty_actual ty_expected

-----------------
-- needs both un-type-checked (for origins) and type-checked (for wrapping)
-- expressions
tcWrapResult :: HsExpr Name -> HsExpr TcId -> TcSigmaType -> TcRhoType
             -> TcM (HsExpr TcId)
tcWrapResult rn_expr = tcWrapResultO (exprCtOrigin rn_expr)

-- | Sometimes we don't have a @HsExpr Name@ to hand, and this is more
-- convenient.
tcWrapResultO :: CtOrigin -> HsExpr TcId -> TcSigmaType -> TcRhoType
               -> TcM (HsExpr TcId)
tcWrapResultO orig expr actual_ty res_ty
  = do { traceTc "tcWrapResult" (vcat [ text "Actual:  " <+> ppr actual_ty
                                      , text "Expected:" <+> ppr res_ty ])
       ; cow <- tcSubTypeDS_NC_O orig GenSigCtxt
                                 (Just expr) actual_ty res_ty
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

-----------------------------------
-- | Infer a type using a type "checking" function by passing in a ReturnTv,
-- which can unify with *anything*. See also Note [ReturnTv] in TcType
tcInfer :: (TcType -> TcM a) -> TcM (a, TcType)
tcInfer tc_check
  = do { (ret_tv, ret_kind) <- newOpenReturnTyVar
       ; res <- tc_check (mkTyVarTy ret_tv)
       ; details <- readMetaTyVar ret_tv
       ; res_ty <- case details of
            Indirect ty -> return ty
            Flexi ->    -- Checking was uninformative
                     do { traceTc "Defaulting un-filled ReturnTv to a TauTv" (ppr ret_tv)
                        ; tau_ty <- newFlexiTyVarTy ret_kind
                        ; writeMetaTyVar ret_tv tau_ty
                        ; return tau_ty }
       ; return (res, res_ty) }

{-
************************************************************************
*                                                                      *
\subsection{Generalisation}
*                                                                      *
************************************************************************
-}

-- | Take an "expected type" and strip off quantifiers to expose the
-- type underneath, binding the new skolems for the @thing_inside@.
-- The returned 'HsWrapper' has type @specific_ty -> expected_ty@.
tcSkolemise :: UserTypeCtxt -> TcSigmaType
            -> ([TcTyVar] -> TcType -> TcM result)
         -- ^ thing_inside is passed only the *type* variables, not
         -- *coercion* variables. They are only ever used for scoped type
         -- variables.
            -> TcM (HsWrapper, result)
        -- ^ The expression has type: spec_ty -> expected_ty

tcSkolemise ctxt expected_ty thing_inside
   -- We expect expected_ty to be a forall-type
   -- If not, the call is a no-op
  = do  { traceTc "tcSkolemise" Outputable.empty
        ; (wrap, tvs', given, rho') <- deeplySkolemise expected_ty

        ; lvl <- getTcLevel
        ; when debugIsOn $
              traceTc "tcSkolemise" $ vcat [
                ppr lvl,
                text "expected_ty" <+> ppr expected_ty,
                text "inst tyvars" <+> ppr tvs',
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

        -- Use the *instantiated* type in the SkolemInfo
        -- so that the names of displayed type variables line up
        ; let skol_info = SigSkol ctxt (mkFunTys (map varType given) rho')

        ; (ev_binds, result) <- checkConstraints skol_info tvs' given $
                                thing_inside tvs' rho'

        ; return (wrap <.> mkWpLet ev_binds, result) }
          -- The ev_binds returned by checkConstraints is very
          -- often empty, in which case mkWpLet is a no-op

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
  = ASSERT2( all isTcTyVar skol_tvs, ppr skol_tvs )
    ASSERT2( all isSkolemTyVar skol_tvs, ppr skol_tvs )
    do { ev_binds_var <- newTcEvBinds
       ; env <- getLclEnv
       ; let implic = Implic { ic_tclvl = tclvl
                             , ic_skols = skol_tvs
                             , ic_no_eqs = False
                             , ic_given = given
                             , ic_wanted = wanted
                             , ic_status  = IC_Unsolved
                             , ic_binds = Just ev_binds_var
                             , ic_env = env
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

-- | Unify two types, discarding a resultant coercion. Any constraints
-- generated will still need to be solved, however.
unifyType_ :: Outputable a => Maybe a  -- ^ If present, has type 'ty1'
           -> TcTauType -> TcTauType -> TcM ()
unifyType_ thing ty1 ty2 = void $ unifyType thing ty1 ty2

unifyType :: Outputable a => Maybe a   -- ^ If present, has type 'ty1'
          -> TcTauType -> TcTauType -> TcM TcCoercion
-- Actual and expected types
-- Returns a coercion : ty1 ~ ty2
unifyType thing ty1 ty2 = uType origin TypeLevel ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual = ty1, uo_expected = ty2
                          , uo_thing  = mkErrorThing <$> thing }

-- | Use this instead of 'Nothing' when calling 'unifyType' without
-- a good "thing" (where the "thing" has the "actual" type passed in)
-- This has an 'Outputable' instance, avoiding amgiguity problems.
noThing :: Maybe (HsExpr Name)
noThing = Nothing

unifyKind :: Outputable a => Maybe a -> TcKind -> TcKind -> TcM Coercion
unifyKind thing ty1 ty2 = uType origin KindLevel ty1 ty2
  where origin = TypeEqOrigin { uo_actual = ty1, uo_expected = ty2
                              , uo_thing  = mkErrorThing <$> thing }

---------------
unifyPred :: PredType -> PredType -> TcM TcCoercion
-- Actual and expected types
unifyPred = unifyType noThing

---------------
unifyTheta :: TcThetaType -> TcThetaType -> TcM [TcCoercion]
-- Actual and expected types
unifyTheta theta1 theta2
  = do  { checkTc (equalLength theta1 theta2)
                  (vcat [ptext (sLit "Contexts differ in length"),
                         nest 2 $ parens $ ptext (sLit "Use RelaxedPolyRec to allow this")])
        ; zipWithM unifyPred theta1 theta2 }

{-
%************************************************************************
%*                                                                      *
                 uType and friends
%*                                                                      *
%************************************************************************

uType is the heart of the unifier.
-}

------------
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
  = do { hole <- newCoercionHole
       ; loc <- getCtLocM origin (Just t_or_k)
       ; emitSimple $ mkNonCanonical $
             CtWanted { ctev_dest = HoleDest hole
                      , ctev_pred = mkPrimEqPred ty1 ty2
                      , ctev_loc = loc }

       -- Error trace only
       -- NB. do *not* call mkErrInfo unless tracing is on, because
       -- it is hugely expensive (#5631)
       ; whenDOptM Opt_D_dump_tc_trace $ do
            { ctxt <- getErrCtxt
            ; doc <- mkErrInfo emptyTidyEnv ctxt
            ; traceTc "utype_defer" (vcat [ppr hole, ppr ty1,
                                           ppr ty2, pprCtOrigin origin, doc])
            }
       ; return (mkHoleCo hole Nominal ty1 ty2) }

--------------
uType origin t_or_k orig_ty1 orig_ty2
  = do { tclvl <- getTcLevel
       ; traceTc "u_tys " $ vcat
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
               Unfilled ds1 -> uUnfilledVar origin t_or_k NotSwapped tv1 ds1 ty2 }
    go ty1 (TyVarTy tv2)
      = do { lookup_res <- lookupTcTyVar tv2
           ; case lookup_res of
               Filled ty2   -> do { traceTc "found filled tyvar" (ppr tv2 <+> text ":->" <+> ppr ty2)
                                  ; go ty1 ty2 }
               Unfilled ds2 -> uUnfilledVar origin t_or_k IsSwapped tv2 ds2 ty1 }

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
      | Just ty1' <- coreView ty1 = go ty1' ty2
      | Just ty2' <- coreView ty2 = go ty1  ty2'

    go (CastTy t1 co1) t2
      = do { co_tys <- go t1 t2
           ; return (mkCoherenceLeftCo co_tys co1) }

    go t1 (CastTy t2 co2)
      = do { co_tys <- go t1 t2
           ; return (mkCoherenceRightCo co_tys co2) }

        -- Functions (or predicate functions) just check the two parts
    go (ForAllTy (Anon fun1) arg1) (ForAllTy (Anon fun2) arg2)
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
      | tc1 == tc2, length tys1 == length tys2
      = ASSERT2( isGenerativeTyCon tc1 Nominal, ppr tc1 )
        do { cos <- zipWith3M (uType origin) t_or_ks tys1 tys2
           ; return $ mkTyConAppCo Nominal tc1 cos }
      where
        (bndrs, _) = splitPiTys (tyConKind tc1)
        t_or_ks    = case t_or_k of
                       KindLevel -> repeat KindLevel
                       TypeLevel -> map (\bndr -> if isNamedBinder bndr
                                                  then KindLevel
                                                  else TypeLevel)
                                        bndrs

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
           ; kco <- uType (KindEqOrigin orig_ty1 orig_ty2 origin (Just t_or_k))
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

uUnfilledVar :: CtOrigin
             -> TypeOrKind
             -> SwapFlag
             -> TcTyVar -> TcTyVarDetails       -- Tyvar 1
             -> TcTauType                       -- Type 2
             -> TcM Coercion
-- "Unfilled" means that the variable is definitely not a filled-in meta tyvar
--            It might be a skolem, or untouchable, or meta

uUnfilledVar origin t_or_k swapped tv1 details1 (TyVarTy tv2)
  | tv1 == tv2  -- Same type variable => no-op
  = return (mkNomReflCo (mkTyVarTy tv1))

  | otherwise  -- Distinct type variables
  = do  { lookup2 <- lookupTcTyVar tv2
        ; case lookup2 of
            Filled ty2'
              -> uUnfilledVar origin t_or_k swapped tv1 details1 ty2'
            Unfilled details2
              -> uUnfilledVars origin t_or_k swapped tv1 details1 tv2 details2
        }

uUnfilledVar origin t_or_k swapped tv1 details1 non_var_ty2
-- ty2 is not a type variable
  = case details1 of
      MetaTv { mtv_ref = ref1 }
        -> do { dflags <- getDynFlags
              ; mb_ty2' <- checkTauTvUpdate dflags origin t_or_k tv1 non_var_ty2
              ; case mb_ty2' of
                  Just (ty2', co_k) -> maybe_sym swapped <$>
                                       updateMeta tv1 ref1 ty2' co_k
                  Nothing   -> do { traceTc "Occ/type-family defer"
                                        (ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1)
                                         $$ ppr non_var_ty2 $$ ppr (typeKind non_var_ty2))
                                  ; defer }
              }

      _other -> do { traceTc "Skolem defer" (ppr tv1); defer }  -- Skolems of all sorts
  where
    defer = unSwap swapped (uType_defer origin t_or_k) (mkTyVarTy tv1) non_var_ty2
               -- Occurs check or an untouchable: just defer
               -- NB: occurs check isn't necessarily fatal:
               --     eg tv1 occured in type family parameter

----------------
uUnfilledVars :: CtOrigin
              -> TypeOrKind
              -> SwapFlag
              -> TcTyVar -> TcTyVarDetails      -- Tyvar 1
              -> TcTyVar -> TcTyVarDetails      -- Tyvar 2
              -> TcM Coercion
-- Invarant: The type variables are distinct,
--           Neither is filled in yet

uUnfilledVars origin t_or_k swapped tv1 details1 tv2 details2
  = do { traceTc "uUnfilledVars for" (ppr tv1 <+> text "and" <+> ppr tv2)
       ; traceTc "uUnfilledVars" (    text "trying to unify" <+> ppr k1
                                  <+> text "with"            <+> ppr k2)
       ; co_k <- uType kind_origin KindLevel k1 k2
       ; let no_swap ref = maybe_sym swapped <$>
                           updateMeta tv1 ref ty2 (mkSymCo co_k)
             do_swap ref = maybe_sym (flipSwap swapped) <$>
                           updateMeta tv2 ref ty1 co_k
       ; case (details1, details2) of
         { ( MetaTv { mtv_info = i1, mtv_ref = ref1 }
           , MetaTv { mtv_info = i2, mtv_ref = ref2 } )
             | nicer_to_update_tv1 tv1 i1 i2 -> no_swap ref1
             | otherwise                     -> do_swap ref2
         ; (MetaTv { mtv_ref = ref1 }, _) -> no_swap ref1
         ; (_, MetaTv { mtv_ref = ref2 }) -> do_swap ref2

           -- Can't do it in-place, so defer
           -- This happens for skolems of all sorts
         ; _ -> do { traceTc "deferring because I can't find a meta-tyvar:"
                       (pprTcTyVarDetails details1 <+> pprTcTyVarDetails details2)
                   ; unSwap swapped (uType_defer origin t_or_k) ty1 ty2 } } }
  where
    k1  = tyVarKind tv1
    k2  = tyVarKind tv2
    ty1 = mkTyVarTy tv1
    ty2 = mkTyVarTy tv2
    kind_origin = KindEqOrigin ty1 ty2 origin (Just t_or_k)

-- | apply sym iff swapped
maybe_sym :: SwapFlag -> Coercion -> Coercion
maybe_sym IsSwapped  = mkSymCo
maybe_sym NotSwapped = id

nicer_to_update_tv1 :: TcTyVar -> MetaInfo -> MetaInfo -> Bool
nicer_to_update_tv1 _   _     SigTv = True
nicer_to_update_tv1 _   SigTv _     = False
        -- Try not to update SigTvs; and try to update sys-y type
        -- variables in preference to ones gotten (say) by
        -- instantiating a polymorphic function with a user-written
        -- type sig
nicer_to_update_tv1 _   ReturnTv _        = True
nicer_to_update_tv1 _   _        ReturnTv = False
        -- ReturnTvs are really holes just begging to be filled in.
        -- Let's oblige.
nicer_to_update_tv1 tv1 _     _     = isSystemName (Var.varName tv1)

----------------
checkTauTvUpdate :: DynFlags
                 -> CtOrigin
                 -> TypeOrKind
                 -> TcTyVar             -- tv :: k1
                 -> TcType              -- ty :: k2
                 -> TcM (Maybe ( TcType        -- possibly-expanded ty
                               , Coercion )) -- :: k2 ~N k1
--    (checkTauTvUpdate tv ty)
-- We are about to update the TauTv/ReturnTv tv with ty.
-- Check (a) that tv doesn't occur in ty (occurs check)
--       (b) that kind(ty) is a sub-kind of kind(tv)
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

checkTauTvUpdate dflags origin t_or_k tv ty
  | SigTv <- info
  = ASSERT( not (isTyVarTy ty) )
    return Nothing
  | otherwise
  = do { ty   <- zonkTcType ty
       ; co_k <- uType kind_origin KindLevel (typeKind ty) (tyVarKind tv)
       ; if | is_return_tv -> -- ReturnTv: a simple occurs-check is all that we need
                              -- See Note [ReturnTv] in TcType
                if tv `elemVarSet` tyCoVarsOfType ty
                then return Nothing
                else return (Just (ty, co_k))
            | defer_me ty ->  -- Quick test
                -- Failed quick test so try harder
                case occurCheckExpand dflags tv ty of
                   OC_OK ty2 | defer_me ty2 -> return Nothing
                             | otherwise    -> return (Just (ty2, co_k))
                   _                        -> return Nothing
            | otherwise   -> return (Just (ty, co_k)) }
  where
    kind_origin   = KindEqOrigin (mkTyVarTy tv) ty origin (Just t_or_k)
    details       = tcTyVarDetails tv
    info          = mtv_info details
    is_return_tv  = isReturnTyVar tv
    impredicative = canUnifyWithPolyType dflags details

    defer_me :: TcType -> Bool
    -- Checks for (a) occurrence of tv
    --            (b) type family applications
    --            (c) foralls
    -- See Note [Conservative unification check]
    defer_me (LitTy {})        = False
    defer_me (TyVarTy tv')     = tv == tv'
    defer_me (TyConApp tc tys) = isTypeFamilyTyCon tc || any defer_me tys
                                 || not (impredicative || isTauTyCon tc)
    defer_me (ForAllTy bndr t) = defer_me (binderType bndr) || defer_me t
                                 || (isNamedBinder bndr && not impredicative)
    defer_me (AppTy fun arg)   = defer_me fun || defer_me arg
    defer_me (CastTy ty co)    = defer_me ty || defer_me_co co
    defer_me (CoercionTy co)   = defer_me_co co

      -- We don't really care if there are type families in a coercion,
      -- but we still can't have an occurs-check failure
    defer_me_co co = tv `elemVarSet` tyCoVarsOfCo co

{-
Note [Conservative unification check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When unifying (tv ~ rhs), w try to avoid creating deferred constraints
only for efficiency.  However, we do not unify (the defer_me check) if
  a) There's an occurs check (tv is in fvs(rhs))
  b) There's a type-function call in 'rhs'

If we fail defer_me we use occurCheckExpand to try to make it pass,
(see Note [Type synonyms and the occur check]) and then use defer_me
again to check.  Example: Trac #4917)
       a ~ Const a b
where type Const a b = a.  We can solve this immediately, even when
'a' is a skolem, just by expanding the synonym.

We always defer type-function calls, even if it's be perfectly safe to
unify, eg (a ~ F [b]).  Reason: this ensures that the constraint
solver gets to see, and hence simplify the type-function call, which
in turn might simplify the type of an inferred function.  Test ghci046
is a case in point.

More mysteriously, test T7010 gave a horrible error
  T7010.hs:29:21:
    Couldn't match type `Serial (ValueTuple Float)' with `IO Float'
    Expected type: (ValueTuple Vector, ValueTuple Vector)
      Actual type: (ValueTuple Vector, ValueTuple Vector)
because an insoluble type function constraint got mixed up with
a soluble one when flattening.  I never fully understood this, but
deferring type-function applications made it go away :-(.
T5853 also got a less-good error message with more aggressive
unification of type functions.

Moreover the Note [Type family sharing] gives another reason, but
again I'm not sure if it's really valid.

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

Note [Type family sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We must avoid eagerly unifying type variables to types that contain function symbols,
because this may lead to loss of sharing, and in turn, in very poor performance of the
constraint simplifier. Assume that we have a wanted constraint:
{
  m1 ~ [F m2],
  m2 ~ [F m3],
  m3 ~ [F m4],
  D m1,
  D m2,
  D m3
}
where D is some type class. If we eagerly unify m1 := [F m2], m2 := [F m3], m3 := [F m4],
then, after zonking, our constraint simplifier will be faced with the following wanted
constraint:
{
  D [F [F [F m4]]],
  D [F [F m4]],
  D [F m4]
}
which has to be flattened by the constraint solver. In the absence of
a flat-cache, this may generate a polynomially larger number of
flatten skolems and the constraint sets we are working with will be
polynomially larger.

Instead, if we defer the unifications m1 := [F m2], etc. we will only
be generating three flatten skolems, which is the maximum possible
sharing arising from the original constraint.  That's why we used to
use a local "ok" function, a variant of TcType.occurCheckExpand.

HOWEVER, we *do* now have a flat-cache, which effectively recovers the
sharing, so there's no great harm in losing it -- and it's generally
more efficient to do the unification up-front.

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

-- | Fill in a meta-tyvar
updateMeta :: TcTyVar            -- ^ tv to fill in, tv :: k1
           -> TcRef MetaDetails  -- ^ ref to tv's metadetails
           -> TcType             -- ^ ty2 :: k2
           -> Coercion           -- ^ kind_co :: k2 ~N k1
           -> TcM Coercion       -- ^ :: tv ~N ty2 (= ty2 |> kind_co ~N ty2)
updateMeta tv1 ref1 ty2 kind_co
  = do { let ty2_refl   = mkNomReflCo ty2
             (ty2', co) = ( ty2 `mkCastTy` kind_co
                          , mkCoherenceLeftCo ty2_refl kind_co )
       ; writeMetaTyVarRef tv1 ref1 ty2'
       ; return co }

{-
Note [Unifying untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We treat an untouchable type variable as if it was a skolem.  That
ensures it won't unify with anything.  It's a slight had, because
we return a made-up TcTyVarDetails, but I think it works smoothly.
-}

-- | Breaks apart a function kind into its pieces.
matchExpectedFunKind :: Arity           -- ^ # of args remaining, only for errors
                     -> TcType          -- ^ type, only for errors
                     -> TcKind          -- ^ function kind
                     -> TcM (Coercion, TcKind, TcKind)
                                  -- ^ co :: old_kind ~ arg -> res
matchExpectedFunKind num_args_remaining ty = go
  where
    go k | Just k' <- coreView k = go k'

    go k@(TyVarTy kvar)
      | isTcTyVar kvar, isMetaTyVar kvar
      = do { maybe_kind <- readMetaTyVar kvar
           ; case maybe_kind of
                Indirect fun_kind -> go fun_kind
                Flexi ->             defer (isReturnTyVar kvar) k }

    go k@(ForAllTy (Anon arg) res)
      = return (mkNomReflCo k, arg, res)

    go other = defer False other

    defer is_return k
      = do { arg_kind <- new_flexi
           ; res_kind <- new_flexi
           ; let new_fun = mkFunTy arg_kind res_kind
                 thing   = mkTypeErrorThingArgs ty num_args_remaining
                 origin  = TypeEqOrigin { uo_actual   = k
                                        , uo_expected = new_fun
                                        , uo_thing    = Just thing
                                        }
           ; co <- uType origin KindLevel k new_fun
           ; return (co, arg_kind, res_kind) }
      where
        new_flexi | is_return = newReturnTyVarTy liftedTypeKind
                  | otherwise = newMetaKindVar
