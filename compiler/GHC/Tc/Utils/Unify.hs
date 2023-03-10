{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecordWildCards     #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | Type subsumption and unification
module GHC.Tc.Utils.Unify (
  -- Full-blown subsumption
  tcWrapResult, tcWrapResultO, tcWrapResultMono,
  tcTopSkolemise, tcSkolemiseScoped, tcSkolemiseExpType,
  tcSubType, tcSubTypeSigma, tcSubTypePat, tcSubTypeDS,
  tcSubTypeAmbiguity, tcSubMult,
  checkConstraints, checkTvConstraints,
  buildImplicationFor, buildTvImplication, emitResidualTvConstraint,

  -- Various unifications
  unifyType, unifyKind, unifyExpectedType,
  uType, promoteTcType,
  swapOverTyVars, touchabilityAndShapeTest,

  --------------------------------
  -- Holes
  tcInfer,
  matchExpectedListTy,
  matchExpectedTyConApp,
  matchExpectedAppTy,
  matchExpectedFunTys,
  matchExpectedFunKind,
  matchActualFunTySigma, matchActualFunTysRho,

  checkTyEqRhs, recurseIntoTyConApp,
  PuResult(..), failCheckWith, okCheckRefl, mapCheck,
  TyEqFlags(..), TyEqFamApp(..), AreUnifying(..), LevelCheck(..), FamAppBreaker,
  famAppArgFlags, simpleUnifyCheck, checkPromoteFreeVars,
  ) where

import GHC.Prelude

import GHC.Hs

import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep, hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Types.Name( Name, isSystemName )


import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs( isInjectiveInType )
import GHC.Core.TyCo.Ppr( debugPprType {- pprTyVar -} )
import GHC.Core.TyCon
import GHC.Core.Coercion
import GHC.Core.Multiplicity
import GHC.Core.Reduction

import qualified GHC.LanguageExtensions as LangExt

import GHC.Builtin.Types
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Basic
import GHC.Types.Unique.Set (nonDetEltsUniqSet)

import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import GHC.Driver.Session
import GHC.Data.Bag
import GHC.Data.FastString( fsLit )

import Control.Monad
import Data.Monoid as DM ( Any(..) )
import qualified Data.Semigroup as S ( (<>) )

{- *********************************************************************
*                                                                      *
              matchActualFunTys
*                                                                      *
********************************************************************* -}

-- | 'matchActualFunTySigma' looks for just one function arrow,
-- returning an uninstantiated sigma-type.
--
-- Invariant: the returned argument type has a syntactically fixed
-- RuntimeRep in the sense of Note [Fixed RuntimeRep]
-- in GHC.Tc.Utils.Concrete.
--
-- See Note [Return arguments with a fixed RuntimeRep].
matchActualFunTySigma
  :: ExpectedFunTyOrigin
      -- ^ See Note [Herald for matchExpectedFunTys]
  -> Maybe TypedThing
      -- ^ The thing with type TcSigmaType
  -> (Arity, [Scaled TcSigmaType])
      -- ^ Total number of value args in the call, and
      -- types of values args to which function has
      --   been applied already (reversed)
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

-- If   (wrap, arg_ty, res_ty) = matchActualFunTySigma ... fun_ty
-- then wrap :: fun_ty ~> (arg_ty -> res_ty)
-- and NB: res_ty is an (uninstantiated) SigmaType

matchActualFunTySigma herald mb_thing err_info fun_ty
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
      = do { arg_ty <- newOpenFlexiTyVarTy
           ; res_ty <- newOpenFlexiTyVarTy
           ; mult <- newFlexiTyVarTy multiplicityTy
           ; let unif_fun_ty = tcMkVisFunTy mult arg_ty res_ty
           ; co <- unifyType mb_thing fun_ty unif_fun_ty
           ; hasFixedRuntimeRep_syntactic (FRRExpectedFunTy herald 1) arg_ty
           ; return (mkWpCastN co, Scaled mult arg_ty, res_ty) }

    ------------
    mk_ctxt :: TcType -> TidyEnv -> TcM (TidyEnv, SDoc)
    mk_ctxt res_ty env = mkFunTysMsg env herald (reverse arg_tys_so_far)
                                     res_ty n_val_args_in_call
    (n_val_args_in_call, arg_tys_so_far) = err_info

{- Note [matchActualFunTy error handling]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
matchActualFunTySigma is made much more complicated by the
desire to produce good error messages. Consider the application
    f @Int x y
In GHC.Tc.Gen.Expr.tcArgs we deal with visible type arguments,
and then call matchActualFunTysPart for each individual value
argument. It, in turn, must instantiate any type/dictionary args,
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
matchActualFunTysRho :: ExpectedFunTyOrigin -- ^ See Note [Herald for matchExpectedFunTys]
                     -> CtOrigin
                     -> Maybe TypedThing -- ^ the thing with type TcSigmaType
                     -> Arity
                     -> TcSigmaType
                     -> TcM (HsWrapper, [Scaled TcSigmaTypeFRR], TcRhoType)
-- If    matchActualFunTysRho n ty = (wrap, [t1,..,tn], res_ty)
-- then  wrap : ty ~> (t1 -> ... -> tn -> res_ty)
--       and res_ty is a RhoType
-- NB: the returned type is top-instantiated; it's a RhoType
matchActualFunTysRho herald ct_orig mb_thing n_val_args_wanted fun_ty
  = go n_val_args_wanted [] fun_ty
  where
    go n so_far fun_ty
      | not (isRhoTy fun_ty)
      = do { (wrap1, rho) <- topInstantiate ct_orig fun_ty
           ; (wrap2, arg_tys, res_ty) <- go n so_far rho
           ; return (wrap2 <.> wrap1, arg_tys, res_ty) }

    go 0 _ fun_ty = return (idHsWrapper, [], fun_ty)

    go n so_far fun_ty
      = do { (wrap_fun1, arg_ty1, res_ty1) <- matchActualFunTySigma
                                                 herald mb_thing
                                                 (n_val_args_wanted, so_far)
                                                 fun_ty
           ; (wrap_res, arg_tys, res_ty)   <- go (n-1) (arg_ty1:so_far) res_ty1
           ; let wrap_fun2 = mkWpFun idHsWrapper wrap_res arg_ty1 res_ty
           -- NB: arg_ty1 comes from matchActualFunTySigma, so it has
           -- a syntactically fixed RuntimeRep as needed to call mkWpFun.
           ; return (wrap_fun2 <.> wrap_fun1, arg_ty1:arg_tys, res_ty) }

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
  - matchActualFunTySigma,
  - matchActualFunTysRho,

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
  (see 'GHC.Tc.Gen.Match.tcMatchLambda'). We end up with desugared Core that
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
                       ExpectedFunTyOrigin -- See Note [Herald for matchExpectedFunTys]
                    -> UserTypeCtxt
                    -> Arity
                    -> ExpRhoType      -- Skolemised
                    -> ([Scaled ExpSigmaTypeFRR] -> ExpRhoType -> TcM a)
                    -> TcM (HsWrapper, a)
-- If    matchExpectedFunTys n ty = (wrap, _)
-- then  wrap : (t1 -> ... -> tn -> ty_r) ~> ty,
--   where [t1, ..., tn], ty_r are passed to the thing_inside
matchExpectedFunTys herald ctx arity orig_ty thing_inside
  = case orig_ty of
      Check ty -> go [] arity ty
      _        -> defer [] arity orig_ty
  where
    -- Skolemise any foralls /before/ the zero-arg case
    -- so that we guarantee to return a rho-type
    go acc_arg_tys n ty
      | (tvs, theta, _) <- tcSplitSigmaTy ty
      , not (null tvs && null theta)
      = do { (wrap_gen, (wrap_res, result)) <- tcTopSkolemise ctx ty $ \ty' ->
                                               go acc_arg_tys n ty'
           ; return (wrap_gen <.> wrap_res, result) }

    -- No more args; do this /before/ coreView, so
    -- that we do not unnecessarily unwrap synonyms
    go acc_arg_tys 0 rho_ty
      = do { result <- thing_inside (reverse acc_arg_tys) (mkCheckExpType rho_ty)
           ; return (idHsWrapper, result) }

    go acc_arg_tys n ty
      | Just ty' <- coreView ty = go acc_arg_tys n ty'

    go acc_arg_tys n (FunTy { ft_af = af, ft_mult = mult, ft_arg = arg_ty, ft_res = res_ty })
      = assert (isVisibleFunArg af) $
        do { let arg_pos = 1 + length acc_arg_tys -- for error messages only
           ; (arg_co, arg_ty) <- hasFixedRuntimeRep (FRRExpectedFunTy herald arg_pos) arg_ty
           ; (wrap_res, result) <- go ((Scaled mult $ mkCheckExpType arg_ty) : acc_arg_tys)
                                      (n-1) res_ty
           ; let wrap_arg = mkWpCastN arg_co
                 fun_wrap = mkWpFun wrap_arg wrap_res (Scaled mult arg_ty) res_ty
           ; return (fun_wrap, result) }

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
    go acc_arg_tys n ty = addErrCtxtM (mk_ctxt acc_arg_tys ty) $
                          defer acc_arg_tys n (mkCheckExpType ty)

    ------------
    defer :: [Scaled ExpSigmaTypeFRR] -> Arity -> ExpRhoType -> TcM (HsWrapper, a)
    defer acc_arg_tys n fun_ty
      = do { let last_acc_arg_pos = length acc_arg_tys
           ; more_arg_tys <- mapM new_exp_arg_ty [last_acc_arg_pos + 1 .. last_acc_arg_pos + n]
           ; res_ty       <- newInferExpType
           ; result       <- thing_inside (reverse acc_arg_tys ++ more_arg_tys) res_ty
           ; more_arg_tys <- mapM (\(Scaled m t) -> Scaled m <$> readExpType t) more_arg_tys
           ; res_ty       <- readExpType res_ty
           ; let unif_fun_ty = mkScaledFunTys more_arg_tys res_ty
           ; wrap <- tcSubType AppOrigin ctx unif_fun_ty fun_ty
                         -- Not a good origin at all :-(
           ; return (wrap, result) }

    new_exp_arg_ty :: Int -> TcM (Scaled ExpSigmaTypeFRR)
    new_exp_arg_ty arg_pos -- position for error messages only
      = mkScaled <$> newFlexiTyVarTy multiplicityTy
                 <*> newInferExpTypeFRR (FRRExpectedFunTy herald arg_pos)

    ------------
    mk_ctxt :: [Scaled ExpSigmaTypeFRR] -> TcType -> TidyEnv -> TcM (TidyEnv, SDoc)
    mk_ctxt arg_tys res_ty env
      = mkFunTysMsg env herald arg_tys' res_ty arity
      where
        arg_tys' = map (\(Scaled u v) -> Scaled u (checkingExpType "matchExpectedFunTys" v)) $
                   reverse arg_tys
            -- this is safe b/c we're called from "go"

mkFunTysMsg :: TidyEnv
            -> ExpectedFunTyOrigin
            -> [Scaled TcType] -> TcType -> Arity
            -> TcM (TidyEnv, SDoc)
mkFunTysMsg env herald arg_tys res_ty n_val_args_in_call
  = do { (env', fun_rho) <- zonkTidyTcType env $
                            mkScaledFunTys arg_tys res_ty

       ; let (all_arg_tys, _) = splitFunTys fun_rho
             n_fun_args = length all_arg_tys

             msg | n_val_args_in_call <= n_fun_args  -- Enough args, in the end
                 = text "In the result of a function call"
                 | otherwise
                 = hang (full_herald <> comma)
                      2 (sep [ text "but its type" <+> quotes (pprType fun_rho)
                             , if n_fun_args == 0 then text "has none"
                               else text "has only" <+> speakN n_fun_args])

       ; return (env', msg) }
 where
  full_herald = pprExpectedFunTyHerald herald
            <+> speakNOf n_val_args_in_call (text "value argument")

----------------------
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
            -> TcRhoType   -- Actual -- a rho-type not a sigma-type
            -> ExpRhoType  -- Expected
            -> TcM HsWrapper
-- Similar signature to unifyExpectedType; does deep subsumption
-- Only one call site, in GHC.Tc.Gen.App.tcApp
tcSubTypeDS rn_expr act_rho res_ty
  = case res_ty of
      Check exp_rho -> tc_sub_type_deep (unifyType m_thing) orig
                                        GenSigCtxt act_rho exp_rho

      Infer inf_res -> do { co <- fillInferResult act_rho inf_res
                          ; return (mkWpCastN co) }
  where
    orig    = exprCtOrigin rn_expr
    m_thing = Just (HsExprRnThing rn_expr)

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
  = tc_sub_type_shallow (unifyType Nothing)
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
           ; let msg = vcat [ hang (text "When checking that:")
                                 4 (ppr ty_actual)
                            , nest 2 (hang (text "is more polymorphic than:")
                                         2 (ppr ty_expected)) ]
           ; return (tidy_env, msg) }


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
tc_sub_type, tc_sub_type_deep, tc_sub_type_shallow
    :: (TcType -> TcType -> TcM TcCoercionN)  -- How to unify
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
  = do { deep_subsumption <- xoptM LangExt.DeepSubsumption
       ; if deep_subsumption
         then tc_sub_type_deep    unify inst_orig ctxt ty_actual ty_expected
         else tc_sub_type_shallow unify inst_orig ctxt ty_actual ty_expected
  }

----------------------
tc_sub_type_shallow unify inst_orig ctxt ty_actual ty_expected
  | definitely_poly ty_expected   -- See Note [Don't skolemise unnecessarily]
  , definitely_mono_shallow ty_actual
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
           <- tcTopSkolemise ctxt ty_expected $ \ sk_rho ->
              do { (wrap, rho_a) <- topInstantiate inst_orig ty_actual
                 ; cow           <- unify rho_a sk_rho
                 ; return (mkWpCastN cow <.> wrap) }

       ; return (sk_wrap <.> inner_wrap) }

----------------------
tc_sub_type_deep unify inst_orig ctxt ty_actual ty_expected
  | definitely_poly ty_expected      -- See Note [Don't skolemise unnecessarily]
  , definitely_mono_deep ty_actual
  = do { traceTc "tc_sub_type_deep (drop to equality)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]
       ; mkWpCastN <$>
         unify ty_actual ty_expected }

  | otherwise   -- This is the general case
  = do { traceTc "tc_sub_type_deep (general case)" $
         vcat [ text "ty_actual   =" <+> ppr ty_actual
              , text "ty_expected =" <+> ppr ty_expected ]

       ; (sk_wrap, inner_wrap)
           <- tcDeeplySkolemise ctxt ty_expected $ \ sk_rho ->
              -- See Note [Deep subsumption]
              tc_sub_type_ds unify inst_orig ctxt ty_actual sk_rho

       ; return (sk_wrap <.> inner_wrap) }

definitely_mono_shallow :: TcType -> Bool
definitely_mono_shallow ty = isRhoTy ty
    -- isRhoTy: no top level forall or (=>)

definitely_mono_deep :: TcType -> Bool
definitely_mono_deep ty
  | not (definitely_mono_shallow ty)     = False
    -- isRhoTy: False means top level forall or (=>)
  | Just (_, res) <- tcSplitFunTy_maybe ty = definitely_mono_deep res
    -- Top level (->)
  | otherwise                              = True

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


Note [Wrapper returned from tcSubMult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is no notion of multiplicity coercion in Core, therefore the wrapper
returned by tcSubMult (and derived functions such as tcCheckUsage and
checkManyPattern) is quite unlike any other wrapper: it checks whether the
coercion produced by the constraint solver is trivial, producing a type error
if it is not. This is implemented via the WpMultCoercion wrapper, as desugared
by GHC.HsToCore.Binds.dsHsWrapper, which does the reflexivity check.

This wrapper needs to be placed in the term; otherwise, checking of the
eventual coercion won't be triggered during desugaring. But it can be put
anywhere, since it doesn't affect the desugared code.

Why do we check this in the desugarer? It's a convenient place, since it's
right after all the constraints are solved. We need the constraints to be
solved to check whether they are trivial or not.

An alternative would be to have a kind of constraint which can
only produce trivial evidence. This would allow such checks to happen
in the constraint solver (#18756).
This would be similar to the existing setup for Concrete, see
  Note [The Concrete mechanism] in GHC.Tc.Utils.Concrete
    (PHASE 1 in particular).
-}

tcSubMult :: CtOrigin -> Mult -> Mult -> TcM HsWrapper
tcSubMult origin w_actual w_expected
  | Just (w1, w2) <- isMultMul w_actual =
  do { w1 <- tcSubMult origin w1 w_expected
     ; w2 <- tcSubMult origin w2 w_expected
     ; return (w1 <.> w2) }
  -- Currently, we consider p*q and sup p q to be equal.  Therefore, p*q <= r is
  -- equivalent to p <= r and q <= r.  For other cases, we approximate p <= q by p
  -- ~ q.  This is not complete, but it's sound. See also Note [Overapproximating
  -- multiplicities] in Multiplicity.
tcSubMult origin w_actual w_expected =
  case submult w_actual w_expected of
    Submult -> return WpHole
    Unknown -> tcEqMult origin w_actual w_expected

tcEqMult :: CtOrigin -> Mult -> Mult -> TcM HsWrapper
tcEqMult origin w_actual w_expected = do
  {
  -- Note that here we do not call to `submult`, so we check
  -- for strict equality.
  ; coercion <- uType TypeLevel origin w_actual w_expected
  ; return $ if isReflCo coercion then WpHole else WpMultCoercion coercion }


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
   see the call to tcDeeplySkolemise in tc_sub_type_deep

2. In tcPolyExpr we must do deep skolemisation:
   see the call to tcDeeplySkolemise in tcSkolemiseExpType

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

Note that we /always/ use shallow subsumption in the ambiguity check.
See Note [Ambiguity check and deep subsumption].

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

The easiest solution was to use tcEqMult in tc_sub_type_ds, and
insist on equality. This is only in the DeepSubsumption code anyway.
-}

tc_sub_type_ds :: (TcType -> TcType -> TcM TcCoercionN)  -- How to unify
               -> CtOrigin       -- Used when instantiating
               -> UserTypeCtxt   -- Used when skolemising
               -> TcSigmaType    -- Actual; a sigma-type
               -> TcRhoType      -- Expected; deeply skolemised
               -> TcM HsWrapper

-- If wrap = tc_sub_type_ds t1 t2
--    => wrap :: t1 ~> t2
-- Here is where the work actually happens!
-- Precondition: ty_expected is deeply skolemised

tc_sub_type_ds unify inst_orig ctxt ty_actual ty_expected
  = do { traceTc "tc_sub_type_ds" $
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
                 do { traceTc "tc_sub_type_ds following filled meta-tyvar:"
                        (ppr tv_a <+> text "-->" <+> ppr ty_a')
                    ; tc_sub_type_ds unify inst_orig ctxt ty_a' ty_e }
               Nothing -> just_unify ty_actual ty_expected }

    go ty_a@(FunTy { ft_af = af1, ft_mult = act_mult, ft_arg = act_arg, ft_res = act_res })
       ty_e@(FunTy { ft_af = af2, ft_mult = exp_mult, ft_arg = exp_arg, ft_res = exp_res })
      | isVisibleFunArg af1, isVisibleFunArg af2
      = if (isTauTy ty_a && isTauTy ty_e)       -- Short cut common case to avoid
        then just_unify ty_actual ty_expected   -- unnecessary eta expansion
        else
        -- This is where we do the co/contra thing, and generate a WpFun, which in turn
        -- causes eta-expansion, which we don't like; hence encouraging NoDeepSubsumption
        do { arg_wrap  <- tc_sub_type_deep unify given_orig GenSigCtxt exp_arg act_arg
                          -- GenSigCtxt: See Note [Setting the argument context]
           ; res_wrap  <- tc_sub_type_ds   unify inst_orig  ctxt       act_res exp_res
           ; mult_wrap <- tcEqMult inst_orig act_mult exp_mult
                          -- See Note [Multiplicity in deep subsumption]
           ; return (mult_wrap <.>
                     mkWpFun arg_wrap res_wrap (Scaled exp_mult exp_arg) exp_res) }
                     -- arg_wrap :: exp_arg ~> act_arg
                     -- res_wrap :: act-res ~> exp_res
      where
        given_orig = GivenOrigin (SigSkol GenSigCtxt exp_arg [])

    go ty_a ty_e
      | let (tvs, theta, _) = tcSplitSigmaTy ty_a
      , not (null tvs && null theta)
      = do { (in_wrap, in_rho) <- topInstantiate inst_orig ty_a
           ; body_wrap <- tc_sub_type_ds unify inst_orig ctxt in_rho ty_e
           ; return (body_wrap <.> in_wrap) }

      | otherwise   -- Revert to unification
      = do { -- It's still possible that ty_actual has nested foralls. Instantiate
             -- these, as there's no way unification will succeed with them in.
             -- See typecheck/should_compile/T11305 for an example of when this
             -- is important. The problem is that we're checking something like
             --  a -> forall b. b -> b     <=   alpha beta gamma
             -- where we end up with alpha := (->)
             (inst_wrap, rho_a) <- deeplyInstantiate inst_orig ty_actual
           ; unify_wrap         <- just_unify rho_a ty_expected
           ; return (unify_wrap <.> inst_wrap) }

    just_unify ty_a ty_e = do { cow <- unify ty_a ty_e
                              ; return (mkWpCastN cow) }

tcDeeplySkolemise
    :: UserTypeCtxt -> TcSigmaType
    -> (TcType -> TcM result)
    -> TcM (HsWrapper, result)
        -- ^ The wrapper has type: spec_ty ~> expected_ty
-- Just like tcTopSkolemise, but calls
-- deeplySkolemise instead of topSkolemise
-- See Note [Deep skolemisation]
tcDeeplySkolemise ctxt expected_ty thing_inside
  | isTauTy expected_ty  -- Short cut for common case
  = do { res <- thing_inside expected_ty
       ; return (idHsWrapper, res) }
  | otherwise
  = do  { -- This (unpleasant) rec block allows us to pass skol_info to deeplySkolemise;
          -- but skol_info can't be built until we have tv_prs
          rec { (wrap, tv_prs, given, rho_ty) <- deeplySkolemise skol_info expected_ty
              ; skol_info <- mkSkolemInfo (SigSkol ctxt expected_ty tv_prs) }

        ; traceTc "tcDeeplySkolemise" (ppr expected_ty $$ ppr rho_ty $$ ppr tv_prs)

        ; let skol_tvs  = map snd tv_prs
        ; (ev_binds, result)
              <- checkConstraints (getSkolemInfo skol_info) skol_tvs given $
                 thing_inside rho_ty

        ; return (wrap <.> mkWpLet ev_binds, result) }
          -- The ev_binds returned by checkConstraints is very
          -- often empty, in which case mkWpLet is a no-op

deeplySkolemise :: SkolemInfo -> TcSigmaType
                -> TcM ( HsWrapper
                       , [(Name,TyVar)]     -- All skolemised variables
                       , [EvVar]            -- All "given"s
                       , TcRhoType )
-- See Note [Deep skolemisation]
deeplySkolemise skol_info ty
  = go init_subst ty
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType ty))

    go subst ty
      | Just (arg_tys, tvs, theta, ty') <- tcDeepSplitSigmaTy_maybe ty
      = do { let arg_tys' = substScaledTys subst arg_tys
           ; ids1           <- newSysLocalIds (fsLit "dk") arg_tys'
           ; (subst', tvs1) <- tcInstSkolTyVarsX skol_info subst tvs
           ; ev_vars1       <- newEvVars (substTheta subst' theta)
           ; (wrap, tvs_prs2, ev_vars2, rho) <- go subst' ty'
           ; let tv_prs1 = map tyVarName tvs `zip` tvs1
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
      | Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe ty
      = do { (subst', tvs') <- newMetaTyVarsX subst tvs
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
  :: TcSigmaType -> Maybe ([Scaled TcType], [TyVar], ThetaType, TcSigmaType)
-- Looks for a *non-trivial* quantified type, under zero or more function arrows
-- By "non-trivial" we mean either tyvars or constraints are non-empty

tcDeepSplitSigmaTy_maybe ty
  | Just (arg_ty, res_ty)           <- tcSplitFunTy_maybe ty
  , Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe res_ty
  = Just (arg_ty:arg_tys, tvs, theta, rho)

  | (tvs, theta, rho) <- tcSplitSigmaTy ty
  , not (null tvs && null theta)
  = Just ([], tvs, theta, rho)

  | otherwise = Nothing


{- *********************************************************************
*                                                                      *
                    Generalisation
*                                                                      *
********************************************************************* -}

{- Note [Skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~
tcTopSkolemise takes "expected type" and strip off quantifiers to expose the
type underneath, binding the new skolems for the 'thing_inside'
The returned 'HsWrapper' has type (specific_ty -> expected_ty).

Note that for a nested type like
   forall a. Eq a => forall b. Ord b => blah
we still only build one implication constraint
   forall a b. (Eq a, Ord b) => <constraints>
This is just an optimisation, but it's why we use topSkolemise to
build the pieces from all the layers, before making a single call
to checkConstraints.

tcSkolemiseScoped is very similar, but differs in two ways:

* It deals specially with just the outer forall, bringing those type
  variables into lexical scope.  To my surprise, I found that doing
  this unconditionally in tcTopSkolemise (i.e. doing it even if we don't
  need to bring the variables into lexical scope, which is harmless)
  caused a non-trivial (1%-ish) perf hit on the compiler.

* It handles deep subumption, wheres tcTopSkolemise never looks under
  function arrows.

* It always calls checkConstraints, even if there are no skolem
  variables at all.  Reason: there might be nested deferred errors
  that must not be allowed to float to top level.
  See Note [When to build an implication] below.
-}

tcTopSkolemise, tcSkolemiseScoped
    :: UserTypeCtxt -> TcSigmaType
    -> (TcType -> TcM result)
    -> TcM (HsWrapper, result)
        -- ^ The wrapper has type: spec_ty ~> expected_ty
-- See Note [Skolemisation] for the differences between
-- tcSkolemiseScoped and tcTopSkolemise

tcSkolemiseScoped ctxt expected_ty thing_inside
  = do { deep_subsumption <- xoptM LangExt.DeepSubsumption
       ; let skolemise | deep_subsumption = deeplySkolemise
                       | otherwise        = topSkolemise
       ; -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
         rec { (wrap, tv_prs, given, rho_ty) <- skolemise skol_info expected_ty
             ; skol_info <- mkSkolemInfo (SigSkol ctxt expected_ty tv_prs) }

       ; let skol_tvs = map snd tv_prs
       ; (ev_binds, res)
             <- checkConstraints (getSkolemInfo skol_info) skol_tvs given $
                tcExtendNameTyVarEnv tv_prs               $
                thing_inside rho_ty

       ; return (wrap <.> mkWpLet ev_binds, res) }

tcTopSkolemise ctxt expected_ty thing_inside
  | isRhoTy expected_ty  -- Short cut for common case
  = do { res <- thing_inside expected_ty
       ; return (idHsWrapper, res) }
  | otherwise
  = do { -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
         rec { (wrap, tv_prs, given, rho_ty) <- topSkolemise skol_info expected_ty
             ; skol_info <- mkSkolemInfo (SigSkol ctxt expected_ty tv_prs) }

       ; let skol_tvs = map snd tv_prs
       ; (ev_binds, result)
             <- checkConstraints (getSkolemInfo skol_info) skol_tvs given $
                thing_inside rho_ty

       ; return (wrap <.> mkWpLet ev_binds, result) }
         -- The ev_binds returned by checkConstraints is very
        -- often empty, in which case mkWpLet is a no-op

-- | Variant of 'tcTopSkolemise' that takes an ExpType
tcSkolemiseExpType :: UserTypeCtxt -> ExpSigmaType
                   -> (ExpRhoType -> TcM result)
                   -> TcM (HsWrapper, result)
tcSkolemiseExpType _ et@(Infer {}) thing_inside
  = (idHsWrapper, ) <$> thing_inside et
tcSkolemiseExpType ctxt (Check ty) thing_inside
  = do { deep_subsumption <- xoptM LangExt.DeepSubsumption
       ; let skolemise | deep_subsumption = tcDeeplySkolemise
                       | otherwise        = tcTopSkolemise
       ; skolemise ctxt ty $ \rho_ty ->
         thing_inside (mkCheckExpType rho_ty) }

checkConstraints :: SkolemInfoAnon
                 -> [TcTyVar]           -- Skolems
                 -> [EvVar]             -- Given
                 -> TcM result
                 -> TcM (TcEvBinds, result)

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
implication.  We must /always/ so if either 'skolems' or 'givens' are
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

* If you have
     f :: Int;  f = f_blah
     g :: Bool; g = g_blah
  If we don't build an implication for f or g (no tyvars, no givens),
  the constraints for f_blah and g_blah are solved together.  And that
  can yield /very/ confusing error messages, because we can get
      [W] C Int b1    -- from f_blah
      [W] C Int b2    -- from g_blan
  and fundpes can yield [W] b1 ~ b2, even though the two functions have
  literally nothing to do with each other.  #14185 is an example.
  Building an implication keeps them separate.
-}

{-
************************************************************************
*                                                                      *
                Boxy unification
*                                                                      *
************************************************************************

The exported functions are all defined as versions of some
non-exported generic functions.
-}

unifyType :: Maybe TypedThing  -- ^ If present, the thing that has type ty1
          -> TcTauType -> TcTauType    -- ty1 (actual), ty2 (expected)
          -> TcM TcCoercionN           -- :: ty1 ~# ty2
-- Actual and expected types
-- Returns a coercion : ty1 ~ ty2
unifyType thing ty1 ty2
  = uType TypeLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty1
                          , uo_expected = ty2
                          , uo_thing    = thing
                          , uo_visible  = True }

unifyTypeET :: TcTauType -> TcTauType -> TcM CoercionN
-- Like unifyType, but swap expected and actual in error messages
-- This is used when typechecking patterns
unifyTypeET ty1 ty2
  = uType TypeLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty2   -- NB swapped
                          , uo_expected = ty1   -- NB swapped
                          , uo_thing    = Nothing
                          , uo_visible  = True }


unifyKind :: Maybe TypedThing -> TcKind -> TcKind -> TcM CoercionN
unifyKind mb_thing ty1 ty2
  = uType KindLevel origin ty1 ty2
  where
    origin = TypeEqOrigin { uo_actual   = ty1
                          , uo_expected = ty2
                          , uo_thing    = mb_thing
                          , uo_visible  = True }


{-
%************************************************************************
%*                                                                      *
                 uType and friends
%*                                                                      *
%************************************************************************

uType is the heart of the unifier.
-}

uType, uType_defer
  :: TypeOrKind
  -> CtOrigin
  -> TcType    -- ty1 is the *actual* type
  -> TcType    -- ty2 is the *expected* type
  -> TcM CoercionN

--------------
-- It is always safe to defer unification to the main constraint solver
-- See Note [Deferred unification]
--    ty1 is "actual"
--    ty2 is "expected"
uType_defer t_or_k origin ty1 ty2
  = do { co <- emitWantedEq origin t_or_k Nominal ty1 ty2

       -- Error trace only
       -- NB. do *not* call mkErrInfo unless tracing is on,
       --     because it is hugely expensive (#5631)
       ; whenDOptM Opt_D_dump_tc_trace $ do
            { ctxt <- getErrCtxt
            ; doc <- mkErrInfo emptyTidyEnv ctxt
            ; traceTc "utype_defer" (vcat [ debugPprType ty1
                                          , debugPprType ty2
                                          , pprCtOrigin origin
                                          , doc])
            ; traceTc "utype_defer2" (ppr co)
            }
       ; return co }

--------------
uType t_or_k origin orig_ty1 orig_ty2
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
    go :: TcType -> TcType -> TcM CoercionN
        -- The arguments to 'go' are always semantically identical
        -- to orig_ty{1,2} except for looking through type synonyms

     -- Unwrap casts before looking for variables. This way, we can easily
     -- recognize (t |> co) ~ (t |> co), which is nice. Previously, we
     -- didn't do it this way, and then the unification above was deferred.
    go (CastTy t1 co1) t2
      = do { co_tys <- uType t_or_k origin t1 t2
           ; return (mkCoherenceLeftCo Nominal t1 co1 co_tys) }

    go t1 (CastTy t2 co2)
      = do { co_tys <- uType t_or_k origin t1 t2
           ; return (mkCoherenceRightCo Nominal t2 co2 co_tys) }

        -- Variables; go for uUnfilledVar
        -- Note that we pass in *original* (before synonym expansion),
        -- so that type variables tend to get filled in with
        -- the most informative version of the type
    go (TyVarTy tv1) ty2
      = do { lookup_res <- isFilledMetaTyVar_maybe tv1
           ; case lookup_res of
               Just ty1 -> do { traceTc "found filled tyvar" (ppr tv1 <+> text ":->" <+> ppr ty1)
                                ; go ty1 ty2 }
               Nothing  -> uUnfilledVar origin t_or_k NotSwapped tv1 ty2 }
    go ty1 (TyVarTy tv2)
      = do { lookup_res <- isFilledMetaTyVar_maybe tv2
           ; case lookup_res of
               Just ty2 -> do { traceTc "found filled tyvar" (ppr tv2 <+> text ":->" <+> ppr ty2)
                              ; go ty1 ty2 }
               Nothing  -> uUnfilledVar origin t_or_k IsSwapped tv2 ty1 }

      -- See Note [Expanding synonyms during unification]
    go ty1@(TyConApp tc1 []) (TyConApp tc2 [])
      | tc1 == tc2
      = return $ mkNomReflCo ty1

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
    -- Do not attempt (c => t); just defer
    go (FunTy { ft_af = af1, ft_mult = w1, ft_arg = arg1, ft_res = res1 })
       (FunTy { ft_af = af2, ft_mult = w2, ft_arg = arg2, ft_res = res2 })
      | isVisibleFunArg af1, af1 == af2
      = do { co_l <- uType t_or_k origin arg1 arg2
           ; co_r <- uType t_or_k origin res1 res2
           ; co_w <- uType t_or_k origin w1 w2
           ; return $ mkNakedFunCo1 Nominal af1 co_w co_l co_r }

        -- Always defer if a type synonym family (type function)
        -- is involved.  (Data families behave rigidly.)
    go ty1@(TyConApp tc1 _) ty2
      | isTypeFamilyTyCon tc1 = defer ty1 ty2
    go ty1 ty2@(TyConApp tc2 _)
      | isTypeFamilyTyCon tc2 = defer ty1 ty2

    go (TyConApp tc1 tys1) (TyConApp tc2 tys2)
      -- See Note [Mismatched type lists and application decomposition]
      | tc1 == tc2, equalLength tys1 tys2
      = assertPpr (isGenerativeTyCon tc1 Nominal) (ppr tc1) $
        do { cos <- zipWith3M (uType t_or_k) origins' tys1 tys2
           ; return $ mkTyConAppCo Nominal tc1 cos }
      where
        origins' = map (\is_vis -> if is_vis then origin else toInvisibleOrigin origin)
                       (tcTyConVisibilities tc1)

    go (LitTy m) ty@(LitTy n)
      | m == n
      = return $ mkNomReflCo ty

        -- See Note [Care with type applications]
        -- Do not decompose FunTy against App;
        -- it's often a type error, so leave it for the constraint solver
    go (AppTy s1 t1) (AppTy s2 t2)
      = go_app (isNextArgVisible s1) s1 t1 s2 t2

    go (AppTy s1 t1) (TyConApp tc2 ts2)
      | Just (ts2', t2') <- snocView ts2
      = assert (not (tyConMustBeSaturated tc2)) $
        go_app (isNextTyConArgVisible tc2 ts2') s1 t1 (TyConApp tc2 ts2') t2'

    go (TyConApp tc1 ts1) (AppTy s2 t2)
      | Just (ts1', t1') <- snocView ts1
      = assert (not (tyConMustBeSaturated tc1)) $
        go_app (isNextTyConArgVisible tc1 ts1') (TyConApp tc1 ts1') t1' s2 t2

    go (CoercionTy co1) (CoercionTy co2)
      = do { let ty1 = coercionType co1
                 ty2 = coercionType co2
           ; kco <- uType KindLevel
                          (KindEqOrigin orig_ty1 orig_ty2 origin
                                        (Just t_or_k))
                          ty1 ty2
           ; return $ mkProofIrrelCo Nominal kco co1 co2 }

        -- Anything else fails
        -- E.g. unifying for-all types, which is relative unusual
    go ty1 ty2 = defer ty1 ty2

    ------------------
    defer ty1 ty2   -- See Note [Check for equality before deferring]
      | ty1 `tcEqType` ty2 = return (mkNomReflCo ty1)
      | otherwise          = uType_defer t_or_k origin ty1 ty2

    ------------------
    go_app vis s1 t1 s2 t2
      = do { co_s <- uType t_or_k origin s1 s2
           ; let arg_origin
                   | vis       = origin
                   | otherwise = toInvisibleOrigin origin
           ; co_t <- uType t_or_k arg_origin t1 t2
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
   now a type synonym.

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
uUnfilledVar :: CtOrigin
             -> TypeOrKind
             -> SwapFlag
             -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                               --    definitely not a /filled/ meta-tyvar
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
              -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                                --    definitely not a /filled/ meta-tyvar
              -> TcTauType      -- Type 2, zonked
              -> TcM Coercion
uUnfilledVar1 origin t_or_k swapped tv1 ty2
  | Just tv2 <- getTyVar_maybe ty2
  = go tv2

  | otherwise
  = uUnfilledVar2 origin t_or_k swapped tv1 ty2

  where
    -- 'go' handles the case where both are
    -- tyvars so we might want to swap
    -- E.g. maybe tv2 is a meta-tyvar and tv1 is not
    go tv2 | tv1 == tv2  -- Same type variable => no-op
           = return (mkNomReflCo (mkTyVarTy tv1))

           | swapOverTyVars False tv1 tv2   -- Distinct type variables
               -- Swap meta tyvar to the left if poss
           = do { tv1 <- zonkTyCoVarKind tv1
                     -- We must zonk tv1's kind because that might
                     -- not have happened yet, and it's an invariant of
                     -- uUnfilledTyVar2 that ty2 is fully zonked
                     -- Omitting this caused #16902
                ; uUnfilledVar2 origin t_or_k (flipSwap swapped)
                           tv2 (mkTyVarTy tv1) }

           | otherwise
           = uUnfilledVar2 origin t_or_k swapped tv1 ty2

----------
uUnfilledVar2 :: CtOrigin
              -> TypeOrKind
              -> SwapFlag
              -> TcTyVar        -- Tyvar 1: not necessarily a meta-tyvar
                                --    definitely not a /filled/ meta-tyvar
              -> TcTauType      -- Type 2, zonked
              -> TcM Coercion
uUnfilledVar2 origin t_or_k swapped tv1 ty2
  = do { cur_lvl <- getTcLevel
           -- See Note [Unification preconditions], (UNTOUCHABLE) wrinkles
           -- Here we don't know about given equalities here; so we treat
           -- /any/ level outside this one as untouchable.  Hence cur_lvl.
       ; if not (touchabilityAndShapeTest cur_lvl tv1 ty2
                 && simpleUnifyCheck False tv1 ty2)
         then not_ok_so_defer
         else
    do { co_k <- uType KindLevel kind_origin (typeKind ty2) (tyVarKind tv1)
       ; traceTc "uUnfilledVar2 ok" $
         vcat [ ppr tv1 <+> dcolon <+> ppr (tyVarKind tv1)
              , ppr ty2 <+> dcolon <+> ppr (typeKind  ty2)
              , ppr (isReflCo co_k), ppr co_k ]

       ; if isReflCo co_k
           -- Only proceed if the kinds match
           -- NB: tv1 should still be unfilled, despite the kind unification
           --     because tv1 is not free in ty2' (or, hence, in its kind)
         then do { writeMetaTyVar tv1 ty2
                 ; return (mkNomReflCo ty2) }

         else defer  -- This cannot be solved now.  See GHC.Tc.Solver.Canonical
                     -- Note [Equalities with incompatible kinds] for how
                     -- this will be dealt with in the solver
         }}
  where
    ty1 = mkTyVarTy tv1
    kind_origin = KindEqOrigin ty1 ty2 origin (Just t_or_k)

    defer = unSwap swapped (uType_defer t_or_k origin) ty1 ty2

    not_ok_so_defer =
      do { traceTc "uUnfilledVar2 not ok" (ppr tv1 $$ ppr ty2)
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
-- See Note [TyVar/TyVar orientation]
lhsPriority tv
  = assertPpr (isTyVar tv) (ppr tv) $
    case tcTyVarDetails tv of
      RuntimeUnk  -> 0
      SkolemTv {} -> 0
      MetaTv { mtv_info = info } -> case info of
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
   where co :: Type ~ k is an unsolved wanted. Note that this
   equality is homogeneous; both sides have kind k. Unifying here
   is sensible, but it can lead to very confusing error messages.
   It's very much like a Wanted rewriting a Wanted. Even worse,
   unifying a variable essentially turns an equality into a Given,
   and so we could not use the tracking mechanism in
   Note [Wanteds rewrite Wanteds] in GHC.Tc.Types.Constraint.
   We thus simply do not unify in this case.

   This is expanded as Wrinkle (2) in Note [Equalities with incompatible kinds]
   in GHC.Tc.Solver.Equality


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
    GHC.Tc.Solver.Canonical.touchabilityAndShapeTest. More details in
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
  - in the constraint solver, in GHC.Tc.Solver.Canonical.canEqCanLHS2

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

The same applies later on in the constraint interaction code; see GHC.Tc.Solver.Interact,
function @occ_check_ok@.

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
           ; return (mkNakedFunCo1 Nominal af (mkNomReflCo w) (mkNomReflCo arg) co) }

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
           ; uType KindLevel origin k new_fun }

{- *********************************************************************
*                                                                      *
                 Checking alpha ~ ty
              for the on-the-fly unifier
*                                                                      *
********************************************************************* -}

{- Commented out because I think we can just use the simple,
   efficient simpleUnifyCheck instead; we can always defer.

uTypeCheckTouchableTyVarEq :: TcTyVar -> TcType -> TcM (PuResult () TcType)
-- The check may expand type synonyms to avoid an occurs check,
-- so we must use the return type
--
-- Precondition: rhs is fully zonked
uTypeCheckTouchableTyVarEq lhs_tv rhs
  | simpleUnifyCheck False lhs_tv rhs  -- Do a fast-path check
     -- False <=> See Note [Prevent unification with type families]
  = return (pure rhs)

  | otherwise
  = do { traceTc "uTypeCheckTouchableTyVarEq {" (pprTyVar lhs_tv $$ ppr rhs)
       ; check_result <- checkTyEqRhs flags rhs :: TcM (PuResult () Reduction)
       ; traceTc "uTypeCheckTouchableTyVarEq }" (ppr check_result)
       ; case check_result of
            PuFail reason -> return (PuFail reason)
            PuOK redn _   -> assertPpr (isReflCo (reductionCoercion redn))
                                       (ppr lhs_tv $$ ppr rhs $$ ppr redn) $
                             return (pure (reductionReducedType redn)) }
  where
    flags | MetaTv { mtv_info = tv_info, mtv_tclvl = tv_lvl } <- tcTyVarDetails lhs_tv
          = TEF { tef_foralls  = isRuntimeUnkSkol lhs_tv
                , tef_fam_app  = TEFA_Fail
                , tef_unifying = Unifying tv_info tv_lvl LC_None
                , tef_lhs      = TyVarLHS lhs_tv
                , tef_occurs   = cteInsolubleOccurs }
          | otherwise
          = pprPanic "uTypeCheckTouchableTyVarEq" (ppr lhs_tv)
          -- TEFA_Fail: See Note [Prevent unification with type families]
-}

simpleUnifyCheck :: Bool -> TcTyVar -> TcType -> Bool
-- A fast check: True <=> unification is OK
-- If it says 'False' then unification might still be OK, but
-- it'll take more work to do -- use the full checkTypeEq
--
-- * Always rejects foralls unless lhs_tv is RuntimeUnk
--   (used by GHCi debugger)
-- * Rejects a non-concrete type if lhs_tv is concrete
-- * Rejects type families unless fam_ok=True
-- * Does a level-check for type variables
--
-- This function is pretty heavily used, so it's optimised not to allocate
simpleUnifyCheck fam_ok lhs_tv rhs
  = go rhs
  where
    !(occ_in_ty, occ_in_co) = mkOccFolders lhs_tv

    lhs_tv_lvl         = tcTyVarLevel lhs_tv
    lhs_tv_is_concrete = isConcreteTyVar lhs_tv
    forall_ok          = case tcTyVarDetails lhs_tv of
                            MetaTv { mtv_info = RuntimeUnkTv } -> True
                            _                                  -> False

    go (TyVarTy tv)
      | lhs_tv == tv                                 = False
      | tcTyVarLevel tv > lhs_tv_lvl                 = False
      | lhs_tv_is_concrete, not (isConcreteTyVar tv) = False
      | occ_in_ty $! (tyVarKind tv)                  = False
      | otherwise                                    = True

    go (FunTy {ft_af = af, ft_mult = w, ft_arg = a, ft_res = r})
      | isInvisibleFunArg af, not forall_ok = False
      | otherwise                           = go w && go a && go r

    go (TyConApp tc tys)
      | lhs_tv_is_concrete, not (isConcreteTyCon tc) = False
      | not (isTauTyCon tc)                          = False
      | not fam_ok, not (isFamFreeTyCon tc)          = False
      | otherwise                                    = all go tys

    go (AppTy t1 t2)    = go t1 && go t2
    go (ForAllTy (Bndr tv _) ty)
      | forall_ok = go (tyVarKind tv) && (tv == lhs_tv || go ty)
      | otherwise = False

    go (CastTy ty co)   = not (occ_in_co co) && go ty
    go (CoercionTy co)  = not (occ_in_co co)
    go (LitTy {})       = True


mkOccFolders :: TcTyVar -> (TcType -> Bool, TcCoercion -> Bool)
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

    do_tcv is v = Any (not (v `elemVarSet` is) && v == lhs_tv)
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
                  | PuOK b (Bag a)

instance Functor (PuResult a) where
  fmap _ (PuFail prob) = PuFail prob
  fmap f (PuOK x cts)  = PuOK (f x) cts

instance Applicative (PuResult a) where
  pure x = PuOK x emptyBag
  PuFail p1 <*> PuFail p2 = PuFail (p1 S.<> p2)
  PuFail p1 <*> PuOK {}   = PuFail p1
  PuOK {}   <*> PuFail p2 = PuFail p2
  PuOK f c1 <*> PuOK x c2 = PuOK (f x) (c1 `unionBags` c2)

instance (Outputable a, Outputable b) => Outputable (PuResult a b) where
  ppr (PuFail prob) = text "PuFail" <+> (ppr prob)
  ppr (PuOK x cts)  = text "PuOK" <> braces
                        (vcat [ text "redn:" <+> ppr x
                              , text "cts:" <+> ppr cts ])

pprPur :: PuResult a b -> SDoc
-- For debugging
pprPur (PuFail prob) = text "PuFail:" <> ppr prob
pprPur (PuOK {})     = text "PuOK"

okCheckRefl :: TcType -> TcM (PuResult a Reduction)
okCheckRefl ty = return (PuOK (mkReflRedn Nominal ty) emptyBag)

failCheckWith :: CheckTyEqResult -> TcM (PuResult a b)
failCheckWith p = return (PuFail p)

mapCheck :: (x -> TcM (PuResult a Reduction))
         -> [x]
         -> TcM (PuResult a Reductions)
mapCheck f xs
  = do { (ress :: [PuResult a Reduction]) <- mapM f xs
       ; return (unzipRedns <$> sequenceA ress) }
         -- sequenceA :: [PuResult a Reduction] -> PuResult a [Reduction]
         -- unzipRedns :: [Reduction] -> Reductions

-----------------------------
-- | Options describing how to deal with a type equality
-- in the pure unifier. See 'checkTyEqRhs'
data TyEqFlags a
  = TEF { tef_foralls  :: Bool         -- Allow foralls
        , tef_lhs      :: CanEqLHS     -- LHS of the constraint
        , tef_unifying :: AreUnifying  -- Always NotUnifying if tef_lhs is TyFamLHS
        , tef_fam_app  :: TyEqFamApp a
        , tef_occurs   :: CheckTyEqProblem }  -- Soluble or insoluble occurs check

-- | What to do when encountering a type-family application while processing
-- a type equality in the pure unifier.
--
-- See Note [Family applications in canonical constraints]
data TyEqFamApp a
  = TEFA_Fail                    -- Always fail
  | TEFA_Recurse                 -- Just recurse
  | TEFA_Break (FamAppBreaker a) -- Recurse, but replace with cycle breaker if fails,
                                 -- using the FamAppBreaker

data AreUnifying
  = Unifying
       MetaInfo         -- MetaInfo of the LHS tyvar (which is a meta-tyvar)
       TcLevel          -- Level of the LHS tyvar
       LevelCheck

  | NotUnifying         -- Not attempting to unify

data LevelCheck
  = LC_None       -- Level check not needed: we should never encounter
                  -- a tyvar at deeper level than the LHS

  | LC_Check      -- Do a level check between the LHS tyvar and the occurrence tyvar
                  -- Fail if the level check fails

  | LC_Promote    -- Do a level check between the LHS tyvar and the occurrence tyvar
                  -- If the level check fails, and the occurrence is a unification
                  -- variable, promote it

instance Outputable (TyEqFlags a) where
  ppr (TEF { .. }) = text "TEF" <> braces (
                        vcat [ text "tef_foralls =" <+> ppr tef_foralls
                             , text "tef_lhs =" <+> ppr tef_lhs
                             , text "tef_unifying =" <+> ppr tef_unifying
                             , text "tef_fam_app =" <+> ppr tef_fam_app
                             , text "tef_occurs =" <+> ppr tef_occurs ])

instance Outputable (TyEqFamApp a) where
  ppr TEFA_Fail       = text "TEFA_Fail"
  ppr TEFA_Recurse    = text "TEFA_Fail"
  ppr (TEFA_Break {}) = text "TEFA_Break"

instance Outputable AreUnifying where
  ppr NotUnifying = text "NotUnifying"
  ppr (Unifying mi lvl lc) = text "Unifying" <+>
         braces (ppr mi <> comma <+> ppr lvl <> comma <+> ppr lc)

instance Outputable LevelCheck where
  ppr LC_None    = text "LC_None"
  ppr LC_Check   = text "LC_Check"
  ppr LC_Promote = text "LC_Promote"

famAppArgFlags :: TyEqFlags a -> TyEqFlags a
-- Adjust the flags when going undter a type family
-- Only the outer family application gets the loop-breaker treatment
-- Ditto tyvar promotion.  E.g.
--        [W] alpha[2] ~ Maybe (F beta[3])
-- Do not promote beta[3]; instead promote (F beta[3])
famAppArgFlags flags@(TEF { tef_unifying = unifying })
  = flags { tef_fam_app  = TEFA_Recurse
          , tef_unifying = zap_promotion unifying
          , tef_occurs   = cteSolubleOccurs }
            -- tef_occurs: under a type family, an occurs check is not definitely-insoluble
  where
    zap_promotion (Unifying info lvl LC_Promote) = Unifying info lvl LC_Check
    zap_promotion unifying                       = unifying

type FamAppBreaker a = TcType -> TcM (PuResult a Reduction)
     -- Given a family-application ty, return a Reduction :: ty ~ cvb
     -- where 'cbv' is a fresh loop-breaker tyvar (for Given), or
     -- just a fresh TauTv (for Wanted)

checkTyEqRhs :: forall a. TyEqFlags a -> TcType -> TcM (PuResult a Reduction)
checkTyEqRhs flags ty
  = case ty of
      LitTy {}        -> okCheckRefl ty
      TyConApp tc tys -> checkTyConApp flags ty tc tys
      TyVarTy tv      -> checkTyVar flags tv
        -- Don't worry about foralls inside the kind; see Note [Checking for foralls]
        -- Nor can we expand synonyms; see Note [Occurrence checking: look inside kinds]
        --                             in GHC.Core.FVs

      FunTy {ft_af = af, ft_mult = w, ft_arg = a, ft_res = r}
       | isInvisibleFunArg af  -- e.g.  Num a => blah
       , not (tef_foralls flags)
       -> failCheckWith impredicativeProblem -- Not allowed (TyEq:F)
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

      ForAllTy {}
         | tef_foralls flags -> okCheckRefl ty
         | otherwise         -> failCheckWith impredicativeProblem  -- Not allowed (TyEq:F)


-------------------
checkCo :: TyEqFlags a -> Coercion -> TcM (PuResult a Coercion)
-- See Note [checkCo]
checkCo (TEF { tef_lhs = TyFamLHS {} }) co
  = return (PuOK co emptyBag)

checkCo (TEF { tef_lhs = TyVarLHS lhs_tv
             , tef_unifying = unifying
             , tef_occurs = occ_prob }) co
  -- Check for coercion holes, if unifying
  -- See (COERCION-HOLE) in Note [Unification preconditions]
  | Unifying {} <- unifying
  , hasCoercionHoleCo co
  = failCheckWith (cteProblem cteCoercionHole)

  -- Occurs check (can promote)
  | Unifying _ lhs_tv_lvl LC_Promote <- unifying
  = do { reason <- checkPromoteFreeVars occ_prob lhs_tv lhs_tv_lvl (tyCoVarsOfCo co)
       ; if cterHasNoProblem reason
         then return (pure co)
         else failCheckWith reason }

  -- Occurs check (no promotion)
  | lhs_tv `elemVarSet` tyCoVarsOfCo co
  = failCheckWith (cteProblem occ_prob)

  | otherwise
  = return (PuOK co emptyBag)

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
checkTyConApp :: TyEqFlags a
              -> TcType -> TyCon -> [TcType]
              -> TcM (PuResult a Reduction)
checkTyConApp flags@(TEF { tef_unifying = unifying, tef_foralls = foralls_ok })
              tc_app tc tys
  | isTypeFamilyTyCon tc
  , let arity = tyConArity tc
  = if tys `lengthIs` arity
    then checkFamApp flags tc_app tc tys  -- Common case
    else do { let (fun_args, extra_args) = splitAt (tyConArity tc) tys
                  fun_app                = mkTyConApp tc fun_args
            ; fun_res   <- checkFamApp flags fun_app tc fun_args
            ; extra_res <- mapCheck (checkTyEqRhs flags) extra_args
            ; traceTc "Over-sat" (ppr tc <+> ppr tys $$ ppr arity $$ pprPur fun_res $$ pprPur extra_res)
            ; return (mkAppRedns <$> fun_res <*> extra_res) }

  | Just ty' <- rewriterView tc_app
       -- e.g. S a  where  type S a = F [a]
       --             or   type S a = Int
       -- See Note [Forgetful synonyms in checkTyConApp]
  = checkTyEqRhs flags ty'

  | not (isTauTyCon tc || foralls_ok)
  = failCheckWith impredicativeProblem

  | Unifying info _ _ <- unifying
  , isConcreteInfo info
  , not (isConcreteTyCon tc)
  = failCheckWith (cteProblem cteConcrete)

  | otherwise  -- Recurse on arguments
  = recurseIntoTyConApp flags tc tys

recurseIntoTyConApp :: TyEqFlags a -> TyCon -> [TcType] -> TcM (PuResult a Reduction)
recurseIntoTyConApp flags tc tys
  = do { tys_res <- mapCheck (checkTyEqRhs flags) tys
       ; return (mkTyConAppRedn Nominal tc <$> tys_res) }

-------------------
checkFamApp :: TyEqFlags a
            -> TcType -> TyCon -> [TcType]  -- Saturated family application
            -> TcM (PuResult a Reduction)
-- See Note [Family applications in canonical constraints]
checkFamApp flags@(TEF { tef_unifying = unifying, tef_occurs = occ_prob
                       , tef_fam_app = fam_app_flag, tef_lhs = lhs })
            fam_app tc tys
  = case fam_app_flag of
      TEFA_Fail -> failCheckWith (cteProblem cteTypeFamily)

      _ | TyFamLHS lhs_tc lhs_tys <- lhs
        , tcEqTyConApps lhs_tc lhs_tys tc tys   -- F ty ~ ...(F ty)...
        -> case fam_app_flag of
             TEFA_Recurse       -> failCheckWith (cteProblem occ_prob)
             TEFA_Break breaker -> breaker fam_app

      _ | Unifying lhs_info _ _ <- unifying
        , isConcreteInfo lhs_info
        -> case fam_app_flag of
             TEFA_Recurse       -> failCheckWith (cteProblem cteConcrete)
             TEFA_Break breaker -> breaker fam_app

      TEFA_Recurse
        -> do { tys_res <- mapCheck (checkTyEqRhs arg_flags) tys
              ; traceTc "under" (ppr tc $$ pprPur tys_res $$ ppr flags)
              ; return (mkTyConAppRedn Nominal tc <$> tys_res) }

      TEFA_Break breaker    -- Recurse; and break if there is a problem
        -> do { tys_res <- mapCheck (checkTyEqRhs arg_flags) tys
              ; case tys_res of
                  PuOK redns cts -> return (PuOK (mkTyConAppRedn Nominal tc redns) cts)
                  PuFail {}      -> breaker fam_app }
  where
    arg_flags = famAppArgFlags flags

-------------------
checkTyVar :: forall a. TyEqFlags a -> TcTyVar -> TcM (PuResult a Reduction)
checkTyVar (TEF { tef_lhs = lhs, tef_unifying = unifying, tef_occurs = occ_prob }) occ_tv
  = case lhs of
      TyFamLHS {}     -> success   -- Nothing to do if the LHS is a type-family
      TyVarLHS lhs_tv -> check_tv unifying lhs_tv
  where
    lvl_occ = tcTyVarLevel occ_tv
    success = okCheckRefl (mkTyVarTy occ_tv)

    ---------------------
    check_tv NotUnifying lhs_tv
      = simple_occurs_check lhs_tv
      -- We need an occurs-check here, but no level check
      -- See Note [Promotion and level-checking] wrinkle (W1)

    check_tv (Unifying info lvl prom) lhs_tv
      = do { mb_done <- isFilledMetaTyVar_maybe occ_tv
           ; case mb_done of
               Just {} -> success
               -- Already promoted; job done
               -- Example alpha[2] ~ Maybe (beta[4], beta[4])
               -- We promote the first occurrence, and then encounter it
               -- a second time; we don't want to re-promote it!
               -- Remember, the entire process started with a fully zonked type

               Nothing -> check_unif info lvl prom lhs_tv }

    ---------------------
    -- We are in the Unifying branch of AreUnifing
    check_unif :: MetaInfo -> TcLevel -> LevelCheck
               -> TcTyVar -> TcM (PuResult a Reduction)
    check_unif lhs_tv_info lhs_tv_lvl prom lhs_tv
      | isConcreteInfo lhs_tv_info
      , not (isConcreteTyVar occ_tv)
      = if can_make_concrete occ_tv
        then promote lhs_tv lhs_tv_info lhs_tv_lvl
        else failCheckWith (cteProblem cteConcrete)

      | lvl_occ `strictlyDeeperThan` lhs_tv_lvl
      = case prom of
           LC_None    -> pprPanic "check_unif" (ppr lhs_tv $$ ppr occ_tv)
           LC_Check   -> failCheckWith (cteProblem cteSkolemEscape)
           LC_Promote
             | isSkolemTyVar occ_tv  -> failCheckWith (cteProblem cteSkolemEscape)
             | otherwise             -> promote lhs_tv lhs_tv_info lhs_tv_lvl

      | otherwise
      = simple_occurs_check lhs_tv

    ---------------------
    simple_occurs_check lhs_tv
      | lhs_tv == occ_tv || check_kind (tyVarKind occ_tv)
      = failCheckWith (cteProblem occ_prob)
      | otherwise
      = success
      where
        (check_kind, _) = mkOccFolders lhs_tv

    ---------------------
    can_make_concrete occ_tv = case tcTyVarDetails occ_tv of
      MetaTv { mtv_info = info } -> case info of
                                      ConcreteTv {} -> True
                                      TauTv {}      -> True
                                      _             -> False
      _ -> False  -- Don't attempt to make other type variables concrete
                  -- (e.g. SkolemTv, TyVarTv, CycleBreakerTv, RuntimeUnkTv).

    ---------------------
    -- occ_tv is definitely a MetaTyVar
    promote lhs_tv lhs_tv_info lhs_tv_lvl
      | MetaTv { mtv_info = info_occ, mtv_tclvl = lvl_occ } <- tcTyVarDetails occ_tv
      = do { let new_info | isConcreteInfo lhs_tv_info = lhs_tv_info
                          | otherwise                  = info_occ
                 new_lvl = lhs_tv_lvl `minTcLevel` lvl_occ
                           -- c[conc,3] ~ p[tau,2]: want to clone p:=p'[conc,2]
                           -- c[tau,2]  ~ p[tau,3]: want to clone p:=p'[tau,2]

           -- Check the kind of occ_tv
           ; reason <- checkPromoteFreeVars occ_prob lhs_tv lhs_tv_lvl (tyCoVarsOfType (tyVarKind occ_tv))

           ; if cterHasNoProblem reason  -- Successfully promoted
             then do { new_tv_ty <- promote_meta_tyvar new_info new_lvl occ_tv
                     ; okCheckRefl new_tv_ty }
             else failCheckWith reason }

      | otherwise = pprPanic "promote" (ppr occ_tv)

-------------------------
checkPromoteFreeVars :: CheckTyEqProblem    -- What occurs check problem to report
                     -> TcTyVar -> TcLevel
                     -> TyCoVarSet -> TcM CheckTyEqResult
-- Check this set of TyCoVars for
--   (a) occurs check
--   (b) promote if necessary, or report skolem escape
checkPromoteFreeVars occ_prob lhs_tv lhs_tv_lvl vs
  = do { oks <- mapM do_one (nonDetEltsUniqSet vs)
       ; return (mconcat oks) }
  where
    do_one :: TyCoVar -> TcM CheckTyEqResult
    do_one v | isCoVar v           = return cteOK
             | lhs_tv == v         = return (cteProblem occ_prob)
             | no_promotion        = return cteOK
             | not (isMetaTyVar v) = return (cteProblem cteSkolemEscape)
             | otherwise           = promote_one v
      where
        no_promotion = not (tcTyVarLevel v `strictlyDeeperThan` lhs_tv_lvl)

    -- isCoVar case: coercion variables are not an escape risk
    -- If an implication binds a coercion variable, it'll have equalities,
    -- so the "intervening given equalities" test above will catch it
    -- Coercion holes get filled with coercions, so again no problem.

    promote_one tv = do { _ <- promote_meta_tyvar TauTv lhs_tv_lvl tv
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
       ; writeMetaTyVar occ_tv (mkTyVarTy new_tv)
       ; traceTc "promoteTyVar" (ppr occ_tv <+> text "-->" <+> ppr new_tv)
       ; return (mkTyVarTy new_tv) } } }



-------------------------
touchabilityAndShapeTest :: TcLevel -> TcTyVar -> TcType -> Bool
-- This is the key test for untouchability:
-- See Note [Unification preconditions] in GHC.Tc.Utils.Unify
-- and Note [Solve by unification] in GHC.Tc.Solver.Interact
-- True <=> touchability and shape are OK
touchabilityAndShapeTest given_eq_lvl tv rhs
  | MetaTv { mtv_info = info, mtv_tclvl = tv_lvl } <- tcTyVarDetails tv
  , checkTopShape info rhs
  = tv_lvl `deeperThanOrSame` given_eq_lvl
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

