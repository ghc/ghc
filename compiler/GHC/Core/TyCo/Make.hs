-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998
--
-- Type - public interface

{-# LANGUAGE PatternSynonyms, ViewPatterns, MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Simple contructors and simple views for types and coercions
--   In particular, no use of substitution
module GHC.Core.TyCo.Make (
    -- TyVarTy
      mkTyVarTy, mkTyVarTys

    -- AppTy
    , mkAppTy, mkAppTys

    -- FunTy
    , mkFunTy, mkVisFunTy
    , mkVisFunTyMany, mkVisFunTysMany
    , mkScaledFunTys, mkInvisFunTy, mkInvisFunTys
    , tcMkVisFunTy, tcMkScaledFunTys, tcMkInvisFunTy

    -- TyConApp
    , mkTyConApp, mkTyConTy
    , typeOrConstraintKind, liftedTypeOrConstraintKind
    , tyConAppFunTy_maybe, tyConAppFunCo_maybe
    , mkTYPEapp, mkTYPEapp_maybe, mkCONSTRAINTapp, mkCONSTRAINTapp_maybe
    , mkBoxedRepApp_maybe, mkTupleRepApp_maybe

    -- ForAllTy
    , mkForAllTy, mkForAllTys, mkInvisForAllTys, mkTyCoInvForAllTys
    , mkSpecForAllTy, mkSpecForAllTys
    , mkVisForAllTys, mkTyCoForAllTy, mkTyCoForAllTys, mkTyCoInvForAllTy
    , mkInfForAllTy, mkInfForAllTys

    -- LitTy
    , mkNumLitTy
    , mkStrLitTy
    , mkCharLitTy

    -- CastTy
    , mkCastTy, mkCastTyMCo

    -- CoercionTy
    , mkCoercionTy

    -- Coercions
    , mkReflCo, mkNomReflCo, mkRepReflCo
    , mkCoVarCo, mkCoVarCos, mkHoleCo
    , mkKindCo, mkSelCo
    , mkGReflCo, mkGReflRightCo, mkGReflLeftCo
    , mkGReflMCo, mkGReflRightMCo, mkGReflLeftMCo
    , mkSubCo, mkSymCo, mkSymMCo
    , mkFunCo2, mkForAllCo, mkUnivCo
    , mkAxiomCo, mkUnbranchedAxInstCo
    , mkAppCo, mkTyConAppCo
    , mkInstCo, mkLRCo
    , mkFunCo, mkNakedFunCo
    , mkNakedForAllCo, mkForAllVisCos, mkHomoForAllCo, mkHomoForAllCos
    , mkAxInstCo
    , mkTransCo, mkTransMCo, mkTransMCoL, mkTransMCoR
    , mkProofIrrelCo
    , coToMCo, kindCoToMKindCo
    , mkCoherenceLeftCo, mkCoherenceRightCo, mkCoherenceRightMCo
    , toPhantomCo, mkPhantomCo
    , isReflCo, isReflMCo, isReflKindCo, isReflKindMCo, isReflCo_maybe

    -- Roles
    , tyConRole, tyConRolesX, tyConRoleListX
    , tyConRolesRepresentational, tyConRoleListRepresentational
    , eqTyConRole, ltRole
    , funRole, downgradeRole, downgradeRole_maybe

    -- Binders
    , mkTyConBindersPreferAnon

    -- Mapping over types
    , TyCoMapper(..), mapTyCo, mapTyCoX
  ) where


import GHC.Prelude

import {-# SOURCE #-} GHC.Core.Coercion
    ( mkLRCo, mkInstCo, mkAppCo, mkAppCos, mkTyConAppCo, mkSelCo
    , decomposePiCos
    , isKindCo
    , coercionKind -- Used in toPhantomCo
    -- Used in assertions only
    , assertGoodForAllCo, coercionRole
    )

import {-# SOURCE #-} GHC.Core.Type
   ( mkCastTy
   , typeKind     -- Used in mkKindCo
   )

import {-# SOURCE #-} GHC.Builtin.Types
   ( liftedTypeKind, unliftedTypeKind
   , constraintKind, zeroBitTypeKind
   , manyDataConTy, liftedRepTy, unliftedRepTy, zeroBitRepTy
   )

import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.Coercion.Axiom

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim( tYPETyCon, cONSTRAINTTyCon )

import GHC.Types.Basic
import GHC.Types.Var
import GHC.Types.Var.Set

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import qualified GHC.Data.List.Infinite as Inf
import GHC.Data.List.Infinite (Infinite (..))

import GHC.Data.FastString
import GHC.Data.Pair


{- *********************************************************************
*                                                                      *
               mapType
*                                                                      *
************************************************************************

These functions do a map-like operation over types, performing some operation
on all variables and binding sites. Primarily used for zonking.

Note [Efficiency for ForAllCo case of mapTyCoX]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As noted in Note [ForAllCo] in GHC.Core.TyCo.Rep, a ForAllCo is a bit redundant.
It stores a TyCoVar and a Coercion, where the kind of the TyCoVar always matches
the left-hand kind of the coercion. This is convenient lots of the time, but
not when mapping a function over a coercion.

The problem is that tcm_tybinder will affect the TyCoVar's kind and
mapCoercion will affect the Coercion, and we hope that the results will be
the same. Even if they are the same (which should generally happen with
correct algorithms), then there is an efficiency issue. In particular,
this problem seems to make what should be a linear algorithm into a potentially
exponential one. But it's only going to be bad in the case where there's
lots of foralls in the kinds of other foralls. Like this:

  forall a : (forall b : (forall c : ...). ...). ...

This construction seems unlikely. So we'll do the inefficient, easy way
for now.

Note [Specialising mappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
These INLINE pragmas are indispensable. mapTyCo and mapTyCoX are used
to implement zonking, and it's vital that they get specialised to the TcM
monad and the particular mapper in use.

Even specialising to the monad alone made a 20% allocation difference
in perf/compiler/T5030.

See Note [Specialising foldType] in "GHC.Core.TyCo.Rep" for more details of this
idiom.
-}

-- | This describes how a "map" operation over a type/coercion should behave
data TyCoMapper env m
  = TyCoMapper
      { tcm_tyvar :: env -> TyVar -> m Type
      , tcm_covar :: env -> CoVar -> m Coercion
      , tcm_hole  :: env -> CoercionHole -> m Coercion
          -- ^ What to do with coercion holes.
          -- See Note [Coercion holes] in "GHC.Core.TyCo.Rep".

      , tcm_tycobinder :: forall r. env -> TyCoVar -> ForAllTyFlag
                       -> (env -> TyCoVar -> m r) -> m r
          -- ^ The returned env is used in the extended scope

      -- TyConApp and TyConAppCo
      -- Incoming Type is the original (T tys), pre-mapping
      --          [Type] are post-mapping
      -- Similarly for the coercion
      , tcm_tcapp_ty :: env -> Type             -> TyCon -> [Type]     -> m Type
      , tcm_tcapp_co :: env -> Coercion -> Role -> TyCon -> [Coercion] -> m Coercion
           -- ^ The [Type] have already had the mapping applied
           -- This smart constructor can:
           -- a) To zonk TcTyCons
           -- b) To turn TcTyCons into TyCons.
           --    See Note [Type checking recursive type and class declarations]
           --    in "GHC.Tc.TyCl"
           -- c) Expand type synonyms
      }

{-# INLINE mapTyCo #-}  -- See Note [Specialising mappers]
mapTyCo :: Monad m => TyCoMapper () m
        -> ( Type       -> m  Type
           , [Type]     -> m  [Type]
           , Coercion   -> m  Coercion
           , [Coercion] -> m [Coercion] )
mapTyCo mapper
  = case mapTyCoX mapper of
     (go_ty, go_tys, go_co, go_cos)
        -> (go_ty (), go_tys (), go_co (), go_cos ())

{-# INLINE mapTyCoX #-}  -- See Note [Specialising mappers]
mapTyCoX :: forall m env. Monad m
         => TyCoMapper env m
         -> ( env -> Type       -> m Type
            , env -> [Type]     -> m [Type]
            , env -> Coercion   -> m Coercion
            , env -> [Coercion] -> m [Coercion] )
mapTyCoX (TyCoMapper { tcm_tyvar = tyvar
                     , tcm_tycobinder = tycobinder
                     , tcm_tcapp_ty = tcapp_ty
                     , tcm_tcapp_co = tcapp_co
                     , tcm_covar = covar
                     , tcm_hole = cohole })
  = (go_ty, go_tys, go_co, go_cos)
  where
    -- See Note [Use explicit recursion in mapTyCo]
    go_tys !_   []       = return []
    go_tys !env (ty:tys) = (:) <$> go_ty env ty <*> go_tys env tys

    go_ty !env (TyVarTy tv)    = tyvar env tv
    go_ty !env (AppTy t1 t2)   = mkAppTy <$> go_ty env t1 <*> go_ty env t2
    go_ty !_   ty@(LitTy {})   = return ty
    go_ty !env (CastTy ty co)  = mkCastTy <$> go_ty env ty <*> go_co env co
    go_ty !env (CoercionTy co) = CoercionTy <$> go_co env co

    go_ty !env ty@(FunTy _ w arg res)
      = do { w' <- go_ty env w; arg' <- go_ty env arg; res' <- go_ty env res
           ; return (ty { ft_mult = w', ft_arg = arg', ft_res = res' }) }

    go_ty !env ty@(TyConApp tc tys)
      = do { tys' <- go_tys env tys; tcapp_ty env ty tc tys' }

    go_ty !env (ForAllTy (Bndr tv vis) inner)
      = do { tycobinder env tv vis $ \env' tv' -> do
           ; inner' <- go_ty env' inner
           ; return $ ForAllTy (Bndr tv' vis) inner' }

    -- See Note [Use explicit recursion in mapTyCo]
    go_cos !_   []       = return []
    go_cos !env (co:cos) = (:) <$> go_co env co <*> go_cos env cos

    go_mco !_   MRefl    = return MRefl
    go_mco !env (MCo co) = MCo <$> (go_co env co)

    go_co :: env -> Coercion -> m Coercion
    go_co !env (Refl ty)                  = Refl <$> go_ty env ty
    go_co !env (GRefl r ty mco)           = mkGReflCo r <$> go_ty env ty <*> go_mco env mco
    go_co !env (AppCo c1 c2)              = mkAppCo <$> go_co env c1 <*> go_co env c2
    go_co !env (FunCo r afl afr cw c1 c2) = mkFunCo2 r afl afr <$> go_co env cw
                                           <*> go_co env c1 <*> go_co env c2
    go_co !env (CoVarCo cv)               = covar env cv
    go_co !env (HoleCo hole)              = cohole env hole
    go_co !env (UnivCo { uco_prov = p, uco_role = r
                       , uco_lty = t1, uco_rty = t2, uco_deps = deps })
                                          = mkUnivCo <$> pure p
                                                     <*> go_cos env deps
                                                     <*> pure r
                                                     <*> go_ty env t1 <*> go_ty env t2
    go_co !env (SymCo co)                 = mkSymCo <$> go_co env co
    go_co !env (TransCo c1 c2)            = mkTransCo <$> go_co env c1 <*> go_co env c2
    go_co !env (AxiomCo r cos)            = mkAxiomCo r <$> go_cos env cos
    go_co !env (SelCo i co)               = SelCo i <$> go_co env co
    go_co !env (LRCo lr co)               = mkLRCo lr <$> go_co env co
    go_co !env (InstCo co arg)            = mkInstCo <$> go_co env co <*> go_co env arg
    go_co !env (KindCo co)                = mkKindCo <$> go_co env co
    go_co !env (SubCo co)                 = mkSubCo <$> go_co env co
    go_co !env co@(TyConAppCo r tc cos)   = do { cos' <- go_cos env cos
                                               ; tcapp_co env co r tc cos' }
    go_co !env (ForAllCo { fco_tcv = tv, fco_visL = visL, fco_visR = visR
                         , fco_kind = kind_co, fco_body = co })
      = do { kind_co' <- go_mco env kind_co
           ; tycobinder env tv visL $ \env' tv' ->  do
           ; co' <- go_co env' co
           ; return $ mkForAllCo tv' visL visR kind_co' co' }
        -- See Note [Efficiency for ForAllCo case of mapTyCoX]


{- Note [Use explicit recursion in mapTyCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use explicit recursion in `mapTyCo`, rather than calling, say, `strictFoldDVarSet`,
for exactly the same reason as in Note [Use explicit recursion in foldTyCo] in
GHC.Core.TyCo.Rep. We are in a monadic context, and using too-clever higher order
functions makes the strictness analyser produce worse results.

We could probably use `foldr`, since it is inlined bodily, fairly early; but
I'm doing the simple thing and inlining it by hand.

See !12037 for performance glitches caused by using `strictFoldDVarSet` (which is
definitely not inlined bodily).
-}


{- *********************************************************************
*                                                                      *
                      AppTy
*                                                                      *
********************************************************************* -}

{- We need to be pretty careful with AppTy to make sure we obey the
invariant that a TyConApp is always visibly so.  mkAppTy maintains the
invariant: use it.

Note [Decomposing fat arrow c=>t]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can we unify (a b) with (Eq a => ty)?   If we do so, we end up with
a partial application like ((=>) (Eq a)) which doesn't make sense in
source Haskell.  In contrast, we *can* unify (a b) with (t1 -> t2).
Here's an example (#9858) of how you might do it:
   i :: (Typeable a, Typeable b) => Proxy (a b) -> TypeRep
   i p = typeRep p

   j = i (Proxy :: Proxy (Eq Int => Int))
The type (Proxy (Eq Int => Int)) is only accepted with -XImpredicativeTypes,
but suppose we want that.  But then in the call to 'i', we end
up decomposing (Eq Int => Int), and we definitely don't want that.

We are willing to split (t1 -=> t2) because the argument is still of
kind Type, not Constraint.  So the criterion is isVisibleFunArg.

In Core there is no real reason to avoid such decomposition.  But for now I've
put the test in splitAppTyNoView_maybe, which applies throughout, because the
other calls to splitAppTy are in GHC.Core.Unify, which is also used by the
type checker (e.g. when matching type-function equations).
-}

-- | Applies a type to another, as in e.g. @k a@
mkAppTy :: Type -> Type -> Type
  -- See Note [Respecting definitional equality], invariant (EQ1).
mkAppTy (CastTy fun_ty co) arg_ty
  | ([arg_co], res_co) <- decomposePiCos co [arg_ty]
  = (fun_ty `mkAppTy` (arg_ty `mkCastTy` arg_co)) `mkCastTy` res_co

mkAppTy (TyConApp tc tys) ty2 = mkTyConApp tc (tys ++ [ty2])
mkAppTy ty1               ty2 = AppTy ty1 ty2
        -- Note that the TyConApp could be an
        -- under-saturated type synonym.  GHC allows that; e.g.
        --      type Foo k = k a -> k a
        --      type Id x = x
        --      foo :: Foo Id -> Foo Id
        --
        -- Here Id is partially applied in the type sig for Foo,
        -- but once the type synonyms are expanded all is well
        --
        -- Moreover in GHC.Tc.Types.tcInferTyApps we build up a type
        --   (T t1 t2 t3) one argument at a type, thus forming
        --   (T t1), (T t1 t2), etc

mkAppTys :: Type -> [Type] -> Type
mkAppTys ty1                []   = ty1
mkAppTys (CastTy fun_ty co) arg_tys  -- much more efficient then nested mkAppTy
                                     -- Why do this? See (EQ1) of
                                     -- Note [Respecting definitional equality]
                                     -- in GHC.Core.TyCo.Rep
  = foldl' AppTy ((mkAppTys fun_ty casted_arg_tys) `mkCastTy` res_co) leftovers
  where
    (arg_cos, res_co) = decomposePiCos co arg_tys
    (args_to_cast, leftovers) = splitAtList arg_cos arg_tys
    casted_arg_tys = zipWith mkCastTy args_to_cast arg_cos
mkAppTys (TyConApp tc tys1) tys2 = mkTyConApp tc (tys1 ++ tys2)
mkAppTys ty1                tys2 = foldl' AppTy ty1 tys2


{- *********************************************************************
*                                                                      *
                      LitTy
*                                                                      *
********************************************************************* -}

mkNumLitTy :: Integer -> Type
mkNumLitTy n = LitTy (NumTyLit n)

mkStrLitTy :: FastString -> Type
mkStrLitTy s = LitTy (StrTyLit s)

mkCharLitTy :: Char -> Type
mkCharLitTy c = LitTy (CharTyLit c)



{- *********************************************************************
*                                                                      *
                    Space-saving construction
*                                                                      *
********************************************************************* -}

{- Note [Using synonyms to compress types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Was: [Prefer Type over TYPE (BoxedRep Lifted)]

The Core of nearly any program will have numerous occurrences of the Types

   TyConApp BoxedRep [TyConApp Lifted []]    -- Synonym LiftedRep
   TyConApp BoxedRep [TyConApp Unlifted []]  -- Synonym UnliftedREp
   TyConApp TYPE [TyConApp LiftedRep []]     -- Synonym Type
   TyConApp TYPE [TyConApp UnliftedRep []]   -- Synonym UnliftedType

While investigating #17292 we found that these constituted a majority
of all TyConApp constructors on the heap:

    (From a sample of 100000 TyConApp closures)
    0x45f3523    - 28732 - `Type`
    0x420b840702 - 9629  - generic type constructors
    0x42055b7e46 - 9596
    0x420559b582 - 9511
    0x420bb15a1e - 9509
    0x420b86c6ba - 9501
    0x42055bac1e - 9496
    0x45e68fd    - 538   - `TYPE ...`

Consequently, we try hard to ensure that operations on such types are
efficient. Specifically, we strive to

 a. Avoid heap allocation of such types; use a single static TyConApp
 b. Use a small (shallow in the tree-depth sense) representation
    for such types

Goal (b) is particularly useful as it makes traversals (e.g. free variable
traversal, substitution, and comparison) more efficient.
Comparison in particular takes special advantage of nullary type synonym
applications (e.g. things like @TyConApp typeTyCon []@). See
* Note [Comparing type synonyms] in "GHC.Core.TyCo.Compare"
* Note [Unifying type synonyms] in "GHC.Core.Unify"

To accomplish these we use a number of tricks, implemented by mkTyConApp.

 1. Instead of (TyConApp BoxedRep [TyConApp Lifted []]),
    we prefer a statically-allocated (TyConApp LiftedRep [])
    where `LiftedRep` is a type synonym:
       type LiftedRep = BoxedRep Lifted
    Similarly for UnliftedRep

 2. Instead of (TyConApp TYPE [TyConApp LiftedRep []])
    we prefer the statically-allocated (TyConApp Type [])
    where `Type` is a type synonym
       type Type = TYPE LiftedRep
    Similarly for UnliftedType

These serve goal (b) since there are no applied type arguments to traverse,
e.g., during comparison.

 3. We have a single, statically allocated top-level binding to
    represent `TyConApp GHC.Types.Type []` (namely
    'GHC.Builtin.Types.Prim.liftedTypeKind'), ensuring that we don't
    need to allocate such types (goal (a)).  See functions
    mkTYPEapp and mkBoxedRepApp

 4. We use the sharing mechanism described in Note [Sharing nullary TyConApps]
    in GHC.Core.TyCon to ensure that we never need to allocate such
    nullary applications (goal (a)).

See #17958, #20541
-}

-- | A key function: builds a 'TyConApp' or 'FunTy' as appropriate to
-- its arguments.  Applies its arguments to the constructor from left to right.
mkTyConApp :: TyCon -> [Type] -> Type
mkTyConApp tycon []
  = -- See Note [Sharing nullary TyConApps] in GHC.Core.TyCon
    mkTyConTy tycon

mkTyConApp tycon tys@(ty1:rest)
  | Just fun_ty <- tyConAppFunTy_maybe tycon tys
  = fun_ty

  -- See Note [Using synonyms to compress types]
  | key == tYPETyConKey
  , Just ty <- mkTYPEapp_maybe ty1
  = assert (null rest) ty

  | key == cONSTRAINTTyConKey
  , Just ty <- mkCONSTRAINTapp_maybe ty1
  = assert (null rest) ty

  -- See Note [Using synonyms to compress types]
  | key == boxedRepDataConTyConKey
  , Just ty <- mkBoxedRepApp_maybe ty1
  = assert (null rest) ty

  | key == tupleRepDataConTyConKey
  , Just ty <- mkTupleRepApp_maybe ty1
  = assert (null rest) ty

  -- The catch-all case
  | otherwise
  = TyConApp tycon tys
  where
    key = tyConUnique tycon

tyConAppFunTy_maybe :: HasDebugCallStack => TyCon -> [Type] -> Maybe Type
-- ^ Return Just if this TyConApp should be represented as a FunTy
tyConAppFunTy_maybe tc tys
  | Just (af, mult, arg, res) <- ty_con_app_fun_maybe manyDataConTy tc tys
  = Just (FunTy { ft_af = af, ft_mult = mult, ft_arg = arg, ft_res = res })
  | otherwise = Nothing

tyConAppFunCo_maybe :: HasDebugCallStack => Role -> TyCon -> [Coercion]
                    -> Maybe Coercion
-- ^ Return Just if this TyConAppCo should be represented as a FunCo
tyConAppFunCo_maybe r tc cos
  | Just (af, mult, arg, res) <- ty_con_app_fun_maybe mult_refl tc cos
  = Just (mkFunCo r af mult arg res)
  | otherwise
  = Nothing
  where
    mult_refl = mkReflCo (funRole r SelMult) manyDataConTy

ty_con_app_fun_maybe :: (HasDebugCallStack, Outputable a) => a -> TyCon -> [a]
                     -> Maybe (FunTyFlag, a, a, a)
{-# INLINE ty_con_app_fun_maybe #-}
-- Specialise this function for its two call sites
ty_con_app_fun_maybe many_ty_co tc args
  | tc_uniq == fUNTyConKey     = fUN_case
  | tc_uniq == tcArrowTyConKey = non_FUN_case FTF_T_C
  | tc_uniq == ctArrowTyConKey = non_FUN_case FTF_C_T
  | tc_uniq == ccArrowTyConKey = non_FUN_case FTF_C_C
  | otherwise                  = Nothing
  where
    tc_uniq = tyConUnique tc

    -- There is a multiplicity argument, `w`
    -- See Note [Function type constructors and FunTy]
    fUN_case
      | (w:_r1:_r2:a1:a2:rest) <- args
      = assertPpr (null rest) (ppr tc <+> ppr args) $
        Just (FTF_T_T, w, a1, a2)
      | otherwise = Nothing

    -- No multiplicity argument
    -- See Note [Function type constructors and FunTy]
    non_FUN_case ftf
      | (_r1:_r2:a1:a2:rest) <- args
      = assertPpr (null rest) (ppr tc <+> ppr args) $
        Just (ftf, many_ty_co, a1, a2)
      | otherwise
      = Nothing


{- Note [Care using synonyms to compress types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Using a synonym to compress a types has a tricky wrinkle. Consider
coreView applied to (TyConApp LiftedRep [])

* coreView expands the LiftedRep synonym:
     type LiftedRep = BoxedRep Lifted

* Danger: we might apply the empty substitution to the RHS of the
  synonym.  And substTy calls mkTyConApp BoxedRep [Lifted]. And
  mkTyConApp compresses that back to LiftedRep.  Loop!

* Solution: in expandSynTyConApp_maybe, don't call substTy for nullary
  type synonyms.  That's more efficient anyway.
-}

mkTYPEapp :: RuntimeRepType -> Type
mkTYPEapp rr
  = case mkTYPEapp_maybe rr of
       Just ty -> ty
       Nothing -> TyConApp tYPETyCon [rr]

mkTYPEapp_maybe :: RuntimeRepType -> Maybe Type
-- ^ Given a @RuntimeRep@, applies @TYPE@ to it.
-- On the fly it rewrites
--      TYPE LiftedRep      -->   liftedTypeKind    (a synonym)
--      TYPE UnliftedRep    -->   unliftedTypeKind  (ditto)
--      TYPE ZeroBitRep     -->   zeroBitTypeKind   (ditto)
-- NB: no need to check for TYPE (BoxedRep Lifted), TYPE (BoxedRep Unlifted)
--     because those inner types should already have been rewritten
--     to LiftedRep and UnliftedRep respectively, by mkTyConApp
--
-- see Note [TYPE and CONSTRAINT] in GHC.Builtin.Types.Prim.
-- See Note [Using synonyms to compress types] in GHC.Core.Type
{-# NOINLINE mkTYPEapp_maybe #-}
mkTYPEapp_maybe (TyConApp tc args)
  | key == liftedRepTyConKey    = assert (null args) $ Just liftedTypeKind   -- TYPE LiftedRep
  | key == unliftedRepTyConKey  = assert (null args) $ Just unliftedTypeKind -- TYPE UnliftedRep
  | key == zeroBitRepTyConKey   = assert (null args) $ Just zeroBitTypeKind  -- TYPE ZeroBitRep
  where
    key = tyConUnique tc
mkTYPEapp_maybe _ = Nothing

------------------
mkCONSTRAINTapp :: RuntimeRepType -> Type
-- ^ Just like mkTYPEapp
mkCONSTRAINTapp rr
  = case mkCONSTRAINTapp_maybe rr of
       Just ty -> ty
       Nothing -> TyConApp cONSTRAINTTyCon [rr]

mkCONSTRAINTapp_maybe :: RuntimeRepType -> Maybe Type
-- ^ Just like mkTYPEapp_maybe
{-# NOINLINE mkCONSTRAINTapp_maybe #-}
mkCONSTRAINTapp_maybe (TyConApp tc args)
  | tc `hasKey` liftedRepTyConKey = assert (null args) $
                                    Just constraintKind   -- CONSTRAINT LiftedRep
mkCONSTRAINTapp_maybe _ = Nothing

------------------
mkBoxedRepApp_maybe :: LevityType -> Maybe Type
-- ^ Given a `Levity`, apply `BoxedRep` to it
-- On the fly, rewrite
--      BoxedRep Lifted     -->   liftedRepTy    (a synonym)
--      BoxedRep Unlifted   -->   unliftedRepTy  (ditto)
-- See Note [TYPE and CONSTRAINT] in GHC.Builtin.Types.Prim.
-- See Note [Using synonyms to compress types] in GHC.Core.Type
{-# NOINLINE mkBoxedRepApp_maybe #-}
mkBoxedRepApp_maybe (TyConApp tc args)
  | key == liftedDataConKey   = assert (null args) $ Just liftedRepTy    -- BoxedRep Lifted
  | key == unliftedDataConKey = assert (null args) $ Just unliftedRepTy  -- BoxedRep Unlifted
  where
    key = tyConUnique tc
mkBoxedRepApp_maybe _ = Nothing

mkTupleRepApp_maybe :: Type -> Maybe Type
-- ^ Given a `[RuntimeRep]`, apply `TupleRep` to it
-- On the fly, rewrite
--      TupleRep [] -> zeroBitRepTy   (a synonym)
-- See Note [TYPE and CONSTRAINT] in GHC.Builtin.Types.Prim.
-- See Note [Using synonyms to compress types] in GHC.Core.Type
{-# NOINLINE mkTupleRepApp_maybe #-}
mkTupleRepApp_maybe (TyConApp tc args)
  | key == nilDataConKey = assert (isSingleton args) $ Just zeroBitRepTy  -- ZeroBitRep
  where
    key = tyConUnique tc
mkTupleRepApp_maybe _ = Nothing

typeOrConstraintKind :: TypeOrConstraint -> RuntimeRepType -> Kind
typeOrConstraintKind TypeLike       rep = mkTYPEapp       rep
typeOrConstraintKind ConstraintLike rep = mkCONSTRAINTapp rep

liftedTypeOrConstraintKind :: TypeOrConstraint -> Kind
liftedTypeOrConstraintKind TypeLike       = liftedTypeKind
liftedTypeOrConstraintKind ConstraintLike = constraintKind


{- *********************************************************************
*                                                                      *
                     CoercionTy
  CoercionTy allows us to inject coercions into types. A CoercionTy
  should appear only in the right-hand side of an application.
*                                                                      *
********************************************************************* -}

mkCoercionTy :: Coercion -> Type
mkCoercionTy = CoercionTy

{- *********************************************************************
*                                                                      *
                      ForAllTy
*                                                                      *
********************************************************************* -}

-- | Make a dependent forall over a TyCoVar
mkTyCoForAllTy :: TyCoVar -> ForAllTyFlag -> Type -> Type
mkTyCoForAllTy tv vis ty
  | isCoVar tv
  , not (tv `elemVarSet` tyCoVarsOfType ty)
   -- Maintain ForAllTy's invariants
    -- See Note [Unused coercion variable in ForAllTy] in GHC.Core.TyCo.Rep
  = mkVisFunTyMany (varType tv) ty
  | otherwise
  = ForAllTy (mkForAllTyBinder vis tv) ty

-- | Make a dependent forall over a TyCoVar
mkTyCoForAllTys :: [ForAllTyBinder] -> Type -> Type
mkTyCoForAllTys bndrs ty
  = foldr (\(Bndr var vis) -> mkTyCoForAllTy var vis) ty bndrs

-- | Make a dependent forall over an 'Inferred' variable
mkTyCoInvForAllTy :: TyCoVar -> Type -> Type
mkTyCoInvForAllTy tv ty = mkTyCoForAllTy tv Inferred ty

-- | Like 'mkTyCoInvForAllTy', but tv should be a tyvar
mkInfForAllTy :: TyVar -> Type -> Type
mkInfForAllTy tv ty = assert (isTyVar tv )
                      ForAllTy (Bndr tv Inferred) ty

-- | Like 'mkForAllTys', but assumes all variables are dependent and
-- 'Inferred', a common case
mkTyCoInvForAllTys :: [TyCoVar] -> Type -> Type
mkTyCoInvForAllTys tvs ty = foldr mkTyCoInvForAllTy ty tvs

-- | Like 'mkTyCoInvForAllTys', but tvs should be a list of tyvar
mkInfForAllTys :: [TyVar] -> Type -> Type
mkInfForAllTys tvs ty = foldr mkInfForAllTy ty tvs

-- | Like 'mkForAllTy', but assumes the variable is dependent and 'Specified',
-- a common case
mkSpecForAllTy :: TyVar -> Type -> Type
mkSpecForAllTy tv ty = assert (isTyVar tv )
                       -- covar is always Inferred, so input should be tyvar
                       ForAllTy (Bndr tv Specified) ty

-- | Like 'mkForAllTys', but assumes all variables are dependent and
-- 'Specified', a common case
mkSpecForAllTys :: [TyVar] -> Type -> Type
mkSpecForAllTys tvs ty = foldr mkSpecForAllTy ty tvs

-- | Like mkForAllTys, but assumes all variables are dependent and visible
mkVisForAllTys :: [TyVar] -> Type -> Type
mkVisForAllTys tvs = assert (all isTyVar tvs )
                     -- covar is always Inferred, so all inputs should be tyvar
                     mkForAllTys [ Bndr tv Required | tv <- tvs ]

-- | Given a list of type-level vars and the free vars of a result kind,
-- makes PiTyBinders, preferring anonymous binders
-- if the variable is, in fact, not dependent.
-- e.g.    mkTyConBindersPreferAnon [(k:*),(b:k),(c:k)] (k->k)
-- We want (k:*) Named, (b:k) Anon, (c:k) Anon
--
-- All non-coercion binders are /visible/.
mkTyConBindersPreferAnon :: [TyVar]      -- ^ binders
                         -> TyCoVarSet   -- ^ free variables of result
                         -> [TyConBinder]
mkTyConBindersPreferAnon vars inner_tkvs = assert (all isTyVar vars)
                                           fst (go vars)
  where
    go :: [TyVar] -> ([TyConBinder], VarSet) -- also returns the free vars
    go [] = ([], inner_tkvs)
    go (v:vs) | v `elemVarSet` fvs
              = ( Bndr v (NamedTCB Required) : binders
                , fvs `delVarSet` v `unionVarSet` kind_vars )
              | otherwise
              = ( Bndr v AnonTCB : binders
                , fvs `unionVarSet` kind_vars )
      where
        (binders, fvs) = go vs
        kind_vars      = tyCoVarsOfType $ tyVarKind v



{-
%************************************************************************
%*                                                                      *
            Building coercions
%*                                                                      *
%************************************************************************

These "smart constructors" maintain the invariants listed in the definition
of Coercion, and they perform very basic optimizations.

-}

-- | Make a reflexive coercion
mkReflCo :: Role -> Type -> Coercion
mkReflCo Nominal ty = Refl ty
mkReflCo r       ty = GRefl r ty MRefl

-- | Make a representational reflexive coercion
mkRepReflCo :: Type -> Coercion
mkRepReflCo ty = GRefl Representational ty MRefl

-- | Make a nominal reflexive coercion
mkNomReflCo :: Type -> Coercion
mkNomReflCo = Refl

-- | Build a function 'Coercion' from two other 'Coercion's. That is,
-- given @co1 :: a ~ b@ and @co2 :: x ~ y@ produce @co :: (a -> x) ~ (b -> y)@
-- or @(a => x) ~ (b => y)@, depending on the kind of @a@/@b@.
-- This (most common) version takes a single FunTyFlag, which is used
--   for both fco_afl and ftf_afr of the FunCo
mkFunCo :: Role -> FunTyFlag -> CoercionN -> Coercion -> Coercion -> Coercion
mkFunCo r af w arg_co res_co
  = mkFunCo2 r af af w arg_co res_co

mkNakedFunCo :: Role -> FunTyFlag -> CoercionN -> Coercion -> Coercion -> Coercion
-- This version of mkFunCo does not check FunCo invariants (checkFunCo)
-- It's a historical vestige; See Note [No assertion check on mkFunCo]
mkNakedFunCo = mkFunCo

mkFunCo2 :: Role -> FunTyFlag -> FunTyFlag
         -> CoercionN -> Coercion -> Coercion -> Coercion
-- This is the smart constructor for FunCo; it checks invariants
mkFunCo2 r afl afr w arg_co res_co
  -- See Note [No assertion check on mkFunCo]
  | Just (ty1, _) <- isReflCo_maybe arg_co
  , Just (ty2, _) <- isReflCo_maybe res_co
  , Just (w, _)   <- isReflCo_maybe w
  = mkReflCo r (mkFunTy afl w ty1 ty2)  -- See Note [Refl invariant]

  | otherwise
  = FunCo { fco_role = r, fco_afl = afl, fco_afr = afr
          , fco_mult = w, fco_arg = arg_co, fco_res = res_co }


{- Note [No assertion check on mkFunCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to have a checkFunCo assertion on mkFunCo, but during typechecking
we can (legitimately) have not-full-zonked types or coercion variables, so
the assertion spuriously fails (test T11480b is a case in point).  Lint
checks all these things anyway.

We used to get around the problem by calling mkNakedFunCo from within the
typechecker, which dodged the assertion check.  But then mkAppCo calls
mkTyConAppCo, which calls tyConAppFunCo_maybe, which calls mkFunCo.
Duplicating this stack of calls with "naked" versions of each seems too much.

-- Commented out: see Note [No assertion check on mkFunCo]
checkFunCo :: Role -> FunTyFlag -> FunTyFlag
           -> CoercionN -> Coercion -> Coercion
           -> Maybe SDoc
-- Checks well-formed-ness for FunCo
-- Used only in assertions and Lint
{-# NOINLINE checkFunCo #-}
checkFunCo _r afl afr _w arg_co res_co
  | not (ok argl_ty && ok argr_ty && ok resl_ty && ok resr_ty)
  = Just (hang (text "Bad arg or res types") 2 pp_inputs)

  | afl == computed_afl
  , afr == computed_afr
  = Nothing
  | otherwise
  = Just (vcat [ text "afl (provided,computed):" <+> ppr afl <+> ppr computed_afl
               , text "afr (provided,computed):" <+> ppr afr <+> ppr computed_afr
               , pp_inputs ])
  where
    computed_afl = chooseFunTyFlag argl_ty resl_ty
    computed_afr = chooseFunTyFlag argr_ty resr_ty
    Pair argl_ty argr_ty = coercionKind arg_co
    Pair resl_ty resr_ty = coercionKind res_co

    pp_inputs = vcat [ pp_ty "argl" argl_ty, pp_ty "argr" argr_ty
                     , pp_ty "resl" resl_ty, pp_ty "resr" resr_ty
                     , text "arg_co:" <+> ppr arg_co
                     , text "res_co:" <+> ppr res_co ]

    ok ty = isTYPEorCONSTRAINT (typeKind ty)
    pp_ty str ty = text str <> colon <+> hang (ppr ty)
                                            2 (dcolon <+> ppr (typeKind ty))
-}

-- mkForAllVisCos [tv{vis}] constructs a cast
--   forall tv. res  ~R#   forall tv{vis} res`.
-- See Note [Required foralls in Core] in GHC.Core.TyCo.Rep
mkForAllVisCos :: HasDebugCallStack => [ForAllTyBinder] -> Coercion -> Coercion
mkForAllVisCos bndrs orig_co = foldr go orig_co bndrs
  where
    go (Bndr tv vis) = mkForAllCo tv coreTyLamForAllTyFlag vis MRefl

-- | Make a Coercion quantified over a type/coercion variable;
-- the variable has the same kind and visibility in both sides of the coercion
mkHomoForAllCos :: [ForAllTyBinder] -> Coercion -> Coercion
mkHomoForAllCos vs orig_co
  | Just (ty, r) <- isReflCo_maybe orig_co
  = mkReflCo r (mkTyCoForAllTys vs ty)
  | otherwise
  = foldr go orig_co vs
  where
    go :: ForAllTyBinder -> Coercion -> Coercion
    go (Bndr var vis) co = mk_forall_co var vis vis MRefl co

mkHomoForAllCo :: TyVar -> Coercion -> Coercion
-- Specialised for a single TyVar,
--    and visibility of coreTyLamForAllTyFlag
mkHomoForAllCo tv orig_co
  | Just (ty, r) <- isReflCo_maybe orig_co
  = mkReflCo r (mkForAllTy (Bndr tv vis) ty)
  | otherwise
  = mk_forall_co tv vis vis MRefl orig_co
  where
    vis  = coreTyLamForAllTyFlag

isReflForAllCo :: TyCoVar -> ForAllTyFlag -> ForAllTyFlag
               -> KindMCoercion -> Coercion -> Maybe Coercion
isReflForAllCo tcv visL visR kind_co co
  | Just (ty, r) <- isReflCo_maybe co
  , isReflMCo kind_co
  , visL == visR    -- Just use '==' rather than eqForAllTyVis,
                    -- which isn't conveniently in scope here
  = Just (mkReflCo r (mkTyCoForAllTy tcv visL ty))

  | otherwise
  = Nothing

-- | Make a Coercion from a tycovar, a kind coercion, and a body coercion.
mkForAllCo :: HasDebugCallStack => TyCoVar -> ForAllTyFlag -> ForAllTyFlag
           -> KindMCoercion -> Coercion -> Coercion
mkForAllCo v visL visR kind_co co
  | Just refl_co <- isReflForAllCo v visL visR kind_co co
  = refl_co
  | otherwise
  = mk_forall_co v visL visR kind_co co

-- | `mk_forall_co` just builds a ForAllCo.
-- With debug on, it checks invariants (e.g. he kind of the tycovar should
--   be the left-hand kind of the kind coercion).
-- Callers should have done any isReflCo short-cutting.
mk_forall_co :: TyCoVar -> ForAllTyFlag -> ForAllTyFlag
             -> KindMCoercion -> Coercion -> Coercion
mk_forall_co tcv visL visR kind_co co
  = assertGoodForAllCo tcv visL visR kind_co co $
    assertPpr (not (isReflCo co && isReflMCo kind_co && visL == visR)) (ppr co) $
    ForAllCo { fco_tcv = tcv, fco_visL = visL, fco_visR = visR
             , fco_kind = kind_co, fco_body = co }

mkNakedForAllCo :: TyVar    -- Never a CoVar
                -> ForAllTyFlag -> ForAllTyFlag
                -> CoercionN -> Coercion -> Coercion
-- This version lacks the assertion checks.
-- Used during type checking when the arguments may (legitimately) not be zonked
-- and so the assertions might (bogusly) fail
-- NB: since the coercions are un-zonked, we can't really deal with
--     (FC6) and (FC7) in Note [ForAllCo] in GHC.Core.TyCo.Rep.
--     Fortunately we don't have to: this function is needed only for /type/ variables.
-- In fact, there is only one call site, in `can_eq_nc_forall`
mkNakedForAllCo tv visL visR kind_co co
  | assertPpr (isTyVar tv) (ppr tv) True
  , Just refl_co <- isReflForAllCo tv visL visR (MCo kind_co) co
  = refl_co
  | otherwise
  = ForAllCo { fco_tcv = tv, fco_visL = visL, fco_visR = visR
             , fco_kind = MCo kind_co, fco_body = co }

mkCoVarCo :: CoVar -> Coercion
-- cv :: s ~# t
-- See Note [mkCoVarCo]
mkCoVarCo cv = CoVarCo cv

mkCoVarCos :: [CoVar] -> [Coercion]
mkCoVarCos = map mkCoVarCo

{- Note [mkCoVarCo]
~~~~~~~~~~~~~~~~~~~
In the past, mkCoVarCo optimised (c :: t~t) to (Refl t).  That is
valid (although see Note [Unbound RULE binders] in GHC.Core.Rules), but
it's a relatively expensive test and perhaps better done in
optCoercion.  Not a big deal either way.
-}

mkUnbranchedAxInstCo :: Role -> CoAxiom Unbranched
                     -> [Type] -> [Coercion] -> Coercion
-- To be used only with unbranched axioms
mkUnbranchedAxInstCo role ax tys cos
  = mkAxInstCo role (UnbranchedAxiom ax) tys cos

mkAxInstCo :: Role
           -> CoAxiomRule   -- Always BranchedAxiom or UnbranchedAxiom
           -> [Type] -> [Coercion]
           -> Coercion
-- mkAxInstCo can legitimately be called over-saturated;
-- i.e. with more type arguments than the coercion requires
-- Only called with BranchedAxiom or UnbranchedAxiom
mkAxInstCo role axr tys cos
  | arity == n_tys = downgradeRole role ax_role $
                     AxiomCo axr (rtys `chkAppend` cos)
  | otherwise      = assert (arity < n_tys) $
                     downgradeRole role ax_role $
                     mkAppCos (AxiomCo axr (ax_args `chkAppend` cos))
                              leftover_args
  where
    (ax_role, branch)        = case coAxiomRuleBranch_maybe axr of
                                  Just (_tc, ax_role, branch) -> (ax_role, branch)
                                  Nothing -> pprPanic "mkAxInstCo" (ppr axr)
    n_tys                    = length tys
    arity                    = length (coAxBranchTyVars branch)
    arg_roles                = coAxBranchRoles branch
    rtys                     = zipWith mkReflCo (arg_roles ++ repeat Nominal) tys
    (ax_args, leftover_args) = splitAt arity rtys

-- worker function
mkAxiomCo :: CoAxiomRule -> [Coercion] -> Coercion
mkAxiomCo = AxiomCo

-- | Make a coercion from a coercion hole
mkHoleCo :: CoercionHole -> Coercion
mkHoleCo h = HoleCo h

-- | Make a universal coercion between two arbitrary types.
mkUnivCo :: UnivCoProvenance
         -> [Coercion] -- ^ Coercions on which this depends
         -> Role       -- ^ role of the built coercion, "r"
         -> Type       -- ^ t1 :: k1
         -> Type       -- ^ t2 :: k2
         -> Coercion   -- ^ :: t1 ~r t2
mkUnivCo prov deps role ty1 ty2
  = UnivCo { uco_prov = prov, uco_role = role
           , uco_lty = ty1, uco_rty = ty2
           , uco_deps = deps }

-- | Create a symmetric version of the given 'Coercion' that asserts
--   equality between the same types but in the other "direction", so
--   a kind of @t1 ~ t2@ becomes the kind @t2 ~ t1@.
mkSymCo :: Coercion -> Coercion

-- Do a few simple optimizations, mainly to expose the underlying
-- constructors to other 'mk' functions.  E.g.
--   mkInstCo (mkSymCo (ForAllCo ...)) ty
-- We want to push the SymCo inside the ForallCo, so that we can instantiate
-- This can make a big difference.  E.g without coercion optimisation, GHC.Read
-- totally explodes; but when we push Sym inside ForAll, it's fine.
mkSymCo co | isReflCo co   = co
mkSymCo (SymCo co)         = co
mkSymCo (SubCo (SymCo co)) = SubCo co
mkSymCo co@(ForAllCo { fco_kind = kco, fco_body = body_co })
  | isReflMCo kco          = co { fco_body = mkSymCo body_co }
mkSymCo co                 = SymCo co

-- | mkTransCo creates a new 'Coercion' by composing the two
--   given 'Coercion's transitively: (co1 ; co2)
mkTransCo :: HasDebugCallStack => Coercion -> Coercion -> Coercion
mkTransCo co1 co2
   | isReflCo co1 = co2
   | isReflCo co2 = co1

   | GRefl r t1 (MCo kco1) <- co1
   , GRefl _ _  (MCo kco2) <- co2
   = GRefl r t1 (MCo $ mkTransCo kco1 kco2)

   | otherwise
   = TransCo co1 co2

--------------------
-- | Given @ty :: k1@, @co :: k1 ~ k2@,
-- produces @co' :: ty ~r (ty |> co)@
mkGReflRightCo :: Role -> Type -> KindCoercion -> Coercion
mkGReflRightCo r ty co
  | isReflKindCo co = mkReflCo r ty  -- Homo (tested) AND nominal (I promise) => Refl
  | otherwise       = mkGReflMCo r ty co

-- | Given @r@, @ty :: k1@, and @co :: k1 ~N k2@,
-- produces @co' :: (ty |> co) ~r ty@
mkGReflLeftCo :: Role -> Type -> KindCoercion -> Coercion
mkGReflLeftCo r ty co
  | isReflKindCo co = mkReflCo r ty
  | otherwise       = mkSymCo $ mkGReflMCo r ty co

-- | Given @co :: (a :: k) ~ (b :: k')@ produce @co' :: k ~ k'@.
mkKindCo :: Coercion -> Coercion
mkKindCo co | Just (ty, _) <- isReflCo_maybe co = Refl (typeKind ty)
mkKindCo (GRefl _ _ (MCo co)) = co
mkKindCo co                   = KindCo co

-- | Given @ty :: k1@, @co :: k1 ~ k2@, @co2:: ty ~r ty'@,
-- produces @co' :: (ty |> co) ~r ty'
-- It is not only a utility function, but it saves allocation when co
-- is a GRefl coercion.
mkCoherenceLeftCo :: Role -> Type -> KindCoercion -> Coercion -> Coercion
mkCoherenceLeftCo r ty co co2
  | isReflKindCo co = co2
  | otherwise       = (mkSymCo $ mkGReflMCo r ty co) `mkTransCo` co2

-- | Given @ty :: k1@, @co :: k1 ~ k2@, @co2:: ty' ~r ty@,
-- produces @co' :: ty' ~r (ty |> co)
-- It is not only a utility function, but it saves allocation when co
-- is a GRefl coercion.
mkCoherenceRightCo :: HasDebugCallStack => Role -> Type -> KindCoercion -> Coercion -> Coercion
mkCoherenceRightCo r ty co co2
  | isReflKindCo co = co2
  | otherwise       = co2 `mkTransCo` mkGReflMCo r ty co

mkSubCo :: HasDebugCallStack => Coercion -> Coercion
-- Input coercion is Nominal, result is Representational
-- see also Note [Role twiddling functions]
mkSubCo (Refl ty) = GRefl Representational ty MRefl
mkSubCo (GRefl Nominal ty co) = GRefl Representational ty co
mkSubCo (TyConAppCo Nominal tc cos)
  = TyConAppCo Representational tc (applyRoles tc cos)
mkSubCo co@(FunCo { fco_role = Nominal, fco_arg = arg, fco_res = res })
  = co { fco_role = Representational
       , fco_arg = downgradeRole Representational Nominal arg
       , fco_res = downgradeRole Representational Nominal res }
mkSubCo co = assertPpr (coercionRole co == Nominal) (ppr co <+> ppr (coercionRole co)) $
             SubCo co

{- **********************************************************************
%*                                                                      *
            Role twiddling
%*                                                                      *
%********************************************************************* -}

{- Note [Role twiddling functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a plethora of functions for twiddling roles:

mkSubCo: Requires a nominal input coercion and always produces a
representational output. This is used when you (the programmer) are sure you
know exactly that role you have and what you want.

downgradeRole_maybe: This function takes both the input role and the output role
as parameters. (The *output* role comes first!) It can only *downgrade* a
role -- that is, change it from N to R or P, or from R to P. This one-way
behavior is why there is the "_maybe". If an upgrade is requested, this
function produces Nothing. This is used when you need to change the role of a
coercion, but you're not sure (as you're writing the code) of which roles are
involved.

This function could have been written using coercionRole to ascertain the role
of the input. But, that function is recursive, and the caller of downgradeRole_maybe
often knows the input role. So, this is more efficient.

downgradeRole: This is just like downgradeRole_maybe, but it panics if the
conversion isn't a downgrade.

setNominalRole_maybe: This is the only function that can *upgrade* a coercion.
The result (if it exists) is always Nominal. The input can be at any role. It
works on a "best effort" basis, as it should never be strictly necessary to
upgrade a coercion during compilation. It is currently only used within GHC in
splitAppCo_maybe. In order to be a proper inverse of mkAppCo, the second
coercion that splitAppCo_maybe returns must be nominal. But, it's conceivable
that splitAppCo_maybe is operating over a TyConAppCo that uses a
representational coercion. Hence the need for setNominalRole_maybe.
splitAppCo_maybe, in turn, is used only within coercion optimization -- thus,
it is not absolutely critical that setNominalRole_maybe be complete.

Note that setNominalRole_maybe will never upgrade a phantom UnivCo. Phantom
UnivCos are perfectly type-safe, whereas representational and nominal ones are
not. (Nominal ones are no worse than representational ones, so this function *will*
change a UnivCo Representational to a UnivCo Nominal.)

Conal Elliott also came across a need for this function while working with the
GHC API, as he was decomposing Core casts. The Core casts use representational
coercions, as they must, but his use case required nominal coercions (he was
building a GADT). So, that's why this function is exported from this module.

One might ask: shouldn't downgradeRole_maybe just use setNominalRole_maybe as
appropriate? I (Richard E.) have decided not to do this, because upgrading a
role is bizarre and a caller should have to ask for this behavior explicitly.
-}

eqTyConRole :: TyCon -> Role
-- Given (~#) or (~R#) return the Nominal or Representational respectively
eqTyConRole tc
  | tc `hasKey` eqPrimTyConKey
  = Nominal
  | tc `hasKey` eqReprPrimTyConKey
  = Representational
  | otherwise
  = pprPanic "eqTyConRole: unknown tycon" (ppr tc)


-- | Changes a role, but only a downgrade. See Note [Role twiddling functions]
downgradeRole_maybe :: Role   -- ^ desired role
                    -> Role   -- ^ current role
                    -> Coercion -> Maybe Coercion
-- In (downgradeRole_maybe dr cr co) it's a precondition that
--                                   cr = coercionRole co

downgradeRole_maybe Nominal          Nominal          co = Just co
downgradeRole_maybe Nominal          _                _  = Nothing

downgradeRole_maybe Representational Nominal          co = Just (mkSubCo co)
downgradeRole_maybe Representational Representational co = Just co
downgradeRole_maybe Representational Phantom          _  = Nothing

downgradeRole_maybe Phantom          Phantom          co = Just co
downgradeRole_maybe Phantom          _                co = Just (toPhantomCo co)

-- | Like 'downgradeRole_maybe', but panics if the change isn't a downgrade.
-- See Note [Role twiddling functions]
downgradeRole :: Role  -- desired role
              -> Role  -- current role
              -> Coercion -> Coercion
downgradeRole r1 r2 co
  = case downgradeRole_maybe r1 r2 co of
      Just co' -> co'
      Nothing  -> pprPanic "downgradeRole" (ppr co)

-- | Make a phantom coercion between two types. The coercion passed
-- in must be a nominal coercion between the kinds of the
-- types.
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkPhantomCo h t1 t2
  = mkUnivCo PhantomProv [h] Phantom t1 t2

-- takes any coercion and turns it into a Phantom coercion
toPhantomCo :: Coercion -> Coercion
toPhantomCo co
  = mkPhantomCo (mkKindCo co) ty1 ty2
  where Pair ty1 ty2 = coercionKind co

-- Convert args to a TyConAppCo Nominal to the same TyConAppCo Representational
applyRoles :: TyCon -> [Coercion] -> [Coercion]
applyRoles = zipWith (`downgradeRole` Nominal) . tyConRoleListRepresentational

-- The Role parameter is the Role of the TyConAppCo
-- defined here because this is intimately concerned with the implementation
-- of TyConAppCo
-- Always returns an infinite list (with a infinite tail of Nominal)
tyConRolesX :: Role -> TyCon -> Infinite Role
tyConRolesX Representational tc = tyConRolesRepresentational tc
tyConRolesX role             _  = Inf.repeat role

tyConRoleListX :: Role -> TyCon -> [Role]
tyConRoleListX role = Inf.toList . tyConRolesX role

-- Returns the roles of the parameters of a tycon, with an infinite tail
-- of Nominal
tyConRolesRepresentational :: TyCon -> Infinite Role
tyConRolesRepresentational tc = tyConRoles tc Inf.++ Inf.repeat Nominal

-- Returns the roles of the parameters of a tycon, with an infinite tail
-- of Nominal
tyConRoleListRepresentational :: TyCon -> [Role]
tyConRoleListRepresentational = Inf.toList . tyConRolesRepresentational

tyConRole :: Role -> TyCon -> Int -> Role
tyConRole Nominal          _  _ = Nominal
tyConRole Phantom          _  _ = Phantom
tyConRole Representational tc n = tyConRolesRepresentational tc Inf.!! n

funRole :: Role -> FunSel -> Role
funRole Nominal          _  = Nominal
funRole Phantom          _  = Phantom
funRole Representational fs = funRoleRepresentational fs

funRoleRepresentational :: FunSel -> Role
funRoleRepresentational SelMult = Nominal
funRoleRepresentational SelArg  = Representational
funRoleRepresentational SelRes  = Representational

ltRole :: Role -> Role -> Bool
-- Is one role "less" than another?
--     Nominal < Representational < Phantom
ltRole Phantom          _       = False
ltRole Representational Phantom = True
ltRole Representational _       = False
ltRole Nominal          Nominal = False
ltRole Nominal          _       = True

-- | Make a "coercion between coercions".
mkProofIrrelCo :: Role          -- ^ role of the created coercion, "r"
               -> KindCoercion  -- ^ :: phi1 ~N phi2
               -> Coercion      -- ^ g1 :: phi1
               -> Coercion      -- ^ g2 :: phi2
               -> Coercion      -- ^ :: g1 ~r g2

-- if the two coercion prove the same fact, I just don't care what
-- the individual coercions are.
mkProofIrrelCo r kco g1 g2
  | isReflKindCo kco  = mkReflCo r (mkCoercionTy g1)
         -- kco is a kind coercion, thus @isReflKindCo@ rather than @isReflCo@
  | otherwise         = mkUnivCo ProofIrrelProv [kco] r
                                 (mkCoercionTy g1) (mkCoercionTy g2)


-- | Tests if this /kind/ coercion is Refl
-- Guaranteed to work very quickly.
-- PRECONDITION: the argument is a KindCoercion
-- So if it sees  (GRefl k (MCo kk)) :: k ~ (k |> kk)
--    then we know that kk must be reflexive.
-- And hence if co = GRefl {} then it is equivalent to Refl,
--    because GRefl N ty MRefl = Refl ty
--    so we return True
-- See Note [KindCoercion] in GHC.Core.TyCo.Rep
isReflKindCo :: HasDebugCallStack => KindCoercion -> Bool
isReflKindCo co@(GRefl {}) = assertPpr (isKindCo co) (ppr co) $
                             True
isReflKindCo (Refl{})      = True -- Refl ty == GRefl N ty MRefl
isReflKindCo _             = False

-- | Tests if this /kind/ MCoercion is obviously generalized reflexive
-- Guaranteed to work very quickly.
isReflKindMCo :: KindMCoercion -> Bool
isReflKindMCo MRefl    = True
isReflKindMCo (MCo co) = isReflKindCo co

-- | Tests if this coercion is obviously reflexive. Guaranteed to work
-- very quickly. Sometimes a coercion can be reflexive, but not obviously
-- so. c.f. 'isReflexiveCo'
isReflCo :: Coercion -> Bool
isReflCo (Refl{}) = True
isReflCo (GRefl _ _ mco) | isReflKindMCo mco = True
isReflCo _ = False

-- | Returns the type coerced if this coercion is reflexive. Guaranteed
-- to work very quickly. Sometimes a coercion can be reflexive, but not
-- obviously so. c.f. 'isReflexiveCo_maybe'
isReflCo_maybe :: Coercion -> Maybe (Type, Role)
isReflCo_maybe (Refl ty) = Just (ty, Nominal)
isReflCo_maybe (GRefl r ty mco) | isReflKindMCo mco = Just (ty, r)
isReflCo_maybe _ = Nothing


{- *********************************************************************
*                                                                      *
              MCoercion
*                                                                      *
********************************************************************* -}

coToMCo :: Coercion -> MCoercion
-- Convert a coercion to a MCoercion,
-- It's not clear whether or not isReflexiveCo would be better here
--    See #19815 for a bit of data and discussion on this point
coToMCo co | isReflCo co = MRefl
           | otherwise   = MCo co

kindCoToMKindCo :: KindCoercion -> KindMCoercion
-- Convert a KindCoercion to a KindMCoercion,
-- coToMCo doesn't eliminate GRefl, but kindCoToMCo can
-- See Note [KindCoercion]
kindCoToMKindCo co | isReflKindCo co = MRefl
                   | otherwise       = MCo co

-- | Make a generalized reflexive coercion
mkGReflCo :: Role -> Type -> MCoercionN -> Coercion
mkGReflCo r ty mco
  | isReflKindMCo mco = if r == Nominal then Refl ty
                                        else GRefl r ty MRefl
  | otherwise
  = -- I'd like to have this assert, but sadly it's not true during type
    -- inference because the types are not fully zonked
    -- assertPpr (case mco of
    --              MCo co -> typeKind ty `eqType` coercionLKind co
    --              MRefl  -> True)
    --          (vcat [ text "ty" <+> ppr ty <+> dcolon <+> ppr (typeKind ty)
    --                , case mco of
    --                     MCo co -> text "co" <+> ppr co
    --                                  <+> dcolon <+> ppr (coercionKind co)
    --                     MRefl  -> text "MRefl"
    --                , callStackDoc ]) $
    GRefl r ty mco

mkGReflMCo :: HasDebugCallStack => Role -> Type -> CoercionN -> Coercion
mkGReflMCo r ty co = mkGReflCo r ty (MCo co)

-- | Compose two MCoercions via transitivity
mkTransMCo :: MCoercion -> MCoercion -> MCoercion
mkTransMCo MRefl     co2       = co2
mkTransMCo co1       MRefl     = co1
mkTransMCo (MCo co1) (MCo co2) = MCo (mkTransCo co1 co2)

mkTransMCoL :: MCoercion -> Coercion -> MCoercion
mkTransMCoL MRefl     co2 = coToMCo co2
mkTransMCoL (MCo co1) co2 = MCo (mkTransCo co1 co2)

mkTransMCoR :: Coercion -> MCoercion -> MCoercion
mkTransMCoR co1 MRefl     = coToMCo co1
mkTransMCoR co1 (MCo co2) = MCo (mkTransCo co1 co2)

-- | Get the reverse of an 'MCoercion'
mkSymMCo :: MCoercion -> MCoercion
mkSymMCo MRefl    = MRefl
mkSymMCo (MCo co) = MCo (mkSymCo co)

-- | Cast a type by an 'MCoercion'
mkCastTyMCo :: Type -> MCoercion -> Type
mkCastTyMCo ty MRefl    = ty
mkCastTyMCo ty (MCo co) = ty `mkCastTy` co

mkGReflLeftMCo :: Role -> Type -> MCoercionN -> Coercion
mkGReflLeftMCo r ty MRefl    = mkReflCo r ty
mkGReflLeftMCo r ty (MCo co) = mkGReflLeftCo r ty co

mkGReflRightMCo :: Role -> Type -> MCoercionN -> Coercion
mkGReflRightMCo r ty MRefl    = mkReflCo r ty
mkGReflRightMCo r ty (MCo co) = mkGReflRightCo r ty co

-- | Like 'mkCoherenceRightCo', but with an 'MCoercion'
mkCoherenceRightMCo :: Role -> Type -> MCoercionN -> Coercion -> Coercion
mkCoherenceRightMCo _ _  MRefl    co2 = co2
mkCoherenceRightMCo r ty (MCo co) co2 = mkCoherenceRightCo r ty co co2

isReflMCo :: MCoercion -> Bool
isReflMCo MRefl = True
isReflMCo _     = False

