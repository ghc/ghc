-- (c) The University of Glasgow 2006
-- (c) The GRASP/AQUA Project, Glasgow University, 1998
--
-- Type - public interface

{-# LANGUAGE FlexibleContexts, PatternSynonyms, ViewPatterns, MultiWayIf #-}

module GHC.Core.TyCo.Utils (

   -- * Construction
   mkAppTy, mkCastTy, mkTyConAppTy,

   -- * Kind of a type
   typeKind,

   -- * Occurrence-check expansion
   occCheckExpand

   ) where

import GHC.Prelude

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCon

import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Basic( TypeOrConstraint(..) )

import GHC.Builtin.Names
import GHC.Builtin.Types.Prim

import {-# SOURCE #-} GHC.Builtin.Types
   ( charTy, naturalTy
   , typeSymbolKind, liftedTypeKind, unliftedTypeKind
   , boxedRepDataConTyCon, constraintKind, zeroBitTypeKind
   , manyDataConTy, oneDataConTy
   , liftedRepTy, unliftedRepTy, zeroBitRepTy )

import {-# SOURCE #-} GHC.Core.Coercion
   ( mkNomReflCo, mkGReflCo, mkReflCo
   , mkTyConAppCo, mkAppCo
   , mkForAllCo, mkFunCo, mkAxiomInstCo, mkUnivCo
   , mkSymCo, mkTransCo, mkSelCo, mkLRCo, mkInstCo
   , mkKindCo, mkSubCo, mkCoVarCo, mkAxiomRuleCo
   , decomposePiCos, coercionKind, coercionLKind
   , coercionRKind, coercionType
   , isReflexiveCo, seqCo
   , topNormaliseNewType_maybe
   )

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic( pprPanic )
import GHC.Utils.Panic.Plain( assert )

{-
************************************************************************
*                                                                      *
        The kind of a type
*                                                                      *
************************************************************************

Note [Kinding rules for types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In typeKind we consider Constraint and (TYPE LiftedRep) to be identical.
We then have

         t1 : TYPE rep1
         t2 : TYPE rep2
   (FUN) ----------------
         t1 -> t2 : Type

         ty : TYPE rep
         `a` is not free in rep
(FORALL) -----------------------
         forall a. ty : TYPE rep

          t1 : TYPE rep1
          t2 : TYPE rep2
    (FUN) ----------------
          t1 -> t2 : Type

          t1 : Constraint
          t2 : TYPE rep
  (PRED1) ----------------
          t1 => t2 : Type

          t1 : Constraint
          t2 : Constraint
  (PRED2) ---------------------
          t1 => t2 : Constraint

          ty : TYPE rep
          `a` is not free in rep
(FORALL1) -----------------------
          forall a. ty : TYPE rep

          ty : Constraint
(FORALL2) -------------------------
          forall a. ty : Constraint

Note that:
* The only way we distinguish '->' from '=>' is by the fact
  that the argument is a PredTy.  Both are FunTys

Note [Phantom type variables in kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  type K (r :: RuntimeRep) = Type   -- Note 'r' is unused
  data T r :: K r                   -- T :: forall r -> K r
  foo :: forall r. T r

The body of the forall in foo's type has kind (K r), and
normally it would make no sense to have
   forall r. (ty :: K r)
because the kind of the forall would escape the binding
of 'r'.  But in this case it's fine because (K r) exapands
to Type, so we explicitly /permit/ the type
   forall r. T r

To accommodate such a type, in typeKind (forall a.ty) we use
occCheckExpand to expand any type synonyms in the kind of 'ty'
to eliminate 'a'.  See kinding rule (FORALL) in
Note [Kinding rules for types]

See also
 * GHC.Core.Type.occCheckExpand
 * GHC.Core.Utils.coreAltsType
 * GHC.Tc.Validity.checkEscapingKind
all of which grapple with the same problem.

See #14939.
-}

-----------------------------
typeKind :: HasDebugCallStack => Type -> Kind
-- No need to expand synonyms
typeKind (TyConApp tc tys)      = piResultTys (tyConKind tc) tys
typeKind (LitTy l)              = typeLiteralKind l
typeKind (FunTy { ft_af = af }) = case anonArgResultTypeOrConstraint af of
                                     TypeLike       -> liftedTypeKind
                                     ConstraintLike -> constraintKind
typeKind (TyVarTy tyvar)        = tyVarKind tyvar
typeKind (CastTy _ty co)        = coercionRKind co
typeKind (CoercionTy co)        = coercionType co

typeKind (AppTy fun arg)
  = go fun [arg]
  where
    -- Accumulate the type arguments, so we can call piResultTys,
    -- rather than a succession of calls to piResultTy (which is
    -- asymptotically costly as the number of arguments increases)
    go (AppTy fun arg) args = go fun (arg:args)
    go fun             args = piResultTys (typeKind fun) args

typeKind orig_ty@(ForAllTy {})
  = go [] orig_ty
  where
    go tvs body
      | ForAllTy (Bndr tv _) ty <- body
      = go (tv:tvs) ty

      | Just kind' <- occCheckExpand tvs body_kind
      = kind'  -- Make sure tv does not occur in kind
               -- As it is already out of scope!
               -- See Note [Phantom type variables in kinds]

      | otherwise
      = pprPanic "typeKind"
        (ppr orig_ty $$ ppr tvs $$ ppr body <+> dcolon <+> ppr body_kind)

      where
        body_kind   = typeKind body

{- *********************************************************************
*                                                                      *
               mapType
*                                                                      *
************************************************************************

These functions do a map-like operation over types, performing some operation
on all variables and binding sites. Primarily used for zonking.

Note [Efficiency for ForAllCo case of mapTyCoX]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As noted in Note [Forall coercions] in GHC.Core.TyCo.Rep, a ForAllCo is a bit redundant.
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

      , tcm_tycobinder :: env -> TyCoVar -> ArgFlag -> m (env, TyCoVar)
          -- ^ The returned env is used in the extended scope

      , tcm_tycon :: TyCon -> m TyCon
          -- ^ This is used only for TcTyCons
          -- a) To zonk TcTyCons
          -- b) To turn TcTyCons into TyCons.
          --    See Note [Type checking recursive type and class declarations]
          --    in "GHC.Tc.TyCl"
      }

{-# INLINE mapTyCo #-}  -- See Note [Specialising mappers]
mapTyCo :: Monad m => TyCoMapper () m
         -> ( Type       -> m Type
            , [Type]     -> m [Type]
            , Coercion   -> m Coercion
            , [Coercion] -> m[Coercion])
mapTyCo mapper
  = case mapTyCoX mapper of
     (go_ty, go_tys, go_co, go_cos)
        -> (go_ty (), go_tys (), go_co (), go_cos ())

{-# INLINE mapTyCoX #-}  -- See Note [Specialising mappers]
mapTyCoX :: Monad m => TyCoMapper env m
         -> ( env -> Type       -> m Type
            , env -> [Type]     -> m [Type]
            , env -> Coercion   -> m Coercion
            , env -> [Coercion] -> m[Coercion])
mapTyCoX (TyCoMapper { tcm_tyvar = tyvar
                     , tcm_tycobinder = tycobinder
                     , tcm_tycon = tycon
                     , tcm_covar = covar
                     , tcm_hole = cohole })
  = (go_ty, go_tys, go_co, go_cos)
  where
    go_tys _   []       = return []
    go_tys env (ty:tys) = (:) <$> go_ty env ty <*> go_tys env tys

    go_ty env (TyVarTy tv)    = tyvar env tv
    go_ty env (AppTy t1 t2)   = mkAppTy <$> go_ty env t1 <*> go_ty env t2
    go_ty _   ty@(LitTy {})   = return ty
    go_ty env (CastTy ty co)  = mkCastTy <$> go_ty env ty <*> go_co env co
    go_ty env (CoercionTy co) = CoercionTy <$> go_co env co

    go_ty env ty@(FunTy _ w arg res)
      = do { w' <- go_ty env w; arg' <- go_ty env arg; res' <- go_ty env res
           ; return (ty { ft_mult = w', ft_arg = arg', ft_res = res' }) }

    go_ty env ty@(TyConApp tc tys)
      | isTcTyCon tc
      = do { tc' <- tycon tc
           ; mkTyConApp tc' <$> go_tys env tys }

      -- Not a TcTyCon
      | null tys    -- Avoid allocation in this very
      = return ty   -- common case (E.g. Int, LiftedRep etc)

      | otherwise
      = mkTyConApp tc <$> go_tys env tys

    go_ty env (ForAllTy (Bndr tv vis) inner)
      = do { (env', tv') <- tycobinder env tv vis
           ; inner' <- go_ty env' inner
           ; return $ ForAllTy (Bndr tv' vis) inner' }

    go_cos _   []       = return []
    go_cos env (co:cos) = (:) <$> go_co env co <*> go_cos env cos

    go_mco _   MRefl    = return MRefl
    go_mco env (MCo co) = MCo <$> (go_co env co)

    go_co env (Refl ty)           = Refl <$> go_ty env ty
    go_co env (GRefl r ty mco)    = mkGReflCo r <$> go_ty env ty <*> go_mco env mco
    go_co env (AppCo c1 c2)       = mkAppCo <$> go_co env c1 <*> go_co env c2
    go_co env (FunCo r cw c1 c2)  = mkFunCo r <$> go_co env cw
                                      <*> go_co env c1 <*> go_co env c2
    go_co env (CoVarCo cv)        = covar env cv
    go_co env (HoleCo hole)       = cohole env hole
    go_co env (UnivCo p r t1 t2)  = mkUnivCo <$> go_prov env p <*> pure r
                                    <*> go_ty env t1 <*> go_ty env t2
    go_co env (SymCo co)          = mkSymCo <$> go_co env co
    go_co env (TransCo c1 c2)     = mkTransCo <$> go_co env c1 <*> go_co env c2
    go_co env (AxiomRuleCo r cos) = AxiomRuleCo r <$> go_cos env cos
    go_co env (SelCo i co)        = mkSelCo i <$> go_co env co
    go_co env (LRCo lr co)        = mkLRCo lr <$> go_co env co
    go_co env (InstCo co arg)     = mkInstCo <$> go_co env co <*> go_co env arg
    go_co env (KindCo co)         = mkKindCo <$> go_co env co
    go_co env (SubCo co)          = mkSubCo <$> go_co env co
    go_co env (AxiomInstCo ax i cos) = mkAxiomInstCo ax i <$> go_cos env cos
    go_co env co@(TyConAppCo r tc cos)
      | isTcTyCon tc
      = do { tc' <- tycon tc
           ; mkTyConAppCo r tc' <$> go_cos env cos }

      -- Not a TcTyCon
      | null cos    -- Avoid allocation in this very
      = return co   -- common case (E.g. Int, LiftedRep etc)

      | otherwise
      = mkTyConAppCo r tc <$> go_cos env cos
    go_co env (ForAllCo tv kind_co co)
      = do { kind_co' <- go_co env kind_co
           ; (env', tv') <- tycobinder env tv Inferred
           ; co' <- go_co env' co
           ; return $ mkForAllCo tv' kind_co' co' }
        -- See Note [Efficiency for ForAllCo case of mapTyCoX]

    go_prov env (PhantomProv co)    = PhantomProv <$> go_co env co
    go_prov env (ProofIrrelProv co) = ProofIrrelProv <$> go_co env co
    go_prov _   p@(PluginProv _)    = return p
    go_prov _   p@(CorePrepProv _)  = return p


{- **********************************************************************
*                                                                       *
           mkAppTy
%*                                                                      *
%********************************************************************* -}

-- | Applies a type to another, as in e.g. @k a@
mkAppTy :: Type -> Type -> Type
  -- See Note [Respecting definitional equality], invariant (EQ1).
mkAppTy (CastTy fun_ty co) arg_ty
  | ([arg_co], res_co) <- decomposePiCos co (coercionKind co) [arg_ty]
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
    (arg_cos, res_co) = decomposePiCos co (coercionKind co) arg_tys
    (args_to_cast, leftovers) = splitAtList arg_cos arg_tys
    casted_arg_tys = zipWith mkCastTy args_to_cast arg_cos
mkAppTys (TyConApp tc tys1) tys2 = mkTyConApp tc (tys1 ++ tys2)
mkAppTys ty1                tys2 = foldl' AppTy ty1 tys2


{- **********************************************************************
*                                                                       *
           mkCastTy
%*                                                                      *
%********************************************************************* -}

-- | Make a 'CastTy'. The Coercion must be nominal. Checks the
-- Coercion for reflexivity, dropping it if it's reflexive.
-- See @Note [Respecting definitional equality]@ in "GHC.Core.TyCo.Rep"
mkCastTy :: Type -> Coercion -> Type
mkCastTy orig_ty co | isReflexiveCo co = orig_ty  -- (EQ2) from the Note
-- NB: Do the slow check here. This is important to keep the splitXXX
-- functions working properly. Otherwise, we may end up with something
-- like (((->) |> something_reflexive_but_not_obviously_so) biz baz)
-- fails under splitFunTy_maybe. This happened with the cheaper check
-- in test dependent/should_compile/dynamic-paper.
mkCastTy orig_ty co = mk_cast_ty orig_ty co

-- | Like 'mkCastTy', but avoids checking the coercion for reflexivity,
-- as that can be expensive.
mk_cast_ty :: Type -> Coercion -> Type
mk_cast_ty orig_ty co = go orig_ty
  where
    go :: Type -> Type
    -- See Note [Using coreView in mk_cast_ty]
    go ty | Just ty' <- coreView ty = go ty'

    go (CastTy ty co1)
      -- (EQ3) from the Note
      = mkCastTy ty (co1 `mkTransCo` co)
          -- call mkCastTy again for the reflexivity check

    go (ForAllTy (Bndr tv vis) inner_ty)
      -- (EQ4) from the Note
      -- See Note [Weird typing rule for ForAllTy] in GHC.Core.TyCo.Rep.
      | isTyVar tv
      , let fvs = tyCoVarsOfCo co
      = -- have to make sure that pushing the co in doesn't capture the bound var!
        if tv `elemVarSet` fvs
        then let empty_subst = mkEmptySubst (mkInScopeSet fvs)
                 (subst, tv') = substVarBndr empty_subst tv
             in ForAllTy (Bndr tv' vis) (substTy subst inner_ty `mk_cast_ty` co)
        else ForAllTy (Bndr tv vis) (go inner_ty)

    go _ = CastTy orig_ty co -- NB: orig_ty: preserve synonyms if possible

{-
Note [Using coreView in mk_cast_ty]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariants (EQ3) and (EQ4) of Note [Respecting definitional equality] in
GHC.Core.TyCo.Rep must apply regardless of type synonyms. For instance,
consider this example (#19742):

   type EqSameNat = () |> co
   useNatEq :: EqSameNat |> sym co

(Those casts aren't visible in the user-source code, of course; see #19742 for
what the user might write.)

The type `EqSameNat |> sym co` looks as if it satisfies (EQ3), as it has no
nested casts, but if we expand EqSameNat, we see that it doesn't.
And then Bad Things happen.

The solution is easy: just use `coreView` when establishing (EQ3) and (EQ4) in
`mk_cast_ty`.
-}

{- **********************************************************************
*                                                                       *
           mkTyConAppTy
           (space-saving construction)
%*                                                                      *
%********************************************************************* -}

{- Note [Using synonyms to compress types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Was: Prefer Type over TYPE (BoxedRep Lifted)]

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
applications (e.g. things like @TyConApp typeTyCon []@), Note [Comparing
nullary type synonyms] in "GHC.Core.Type".

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
  | Just (af, mult, arg, res) <- tyConAppFun_maybe id tycon tys
  = FunTy { ft_af = af, ft_mult = mult, ft_arg = arg, ft_res = res }

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
  | key == liftedRepTyConKey = assert (null args) $ Just constraintKind   -- CONSTRAINT LiftedRep
  where
    key = tyConUnique tc
mkCONSTRAINTapp_maybe _ = Nothing

------------------
mkBoxedRepApp_maybe :: Type -> Maybe Type
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


{- **********************************************************************
*                                                                       *
           mkCoercionTy
%*                                                                      *
%********************************************************************* -}

mkCoercionTy :: Coercion -> Type
mkCoercionTy = CoercionTy


{- **********************************************************************
*                                                                       *
           Occurs check expansion
%*                                                                      *
%********************************************************************* -}

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

Note [Occurrence checking: look inside kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are considering unifying
   (alpha :: *)  ~  Int -> (beta :: alpha -> alpha)
This may be an error (what is that alpha doing inside beta's kind?),
but we must not make the mistake of actually unifying or we'll
build an infinite data structure.  So when looking for occurrences
of alpha in the rhs, we must look in the kinds of type variables
that occur there.

occCheckExpand tries to expand type synonyms to remove
unnecessary occurrences of a variable, and thereby get past an
occurs-check failure.  This is good; but
     we can't do it in the /kind/ of a variable /occurrence/

For example #18451 built an infinite type:
    type Const a b = a
    data SameKind :: k -> k -> Type
    type T (k :: Const Type a) = forall (b :: k). SameKind a b

We have
  b :: k
  k :: Const Type a
  a :: k   (must be same as b)

So if we aren't careful, a's kind mentions a, which is bad.
And expanding an /occurrence/ of 'a' doesn't help, because the
/binding site/ is the master copy and all the occurrences should
match it.

Here's a related example:
   f :: forall a b (c :: Const Type b). Proxy '[a, c]

The list means that 'a' gets the same kind as 'c'; but that
kind mentions 'b', so the binders are out of order.

Bottom line: in occCheckExpand, do not expand inside the kinds
of occurrences.  See bad_var_occ in occCheckExpand.  And
see #18451 for more debate.
-}

occCheckExpand :: [Var] -> Type -> Maybe Type
-- See Note [Occurs check expansion]
-- We may have needed to do some type synonym unfolding in order to
-- get rid of the variable (or forall), so we also return the unfolded
-- version of the type, which is guaranteed to be syntactically free
-- of the given type variable.  If the type is already syntactically
-- free of the variable, then the same type is returned.
occCheckExpand vs_to_avoid ty
  | null vs_to_avoid  -- Efficient shortcut
  = Just ty           -- Can happen, eg. GHC.Core.Utils.mkSingleAltCase

  | otherwise
  = go (mkVarSet vs_to_avoid, emptyVarEnv) ty
  where
    go :: (VarSet, VarEnv TyCoVar) -> Type -> Maybe Type
          -- The VarSet is the set of variables we are trying to avoid
          -- The VarEnv carries mappings necessary
          -- because of kind expansion
    go (as, env) ty@(TyVarTy tv)
      | Just tv' <- lookupVarEnv env tv = return (mkTyVarTy tv')
      | bad_var_occ as tv               = Nothing
      | otherwise                       = return ty

    go _   ty@(LitTy {}) = return ty
    go cxt (AppTy ty1 ty2) = do { ty1' <- go cxt ty1
                                ; ty2' <- go cxt ty2
                                ; return (mkAppTy ty1' ty2') }
    go cxt ty@(FunTy _ w ty1 ty2)
       = do { w'   <- go cxt w
            ; ty1' <- go cxt ty1
            ; ty2' <- go cxt ty2
            ; return (ty { ft_mult = w', ft_arg = ty1', ft_res = ty2' }) }
    go cxt@(as, env) (ForAllTy (Bndr tv vis) body_ty)
       = do { ki' <- go cxt (varType tv)
            ; let tv'  = setVarType tv ki'
                  env' = extendVarEnv env tv tv'
                  as'  = as `delVarSet` tv
            ; body' <- go (as', env') body_ty
            ; return (ForAllTy (Bndr tv' vis) body') }

    -- For a type constructor application, first try expanding away the
    -- offending variable from the arguments.  If that doesn't work, next
    -- see if the type constructor is a type synonym, and if so, expand
    -- it and try again.
    go cxt ty@(TyConApp tc tys)
      = case mapM (go cxt) tys of
          Just tys' -> return (mkTyConApp tc tys')
          Nothing | Just ty' <- tcView ty -> go cxt ty'
                  | otherwise             -> Nothing
                      -- Failing that, try to expand a synonym

    go cxt (CastTy ty co) =  do { ty' <- go cxt ty
                                ; co' <- go_co cxt co
                                ; return (mkCastTy ty' co') }
    go cxt (CoercionTy co) = do { co' <- go_co cxt co
                                ; return (mkCoercionTy co') }

    ------------------
    bad_var_occ :: VarSet -> Var -> Bool
    -- Works for TyVar and CoVar
    -- See Note [Occurrence checking: look inside kinds]
    bad_var_occ vs_to_avoid v
       =  v                          `elemVarSet`       vs_to_avoid
       || tyCoVarsOfType (varType v) `intersectsVarSet` vs_to_avoid

    ------------------
    go_mco _   MRefl = return MRefl
    go_mco ctx (MCo co) = MCo <$> go_co ctx co

    ------------------
    go_co cxt (Refl ty)                 = do { ty' <- go cxt ty
                                             ; return (mkNomReflCo ty') }
    go_co cxt (GRefl r ty mco)          = do { mco' <- go_mco cxt mco
                                             ; ty' <- go cxt ty
                                             ; return (mkGReflCo r ty' mco') }
      -- Note: Coercions do not contain type synonyms
    go_co cxt (TyConAppCo r tc args)    = do { args' <- mapM (go_co cxt) args
                                             ; return (mkTyConAppCo r tc args') }
    go_co cxt (AppCo co arg)            = do { co' <- go_co cxt co
                                             ; arg' <- go_co cxt arg
                                             ; return (mkAppCo co' arg') }
    go_co cxt@(as, env) (ForAllCo tv kind_co body_co)
      = do { kind_co' <- go_co cxt kind_co
           ; let tv' = setVarType tv $
                       coercionLKind kind_co'
                 env' = extendVarEnv env tv tv'
                 as'  = as `delVarSet` tv
           ; body' <- go_co (as', env') body_co
           ; return (ForAllCo tv' kind_co' body') }
    go_co cxt (FunCo r  w co1 co2)      = do { co1' <- go_co cxt co1
                                             ; co2' <- go_co cxt co2
                                             ; w' <- go_co cxt w
                                             ; return (mkFunCo r w' co1' co2') }
    go_co (as,env) co@(CoVarCo c)
      | Just c' <- lookupVarEnv env c   = return (mkCoVarCo c')
      | bad_var_occ as c                = Nothing
      | otherwise                       = return co

    go_co (as,_) co@(HoleCo h)
      | bad_var_occ as (ch_co_var h)    = Nothing
      | otherwise                       = return co

    go_co cxt (AxiomInstCo ax ind args) = do { args' <- mapM (go_co cxt) args
                                             ; return (mkAxiomInstCo ax ind args') }
    go_co cxt (UnivCo p r ty1 ty2)      = do { p' <- go_prov cxt p
                                             ; ty1' <- go cxt ty1
                                             ; ty2' <- go cxt ty2
                                             ; return (mkUnivCo p' r ty1' ty2') }
    go_co cxt (SymCo co)                = do { co' <- go_co cxt co
                                             ; return (mkSymCo co') }
    go_co cxt (TransCo co1 co2)         = do { co1' <- go_co cxt co1
                                             ; co2' <- go_co cxt co2
                                             ; return (mkTransCo co1' co2') }
    go_co cxt (SelCo n co)              = do { co' <- go_co cxt co
                                             ; return (mkSelCo n co') }
    go_co cxt (LRCo lr co)              = do { co' <- go_co cxt co
                                             ; return (mkLRCo lr co') }
    go_co cxt (InstCo co arg)           = do { co' <- go_co cxt co
                                             ; arg' <- go_co cxt arg
                                             ; return (mkInstCo co' arg') }
    go_co cxt (KindCo co)               = do { co' <- go_co cxt co
                                             ; return (mkKindCo co') }
    go_co cxt (SubCo co)                = do { co' <- go_co cxt co
                                             ; return (mkSubCo co') }
    go_co cxt (AxiomRuleCo ax cs)       = do { cs' <- mapM (go_co cxt) cs
                                             ; return (mkAxiomRuleCo ax cs') }

    ------------------
    go_prov cxt (PhantomProv co)    = PhantomProv <$> go_co cxt co
    go_prov cxt (ProofIrrelProv co) = ProofIrrelProv <$> go_co cxt co
    go_prov _   p@(PluginProv _)    = return p
    go_prov _   p@(CorePrepProv _)  = return p



