{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
module GHC.Core.TyCo.Rep.Open
  ( TypeF
      ( TyVarTyF
      , AppTyF
      , TyConAppF
      , ForAllTyF
      , LitTyF
      , CastTyF
      , CoercionTyF
      , FunTyF
      , ftf_af
      , ftf_mult
      , ftf_arg
      , ftf_res
      )  -- Export the type synonym FunTy too
  ) where

import GHC.Prelude

-- GHC
import GHC.Types.Var.ArgFlag
import GHC.Types.Var.Binder

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Identity
import qualified Data.Data as Data hiding ( TyCon )

-- | Parametized single layer of type abstract syntax.
--
-- With different paramters, it is the "true" abstract syntax, a component of
-- some serialization formats, and perhaps other uses. Were we every to
-- distinguish between HsSyn and Core types, that might be another bunch of parameters.
--
-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
--
-- @argTyp@ means the same as @type@, but is different for (de)serialization purposes.
-- See Note [Efficient serialization of redundant type info]
data TypeF tyVar tyCoVar tyCon tyLit coercion kindCoercion argTys conArgTys ty
  -- See Note [Non-trivial definitional equality]

  -- | Vanilla type or kind variable (*never* a coercion variable)
  = TyVarTyF tyVar

  -- | Type application to something other than a 'TyCon'.
  | AppTyF
        ty
        argTys          -- ^ Type application to something other than a 'TyCon'. Parameters:
                        --
                        --  1) Function: must /not/ be a 'TyConApp' or 'CastTy',
                        --     must be another 'AppTy', or 'TyVarTy'
                        --     See Note [Respecting definitional equality] \(EQ1) about the
                        --     no 'CastTy' requirement
                        --
                        --  2) Argument type

  | TyConAppF
        tyCon
        conArgTys       -- ^ Application of a 'TyCon', including newtypes /and/ synonyms.
                        -- Invariant: saturated applications of 'FunTyCon' must
                        -- use 'FunTy' and saturated synonyms must use their own
                        -- constructors. However, /unsaturated/ 'FunTyCon's
                        -- do appear as 'TyConApp's.
                        -- Parameters:
                        --
                        -- 1) Type constructor being applied to.
                        --
                        -- 2) Type arguments. Might not have enough type arguments
                        --    here to saturate the constructor.
                        --    Even type synonyms are not necessarily saturated;
                        --    for example unsaturated type synonyms
                        --    can appear as the right hand side of a type synonym.

  | ForAllTyF
        {-# UNPACK #-} !(VarBndr tyCoVar ArgFlag)
        ty              -- ^ A Π type.
             -- INVARIANT: If the binder is a coercion variable, it must
             -- be mentioned in the Type. See
             -- Note [Unused coercion variable in ForAllTy]

  | FunTyF      -- ^ FUN m t1 t2   Very common, so an important special case
                -- See Note [Function types]
     { ftf_af  :: AnonArgFlag    -- Is this (->) or (=>)?
     , ftf_mult :: ty {- mult -} -- Multiplicity
     , ftf_arg :: ty             -- Argument type
     , ftf_res :: ty             -- Result type
     }

  | LitTyF tyLit      -- ^ Type literals are similar to type constructors.

  | CastTyF
        ty
        kindCoercion  -- ^ A kind cast. The coercion is always nominal.
                      -- INVARIANT: The cast is never reflexive \(EQ2)
                      -- INVARIANT: The Type is not a CastTy (use TransCo instead) \(EQ3)
                      -- INVARIANT: The Type is not a ForAllTy over a tyvar \(EQ4)
                      -- See Note [Respecting definitional equality]

  | CoercionTyF
        coercion    -- ^ Injection of a Coercion into a type
                    -- This should only ever be used in the RHS of an AppTy,
                    -- in the list of a TyConApp, when applying a promoted
                    -- GADT data constructor

  deriving ( Eq, Data.Data
           , Functor, Foldable, Traversable
           )

instance Bifunctor (TypeF tyVar tyCoVar tyCon tyLit coercion kindCoercion argTys) where
  bimap f g = runIdentity . bitraverse (Identity . f) (Identity . g)

instance Bifoldable (TypeF tyVar tyCoVar tyCon tyLit coercion kindCoercion argTys) where
  bifoldMap = bifoldMapDefault

instance Bitraversable (TypeF tyVar tyCoVar tyCon tyLit coercion kindCoercion argTys) where
  bitraverse f g = \case
    TyVarTyF v -> pure $ TyVarTyF v
    AppTyF tf a -> flip AppTyF a <$> g tf
    TyConAppF tf a -> TyConAppF tf <$> f a
    ForAllTyF b t -> ForAllTyF b <$> g t
    FunTyF flag m i o -> FunTyF flag <$> g m <*> g i <*> g o
    LitTyF l -> pure $ LitTyF l
    CastTyF t co -> flip CastTyF co <$> g t
    CoercionTyF kc -> pure $ CoercionTyF kc
