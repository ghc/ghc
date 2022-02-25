{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Typeable
-- Copyright   :  (c) The University of Glasgow, CWI 2001--2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- The 'Typeable' class reifies types to some extent by associating type
-- representations to types. These type representations can be compared,
-- and one can in turn define a type-safe cast operation. To this end,
-- an unsafe cast is guarded by a test for type (representation)
-- equivalence. The module "Data.Dynamic" uses Typeable for an
-- implementation of dynamics. The module "Data.Data" uses Typeable
-- and type-safe cast (but not dynamics) to support the \"Scrap your
-- boilerplate\" style of generic programming.
--
-- == Compatibility Notes
--
-- Since GHC 8.2, GHC has supported type-indexed type representations.
-- "Data.Typeable" provides type representations which are qualified over this
-- index, providing an interface very similar to the "Typeable" notion seen in
-- previous releases. For the type-indexed interface, see "Type.Reflection".
--
-- Since GHC 7.10, all types automatically have 'Typeable' instances derived.
-- This is in contrast to previous releases where 'Typeable' had to be
-- explicitly derived using the @DeriveDataTypeable@ language extension.
--
-- Since GHC 7.8, 'Typeable' is poly-kinded. The changes required for this might
-- break some old programs involving 'Typeable'. More details on this, including
-- how to fix your code, can be found on the
-- <https://gitlab.haskell.org/ghc/ghc/wikis/ghc-kinds/poly-typeable PolyTypeable wiki page>
--
-----------------------------------------------------------------------------

module Data.Typeable
    ( -- * The Typeable class
      Typeable
    , typeOf
    , typeRep

      -- * Propositional equality
    , (:~:)(Refl)
    , (:~~:)(HRefl)

      -- * Type-safe cast
    , cast
    , eqT
    , gcast                -- a generalisation of cast

      -- * Generalized casts for higher-order kinds
    , gcast1               -- :: ... => c (t a) -> Maybe (c (t' a))
    , gcast2               -- :: ... => c (t a b) -> Maybe (c (t' a b))

      -- * A canonical proxy type
    , Proxy (..)

      -- * Type representations
    , TypeRep
    , rnfTypeRep
    , showsTypeRep
    , mkFunTy

      -- * Observing type representations
    , funResultTy
    , splitTyConApp
    , typeRepArgs
    , typeRepTyCon
    , typeRepFingerprint

      -- * Type constructors
    , I.TyCon          -- abstract, instance of: Eq, Show, Typeable
                       -- For now don't export Module to avoid name clashes
    , I.tyConPackage
    , I.tyConModule
    , I.tyConName
    , I.rnfTyCon
    , I.tyConFingerprint

      -- * For backwards compatibility
    , typeOf1, typeOf2, typeOf3, typeOf4, typeOf5, typeOf6, typeOf7
      -- Jank
    , I.trLiftedRep
    ) where

import qualified Data.Typeable.Internal as I
import Data.Typeable.Internal (Typeable)
import Data.Type.Equality

import Data.Maybe
import Data.Proxy
import GHC.Fingerprint.Type
import GHC.Show
import GHC.Base

-- | A quantified type representation.
type TypeRep = I.SomeTypeRep

-- | Observe a type representation for the type of a value.
typeOf :: forall a. Typeable a => a -> TypeRep
typeOf _ = I.someTypeRep (Proxy :: Proxy a)

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
--
-- @since 4.7.0.0
typeRep :: forall proxy a. Typeable a => proxy a -> TypeRep
typeRep = I.someTypeRep

-- | Show a type representation
showsTypeRep :: TypeRep -> ShowS
showsTypeRep = shows

-- | The type-safe cast operation
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
cast x
  | Just HRefl <- ta `I.eqTypeRep` tb = Just x
  | otherwise                         = Nothing
  where
    ta = I.typeRep :: I.TypeRep a
    tb = I.typeRep :: I.TypeRep b

-- | Extract a witness of equality of two types
--
-- @since 4.7.0.0
eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
eqT
  | Just HRefl <- ta `I.eqTypeRep` tb = Just Refl
  | otherwise                         = Nothing
  where
    ta = I.typeRep :: I.TypeRep a
    tb = I.typeRep :: I.TypeRep b

-- | A flexible variation parameterised in a type constructor
gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast x = fmap (\Refl -> x) (eqT :: Maybe (a :~: b))

-- | Cast over @k1 -> k2@
gcast1 :: forall c t t' a. (Typeable t, Typeable t')
       => c (t a) -> Maybe (c (t' a))
gcast1 x = fmap (\Refl -> x) (eqT :: Maybe (t :~: t'))

-- | Cast over @k1 -> k2 -> k3@
gcast2 :: forall c t t' a b. (Typeable t, Typeable t')
       => c (t a b) -> Maybe (c (t' a b))
gcast2 x = fmap (\Refl -> x) (eqT :: Maybe (t :~: t'))

-- | Applies a type to a function type. Returns: @Just u@ if the first argument
-- represents a function of type @t -> u@ and the second argument represents a
-- function of type @t@. Otherwise, returns @Nothing@.
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy (I.SomeTypeRep f) (I.SomeTypeRep x)
  | Just HRefl <- (I.typeRep :: I.TypeRep Type) `I.eqTypeRep` I.typeRepKind f
  , I.Fun arg res <- f
  , Just HRefl <- arg `I.eqTypeRep` x
  = Just (I.SomeTypeRep res)
  | otherwise = Nothing

-- | Build a function type.
mkFunTy :: TypeRep -> TypeRep -> TypeRep
mkFunTy (I.SomeTypeRep arg) (I.SomeTypeRep res)
  | Just HRefl <- I.typeRepKind arg `I.eqTypeRep` liftedTy
  , Just HRefl <- I.typeRepKind res `I.eqTypeRep` liftedTy
  = I.SomeTypeRep (I.Fun arg res)
  | otherwise
  = error $ "mkFunTy: Attempted to construct function type from non-lifted "++
            "type: arg="++show arg++", res="++show res
  where liftedTy = I.typeRep :: I.TypeRep Type

-- | Splits a type constructor application. Note that if the type constructor is
-- polymorphic, this will not return the kinds that were used.
splitTyConApp :: TypeRep -> (TyCon, [TypeRep])
splitTyConApp (I.SomeTypeRep x) = I.splitApps x

-- | Observe the argument types of a type representation
typeRepArgs :: TypeRep -> [TypeRep]
typeRepArgs ty = case splitTyConApp ty of (_, args) -> args

-- | Observe the type constructor of a quantified type representation.
typeRepTyCon :: TypeRep -> TyCon
typeRepTyCon = I.someTypeRepTyCon

-- | Takes a value of type @a@ and returns a concrete representation
-- of that type.
--
-- @since 4.7.0.0
typeRepFingerprint :: TypeRep -> Fingerprint
typeRepFingerprint = I.someTypeRepFingerprint

-- | Force a 'TypeRep' to normal form.
rnfTypeRep :: TypeRep -> ()
rnfTypeRep = I.rnfSomeTypeRep


-- Keeping backwards-compatibility
typeOf1 :: forall t (a :: Type). Typeable t => t a -> TypeRep
typeOf1 _ = I.someTypeRep (Proxy :: Proxy t)

typeOf2 :: forall t (a :: Type) (b :: Type). Typeable t => t a b -> TypeRep
typeOf2 _ = I.someTypeRep (Proxy :: Proxy t)

typeOf3 :: forall t (a :: Type) (b :: Type) (c :: Type).
           Typeable t => t a b c -> TypeRep
typeOf3 _ = I.someTypeRep (Proxy :: Proxy t)

typeOf4 :: forall t (a :: Type) (b :: Type) (c :: Type) (d :: Type).
           Typeable t => t a b c d -> TypeRep
typeOf4 _ = I.someTypeRep (Proxy :: Proxy t)

typeOf5 :: forall t (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type).
           Typeable t => t a b c d e -> TypeRep
typeOf5 _ = I.someTypeRep (Proxy :: Proxy t)

typeOf6 :: forall t (a :: Type) (b :: Type) (c :: Type)
                    (d :: Type) (e :: Type) (f :: Type).
           Typeable t => t a b c d e f -> TypeRep
typeOf6 _ = I.someTypeRep (Proxy :: Proxy t)

typeOf7 :: forall t (a :: Type) (b :: Type) (c :: Type)
                    (d :: Type) (e :: Type) (f :: Type) (g :: Type).
           Typeable t => t a b c d e f g -> TypeRep
typeOf7 _ = I.someTypeRep (Proxy :: Proxy t)
