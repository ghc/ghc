{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
module T14270 (data App) where

import Data.Kind (Type)
import GHC.Fingerprint (Fingerprint, fingerprintFingerprints)
import GHC.Types (RuntimeRep, TYPE, TyCon)

type (:~~:) :: k1 -> k2 -> Type
data a :~~: b where
  HRefl :: a :~~: a

type TypeRep :: k -> Type
data TypeRep a where
    TrTyCon :: {-# UNPACK #-} !Fingerprint -> !TyCon -> [SomeTypeRep]
            -> TypeRep (a :: k)

    TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
               {-# UNPACK #-} !Fingerprint
            -> TypeRep (a :: k1 -> k2)
            -> TypeRep (b :: k1)
            -> TypeRep (a b)

    TrFun   :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                      (a :: TYPE r1) (b :: TYPE r2).
               {-# UNPACK #-} !Fingerprint
            -> TypeRep a
            -> TypeRep b
            -> TypeRep (a -> b)

data SomeTypeRep where
    SomeTypeRep :: forall k (a :: k). !(TypeRep a) -> SomeTypeRep

typeRepFingerprint :: TypeRep a -> Fingerprint
typeRepFingerprint = undefined

mkTrApp :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
           TypeRep (a :: k1 -> k2)
        -> TypeRep (b :: k1)
        -> TypeRep (a b)
mkTrApp rep@(TrApp _ (TrTyCon _ con _) (x :: TypeRep x)) (y :: TypeRep y)
  | con == funTyCon  -- cheap check first
  , Just (IsTYPE (rx :: TypeRep rx)) <- isTYPE (typeRepKind x)
  , Just (IsTYPE (ry :: TypeRep ry)) <- isTYPE (typeRepKind y)
  , Just HRefl <- withTypeable x $ withTypeable rx $ withTypeable ry
                  $ typeRep @((->) x :: TYPE ry -> Type) `eqTypeRep` rep
  = undefined
mkTrApp a b = TrApp fpr a b
  where
    fpr_a = typeRepFingerprint a
    fpr_b = typeRepFingerprint b
    fpr   = fingerprintFingerprints [fpr_a, fpr_b]

pattern App :: forall k2 (t :: k2). ()
            => forall k1 (a :: k1 -> k2) (b :: k1). (t ~ a b)
            => TypeRep a -> TypeRep b -> TypeRep t
pattern App f x <- (splitApp -> Just (IsApp f x))
  where App f x = mkTrApp f x

data IsApp (a :: k) where
    IsApp :: forall k k' (f :: k' -> k) (x :: k'). ()
          => TypeRep f -> TypeRep x -> IsApp (f x)

splitApp :: forall k (a :: k). ()
         => TypeRep a
         -> Maybe (IsApp a)
splitApp (TrApp _ f x)     = Just (IsApp f x)
splitApp rep@(TrFun _ a b) = Just (IsApp (mkTrApp arr a) b)
  where arr = bareArrow rep
splitApp (TrTyCon{})       = Nothing

withTypeable :: forall a r. TypeRep a -> (Typeable a => r) -> r
withTypeable _ _ = undefined

eqTypeRep :: forall k1 k2 (a :: k1) (b :: k2).
             TypeRep a -> TypeRep b -> Maybe (a :~~: b)
eqTypeRep = undefined

typeRepKind :: TypeRep (a :: k) -> TypeRep k
typeRepKind = undefined

bareArrow :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                    (a :: TYPE r1) (b :: TYPE r2). ()
          => TypeRep (a -> b)
          -> TypeRep ((->) :: TYPE r1 -> TYPE r2 -> Type)
bareArrow = undefined

data IsTYPE (a :: Type) where
    IsTYPE :: forall (r :: RuntimeRep). TypeRep r -> IsTYPE (TYPE r)

isTYPE :: TypeRep (a :: Type) -> Maybe (IsTYPE a)
isTYPE = undefined

class Typeable (a :: k) where

typeRep :: Typeable a => TypeRep a
typeRep = undefined

funTyCon :: TyCon
funTyCon = undefined

instance (Typeable f, Typeable a) => Typeable (f a)
instance Typeable ((->) :: TYPE r -> TYPE s -> Type)
instance Typeable TYPE
