{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

{- # OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -g -O2 #-}

module Bug (bug) where

-- import GHC.Base (seq)
import Unsafe.Coerce (unsafeCoerce)

undefined :: a
undefined = undefined

data TypeRep (a :: k) where
    TrTyCon :: TypeRep (a :: k)
    TrApp   :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
               TypeRep (a b)

data SomeTypeRep where
    SomeTypeRep :: forall k (a :: k).
                   TypeRep a
                -> SomeTypeRep

mkTrApp :: forall k1 k2 (a :: k1 -> k2) (b :: k1).
           TypeRep (a :: k1 -> k2)
        -> TypeRep (a b)
mkTrApp TrTyCon = undefined
mkTrApp TrApp   = undefined

bug :: SomeTypeRep
-- bug = f x -- this works
bug = f (f x)
  where x = SomeTypeRep TrTyCon
        f :: SomeTypeRep -> SomeTypeRep
        f (SomeTypeRep acc) = SomeTypeRep (mkTrApp (unsafeCoerce acc))
