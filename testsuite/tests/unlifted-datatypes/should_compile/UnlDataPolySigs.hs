{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module UnlDataPolySigs where

import GHC.Exts
import GHC.Types

data U1 a :: TYPE (BoxedRep l) where
  MkU1 :: Int -> U1 a -- (MkU1 :: forall {k} (a :: k). Int -> U1 @{k} @'Lifted a)

type U2 :: Type -> TYPE (BoxedRep l)
data U2 a = MkU2 Int -- (MkU2 :: forall (l :: Levity) a. Int -> U2 @l a)

type U3 :: Type -> TYPE (BoxedRep l)
data U3 a where
  MkU3 :: Int -> U3 a -- (MkU3 :: forall a. Int -> U3 @'Lifted a)

type U4 :: Type -> TYPE (BoxedRep l)
data U4 a :: TYPE (BoxedRep l) where
  MkU4 :: Int -> U4 a -- (MkU4 :: forall a. Int -> U4 @'Lifted a)

data U5 a :: forall l. TYPE (BoxedRep l) where
  MkU5 :: Int -> U5 a -- (MkU5 :: forall {k} (a :: k). Int -> U5 @{k} @'Lifted a)

data U6 a :: forall l. TYPE (BoxedRep l) where
  MkU6 :: Int -> U6 a @l -- (MkU6 :: forall {k} (a :: k) (l :: Levity). Int -> U6 @{k} a @l)
