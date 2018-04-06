{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14040a where

import Data.Kind

data family Sing (a :: k)

data WeirdList :: Type -> Type where
  WeirdNil  :: WeirdList a
  WeirdCons :: a -> WeirdList (WeirdList a) -> WeirdList a

data instance Sing (z :: WeirdList a) where
  SWeirdNil  :: Sing WeirdNil
  SWeirdCons :: Sing w -> Sing wws -> Sing (WeirdCons w wws)

elimWeirdList :: forall (a :: Type) (wl :: WeirdList a)
                        (p :: forall (x :: Type). x -> WeirdList x -> Type).
                 Sing wl
              -> (forall (y :: Type). p _ WeirdNil)
              -> (forall (z :: Type) (x :: z) (xs :: WeirdList (WeirdList z)).
                    Sing x -> Sing xs -> p _ xs
                  -> p _ (WeirdCons x xs))
              -> p _ wl
elimWeirdList SWeirdNil pWeirdNil _ = pWeirdNil
elimWeirdList (SWeirdCons (x :: Sing (x :: z))
                          (xs :: Sing (xs :: WeirdList (WeirdList z))))
              pWeirdNil pWeirdCons
  = pWeirdCons @z @x @xs x xs
      (elimWeirdList @(WeirdList z) @xs @p xs pWeirdNil pWeirdCons)
