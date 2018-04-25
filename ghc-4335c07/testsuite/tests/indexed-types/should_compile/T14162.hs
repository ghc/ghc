{-# Language TypeOperators, KindSignatures, DataKinds, PolyKinds, TypeFamilies, GADTs, TypeInType #-}

module T14162 where

import Data.Kind

data SubList (a :: Maybe w) :: Type where
  SubNil :: SubList 'Nothing

data family Sing (a :: k)

data instance Sing (x :: SubList ys) where
  SSubNil :: Sing SubNil

{-
SubList :: forall (w::*). Maybe w -> *
SubNil  :: forall (w::*). SubList w (Nothing w)

wrkSubNil :: forall (w::*) (a :: Maybe w).
             (a ~ Nothing w) =>
             SubList w a

Sing :: forall k. k -> *

RepTc :: forall (w_aSy : *)
                (ys_aRW :: Maybe w_aSy)
                (x_aRX :: SubList w_aSy ys_aRW).
                *

axiom forall (w : *) (ys : Maybe w) (x : SubList ys).
   Sing (SubList ys) (x : SubList ys) ~ RepTc w ys x

data RepTc w ys x where
  SSubNil :: RepTc w (Nothing w) (SubNil w)

SSubNil :: forall (w :: *). Sing (SubList w (Nothing w)) (SubNil w)

wrkSSubMil :: forall (w : *) (ys : Maybe w) (x : Sublist w ys).
              () =>
              RepTc w ys x
-}
