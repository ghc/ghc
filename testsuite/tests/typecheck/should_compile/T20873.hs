
{-# LANGUAGE GADTSyntax, StandaloneKindSignatures, NoDataKinds #-}

module T20873 where

import Data.Kind ( Type )

type U a = Type
type V a = a

type MyMaybe1 :: U Type -> U Type
data MyMaybe1 a = MyJust1 a | MyNothing1

type MyMaybe2 :: V Type -> V Type
data MyMaybe2 a = MyJust2 a | MyNothing2

data MyMaybe3 (a :: U Type) :: U Type where
  MyJust3    :: a -> MyMaybe3 a
  MyNothing3 ::      MyMaybe3 a

data MyMaybe4 (a :: V Type) :: V Type where
  MyJust4    :: a -> MyMaybe4 a
  MyNothing4 ::      MyMaybe4 a
