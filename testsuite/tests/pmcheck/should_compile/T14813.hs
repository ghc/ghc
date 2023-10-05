{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
module T14813 where

import Data.Kind
import Data.Void

data SBool (z :: Bool) where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

type family F (b :: Bool) (a :: Type) :: Type where
  F 'True  a = a
  F 'False _ = Void

dispatch :: forall (b :: Bool) (a :: Type). SBool b -> F b a -> a
dispatch STrue  x = x
dispatch SFalse x = case x of {}

type family G a
type instance G Int = Void

fun :: i ~ Int => G i -> a
fun x = case x of {}
