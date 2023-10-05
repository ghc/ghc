{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

module T23143 where

import Data.Coerce

newtype A a = MkA a

class Pointed a where
  point :: a

class (forall a. Pointed a => Pointed (t a)) => T t where
  points :: Pointed a => t a

instance Pointed a => Pointed (A a) where
  point = MkA point

instance T A where
  points = point

newtype B a = MkB (A a)
  deriving newtype (Pointed, T)

newtype C a = MkC (A a)

instance Pointed a => Pointed (C a) where
  point :: C a
  point = coerce @(A a) @(C a) (point @(A a))

instance T C where
  points :: forall a. Pointed a => C a
  points = coerce @(A a) @(C a) (points @A)
