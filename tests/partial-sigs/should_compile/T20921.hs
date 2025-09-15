{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PsigBug where

import Data.Kind
  ( Constraint )
import GHC.TypeLits
  ( ErrorMessage(..), TypeError )
import GHC.TypeNats ( Nat )

type family OK (i :: Nat) :: Constraint where
  OK 1 = ()
  OK 2 = ()
  OK n = TypeError (ShowType n :<>: Text "is not OK")

class C (i :: Nat) where
  foo :: Int

instance C 1 where
  foo = 1
instance C 2 where
  foo = 2

type family Boo (i :: Nat) :: Nat where
  Boo i = i

bar :: Int
bar =
  let
    quux :: forall (i :: Nat). ( OK (Boo i), _ ) => Int
    quux = foo @(Boo i)
  in quux @1 + quux @2

{-
From RHS of quux
  [W] C (Boo i)
  [W] OK (Boo i)  -- Note [Add signature contexts as wanteds]

Simplifies to
  [W] C i, OK i

Add back in OK (Boo i) (in decideQuantification),
and mkMinimalBySCs (which does not eliminate (OK i)
  (OK (Boo i), OK i, C i)

-}
