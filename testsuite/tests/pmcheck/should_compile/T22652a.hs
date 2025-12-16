{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- This test is a spin-off from the test T15753c

module T22652a where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(..))
import Data.Void (Void)

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

data SUnit :: () -> Type where
  SUnit :: SUnit '()

type family IsUnit (u :: ()) :: Bool where
  IsUnit '() = True

type family If (c :: Bool) (t :: Type) (f :: Type) :: Type where
  If True  t _ = t
  If False _ f = f

type family F (u1 :: ()) (u2 :: ()) :: Type where
  F u1 u2 =
    If (IsUnit u1) (Case u2) Int

type family Case (u :: ()) :: Type where
  Case '() = Int

---------------------------------------
-- The checker can (now, Dec 25) see that (F u1 u2 ~ Char) is
-- unsatisfiable, so the empty pattern match is fine
g1a :: F u1 u2 :~: Char -> SUnit u1 -> SUnit u2 -> Void
g1a r _ _ = case r of {}

{- Why   [G] F u1 u2 ~ Char  is unsatisfiable

[G] F u1 u2 ~ Char =>rewrite   [G] If (IsUnit u1) (Case u2) Int ~ Char
                   =>(fundep)  [W] IsUnit u1 ~ True
                               [W] Case u2 ~ Char   <<--  insoluble: no relevant eqns
-}
