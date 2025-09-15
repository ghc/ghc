{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module T11503 where

import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )
import GHC.TypeNats
  ( Nat, type (+), type (<=?) )
import Data.Kind
  ( Constraint, Type )

-- Example 1: from #11503

type NotInt :: Type -> Constraint
type family NotInt a where
  NotInt Int = TypeError (Text "That's Int, silly.")
  NotInt _   = (() :: Constraint)

data T a where
  MkT1 :: a -> T a
  MkT2 :: NotInt a => T a

foo :: T Int -> Int
foo (MkT1 x) = x
-- Should not have any pattern match warnings for MkT2.

-- Example 2: from #20180

type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True  _errMsg = ()
  Assert _check errMsg  = errMsg

type List :: Nat -> Type -> Type
data List n t where
  Nil  :: List 0 t
  (:-) :: t -> List n t -> List (n+1) t

type (<=) :: Nat -> Nat -> Constraint
type family x <= y where
  x <= y = Assert (x <=? y) (TypeError (Text "Impossible!"))

head' :: 1 <= n => List n t -> t
head' (x :- _) = x
-- Should not have any pattern match warnings for Nil.
