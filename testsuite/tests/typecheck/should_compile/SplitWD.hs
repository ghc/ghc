{-# LANGUAGE ScopedTypeVariables, TypeInType, TypeOperators,
             TypeFamilies, GADTs, StandaloneDeriving #-}

module SplitWD where

import Data.Kind ( Type )

data Nat = Zero | Succ Nat

type family a + b where
  Zero + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)
infixr 5 :>

type family (xs :: Vec a n) +++ (ys :: Vec a m) :: Vec a (n + m) where
  VNil +++ ys = ys
  (x :> xs) +++ ys = x :> (xs +++ ys)
infixr 5 +++

data Exp :: Vec Type n -> Type -> Type where
  Var :: Elem xs x -> Exp xs x

data Elem :: forall a n. Vec a n -> a -> Type where
  Here :: Elem (x :> xs) x
  There :: Elem xs x -> Elem (y :> xs) x

-- | @Length xs@ is a runtime witness for how long a vector @xs@ is.
-- @LZ :: Length xs@ says that @xs@ is empty.
-- @LS len :: Length xs@ tells you that @xs@ has one more element
-- than @len@ says.
-- A term of type @Length xs@ also serves as a proxy for @xs@.
data Length :: forall a n. Vec a n -> Type where
  LZ :: Length VNil
  LS :: Length xs -> Length (x :> xs)

deriving instance Show (Length xs)

-- | Convert an expression typed in one context to one typed in a larger
-- context. Operationally, this amounts to de Bruijn index shifting.
-- As a proposition, this is the weakening lemma.
shift :: forall ts2 t ty. Exp ts2 ty -> Exp (t :> ts2) ty
shift = go LZ
  where
    go :: forall ts1 ty. Length ts1 -> Exp (ts1 +++ ts2) ty -> Exp (ts1 +++ t :> ts2) ty
    go l_ts1 (Var v)          = Var (shift_elem l_ts1 v)

    shift_elem :: Length ts1 -> Elem (ts1 +++ ts2) x
               -> Elem (ts1 +++ t :> ts2) x
    shift_elem = undefined

