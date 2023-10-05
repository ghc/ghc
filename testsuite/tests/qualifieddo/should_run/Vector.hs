{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vector
  ( Vector(..)
  , toList
  , vAppend
    -- exported for QualifiedDo
  , fmap
  , (<*>)
  , pure
  , fail
  , mfix
  ) where

import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Monad.Graded
import Prelude hiding ((>>=), fail, pure, return, (<*>))


data Nat = Zero | Succ Nat

data Vector n a where
  VNil :: Vector Zero a
  VCons :: a -> Vector n a -> Vector (Succ n) a

instance Functor (Vector n) where
  fmap f = \case
    VNil -> VNil
    VCons a v -> VCons (f a) (fmap f v)

vAppend :: Vector m a -> Vector n a -> Vector (Add m n) a
vAppend VNil v = v
vAppend (VCons a u) v = VCons a (vAppend u v)

toList :: Vector n a -> [a]
toList = \case
  VNil -> []
  VCons a v -> a : toList v

fail :: String -> Vector n a
fail = error

class VRepeat n where
  vRepeat :: a -> Vector n a
instance VRepeat Zero where
  vRepeat _ = VNil
instance VRepeat n => VRepeat (Succ n) where
  vRepeat a = VCons a (vRepeat a)

type family Add m n :: Nat where
  Add Zero n = n
  Add (Succ m) n = Succ (Add m n)

type family Times m n :: Nat where
  Times Zero n = Zero
  Times (Succ m) n = Add n (Times m n)

instance GradedMonad Vector where
  type Unit Vector = Succ Zero
  type Plus Vector i j = Times i j
  type Inv  Vector i j = ()
  v >>= f = case v of
    VNil -> VNil
    VCons a v -> vAppend (f a) (v >>= f)
  return a = VCons a VNil

vHead :: Vector (Succ n) a -> a
vHead (VCons a _) = a

vTail :: Vector (Succ n) a -> Vector n a
vTail (VCons _ v) = v

mfix :: forall a n. Show a => (a -> Vector n a) -> Vector n a
mfix f = case fix (f . unsafeHead) of
    VNil -> VNil
    VCons x _ -> VCons x (mfix (vTail . f))
  where
    unsafeHead :: Vector n a -> a
    unsafeHead = \case
      VNil -> error "VNil"
      VCons a _ -> a

pure :: a -> Vector (Succ Zero) a
pure = return

(<*>) :: Vector m (a -> b) -> Vector n a -> Vector (Times m n) b
VNil <*> _ = VNil
VCons _ v <*> VNil = v <*> VNil
VCons f vf <*> v = vAppend (fmap f v) (vf <*> v)
