{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module T21689a
  ( Arity
  , Dim
  , DimM
  , IVector
  , Mutable
  , MVector
  , Vector(..)
  , constructVec
  , inspectVec
  , gfoldl'
  , gunfold'
  ) where

import Control.Monad.ST (ST, runST)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat, type (+), type (-))

-----
-- Data.Vector.Fixed.Cont
-----

data PeanoNum = Z
              | S PeanoNum

type family Peano (n :: Nat) :: PeanoNum where
  Peano 0 = 'Z
  Peano n = 'S (Peano (n - 1))

type family Fn (n :: PeanoNum) (a :: Type) (b :: Type) where
  Fn 'Z     a b = b
  Fn ('S n) a b = a -> Fn n a b

newtype Fun n a b = Fun { unFun :: Fn n a b }

type family Dim (v :: Type -> Type) :: Nat

class Arity (Dim v) => Vector v a where
  construct :: Fun (Peano (Dim v)) a (v a)

  inspect   :: v a -> Fun (Peano (Dim v)) a b -> b

type Arity n = ( ArityPeano (Peano n)
               , KnownNat n
               , Peano (n+1) ~ 'S (Peano n)
               )

class ArityPeano n where
  accum :: (forall k. t ('S k) -> a -> t k)
        -> (t 'Z -> b)
        -> t n
        -> Fun n a b

  applyFun :: (forall k. t ('S k) -> (a, t k))
           -> t n
           -> (CVecPeano n a, t 'Z)

  gunfoldF :: (Data a)
           => (forall b x. Data b => c (b -> x) -> c x)
           -> T_gunfold c r a n -> c r

newtype T_gunfold c r a n = T_gunfold (c (Fn n a r))

gfoldl' :: forall c v a. (Vector v a, Data a)
        => (forall x y. Data x => c (x -> y) -> x -> c y)
        -> (forall x  . x -> c x)
        -> v a -> c (v a)
gfoldl' f inj v
  = inspect v
  $ gfoldlF f (inj $ unFun (construct :: Fun (Peano (Dim v)) a (v a)))

gunfold' :: forall con c v a. (Vector v a, Data a)
         => (forall b r. Data b => c (b -> r) -> c r)
         -> (forall r. r -> c r)
         -> con -> c (v a)
gunfold' f inj _
  = gunfoldF f gun
  where
    con = construct                   :: Fun (Peano (Dim v)) a (v a)
    gun = T_gunfold (inj $ unFun con) :: T_gunfold c (v a) a (Peano (Dim v))

gfoldlF :: (ArityPeano n, Data a)
        => (forall x y. Data x => c (x -> y) -> x -> c y)
        -> c (Fn n a r) -> Fun n a (c r)
gfoldlF f c0 = accum
  (\(T_gfoldl c) x -> T_gfoldl (f c x))
  (\(T_gfoldl c)   -> c)
  (T_gfoldl   c0)

newtype T_gfoldl c r a n = T_gfoldl (c (Fn n a r))

newtype ContVec n a = ContVec (forall r. Fun (Peano n) a r -> r)

type instance Dim (ContVec n) = n

instance Arity n => Vector (ContVec n) a where
  construct = accum
    (\(T_mkN f) a -> T_mkN (f . consPeano a))
    (\(T_mkN f)   -> toContVec $ f (CVecPeano unFun))
    (T_mkN id)
  inspect (ContVec c) f = c f
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

newtype T_mkN n_tot a n = T_mkN (CVecPeano n a -> CVecPeano n_tot a)

toContVec :: CVecPeano (Peano n) a -> ContVec n a
toContVec = coerce

newtype CVecPeano n a = CVecPeano (forall r. Fun n a r -> r)

consPeano :: a -> CVecPeano n a -> CVecPeano ('S n) a
consPeano a (CVecPeano cont) = CVecPeano $ \f -> cont $ curryFirst f a
{-# INLINE consPeano #-}

curryFirst :: Fun ('S n) a b -> a -> Fun n a b
curryFirst = coerce
{-# INLINE curryFirst #-}

apply :: Arity n
      => (forall k. t ('S k) -> (a, t k))
      -> t (Peano n)
      -> ContVec n a
{-# INLINE apply #-}
apply step' z = toContVec $ fst (applyFun step' z)

-----
-- Data.Vector.Fixed.Mutable
-----

type family Mutable (v :: Type -> Type) :: Type -> Type -> Type

type family DimM (v :: Type -> Type -> Type) :: Nat

class (Arity (DimM v)) => MVector v a where
  new   :: PrimMonad m => m (v (PrimState m) a)

  unsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

class (Dim v ~ DimM (Mutable v), MVector (Mutable v) a) => IVector v a where
  unsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)

  unsafeIndex :: v a -> Int -> a

inspectVec :: forall v a b. (Arity (Dim v), IVector v a) => v a -> Fun (Peano (Dim v)) a b -> b
{-# INLINE inspectVec #-}
inspectVec v
  = inspect cv
  where
    cv :: ContVec (Dim v) a
    cv = apply (\(Const i) -> (unsafeIndex v i, Const (i+1)))
               (Const 0 :: Const Int (Peano (Dim v)))

constructVec :: forall v a. (Arity (Dim v), IVector v a) => Fun (Peano (Dim v)) a (v a)
{-# INLINE constructVec #-}
constructVec =
  accum step
        (\(T_new _ st) -> runST $ unsafeFreeze =<< st :: v a)
        (T_new 0 new :: T_new v a (Peano (Dim v)))

data T_new v a n = T_new Int (forall s. ST s (Mutable v s a))

step :: (IVector v a) => T_new v a ('S n) -> a -> T_new v a n
step (T_new i st) x = T_new (i+1) $ do
  mv <- st
  unsafeWrite mv i x
  return mv

-----
-- Control.Monad.Primitive
-----

class Monad m => PrimMonad m where
  type PrimState m

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
