{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module DerivingViaCompile where

import Data.Void
import Data.Complex
import Data.Functor.Const
import Data.Functor.Identity
import Data.Ratio
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative hiding (WrappedMonad(..))

import Data.Bifunctor
import Data.Monoid
import Data.Kind

type f ~> g = forall xx. f xx -> g xx

-----
-- Simple example
-----

data Foo a = MkFoo a a
  deriving Show
       via (Identity (Foo a))

-----
-- Eta reduction at work
-----

newtype Flip p a b = Flip { runFlip :: p b a }

instance Bifunctor p => Bifunctor (Flip p) where
  bimap f g = Flip . bimap g f . runFlip

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip

newtype Bar a = MkBar (Either a Int)
  deriving Functor
       via (Flip Either Int)

-----
-- Monad transformers
-----

type MTrans = (Type -> Type) -> (Type -> Type)

-- From `constraints'
data Dict c where
  Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)

infixl 1 \\
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

-- With `-XQuantifiedConstraints' this just becomes
--
--    type Lifting cls  trans = forall mm. cls mm => cls (trans mm)
--
--    type LiftingMonad trans = Lifting Monad trans
--
class LiftingMonad (trans :: MTrans) where
  proof :: Monad m :- Monad (trans m)

instance LiftingMonad (StateT s :: MTrans) where
  proof :: Monad m :- Monad (StateT s m)
  proof = Sub Dict

instance Monoid w => LiftingMonad (WriterT w :: MTrans) where
  proof :: Monad m :- Monad (WriterT w m)
  proof = Sub Dict

instance (LiftingMonad trans, LiftingMonad trans') => LiftingMonad (ComposeT trans trans' :: MTrans) where
  proof :: forall m. Monad m :- Monad (ComposeT trans trans' m)
  proof = Sub (Dict \\ proof @trans @(trans' m) \\ proof @trans' @m)

newtype Stack :: MTrans where
  Stack :: ReaderT Int (StateT Bool (WriterT String m)) a -> Stack m a
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Int
    , MonadState Bool
    , MonadWriter String
    )
  deriving (MonadTrans, MFunctor)
       via (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String)

class MFunctor (trans :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (trans m ~> trans m')

instance MFunctor (ReaderT r :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (ReaderT r m ~> ReaderT r m')
  hoist nat = ReaderT . fmap nat . runReaderT

instance MFunctor (StateT s :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (StateT s m ~> StateT s m')
  hoist nat = StateT . fmap nat . runStateT

instance MFunctor (WriterT w :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (WriterT w m ~> WriterT w m')
  hoist nat = WriterT . nat . runWriterT

infixr 9 `ComposeT`
newtype ComposeT :: MTrans -> MTrans -> MTrans where
  ComposeT :: { getComposeT :: f (g m) a } -> ComposeT f g m a
  deriving newtype (Functor, Applicative, Monad)

instance (MonadTrans f, MonadTrans g, LiftingMonad g) => MonadTrans (ComposeT f g) where
  lift :: forall m. Monad m => m ~> ComposeT f g m
  lift = ComposeT . lift . lift
    \\ proof @g @m

instance (MFunctor f, MFunctor g, LiftingMonad g) => MFunctor (ComposeT f g) where
  hoist :: forall m m'. Monad m => (m ~> m') -> (ComposeT f g m ~> ComposeT f g m')
  hoist f = ComposeT . hoist (hoist f) . getComposeT
    \\ proof @g @m

-----
-- Using tuples in a `via` type
-----

newtype X a = X (a, a)
  deriving (Semigroup, Monoid)
       via (Product a, Sum a)

  deriving (Show, Eq)
       via (a, a)

-----
-- Abstract data types
-----

class C f where
  c :: f a -> Int

newtype X2 f a = X2 (f a)

instance C (X2 f) where
  c = const 0

deriving via (X2 IO) instance C IO

----
-- Testing parser
----

newtype P0 a = P0 a             deriving Show via a
newtype P1 a = P1 [a]           deriving Show via [a]
newtype P2 a = P2 (a, a)        deriving Show via (a, a)
newtype P3 a = P3 (Maybe a)     deriving Show via (First a)
newtype P4 a = P4 (Maybe a)     deriving Show via (First $ a)
newtype P5 a = P5 a             deriving Show via (Identity $ a)
newtype P6 a = P6 [a]           deriving Show via ([] $ a)
newtype P7 a = P7 (a, a)        deriving Show via (Identity $ (a, a))
newtype P8 a = P8 (Either () a) deriving Functor via (($) (Either ()))

newtype f $ a = APP (f a) deriving newtype Show deriving newtype Functor

----
-- From Baldur's notes
----

----
-- 1
----
newtype WrapApplicative f a = WrappedApplicative (f a)
  deriving (Functor, Applicative)

instance (Applicative f, Num a) => Num (WrapApplicative f a) where
  (+)         = liftA2 (+)
  (*)         = liftA2 (*)
  negate      = fmap negate
  fromInteger = pure . fromInteger
  abs         = fmap abs
  signum      = fmap signum

instance (Applicative f, Fractional a) => Fractional (WrapApplicative f a) where
  recip        = fmap recip
  fromRational = pure . fromRational

instance (Applicative f, Floating a) => Floating (WrapApplicative f a) where
  pi    = pure pi
  sqrt  = fmap sqrt
  exp   = fmap exp
  log   = fmap log
  sin   = fmap sin
  cos   = fmap cos
  asin  = fmap asin
  atan  = fmap atan
  acos  = fmap acos
  sinh  = fmap sinh
  cosh  = fmap cosh
  asinh = fmap asinh
  atanh = fmap atanh
  acosh = fmap acosh

instance (Applicative f, Semigroup s) => Semigroup (WrapApplicative f s) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid m) => Monoid (WrapApplicative f m) where
  mempty = pure mempty

----
-- 2
----
class Pointed p where
  pointed :: a -> p a

newtype WrapMonad f a = WrappedMonad (f a)
  deriving newtype (Pointed, Monad)

instance (Monad m, Pointed m) => Functor (WrapMonad m) where
  fmap = liftM

instance (Monad m, Pointed m) => Applicative (WrapMonad m) where
  pure  = pointed
  (<*>) = ap

-- data
data Sorted a = Sorted a a a
  deriving (Functor, Applicative)
    via (WrapMonad Sorted)
  deriving (Num, Fractional, Floating, Semigroup, Monoid)
    via (WrapApplicative Sorted a)


instance Monad Sorted where
  (>>=) :: Sorted a -> (a -> Sorted b) -> Sorted b
  Sorted a b c >>= f = Sorted a' b' c' where
    Sorted a' _  _  = f a
    Sorted _  b' _  = f b
    Sorted _  _  c' = f c

instance Pointed Sorted where
  pointed :: a -> Sorted a
  pointed a = Sorted a a a

----
-- 3
----
class IsZero a where
  isZero :: a -> Bool

newtype WrappedNumEq  a = WrappedNumEq a
newtype WrappedShow   a = WrappedShow  a
newtype WrappedNumEq2 a = WrappedNumEq2 a

instance (Num a, Eq a) => IsZero (WrappedNumEq a) where
  isZero :: WrappedNumEq a -> Bool
  isZero (WrappedNumEq a) = 0 == a

instance Show a => IsZero (WrappedShow a) where
  isZero :: WrappedShow a -> Bool
  isZero (WrappedShow a) = "0" == show a

instance (Num a, Eq a) => IsZero (WrappedNumEq2 a) where
  isZero :: WrappedNumEq2 a -> Bool
  isZero (WrappedNumEq2 a) = a + a == a

newtype INT = INT Int
  deriving newtype Show
  deriving IsZero via (WrappedNumEq Int)

newtype VOID = VOID Void deriving IsZero via (WrappedShow Void)

----
-- 4
----
class Bifunctor p => Biapplicative p where
  bipure :: a -> b -> p a b

  biliftA2
    :: (a  -> b  -> c)
    -> (a' -> b' -> c')
    -> p a a'
    -> p b b'
    -> p c c'

instance Biapplicative (,) where
  bipure = (,)

  biliftA2 f f' (a, a') (b, b') =
    (f a b, f' a' b')

newtype WrapBiapp p a b = WrapBiap (p a b)
  deriving newtype (Bifunctor, Biapplicative, Eq)

instance (Biapplicative p, Num a, Num b) => Num (WrapBiapp p a b) where
  (+) = biliftA2 (+) (+)
  (-) = biliftA2 (*) (*)
  (*) = biliftA2 (*) (*)
  negate = bimap negate negate
  abs = bimap abs abs
  signum = bimap signum signum
  fromInteger n = fromInteger n `bipure` fromInteger n

newtype INT2 = INT2 (Int, Int)
  deriving IsZero via (WrappedNumEq2 (WrapBiapp (,) Int Int))

----
-- 5
----
class Monoid a => MonoidNull a where
  null :: a -> Bool

newtype WrpMonNull a = WRM a deriving (Eq, Semigroup, Monoid)

instance (Eq a, Monoid a) => MonoidNull (WrpMonNull a) where
  null :: WrpMonNull a -> Bool
  null = (== mempty)

deriving via (WrpMonNull Any) instance MonoidNull Any
deriving via ()               instance MonoidNull ()
deriving via Ordering         instance MonoidNull Ordering

----
-- 6
----
-- https://github.com/mikeizbicki/subhask/blob/f53fd8f465747681c88276c7dabe3646fbdf7d50/src/SubHask/Algebra.hs#L635

class Lattice a where
  sup   :: a -> a -> a
  (.>=) :: a -> a -> Bool
  (.>)  :: a -> a -> Bool

newtype WrapOrd a = WrappedOrd a
  deriving newtype (Eq, Ord)

instance Ord a => Lattice (WrapOrd a) where
  sup   = max
  (.>=) = (>=)
  (.>)  = (>)

deriving via [a]    instance Ord a          => Lattice [a]
deriving via (a, b) instance (Ord a, Ord b) => Lattice (a, b)
--mkLattice_(Bool)
deriving via Bool instance Lattice Bool
--mkLattice_(Char)
deriving via Char instance Lattice Char
--mkLattice_(Int)
deriving via Int instance Lattice Int
--mkLattice_(Integer)
deriving via Integer instance Lattice Integer
--mkLattice_(Float)
deriving via Float instance Lattice Float
--mkLattice_(Double)
deriving via Double instance Lattice Double
--mkLattice_(Rational)
deriving via Rational instance Lattice Rational

----
-- 7
----
-- https://hackage.haskell.org/package/linear-1.20.7/docs/src/Linear-Affine.html

class Functor f => Additive f where
  zero :: Num a => f a
  (^+^) :: Num a => f a -> f a -> f a
  (^+^) = liftU2 (+)
  (^-^) :: Num a => f a -> f a -> f a
  x ^-^ y = x ^+^ fmap negate y
  liftU2 :: (a -> a -> a) -> f a -> f a -> f a

instance Additive [] where
  zero = []
  liftU2 f = go where
    go (x:xs) (y:ys) = f x y : go xs ys
    go [] ys = ys
    go xs [] = xs

instance Additive Maybe where
  zero = Nothing
  liftU2 f (Just a) (Just b) = Just (f a b)
  liftU2 _ Nothing ys = ys
  liftU2 _ xs Nothing = xs

instance Applicative f => Additive (WrapApplicative f) where
  zero   = pure 0
  liftU2 = liftA2

deriving via (WrapApplicative ((->) a)) instance Additive ((->) a)
deriving via (WrapApplicative Complex)  instance Additive Complex
deriving via (WrapApplicative Identity) instance Additive Identity

instance Additive ZipList where
  zero = ZipList []
  liftU2 f (ZipList xs) (ZipList ys) = ZipList (liftU2 f xs ys)

class Additive (Diff p) => Affine p where
  type Diff p :: Type -> Type

  (.-.) :: Num a => p a -> p a -> Diff p a
  (.+^) :: Num a => p a -> Diff p a -> p a
  (.-^) :: Num a => p a -> Diff p a -> p a
  p .-^ v = p .+^ fmap negate v

-- #define ADDITIVEC(CTX,T) instance CTX => Affine T where type Diff T = T ; \
--   (.-.) = (^-^) ; {-# INLINE (.-.) #-} ; (.+^) = (^+^) ; {-# INLINE (.+^) #-} ; \
--   (.-^) = (^-^) ; {-# INLINE (.-^) #-}
-- #define ADDITIVE(T) ADDITIVEC((), T)
newtype WrapAdditive f a = WrappedAdditive (f a)

instance Additive f => Affine (WrapAdditive f) where
  type Diff (WrapAdditive f) = f

  WrappedAdditive a .-. WrappedAdditive b = a ^-^ b
  WrappedAdditive a .+^ b = WrappedAdditive (a ^+^ b)
  WrappedAdditive a .-^ b = WrappedAdditive (a ^-^ b)

-- ADDITIVE(((->) a))
deriving via (WrapAdditive ((->) a)) instance Affine ((->) a)
-- ADDITIVE([])
deriving via (WrapAdditive [])       instance Affine []
-- ADDITIVE(Complex)
deriving via (WrapAdditive Complex)  instance Affine Complex
-- ADDITIVE(Maybe)
deriving via (WrapAdditive Maybe)    instance Affine Maybe
-- ADDITIVE(ZipList)
deriving via (WrapAdditive ZipList)  instance Affine ZipList
-- ADDITIVE(Identity)
deriving via (WrapAdditive Identity) instance Affine Identity

----
-- 8
----

class C2 a b c where
  c2 :: a -> b -> c

instance C2 a b (Const a b) where
  c2 x _ = Const x

newtype Fweemp a = Fweemp a
  deriving (C2 a b)
       via (Const a (b :: Type))
