{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module DerivingViaCompile where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Bifunctor
import Data.Functor.Identity
import Data.Monoid

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

newtype Stack m a = Stack (ReaderT Int (StateT Bool (WriterT String m)) a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Int
    , MonadState Bool
    , MonadWriter String
    )
  deriving MonadTrans
       via (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String)

class MFunctor t where
    hoist :: Monad m => (forall a . m a -> n a) -> t m b -> t n b

instance MFunctor (ReaderT r) where
    hoist nat m = ReaderT (nat . runReaderT m)

instance MFunctor (StateT s) where
    hoist nat m = StateT (nat . runStateT m)

instance MFunctor (WriterT w) where
    hoist nat m = WriterT (nat (runWriterT m))

infixr 9 `ComposeT`
newtype ComposeT (f :: (* -> *) -> * -> *) (g :: (* -> *) -> * -> *) m a
  = ComposeT { getComposeT :: f (g m) a }

instance (MFunctor f, MonadTrans f, MonadTrans g) => MonadTrans (ComposeT f g) where
  lift = ComposeT . hoist lift . lift

-----
-- Using tuples in a `via` type
-----

newtype X a = X (a, a)
  deriving (Semigroup, Monoid)
       via (Product a, Sum a)
