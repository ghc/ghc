{-# LANGUAGE DeriveFunctor, TypeApplications #-}

module T13819 where

import Data.Coerce
import Control.Applicative

newtype A a = A (IO a)
  deriving Functor

instance Applicative A where
  pure = pure @(_ -> WrappedMonad A _) @(_ -> A _) pure

instance Monad A where
