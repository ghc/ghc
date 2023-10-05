{-# LANGUAGE DerivingVia, StandaloneDeriving #-}

module T21871 where

import Control.Monad.Trans.Reader
  ( ReaderT(ReaderT) )
import Data.Kind
  ( Type )

newtype FooT m a = FooT
  { runFooT :: Int -> m a
  }
  deriving Functor via ReaderT Int m
