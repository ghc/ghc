{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}

module T21719 where

import Control.Exception

data Foo = Foo
  deriving (Show, Exception)

class CanThrow e

qux :: Monad m => (CanThrow Foo => m a) -> m a
qux _ = undefined

class Monad m => MonadCheckedThrow m where
    throwChecked :: (Exception e, CanThrow e) => e -> m a

foo :: MonadCheckedThrow m => m Int
foo = do
  qux do
    _ <- baz
    pure 5
  where
    baz :: (CanThrow Foo, _) => _
    baz = throwChecked Foo

