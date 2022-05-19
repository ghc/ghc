{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

import Data.Kind (Type)

class (Monad m, MonadFoo (FooM m)) => MonadFoo m where
  type FooM m :: Type -> Type
  runFoo :: FooM m a -> m a

newtype MyMonad m a = MyMonad { runMyMonad :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadFoo (MyMonad m) where
  type FooM (MyMonad m) = MyMonad m
  runFoo = id

main :: IO ()
main = runMyMonad foo

foo :: MonadFoo m => m ()
foo = runFoo $ return ()
