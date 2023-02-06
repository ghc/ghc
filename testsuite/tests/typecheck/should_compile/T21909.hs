{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T21909 where

import Data.Kind

class (Monad m, MyMonad (Inner m)) => MyMonad m where
  type Inner m :: Type -> Type
  foo :: m Int

works :: MyMonad m => m String
works = show <$> ((+ 1) <$> foo)

fails :: MyMonad m => m String
fails = show <$> fooPlusOne
  where
    fooPlusOne = (+ 1) <$> foo

alsoFails :: MyMonad m => m String
alsoFails =
  let fooPlusOne = (+ 1) <$> foo
   in show <$> fooPlusOne
