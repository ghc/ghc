{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Function (on)

newtype Identity a = Identity a deriving (Eq, Functor)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a
instance Monad Identity where
  Identity a >>= f = f a

data ViewT m a
  = Empty
  | a :< SeqT m a
newtype SeqT m a = SeqT [m (ViewT m a)]

toViewT :: Monad m => SeqT m a -> m (ViewT m a)
toViewT (SeqT []) = pure Empty
toViewT (SeqT (h : t)) = h >>= \case
  Empty -> toViewT (SeqT t)
  hi :< SeqT ti -> pure (hi :< SeqT (ti ++ t))

instance (Eq (m (ViewT m a)), Monad m) => Eq (SeqT m a) where
  (==) = (==) `on` toViewT

deriving instance (Eq a, Eq (SeqT m a)) => Eq (ViewT m a)

example :: SeqT Identity Int
example = SeqT []

main :: IO ()
main = print (example == example)
