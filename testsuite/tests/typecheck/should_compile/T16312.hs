{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T16312 where

newtype Curried g h a =
  Curried { runCurried :: forall r. g (a -> r) -> h r }

instance Functor g => Functor (Curried g h) where
  fmap f (Curried g) = Curried (g . fmap (.f))

instance (Functor g, g ~ h) => Applicative (Curried g h) where
  pure a = Curried (fmap ($a))
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}
