{-# LANGUAGE UnicodeSyntax, RankNTypes, TypeFamilies #-}
module T5595 where

class MonadTransControl t where
  type St t :: * → *

  liftControl :: Monad m =>(Run t → m a) → t m a

  restore :: Monad o => St t y → t o y

type Run t = forall n b. Monad n => t n b → n (St t b)

foo :: (Monad m, MonadTransControl t) => (Run t -> m a) -> t m a
foo f = liftControl f
