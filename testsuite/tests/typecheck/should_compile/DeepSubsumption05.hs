{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeepSubsumption #-}
module DeepSubsumption06 where

type Traversal' s a = forall f . Applicative f => (a -> a) -> f s -> f s
type LensLike f m a = (a -> a) -> f m -> f m

class Ixed m where
  ix :: () -> Traversal' m ()

  default ix :: (Applicative f) => () -> LensLike f m ()
  ix = undefined
