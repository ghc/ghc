{-# LANGUAGE NoPolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module DeepSubsumption06 where

class Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> p i a b -> p i s t

type Iso s t a b = forall p i . Profunctor p => p i a b -> p i s t

iso :: (s -> a) -> (b -> t) -> Iso s t a b
-- iso f g = dimap f g
iso = dimap
