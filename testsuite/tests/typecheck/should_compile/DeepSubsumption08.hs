{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Haskell2010 #-}
module DeepSubsumption08 where

class (Traversable f, Applicative f) => Identical f where
  extract :: f a -> a

type LensLike f s t a b = (a -> f b) -> (s -> f t)
type Setter s t a b = forall f. Identical f => LensLike f s t a b

(.~) :: Setter s t a b -> b -> s -> t
(.~) a = undefined

-- | Sets the provided lens in @a@ to @Nothing@.
clear :: Setter a a' b (Maybe b') -> a -> a'
clear = (.~ Nothing)
