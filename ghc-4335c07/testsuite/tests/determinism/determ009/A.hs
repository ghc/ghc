module A where

newtype Pair1 f g a = Pair1 {unPair1 :: (f a, g a)}
  deriving Eq
