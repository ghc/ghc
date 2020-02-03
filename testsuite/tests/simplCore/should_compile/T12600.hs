module T12600 where

-- We don't want to see any dictionary-passing in foo. Everything
-- should be inlined or specialized away.

class Eq1 f where
  eq1 :: Eq a => f a -> f a -> Bool

data F a = F !a !a
data G f a = G !(f a) !(f a)

instance Eq1 F where
  eq1 = \(F a b) (F c d) ->
    -- In order to reproduce the problem, the body of this function needs to be
    -- large enough to prevent GHC from voluntarily inlining it.
    larger $ larger $ larger $ larger $ larger $ larger $
      a == c && b == d
  {-# INLINE eq1 #-}

larger :: a -> a
larger = id
{-# NOINLINE larger #-}

instance (Eq1 f) => Eq1 (G f) where
  eq1 = \(G a b) (G c d) -> eq1 a c && eq1 b d
  {-# INLINE eq1 #-}

foo :: G F Int -> G F Int -> Bool
foo a b = eq1 a b
{-# NOINLINE foo #-}
