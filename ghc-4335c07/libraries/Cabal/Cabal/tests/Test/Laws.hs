{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Laws where

import Prelude hiding (Num((+), (*)))
import Data.Monoid (Monoid(..), Endo(..))
import qualified Data.Foldable as Foldable

idempotent_unary  f x = f fx == fx where fx = f x

-- Basic laws on binary operators

idempotent_binary (+) x = x + x == x

commutative (+) x y = x + y == y + x

associative (+) x y z = (x + y) + z ==  x + (y + z)

distributive_left  (*) (+) x y z = x * (y + z) == (x * y) + (x * z)

distributive_right (*) (+) x y z = (y + z) * x == (y * x) + (z * x)


-- | The first 'fmap' law
--
-- > fmap id  ==  id
--
fmap_1 :: (Eq (f a), Functor f) => f a -> Bool
fmap_1 x = fmap id x == x

-- | The second 'fmap' law
--
-- > fmap (f . g)  ==  fmap f . fmap g
--
fmap_2 :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
fmap_2 f g x = fmap (f . g) x == (fmap f . fmap g) x


-- | The monoid identity law, 'mempty' is a left and right identity of
-- 'mappend':
--
-- > mempty `mappend` x = x
-- > x `mappend` mempty = x
--
monoid_1 :: (Eq a, Data.Monoid.Monoid a) => a -> Bool
monoid_1 x = mempty `mappend` x == x
          && x `mappend` mempty == x

-- | The monoid associativity law, 'mappend' must be associative.
--
-- > (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
--
monoid_2 :: (Eq a, Data.Monoid.Monoid a) => a -> a -> a -> Bool
monoid_2 x y z = (x `mappend`  y) `mappend` z
              ==  x `mappend` (y  `mappend` z)

-- | The 'mconcat' definition. It can be overidden for the sake of effeciency
-- but it must still satisfy the property given by the default definition:
--
-- > mconcat = foldr mappend mempty
--
monoid_3 :: (Eq a, Data.Monoid.Monoid a) => [a] -> Bool
monoid_3 xs = mconcat xs == foldr mappend mempty xs


-- | First 'Foldable' law
--
-- > Foldable.fold = Foldable.foldr mappend mempty
--
foldable_1 :: (Foldable.Foldable t, Monoid m, Eq m) => t m -> Bool
foldable_1 x = Foldable.fold x == Foldable.foldr mappend mempty x

-- | Second 'Foldable' law
--
-- > foldr f z t = appEndo (foldMap (Endo . f) t) z
--
foldable_2 :: (Foldable.Foldable t, Eq b)
               => (a -> b -> b) -> b -> t a -> Bool
foldable_2 f z t = Foldable.foldr f z t
                    == appEndo (Foldable.foldMap (Endo . f) t) z
