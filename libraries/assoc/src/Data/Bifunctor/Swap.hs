{-# LANGUAGE TypeFamilies #-}
module Data.Bifunctor.Swap (
    Swap (..),
    ) where

import Data.Bifunctor         (Bifunctor (..))
import Data.Bifunctor.Biff    (Biff (..))
import Data.Bifunctor.Flip    (Flip (..))
import Data.Bifunctor.Product (Product (..))
import Data.Bifunctor.Sum     (Sum (..))
import Data.Bifunctor.Tannen  (Tannen (..))

import qualified Data.Tuple

-- | Symmetric 'Bifunctor's.
--
-- @
-- 'swap' . 'swap' = 'id'
-- @
--
-- If @p@ is a 'Bifunctor' the following property is assumed to hold:
--
-- @
-- 'swap' . 'bimap' f g = 'bimap' g f . 'swap'
-- @
--
-- 'Swap' isn't a subclass of 'Bifunctor', as for example
--
-- >>> newtype Bipredicate a b = Bipredicate (a -> b -> Bool)
--
-- is not a 'Bifunctor' but has 'Swap' instance
--
-- >>> instance Swap Bipredicate where swap (Bipredicate p) = Bipredicate (flip p)
--
class Swap p where
    swap :: p a b -> p b a

instance Swap (,) where
    swap = Data.Tuple.swap

instance Swap Either where
    swap (Left x) = Right x
    swap (Right x) = Left x

instance Swap p => Swap (Flip p) where
    swap = Flip . swap . runFlip

instance (Swap p, Swap q) => Swap (Product p q) where
    swap (Pair p q) = Pair (swap p) (swap q)

instance (Swap p, Swap q) => Swap (Sum p q) where
    swap (L2 p) = L2 (swap p)
    swap (R2 q) = R2 (swap q)

instance (Functor f, Swap p) => Swap (Tannen f p) where
    swap = Tannen . fmap swap . runTannen

instance (f ~ g, Functor f, Swap p) => Swap (Biff p f g) where
    swap = Biff . swap . runBiff

instance Swap ((,,) x) where
    swap (x,a,b) = (x,b,a)

instance Swap ((,,,) x y) where
    swap (x,y,a,b) = (x,y,b,a)

instance Swap ((,,,,) x y z) where
    swap (x,y,z,a,b) = (x,y,z,b,a)

instance Swap ((,,,,,) x y z w) where
    swap (x,y,z,w,a,b) = (x,y,z,w,b,a)

instance Swap ((,,,,,,) x y z w v) where
    swap (x,y,z,w,v,a,b) = (x,y,z,w,v,b,a)
