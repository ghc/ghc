{-# LANGUAGE CPP #-}
#if !(MIN_VERSION_base(4,9,0))
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
#endif
module Basement.Compat.Semigroup
    ( Semigroup(..)
    , ListNonEmpty(..)
    ) where

#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup
import qualified Data.List.NonEmpty as LNE

type ListNonEmpty = LNE.NonEmpty
#else
import Prelude
import Data.Data (Data)
import Data.Monoid (Monoid(..))
import GHC.Generics (Generic)
import Data.Typeable

-- errorWithoutStackTrace

infixr 6 <>
infixr 5 :|

data ListNonEmpty a = a :| [a]
  deriving ( Eq, Ord, Show, Read, Data, Typeable, Generic )

-- | The class of semigroups (types with an associative binary operation).
--
-- @since 4.9.0.0
class Semigroup a where
  -- | An associative operation.
  --
  -- @
  -- (a '<>' b) '<>' c = a '<>' (b '<>' c)
  -- @
  --
  -- If @a@ is also a 'Monoid' we further require
  --
  -- @
  -- ('<>') = 'mappend'
  -- @
  (<>) :: a -> a -> a

  default (<>) :: Monoid a => a -> a -> a
  (<>) = mappend

  -- | Reduce a non-empty list with @\<\>@
  --
  -- The default definition should be sufficient, but this can be
  -- overridden for efficiency.
  --
  sconcat :: ListNonEmpty a -> a
  sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b

  -- | Repeat a value @n@ times.
  --
  -- Given that this works on a 'Semigroup' it is allowed to fail if
  -- you request 0 or fewer repetitions, and the default definition
  -- will do so.
  --
  -- By making this a member of the class, idempotent semigroups and monoids can
  -- upgrade this to execute in /O(1)/ by picking
  -- @stimes = stimesIdempotent@ or @stimes = stimesIdempotentMonoid@
  -- respectively.
  stimes :: Integral b => b -> a -> a
  stimes y0 x0
    | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (pred y  `quot` 2) x
      g x y z
        | even y = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (pred y `quot` 2) (x <> z)

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)
  stimes _ Nothing  = Nothing
  stimes n (Just a) = case compare n 0 of
    LT -> errorWithoutStackTrace "stimes: Maybe, negative multiplier"
    EQ -> Nothing
    GT -> Just (stimes n a)

instance Semigroup [a] where
    (<>) = (++)

instance Semigroup (Either a b) where
  Left _ <> b = b
  a      <> _ = a
  stimes = stimesIdempotent

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a,b) <> (a',b') = (a<>a',b<>b')
  stimes n (a,b) = (stimes n a, stimes n b)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
  stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
  (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
  stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
  (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
  stimes n (a,b,c,d,e) =
      (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

-- | This is a valid definition of 'stimes' for a 'Monoid'.
--
-- Unlike the default definition of 'stimes', it is defined for 0
-- and so it should be preferred where possible.
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid n x0 = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesMonoid: negative multiplier"
  EQ -> mempty
  GT -> f x0 n
    where
      f x y
        | even y = f (x `mappend` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `mappend` x) (pred y  `quot` 2) x
      g x y z
        | even y = g (x `mappend` x) (y `quot` 2) z
        | y == 1 = x `mappend` z
        | otherwise = g (x `mappend` x) (pred y `quot` 2) (x `mappend` z)

-- | This is a valid definition of 'stimes' for an idempotent 'Monoid'.
--
-- When @mappend x x = x@, this definition should be preferred, because it
-- works in /O(1)/ rather than /O(log n)/
stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid n x = case compare n 0 of
  LT -> errorWithoutStackTrace "stimesIdempotentMonoid: negative multiplier"
  EQ -> mempty
  GT -> x

-- | This is a valid definition of 'stimes' for an idempotent 'Semigroup'.
--
-- When @x <> x = x@, this definition should be preferred, because it
-- works in /O(1)/ rather than /O(log n)/.
stimesIdempotent :: Integral b => b -> a -> a
stimesIdempotent n x
  | n <= 0 = errorWithoutStackTrace "stimesIdempotent: positive multiplier expected"
  | otherwise = x

#if !MIN_VERSION_base(4,9,0)
errorWithoutStackTrace = error
#endif

#endif
