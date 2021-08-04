{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Either.Compat (
  module Base
, isLeft
, isRight
, fromLeft
, fromRight
) where
import Data.Either as Base

#if !(MIN_VERSION_base(4,7,0))
import Data.Bool (Bool(..))

-- | Return `True` if the given value is a `Left`-value, `False` otherwise.
--
-- /Since: 4.7.0.0/
isLeft :: Either a b -> Bool
isLeft (Left  _) = True
isLeft (Right _) = False

-- | Return `True` if the given value is a `Right`-value, `False` otherwise.
--
-- /Since: 4.7.0.0/
isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
#endif

#if !(MIN_VERSION_base(4,10,0))
-- | Return the contents of a 'Left'-value or a default value otherwise.
--
-- /Since: 4.10.0.0/
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromLeft 1 (Left 3)
-- 3
-- >>> fromLeft 1 (Right "foo")
-- 1
--
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

-- | Return the contents of a 'Right'-value or a default value otherwise.
--
-- /Since: 4.10.0.0/
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> fromRight 1 (Right 3)
-- 3
-- >>> fromRight 1 (Left "foo")
-- 1
--
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
#endif
