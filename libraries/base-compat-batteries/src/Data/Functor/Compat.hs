{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Compat (
  module Base
, Functor(..)
, ($>)
, void
, (<&>)
) where
import Data.Functor as Base

#if !(MIN_VERSION_base(4,7,0))
import Control.Monad.Compat (void)
import Data.Function (flip)
#endif

#if !(MIN_VERSION_base(4,7,0))
infixl 4 $>

-- | Flipped version of '$>'.
--
-- /Since: 4.7.0.0/
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

#if !(MIN_VERSION_base(4,11,0))
-- | Flipped version of '<$>'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
--
-- /Since: 4.11.0.0/
--
-- ==== __Examples__
-- Apply @(+1)@ to a list, a 'Data.Maybe.Just' and a 'Data.Either.Right':
--
-- >>> Just 2 <&> (+1)
-- Just 3
--
-- >>> [1,2,3] <&> (+1)
-- [2,3,4]
--
-- >>> Right 3 <&> (+1)
-- Right 4
--
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>
#endif
