{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Monoid.Compat (
  module Base
, (<>)
) where

import Data.Monoid as Base
#if MIN_VERSION_base(4,9,0)
  hiding ((<>))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup ((<>))
#endif

#if !(MIN_VERSION_base(4,5,0)) && !(MIN_VERSION_base(4,9,0))

infixr 6 <>

-- | An infix synonym for 'mappend'.
--
-- /Since: 4.5.0.0/
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

#endif
