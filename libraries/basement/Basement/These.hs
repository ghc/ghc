-- |
-- Module      : Basement.These
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : stable
-- Portability : portable
--
-- @These a b@, sum type to represent either @a@ or @b@ or both.
--
module Basement.These
    ( These(..)
    ) where

import Basement.Compat.Base
import Basement.NormalForm
import Basement.Compat.Bifunctor

-- | Either a or b or both.
data These a b
    = This a
    | That b
    | These a b
  deriving (Eq, Ord, Show, Typeable)

instance (NormalForm a, NormalForm b) => NormalForm (These a b) where
    toNormalForm (This a) = toNormalForm a
    toNormalForm (That b) = toNormalForm b
    toNormalForm (These a b) = toNormalForm a `seq` toNormalForm b

instance Bifunctor These where
    bimap fa _  (This a)    = This  (fa a)
    bimap _  fb (That b)    = That  (fb b)
    bimap fa fb (These a b) = These (fa a) (fb b)

instance Functor (These a) where
    fmap = second
