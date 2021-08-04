-- |
-- Module      : Basement.Compat.IsList
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- compat friendly version of IsList
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module Basement.Compat.IsList
    ( IsList(..)
    ) where

#if MIN_VERSION_base(4,7,0)

import           GHC.Exts

#else

import qualified Prelude

class IsList l where
  type Item l
  fromList  :: [Item l] -> l
  toList    :: l -> [Item l]

  fromListN :: Prelude.Int -> [Item l] -> l
  fromListN _ = fromList

instance IsList [a] where
    type Item [a] = a
    fromList = Prelude.id
    toList   = Prelude.id

#endif
