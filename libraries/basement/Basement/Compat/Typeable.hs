-- |
-- Module      : Basement.Compat.Typeable
-- License     : BSD-style
-- Maintainer  : Nicolas Di Prima <nicolas@primetype.co.uk>
-- Stability   : statble
-- Portability : portable
--
-- conveniently provide support for legacy and modern base
--

{-# LANGUAGE CPP #-}

module Basement.Compat.Typeable
    (
#if MIN_VERSION_base(4,7,0)
      Typeable
#else
      Typeable(..)
    , typeRep
#endif
    ) where

#if !MIN_VERSION_base(4,7,0)
import Data.Proxy (Proxy(..))
import qualified Prelude (undefined)
#endif
import Data.Typeable

#if !MIN_VERSION_base(4,7,0)
-- this function does not exist prior base 4.7
typeRep :: Typeable a => Proxy a -> TypeRep
typeRep = typeRep' Prelude.undefined
  where
    typeRep' :: Typeable a => a -> Proxy a -> TypeRep
    typeRep' a _ = typeOf a
    {-# INLINE typeRep' #-}
#endif
