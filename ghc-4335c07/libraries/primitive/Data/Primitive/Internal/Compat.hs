{-# LANGUAGE CPP, MagicHash #-}

-- |
-- Module      : Data.Primitive.Internal.Compat
-- Copyright   : (c) Roman Leshchinskiy 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Compatibility functions
--

module Data.Primitive.Internal.Compat (
    isTrue#
  , mkNoRepType
  ) where

#if MIN_VERSION_base(4,2,0)
import Data.Data (mkNoRepType)
#else
import Data.Data (mkNorepType)
#endif

#if MIN_VERSION_base(4,7,0)
import GHC.Exts (isTrue#)
#endif



#if !MIN_VERSION_base(4,2,0)
mkNoRepType = mkNorepType
#endif

#if !MIN_VERSION_base(4,7,0)
isTrue# :: Bool -> Bool
isTrue# b = b
#endif
