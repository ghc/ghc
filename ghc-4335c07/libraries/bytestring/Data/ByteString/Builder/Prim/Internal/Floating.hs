{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Conversion of 'Float's and 'Double's to 'Word32's and 'Word64's.
--
module Data.ByteString.Builder.Prim.Internal.Floating
    (
      -- coerceFloatToWord32
    -- , coerceDoubleToWord64
    encodeFloatViaWord32F
  , encodeDoubleViaWord64F
  ) where

import Foreign
import Data.ByteString.Builder.Prim.Internal

{-
We work around ticket http://hackage.haskell.org/trac/ghc/ticket/4092 using the
FFI to store the Float/Double in the buffer and peek it out again from there.
-}


-- | Encode a 'Float' using a 'Word32' encoding.
--
-- PRE: The 'Word32' encoding must have a size of at least 4 bytes.
{-# INLINE encodeFloatViaWord32F #-}
encodeFloatViaWord32F :: FixedPrim Word32 -> FixedPrim Float
encodeFloatViaWord32F w32fe
  | size w32fe < sizeOf (undefined :: Float) =
      error $ "encodeFloatViaWord32F: encoding not wide enough"
  | otherwise = fixedPrim (size w32fe) $ \x op -> do
      poke (castPtr op) x
      x' <- peek (castPtr op)
      runF w32fe x' op

-- | Encode a 'Double' using a 'Word64' encoding.
--
-- PRE: The 'Word64' encoding must have a size of at least 8 bytes.
{-# INLINE encodeDoubleViaWord64F #-}
encodeDoubleViaWord64F :: FixedPrim Word64 -> FixedPrim Double
encodeDoubleViaWord64F w64fe
  | size w64fe < sizeOf (undefined :: Float) =
      error $ "encodeDoubleViaWord64F: encoding not wide enough"
  | otherwise = fixedPrim (size w64fe) $ \x op -> do
      poke (castPtr op) x
      x' <- peek (castPtr op)
      runF w64fe x' op

