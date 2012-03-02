
{-# LANGUAGE CApiFFI #-}

module M where

import Foreign.C

foreign import capi "math.h value sqrt" f :: CInt -> CInt

