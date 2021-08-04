{-# Language CPP #-}
-- |
-- Module      : Basement.Compat.C.Types
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Literal support for Integral and Fractional
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
module Basement.Compat.C.Types
    ( CChar(..), CSChar(..), CUChar(..)
    , CShort(..), CUShort(..), CInt(..), CUInt(..), CLong(..), CULong(..)
    , CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..), CLLong(..), CULLong(..)
#if MIN_VERSION_base(4,10,0)
    , CBool(..)
#endif
    , CIntPtr(..), CUIntPtr(..), CIntMax(..), CUIntMax(..)
    , CClock(..), CTime(..), CUSeconds(..), CSUSeconds(..), CFloat(..), CDouble
    , COff(..), CMode(..)
    ) where

import Foreign.C.Types
import System.Posix.Types
