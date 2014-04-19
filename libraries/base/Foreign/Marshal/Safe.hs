{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal.Safe
-- Copyright   :  (c) The FFI task force 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support
--
-- Safe API Only.
--
-----------------------------------------------------------------------------

module Foreign.Marshal.Safe
        (
         -- | The module "Foreign.Marshal.Safe" re-exports the other modules in the
         -- @Foreign.Marshal@ hierarchy:
          module Foreign.Marshal.Alloc
        , module Foreign.Marshal.Array
        , module Foreign.Marshal.Error
        , module Foreign.Marshal.Pool
        , module Foreign.Marshal.Utils
        ) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Pool
import Foreign.Marshal.Utils

