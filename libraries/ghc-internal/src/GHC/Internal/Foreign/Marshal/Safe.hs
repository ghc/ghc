{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Foreign.Marshal.Safe
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

module GHC.Internal.Foreign.Marshal.Safe {-# DEPRECATED "Safe is now the default, please use GHC.Internal.Foreign.Marshal instead" #-}
        (
         -- | The module "Foreign.Marshal.Safe" re-exports the other modules in the
         -- @Foreign.Marshal@ hierarchy:
          module GHC.Internal.Foreign.Marshal.Alloc
        , module GHC.Internal.Foreign.Marshal.Array
        , module GHC.Internal.Foreign.Marshal.Error
        , module GHC.Internal.Foreign.Marshal.Pool
        , module GHC.Internal.Foreign.Marshal.Utils
        ) where

import GHC.Internal.Foreign.Marshal.Alloc
import GHC.Internal.Foreign.Marshal.Array
import GHC.Internal.Foreign.Marshal.Error
import GHC.Internal.Foreign.Marshal.Pool
import GHC.Internal.Foreign.Marshal.Utils

