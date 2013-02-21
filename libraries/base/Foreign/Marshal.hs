{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.Marshal
-- Copyright   :  (c) The FFI task force 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Marshalling support
--
-----------------------------------------------------------------------------

module Foreign.Marshal
        (
         -- | The module "Foreign.Marshal" re-exports the safe content in the
         -- @Foreign.Marshal@ hierarchy:
          module Foreign.Marshal.Safe
        ) where

import Foreign.Marshal.Safe

