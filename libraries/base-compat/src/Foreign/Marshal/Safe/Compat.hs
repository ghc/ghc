{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Foreign.Marshal.Safe.Compat (
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
