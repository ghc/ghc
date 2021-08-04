{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Foreign.Marshal.Compat (
  module Base
, module Alloc
, module Array
, module Utils
) where
import Foreign.Marshal as Base

import Foreign.Marshal.Alloc.Compat as Alloc
import Foreign.Marshal.Array.Compat as Array
import Foreign.Marshal.Utils.Compat as Utils
