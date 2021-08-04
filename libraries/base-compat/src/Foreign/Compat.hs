{-# LANGUAGE CPP, NoImplicitPrelude #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Foreign.Compat (
  module Base
, module Marshal
) where
import Foreign as Base

import Foreign.Marshal.Compat as Marshal
