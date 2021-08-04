{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Control.Monad.IO.Class.Compat (
  module Base
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Control.Monad.IO.Class.Compat as Base
#else
import Control.Monad.IO.Class as Base
#endif
