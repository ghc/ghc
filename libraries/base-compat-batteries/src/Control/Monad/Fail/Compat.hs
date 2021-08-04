{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Control.Monad.Fail.Compat (
  module Base
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Control.Monad.Fail.Compat as Base
#else
import "fail" Control.Monad.Fail as Base
#endif
