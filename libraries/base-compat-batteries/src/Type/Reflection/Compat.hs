{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Type.Reflection.Compat (
#if MIN_VERSION_base(4,10,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,10,0)
import "base-compat" Type.Reflection.Compat as Base
#endif
