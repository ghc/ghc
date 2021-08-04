{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Proxy.Compat (
  module Base
) where

#if MIN_VERSION_base(4,7,0)
import "base-compat" Data.Proxy.Compat as Base
#else
import "tagged" Data.Proxy as Base
#endif
