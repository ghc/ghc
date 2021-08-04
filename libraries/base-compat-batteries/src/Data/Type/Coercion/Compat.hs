{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Type.Coercion.Compat (
#if MIN_VERSION_base(4,7,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,7,0)
import "base-compat" Data.Type.Coercion.Compat as Base
#endif
