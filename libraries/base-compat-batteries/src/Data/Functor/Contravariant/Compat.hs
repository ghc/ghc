{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Functor.Contravariant.Compat (
  module Base
  ) where

#if MIN_VERSION_base(4,12,0)
import "base-compat" Data.Functor.Contravariant.Compat as Base
#else
import "contravariant" Data.Functor.Contravariant as Base
#endif
