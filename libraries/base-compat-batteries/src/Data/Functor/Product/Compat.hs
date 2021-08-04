{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Functor.Product.Compat (
  module Base
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Data.Functor.Product.Compat as Base
#else
import Data.Functor.Product as Base
#endif
