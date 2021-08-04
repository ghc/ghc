{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Bifoldable.Compat (
  module Base
) where

#if MIN_VERSION_base(4,10,0)
import "base-compat" Data.Bifoldable.Compat as Base
#else
import "bifunctors" Data.Bifoldable as Base
#endif
