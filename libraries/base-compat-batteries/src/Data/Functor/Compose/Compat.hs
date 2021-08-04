{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Functor.Compose.Compat (
  module Base
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Data.Functor.Compose.Compat as Base
#else
import Data.Functor.Compose as Base
#endif
