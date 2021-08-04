{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Functor.Identity.Compat (
  module Base
) where

#if MIN_VERSION_base(4,8,0)
import "base-compat" Data.Functor.Identity.Compat as Base
#else
import Data.Functor.Identity as Base
#endif
