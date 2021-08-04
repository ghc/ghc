{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Bifunctor.Compat (
  module Base
) where

#if MIN_VERSION_base(4,8,0)
import "base-compat" Data.Bifunctor.Compat as Base
#else
import "bifunctors" Data.Bifunctor as Base
#endif
