{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Functor.Sum.Compat (
  module Base
) where

#if MIN_VERSION_base(4,9,0)
import "base-compat" Data.Functor.Sum.Compat as Base
#else
import Data.Functor.Sum as Base
#endif
