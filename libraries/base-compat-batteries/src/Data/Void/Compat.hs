{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Void.Compat (
  Void
, absurd
, vacuous
) where

#if MIN_VERSION_base(4,8,0)
import "base-compat" Data.Void.Compat as Base
#else
import "void" Data.Void as Base
#endif
