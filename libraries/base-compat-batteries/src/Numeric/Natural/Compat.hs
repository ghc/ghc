{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Numeric.Natural.Compat (
  module Base
) where

#if MIN_VERSION_base(4,8,0)
import "base-compat" Numeric.Natural.Compat as Base
#else
import "nats" Numeric.Natural as Base
#endif
