{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Numeric.Natural.Compat (
#if MIN_VERSION_base(4,8,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,8,0)
import Numeric.Natural as Base
#endif
