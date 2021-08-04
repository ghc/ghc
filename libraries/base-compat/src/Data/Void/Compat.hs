{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Void.Compat (
#if MIN_VERSION_base(4,8,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,8,0)
import Data.Void as Base
#endif
