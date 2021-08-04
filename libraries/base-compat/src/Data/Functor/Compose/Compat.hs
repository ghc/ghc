{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Compose.Compat (
#if MIN_VERSION_base(4,9,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Compose as Base
#endif
