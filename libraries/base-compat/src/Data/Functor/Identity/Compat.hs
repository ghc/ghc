{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Identity.Compat (
#if MIN_VERSION_base(4,8,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity as Base
#endif
