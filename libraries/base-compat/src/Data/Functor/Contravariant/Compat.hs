{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Contravariant.Compat (
#if MIN_VERSION_base(4,12,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant as Base
#endif
