{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Bitraversable.Compat (
#if MIN_VERSION_base(4,10,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,10,0)
import Data.Bitraversable as Base
#endif
