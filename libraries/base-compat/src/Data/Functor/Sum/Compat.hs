{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Data.Functor.Sum.Compat (
#if MIN_VERSION_base(4,9,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,9,0)
import Data.Functor.Sum as Base
#endif
