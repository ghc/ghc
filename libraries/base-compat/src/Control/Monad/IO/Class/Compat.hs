{-# LANGUAGE CPP, NoImplicitPrelude #-}
module Control.Monad.IO.Class.Compat (
#if MIN_VERSION_base(4,9,0)
  module Base
#endif
) where

#if MIN_VERSION_base(4,9,0)
import Control.Monad.IO.Class as Base
#endif
