{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Control.Monad.Compat (
  module Base
, fail
) where

import "base-compat" Control.Monad.Compat as Base hiding (fail)
import Control.Monad.Fail (fail)
