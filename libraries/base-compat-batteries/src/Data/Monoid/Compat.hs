{-# LANGUAGE CPP, NoImplicitPrelude, PackageImports #-}
module Data.Monoid.Compat (
  module Base
, (<>)
) where

import "base-compat" Data.Monoid.Compat as Base hiding ((<>))
import Data.Semigroup ((<>))
