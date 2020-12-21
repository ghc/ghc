{-# LANGUAGE PackageImports #-}

module Foo (String) where

import "base" Prelude hiding (String)

data String = String
