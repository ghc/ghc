{-# LANGUAGE PackageImports #-}

module Test17388 where

import "base" Prelude
import {-# Source  #-} Foo.Bar

import {-# SOURCE #-}  "base"  Data.Data
import {-# SOURCE #-} qualified  "base"  Data.Data
