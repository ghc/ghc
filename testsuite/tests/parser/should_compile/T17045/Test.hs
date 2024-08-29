{-# LANGUAGE PackageImports #-}
module Test where

import "base" Prelude

-- Test that imports from implicit Prelude (a) are available, as well as package imported Prelude `()`.
b :: ()
b = a


