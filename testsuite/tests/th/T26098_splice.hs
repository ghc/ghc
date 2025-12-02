{-# LANGUAGE ExplicitLevelImports, TemplateHaskell #-}
module T26098_splice where

import T26098A_splice
import quote T26098A_splice

import splice Prelude

x = $(undefined :: Foo)
