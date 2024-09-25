{-# LANGUAGE Haskell2010, NamedDefaults #-}

module NonExportMonoidSum () where

import Data.Monoid (Monoid, Sum)

default Monoid (Sum Integer)
