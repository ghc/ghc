{-# LANGUAGE Haskell2010, NamedDefaults #-}

module ExportWarn ({-# WARNING "Product is also a Monoid" #-} default Monoid) where

import Data.Monoid (Monoid, Sum)

default Monoid (Sum Integer)
