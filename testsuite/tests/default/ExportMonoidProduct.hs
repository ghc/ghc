{-# LANGUAGE Haskell2010, NamedDefaults #-}

module ExportMonoidProduct (default Monoid) where

import Data.Monoid (Monoid, Product)

default Monoid (Product Integer)
