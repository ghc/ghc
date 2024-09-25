{-# LANGUAGE Haskell2010, NamedDefaults #-}

module ExportImplicitMonoidProduct where

import Data.Monoid (Monoid, Product)
import ExportShowSum ()

default Monoid (Product Integer)
