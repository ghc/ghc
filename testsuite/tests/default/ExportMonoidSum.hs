{-# LANGUAGE Haskell2010, NamedDefaults #-}

module ExportMonoidSum (default Monoid) where

import Data.Monoid (Monoid, Sum)

default Monoid (Sum Integer)
