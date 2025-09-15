{-# LANGUAGE Haskell2010, NamedDefaults #-}

module ExportShowSum (default Show) where

import Data.Monoid (Monoid, Sum)

default Show (Sum Integer)
