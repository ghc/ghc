-- | Try to export an undeclared default

{-# LANGUAGE Haskell2010, NamedDefaults #-}

module ReExportUndeclared (default Monoid) where

default Show ((), Int)
