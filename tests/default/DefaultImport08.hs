-- | Import a @default Monoid@ declarations and override it locally without subsuming it

{-# LANGUAGE Haskell2010, NamedDefaults #-}

import Data.Monoid (Monoid, Product, Sum)
import ExportMonoidProduct ()

default Monoid (Sum Integer)

main = putStrLn "Import08"
