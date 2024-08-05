-- | Import two conflicting @default Monoid@ declarations, expect a warning

{-# LANGUAGE Haskell2010, NamedDefaults #-}

import Data.Monoid (Monoid, Product, Sum)
import ExportMonoidProduct ()
import ExportMonoidSum ()

main = putStrLn "Import07"
