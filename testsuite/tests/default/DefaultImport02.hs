-- | Import two conflicting @default Monoid@ declarations and override them

{-# LANGUAGE Haskell2010, NamedDefaults #-}

import Data.Monoid (Monoid, Product, Sum)
import ExportMonoidProduct ()
import ExportMonoidSum ()

default Monoid (Product Integer, Sum Integer)

main = print mempty
