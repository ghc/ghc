{-# LANGUAGE NamedDefaults #-}

import Data.Monoid (Sum)

default Monoid (Sum Integer)

main = print mempty
