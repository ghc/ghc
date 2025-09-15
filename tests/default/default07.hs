{-# LANGUAGE NamedDefaults #-}

import Data.Semigroup (Sum)

default Semigroup (Sum Integer)

main = print mempty
