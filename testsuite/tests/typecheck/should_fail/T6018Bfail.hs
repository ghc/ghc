{-# LANGUAGE TypeFamilyDependencies #-}

module T6018Bfail where

import Data.Kind (Type)

type family H a b c = (result :: Type) | result -> a b c
