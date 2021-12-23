{-# LANGUAGE NoImplicitPrelude, KindSignatures #-}

module GHC.Base (Maybe, Semigroup, Monoid) where

import GHC.Maybe (Maybe)
import GHC.Types (Type)

class Semigroup (a :: Type)
class Monoid (a :: Type)
