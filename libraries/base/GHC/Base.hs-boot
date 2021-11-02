{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base (Maybe, Semigroup, Monoid, String) where

import GHC.Maybe (Maybe)
import GHC.Types (Char)

class Semigroup a
class Monoid a

type String = [Char]
