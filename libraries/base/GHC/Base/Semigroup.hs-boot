{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base.Semigroup (Semigroup, Monoid) where

import GHC.Num.Integer () -- See Note [Depend on GHC.Num.Integer] in GHC.Base

class Semigroup a
class Monoid a

