{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base where

import GHC.Types ()

class Semigroup a
class Monoid a

data Maybe a = Nothing | Just a
