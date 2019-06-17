{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base (NonEmpty, Maybe, Monoid) where

import GHC.Maybe (Maybe)
import GHC.Types ()

class Monoid a

data NonEmpty a = a :| [a]

