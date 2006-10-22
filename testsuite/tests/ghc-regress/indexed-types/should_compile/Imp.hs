{-# OPTIONS -findexed-types #-}

module Imp
where

import Exp (C, T, S)

instance C Int where
  data T Int = TInt

data instance S Int Bool = SIntBool
