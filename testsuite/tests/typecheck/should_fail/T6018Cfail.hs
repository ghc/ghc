{-# LANGUAGE TypeFamilies #-}

module T6018Cfail where

import T6018Bfail

type instance H Int  Char Bool = Bool
type instance H Char Bool Int  = Int
