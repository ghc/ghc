{-# LANGUAGE TypeFamilies #-}

module OverDirectThisModA (C, D)
where

data family C a b :: *

type family D a b :: *
