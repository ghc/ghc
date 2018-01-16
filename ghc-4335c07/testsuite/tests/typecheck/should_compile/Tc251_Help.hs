{-# LANGUAGE TypeFamilies #-}
module Tc251_Help where

class Cls a where
    type Fam a :: *
    type Fam a = Maybe a
