{-# LANGUAGE TypeFamilies #-}

module T13092c_2 (F) where

import T13092c_1

data X
type instance F (X, b) = Bool
