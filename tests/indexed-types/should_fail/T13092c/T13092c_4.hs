{-# LANGUAGE TypeFamilies #-}

module T13092c_4 where

import T13092c_3

type instance F (a, Char) = String
