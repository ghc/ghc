{-# LANGUAGE TypeInType #-}

module T9632 where

import Data.Kind

data B = T | F
data P :: B -> *

type B' = B
data P' :: B' -> *
