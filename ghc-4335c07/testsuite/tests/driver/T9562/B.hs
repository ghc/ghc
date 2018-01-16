{-# LANGUAGE TypeFamilies #-}

module B where

import A
import C

type instance F a b = b

oops :: F a b -> a -> b
oops = const
