{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Proxy
import A
import C

data Y
type instance A (a, Y) = Bool

y :: Proxy a -> Bool -> A (a, Y)
y _ = id

z :: Bool -> ()
z = x (undefined :: Proxy Y) y

main = print (z True)
