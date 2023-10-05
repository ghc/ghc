{-# LANGUAGE Haskell2010 #-}
module RnFail059 where

import RnFail059_aux

-- Id with different type
f1 :: Int -> Float
f1 = undefined
