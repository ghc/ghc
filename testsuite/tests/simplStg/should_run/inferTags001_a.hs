{-# OPTIONS_GHC -fno-worker-wrapper #-}

module A where

data T = T !Int !Bool

{-# NOINLINE g #-}
g :: T -> Int
g (T x _) = x
