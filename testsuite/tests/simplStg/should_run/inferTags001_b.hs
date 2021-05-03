{-# OPTIONS_GHC -fno-worker-wrapper #-}

module B where

-- data T = T !Int !Bool

{-# NOINLINE g #-}
g :: T -> Int
g (T x _) = x
