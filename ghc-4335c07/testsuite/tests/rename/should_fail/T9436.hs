{-# LANGUAGE RecordWildCards #-}

module T9436 where

data T = T { x :: Int }

f :: T -> Int
f (T' { .. }) = x + 1
