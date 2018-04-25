{-# LANGUAGE RecordWildCards #-}
module T8448 where

data R = R {r :: Int}
f x = undefined [] {r = x}


