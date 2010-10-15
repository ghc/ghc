{-# LANGUAGE RecordWildCards #-}

module TT where

data T = T {t1, t2 :: Int}

f :: T -> Int
f d = x
    where T {t1 = x, ..} = d

g :: T -> Int
g (T {t1 = x, ..}) = x
