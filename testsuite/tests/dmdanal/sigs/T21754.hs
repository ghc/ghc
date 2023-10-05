{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Test where

f :: Int -> Int
f n = n+1
{-# NOINLINE f #-}
