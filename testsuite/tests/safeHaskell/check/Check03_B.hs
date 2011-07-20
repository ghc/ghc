{-# LANGUAGE Safe #-}

-- Since Safe we require base package be trusted to compile
module Check03_B where

import Check03_A

mainM :: Int -> Int
mainM n = trace "Allowed Leak" $ n * 2

