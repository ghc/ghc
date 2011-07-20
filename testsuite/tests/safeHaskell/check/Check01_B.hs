{-# LANGUAGE Safe #-}

-- Since Safe we require base package be trusted to compile
module Check01_B where

import Check01_A

mainM :: Int -> Int
mainM n = trace "Allowed Leak" $ n * 2

