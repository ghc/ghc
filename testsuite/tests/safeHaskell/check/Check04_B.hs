{-# LANGUAGE Safe #-}

-- Since Safe we require base package be trusted to compile
module Check04_B where

import Check04_A

mainM :: Int -> Int
mainM n = trace "Allowed Leak" $ n * 2

