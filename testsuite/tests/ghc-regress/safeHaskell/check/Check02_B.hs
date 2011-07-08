{-# LANGUAGE Safe #-}

-- Since Safe we require base package be trusted to compile
module Check02_B where

import Check02_A

mainM :: Int -> Int
mainM n = trace "Allowed Leak" $ n * 2

