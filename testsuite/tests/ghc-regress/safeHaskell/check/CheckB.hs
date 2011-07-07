{-# LANGUAGE Safe #-}

-- Since Safe we require base package be trusted to compile
module CheckB where

import CheckA

mainM :: Int -> Int
mainM n = trace "Allowed Leak" $ n * 2

