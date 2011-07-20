{-# LANGUAGE Safe #-}

-- Since Safe we require base package be trusted to compile
module CheckB where

import CheckB_Aux

mainM :: Int -> Int
mainM n = trace "Allowed Leak" $ n * 2

