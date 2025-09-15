{-# LANGUAGE ParallelListComp #-}

-- !!! Illegal conflicting parallel bindings

module ShouldFail where

xys = [ () | let a = 13 | let a = 17 ]
