{-# LANGUAGE BangPatterns #-}
module T9127 where

f = let !_ = 2 * 2
    in 2*2
