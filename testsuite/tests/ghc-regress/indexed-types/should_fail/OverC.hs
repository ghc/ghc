{-# OPTIONS -findexed-types #-}

module OverC
where
import OverA (C)

data instance C [a] [Int] = C9ListList
