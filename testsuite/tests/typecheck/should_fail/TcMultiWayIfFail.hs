{-# LANGUAGE MultiWayIf #-}

module TcMultiWayIfFail where

x1 = if | True      -> 1 :: Int
        | False     -> "2"
        | otherwise -> [3 :: Int]

