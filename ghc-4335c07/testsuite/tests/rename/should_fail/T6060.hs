{-# LANGUAGE ParallelListComp #-}

module T6060 where

foo = do let bad = [True | x <- [] | y <- []]
