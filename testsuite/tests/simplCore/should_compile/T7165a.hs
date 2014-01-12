{-# LANGUAGE GADTs #-}

module T7165a where

blah :: (dd ~ (Double, Double)) => dd -> dd
blah (ax, bx)
    | ax < bx = blah (bx, ax)
    | otherwise = (0,0)
