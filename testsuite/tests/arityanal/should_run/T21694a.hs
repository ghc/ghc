module Main (main) where

import GHC.Exts
import Control.DeepSeq
import System.Exit

-- If we eta expand the `False` branch will return
-- a lambda \eta -> z instead of z.
-- This behaves differently if the z argument is a bottom.
-- We used to assume that a oneshot annotation would mean
-- we could eta-expand on *all* branches. But this is clearly
-- not sound in this case. So we test for this here.
{-# NOINLINE f #-}
f :: Bool -> (Int -> Int) -> Int -> Int
f b z =
    case b of
        True -> oneShot $ \n -> n + 1
        False -> z



main :: IO Int
main = do
    return $! force $! f False (error "Urkh! But expected!")
    return 0


