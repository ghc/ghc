{-# LANGUAGE UnboxedTuples, MagicHash #-}
-- Test for EnforceEpt mixing up tag signatures between functions and their return values
module Main (main) where

import GHC.Exts
import GHC.Exts.Heap (GenClosure(..), getClosureData)
import System.Exit

-- an evaluated function that returns a tuple
g :: State# RealWorld -> (# State# RealWorld, () #)
g s = (# s, () #)
{-# NOINLINE g #-}

data S = MkS !(State# RealWorld -> (# State# RealWorld, () #)) Int
data T = MkT !S

s :: S
s = MkS g 0
{-# NOINLINE s #-}

t :: T
t = MkT s

main :: IO ()
main = do
    c <- getClosureData t
    case c of
        ThunkClosure{} -> putStrLn "FAIL: ThunkClosure" >> exitFailure
        APClosure{}    -> putStrLn "FAIL: APClosure" >> exitFailure
        _              -> putStrLn "OK"
