{-# LANGUAGE UnboxedSums, UnboxedTuples, MagicHash #-}

module Main where

import GHC.Prim
import GHC.Types

type Either1 a b c = (# a | (# b, c #) #)

-- The bug disappears when this is inlined
{-# NOINLINE showEither1 #-}

showEither1 :: Either1 String Int Bool -> String
showEither1 (# left | #) = "Left " ++ show left
showEither1 (# | (# right1, right2 #) #) = "Right " ++ show right1 ++ " " ++ show right2

main :: IO ()
main = do
    -- This line used to print "Right -4611686018427359531 False"
    putStrLn (showEither1 e1_2)
  where
    -- boxed types only
    e1_2 :: Either1 String Int Bool
    e1_2 = (# | (# 10, True #) #)
