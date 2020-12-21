{-# LANGUAGE NoIncomplete #-}
{-# OPTIONS_GHC -fdefer-incomplete-patterns #-}

module A where

-- incomplete-patterns
myAbs :: Int -> Int
myAbs x | x < 0 = negate x
