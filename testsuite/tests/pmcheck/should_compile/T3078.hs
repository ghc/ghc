{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}

module T3078 where

data T = A Int | B Int

funny :: T -> Int
funny t = n
    where
      n | A x <- t = x
        | B x <- t = x
