{-# LANGUAGE PatternGuards #-}

module T26044 where

import T26044a

x :: a
x | True <- todo = undefined
  | otherwise    = undefined
