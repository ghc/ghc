{-# LANGUAGE TypeFamilies #-}
module T18112 where

type family F a where
  F Int = String

-- This test is really testing the simple optimizer. We expect the
-- optimized desugared output to contain no casts, since the simple
-- optimizer should fuse the two casts together after inlining y.

blah :: Bool -> String
blah x = let y :: F Int
             y = show x
         in y
