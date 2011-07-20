{-# OPTIONS -fwarn-name-shadowing #-}
module ShouldCompile where

-- !!! test shadowing of a global name

g = 42 where f -1 = -1  -- shadows (-), probably by accident!
