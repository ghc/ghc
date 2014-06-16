{-# LANGUAGE GADTs #-}
-- Trac 3013. 
-- This isn't strictly a GADT test, but it uses GADT syntax

module T3013 where

data T where
  A, B :: T
  C :: T
  D, E :: Int -> T

f :: T -> T
f A = D 3
