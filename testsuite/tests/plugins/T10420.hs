module Main where

import T10420a

import RuleDefiningPlugin

{-# NOINLINE x #-}
x = "foo"

main = putStrLn (show x)
