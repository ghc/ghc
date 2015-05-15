module Main where

import Plugins07a

import RuleDefiningPlugin

{-# NOINLINE x #-}
x = "foo"

main = putStrLn (show x)
