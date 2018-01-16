module Deadlock where

import Distribution.TestSuite

import Lib

tests :: IO [Test]
tests = return  [nullt x | x <- [1 .. 1000]]
