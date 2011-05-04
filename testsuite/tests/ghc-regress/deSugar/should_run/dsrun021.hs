-- Transformation stress test

{-# OPTIONS_GHC -XTransformListComp #-}

module Main where

import Data.List(takeWhile)
import GHC.Exts(sortWith)

employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60)]

main = putStrLn (show output)
  where
    output = [ (dept, salary) 
             | (name, dept, salary) <- employees
             , then sortWith by salary
             , then filter by salary > 50
             , then take 1 ]