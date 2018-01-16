-- Transformation and grouping stress test

{-# OPTIONS_GHC -XTransformListComp #-}

module Main where

import GHC.Exts(sortWith, the, groupWith)

employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60) ]

main = putStrLn (show can_still_use_group_function) >> putStrLn (show output)
  where
    output = [ (the dept, map sum salary, (show x) ++ " and " ++ (show y)) 
             | (name, dept, salary) <- employees
             , then group by dept using groupWith
             , x <- [1, 2, 3]
             , y <- [4, 5, 6]
             , then sortWith by sum salary
             , then take 4
             , then group using replicate 2 ]
    group = const "my group function called!"
    can_still_use_group_function = group "Mississippi"