-- -*- haskell -*-
-- partree
-- parallel map over a tree 
-----------------------------------------------------------------------------

module Main where

import System.Environment(getArgs)
import Control.Parallel
import Tree

main = do [arg1,arg2] <- getArgs
          let 
            n = read arg1 :: Int  -- size of tree in nodes
            c = read arg2 :: Int  -- work per node
            res = partree c n
          putStrLn ("partree " ++ unwords [arg1,arg2] ++ " = " ++ show res)

-- worker function to be mapped over the tree; heavily allocating!
bar :: Int -> Int -> Int
bar c n = tree_fold (\x y -> (x+y) `quot` 2) 0 t 
          where forest = [ let 
                             l = take n (iterate (+i) i)
                           in
                             list2tree l 
                         | i <- [1..c + n `mod` 15] ]
                t = foldl1 (^:) forest

-- generate a tree with n nodes;
-- then map and fold 2 functions over it
partree :: Int -> Int -> Int
partree c n = (force_tree t) `seq` (tree_fold max 0 t)
              where t = par_tree_map (bar c) (list2tree [1..n])

