--!!! Testing (List.\\) and related functions
module T where

import List( deleteBy, delete, (\\) )

test1 :: [Int]
test1 = deleteBy (==) 1 [0,1,1,2,3,4]

test2 :: [Int]
test2 = delete 1 [0,1,1,2,3,4]

test3 :: [Int]
test3 = [0,1,1,2,3,4] \\ [3,2,1]

