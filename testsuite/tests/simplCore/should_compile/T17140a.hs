module T17140a where

import {-# SOURCE #-} T17140

data A = A B

mapP_A :: (Int -> Int) -> A -> A
mapP_A _ (A xs) = A (mapP_B id xs)
