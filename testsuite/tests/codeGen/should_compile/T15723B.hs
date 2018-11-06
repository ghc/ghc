module T15723B where

import T15723A

test :: Int -> Int
test x = {-# SCC test1 #-} foo $ foo x
