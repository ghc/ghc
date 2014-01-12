-- Trac #2409

module ShouldCompile where

 f :: Int -> Int
 f _ | () `seq` False = undefined
     | otherwise = error "XXX"

 g :: Int -> Int
 g _ | () `seq` False = undefined
     | otherwise = error "XXX"
