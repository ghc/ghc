-- !!! Jon Hill reported a bug in desugaring this in 0.09
-- !!! (recursive with n+k patts)
--

{-# LANGUAGE NPlusKPatterns #-}

module ShouldCompile where

takeList :: Int -> [a] -> [a]
takeList 0     _      = []
takeList (n+1) []     = []
takeList (n+1) (x:xs) = x : takeList n xs
