{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE NoImplicitPrelude #-}
module T10602b (splitAt, map, foldr) where

import GHC.Classes
import GHC.Types
import GHC.Num
import GHC.Base

splitAt                :: Int -> [a] -> ([a],[a])
splitAt n ls
  | n <= 0 = ([], ls)
  | otherwise          = splitAt' n ls
    where
        splitAt' :: Int -> [a] -> ([a], [a])
        splitAt' _  []     = ([], [])
        splitAt' 1  (x:xs) = ([x], xs)
        splitAt' m  (x:xs) = (x:xs', xs'')
          where
            (xs', xs'') = splitAt' (m - 1) xs
