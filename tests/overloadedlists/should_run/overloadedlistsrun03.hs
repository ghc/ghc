{-# LANGUAGE OverloadedLists, TypeFamilies #-}

import GHC.Exts

-- This nonsensical instance is used to test if fromListN is used properly in
-- desugaring of explicit lists.

instance IsList Int where
 type Item Int = Int
 fromList _ = 0
 toList _ = []
 fromListN n _ = n
 
main = do print ([] :: Int)
          print ([1,2,3,4,5] :: Int)
          print ([1..3] :: Int)
          print ([7] :: Int)
          print ([1,3..] :: Int)
