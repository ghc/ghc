{-# LANGUAGE OverloadedLists, TypeFamilies #-}

import qualified Data.Set as S
import GHC.Exts

main = do putStrLn (f [])       
          putStrLn (f [1,2])    
          putStrLn (f [2,0])    
          putStrLn (f [3,2])     
          putStrLn (f [2,7])
          putStrLn (f [2,2])
          putStrLn (f [1..7])


f :: S.Set Int -> String
f [] = "empty"
f [_] = "one element"
f [2,_] = "two elements, the smaller one is 2"
f [_,2] = "two elements, the bigger one is 2"
f _ = "else"

          
instance Ord a => IsList (S.Set a) where
 type (Item (S.Set a)) = a
 fromList = S.fromList
 toList = S.toList
          

