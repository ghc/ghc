{-# LANGUAGE OverloadedLists, TypeFamilies #-}

import qualified Data.Set as S
import GHC.Exts

main = do print ([] :: (S.Set Int))
          print (['a','b','c'] :: (S.Set Char))
          print (['a','c'..'g'] :: (S.Set Char))
          
instance Ord a => IsList (S.Set a) where
 type (Item (S.Set a)) = a
 fromList = S.fromList
 toList = S.toList
