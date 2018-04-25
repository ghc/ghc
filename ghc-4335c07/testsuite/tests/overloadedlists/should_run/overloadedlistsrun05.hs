{-# LANGUAGE OverloadedLists, TypeFamilies, RebindableSyntax #-}

import Prelude
import Data.List

main = do print []       
          print [0,3..20]    
          print [3]     
          print [2..7]
          print [20,2]
          print [1,2,37]

fromListN _ = length 
fromList = length
          

