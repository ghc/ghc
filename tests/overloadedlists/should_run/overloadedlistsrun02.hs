{-# LANGUAGE OverloadedLists, TypeFamilies #-}

import qualified Data.Set as S
import GHC.Exts

main = do print ([] :: (S.Set Int))
          print (['a','b','c'] :: (S.Set Char))
          print (['a','c'..'g'] :: (S.Set Char))
