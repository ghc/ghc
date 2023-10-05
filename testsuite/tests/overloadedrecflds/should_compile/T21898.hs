{-# LANGUAGE DuplicateRecordFields, PatternSynonyms #-}

module T21898 where

pattern P :: Int -> Int -> (Int, Int)
pattern P { proj_x, proj_y } = (proj_x, proj_y)

pattern Q1 :: Int -> Int
pattern Q1 { proj_x } = proj_x

pattern Q2 :: Int -> Int
pattern Q2 { proj_y } = proj_y

blah :: (Int, Int) -> (Int, Int)
blah p = p { proj_x = 0, proj_y = 1 }
