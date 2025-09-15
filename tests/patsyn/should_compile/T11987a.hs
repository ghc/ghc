{-# LANGUAGE NamedFieldPuns, PatternSynonyms, RecordWildCards #-}
module T11987a where

pattern Point :: Int -> Int -> (Int, Int)
pattern Point{x, y} = (x, y)

-- works
sameFile :: (Int,Int)
sameFile = let { x = 1; y = 2 } in Point { .. }
