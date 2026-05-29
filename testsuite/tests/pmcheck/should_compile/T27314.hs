module T27314 where

data Box = Box { unBox :: Maybe Int }

f :: Box -> Int
f b = case unBox b of
  Nothing -> 0
  Just _  -> let Just x = unBox b in x
