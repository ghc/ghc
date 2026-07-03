module T27287 where

myfun :: String -> String
myfun xs = case reverse xs of
  [] -> "empty"
  xy -> xy
