module T11541 where

g :: Ord k => k -> v -> ()
g k v = ()

f x y =
   let m = min x y
   in  g m foo
