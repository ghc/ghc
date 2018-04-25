module T12732 where

(a ... b) xs
  | a == x
  , (l, _:r) <- break (== x) xs
  = l ++ [x]
