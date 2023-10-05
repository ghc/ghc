module T20245 where

import GHC.Num.Integer

foo :: Int
foo = case 2 of
  IS _ -> 9999
  IP _ -> 7777
  IN _ -> 7777
