-- | Ideally, we want the pattern match in `f` to be discovered as exhaustive,
-- with a redundant match on the second clause.
module Lib where

f :: ()
f = case (True, False) of
  (True, False) -> ()
  (True, True) -> ()
