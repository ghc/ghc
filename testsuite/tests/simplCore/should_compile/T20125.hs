{-# OPTIONS_GHC -fno-cpr-anal -fno-cse #-}
-- CSE recovers good code, but we want to expose it going bad
-- CPR gives a stable unfolding which clutters the output

module T20125 where

data T = MkT Int Int

f x = let y = MkT x x
      in (y, y `seq` (y,y))

{- We expect this to optimise to

f x = let y = MkT x x
      in (y, (y,y))

without MkT being duplicated
-}
