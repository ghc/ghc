module A () where

-- This checks that the order of dictionary binds doesn't depend on the
-- order of Uniques.
-- Consider succ from Enum, it requires Num Int and Eq Int. The order of
-- let binds used to depend on the order of Uniques. Consider:
--
--  let $dEq = GHC.Classes.$fEqInt in
--  let $$dNum = GHC.Num.$fNumInt in ...
--
--  vs
--
--  let $dNum = GHC.Num.$fNumInt in
--  let $dEq = GHC.Classes.$fEqInt in ...

data B = C
  deriving (Enum)

f :: Int -> Int
f n
  | n >= 0 = 0
  | otherwise = 0
