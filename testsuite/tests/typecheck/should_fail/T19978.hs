{-# LANGUAGE ScopedTypeVariables #-}

module T19978 where

-----------------------------------------
--       Type constructor in terms     --
-----------------------------------------
ex1 = Bool

-- to be suggested instead of "Bool" in "ex1":
data T1 = Bowl
bool = Bowl

ex2 = Let  -- should suggest Left (imported from Prelude)

-----------------------------------------
--        Type variable in terms       --
-----------------------------------------

ex3 :: forall mytv. mytv
ex3 = mytv

-- to be suggested instead of "mytv" in "ex3":
data T3 = Mytv
myvv = Mytv
