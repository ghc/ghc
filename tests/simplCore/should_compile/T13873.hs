module T13873 where

data ST s = MkST

class M a where
  foo :: a -> Int

instance M (ST s) where
  foo x = 3

wimwam :: M a => Bool -> a -> Int
wimwam True  x = wimwam False x
wimwam False x = foo x

f :: ST s -> Int
f x = wimwam True x + 1

-- The question is: do we get an auto-generate
-- specialisation for
--    RULE forall s (d:M (ST s)). wimwam @(ST s) d
--                                  = $swimwam @s
--
-- The hand-written pragma would be:
--    SPECIALISE wimwam :: Bool -> ST s -> Int
