module T22761 where

import T22761a

newtype Mod m = Mod m deriving Num

gcdExt :: Integer -> (Integer, Integer)
gcdExt x = go 0 x
  where
    go !_ 0 = (1, 1)
    go r _ = go r r

pow :: (Num m) => Mod m -> Mod m
pow x = x*x*x
{-# NOINLINE [1] pow #-}
{-# RULES
"powMod/3/Int" forall x. pow x = x*x*x
#-}


-- GHC puts `boo1` after `wom1` (since they don't appear connected)
-- Then { wom1 = foo True }  rewrites to  { wom1 = boo False }
-- so we need to do glomming.  And that triggers the bug
-- in the RULE for `pow`!
--
-- wom2/boo2 are there to still elicit the bug if
-- GHC reverses its default ordering

{-# RULES
"wombat1"  foo True = boo1 False
#-}

wom1 = foo True
boo1 x = x

{-# RULES
"wombat2"  foo True = boo2 False
#-}
boo2 x = x
wom2 = foo True
