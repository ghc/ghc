module TestData where

data A = A1 | A2


-- If we put noA2 and yesA2 here, they are out of scope 
-- in ESC's eyes. i.e. they are not in EscEnv.vals
{-
noA2 A1 = True
noA2 A2 = False

yesA2 A1 = False
yesA2 A2 = True
-}

{-# CONTRACT h1 :: {x | noA2 x} -> {r | yesA2 r} #-}
h1 :: A -> A
h1 A1 = A2

g1 :: A -> A
g1 A1 = A1
g1 A2 = A1

{-# CONTRACT h2 :: {x | not1 (noA2 x)} -> {r | not1 (yesA2 r)} #-}
h2 :: A -> A
h2 A2 = A1

noA2 A1 = True
noA2 A2 = False

yesA2 A1 = False
yesA2 A2 = True

not1 True = False
not1 False = True

test = h1 (g1 A2)
