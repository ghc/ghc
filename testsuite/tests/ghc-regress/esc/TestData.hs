module TestData where

data A = A1 | A2

{-
-- If we put noA2 and yesA2 here, they are out of scope 
-- in ESC's eyes. i.e. they are not in EscEnv.vals

noA2 A1 = True
noA2 A2 = False

yesA2 A1 = False
yesA2 A2 = True
-}

-- {-# CONTRACT h1 :: {x | noA2 x} -> {r | yesA2 r} #-}
{-# CONTRACT h1 :: {x | noA2 x} -> {r | True} #-}
-- {-# CONTRACT h1 :: {x | noA2 x} -> _ #-}
h1 :: A -> A
h1 A1 = A2


noA2 A1 = True
noA2 A2 = False

yesA2 A1 = False
yesA2 A2 = True

