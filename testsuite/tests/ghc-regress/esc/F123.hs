module F123 where



data A = A1 | A2
data B = B1 | B2

{-# CONTRACT h1 :: {x | noA2 x} -> {r | yesA2 r} #-}
h1 :: A -> A
h1 A1 = A2


noA2 A1 = True
noA2 A2 = False

yesA2 A1 = False
yesA2 A2 = True

f3 x y =  case y of
	     B1 -> f2 x y

f2 x y = case y of
            B1 -> f1 x

f1 x = h1 A2

 

