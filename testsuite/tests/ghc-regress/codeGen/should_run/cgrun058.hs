-- Not really a code-gen test, but this program gave
-- incorrect results in Hugs (Husg Trac #37), so I 
-- thought I'd add it to GHC's test suite.

module Main where

data MInt = Zero | Succ MInt | Pred MInt deriving Show

tn :: Int -> MInt
tn x | x<0 = Pred (tn (x+1))
tn 0     =  Zero
tn (n+1) = Succ (tn n)

ti :: MInt -> Int
ti Zero = 0
ti (Succ x) = 1+(ti x)
ti (Pred x) = (ti x) -1

testi :: (MInt -> MInt -> MInt) -> (Int -> Int -> Int) -> Int -> Int -> Bool
testi f g x y = (ti (f (tn x) (tn y))) /= (g x y)

myMul x y = tn ((ti x) * (ti y))

-- test should be empty!
test = [ (x,y,ti (myMul (tn x) (tn y)),x * y)
       | x<-[-100, -99, -98, -97, -2, -1, 0, 1, 2, 97, 98, 99, 100],
         y<-([-100..(-1)]++[1..100]),
	 testi myMul (*)  x y ]

main = print test
