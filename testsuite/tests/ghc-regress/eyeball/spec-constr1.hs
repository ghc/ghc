{-# OPTIONS_GHC -O2 -ddump-simpl  #-}
module Roman where

{- From: Roman Leshchinskiy [mailto:rl@cse.unsw.edu.au] 
   Sent: 07 February 2008 03:34
   Subject: Quadratic SpecConstr 

Here is a program which makes SpecConstr generate a quadratic number of
iterations:
-}


bar :: Int -> Int -> Int
bar m n = foo n (n,n) (n,n) (n,n) (n,n)
   where
     foo :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Int
     foo n p q r s
       | n == 0    = m
       | n > 3000  = case p of { (p1,p2) -> foo (n-1) (p2,p1) q r s }
       | n > 2000  = case q of { (q1,q2) -> foo (n-1) p (q2,q1) r s }
       | n > 1000  = case r of { (r1,r2) -> foo (n-1) p q (r2,r1) s }
       | otherwise = case s of { (s1,s2) -> foo (n-1) p q r (s2,s1) }

{- For this particular function, I get 14 specialisations, one for each
   possible combination of arguments.
 
   However, since we can see all the call sites outside the loop, we could
   use that to 'seed' the specialisation, and get just one specialisation.
-}


-- Eyeball guidance: 
-- 	There should be just one specialisation for foo
-- 	Indeed, the original function should disappear,
--		since it isn't used
	
