-- Test Integer performanc, from Sergey Mechveliani

{-
Earlier, i presented a benchmark to compare the Int vs Integer 
performance.
It has to be improved.
For, increasing d from proposed d = 40  on,  brings in the numbers
that may be large enough to spoil the test. 
The enclosed script contains a slight modification - mostly, max' 
instead of sum. This settles everything.

------------------
Sergey Mechveliani
mechvel@botik.ru
-}

import System.Environment

--------------------------------------------------------------------
             -- choose d from [100..9000] and switch Z = Int,Integer
type Z = Integer

main =  do
        (arg:_) <- getArgs
	-- compute  extendedGCD x y = (g,u,v) 
        -- for many  x,y  and find  maximum [abs (g+u+v)]
        let  
          d      = fromIntegral (read arg :: Int) :: Z
          (n,m)  = (5000,10000) :: (Z,Z)
          ns     = [n..(n+d)]
          ms     = [m..(m+d)]
          pairs  = [(x,y)| x<-ns, y<-ms]         -- (d+1)^2 of pairs
          tripls = map (\ (x,y)->(x,y,gcdE x y)) pairs 
      
          rs     = map (\ (_,_,(g,u,v))-> abs (g+u+v)) tripls

          max' [x]      = x
          max' (x:y:xs) = if x<y then max' (y:xs)  else  max' (x:xs)

          -- boo = all test tripls    -- this tests gcdE
	--
        putStr (shows (max' rs) "\n")


test (x,y,(d,u,v)) =  d==(u*x+v*y)  &&  d==(gcd x y)

-- gcdE x y -> (d,u,v):   d = gcd(x,y) = u*x + v*y

gcdE :: Integral a => a -> a -> (a,a,a)

gcdE 0 y = (y,0,1)
gcdE x y = g (1,0,x) (0,1,y)
  where
  g (u1,u2,u3) (v1,v2,v3) =  
                   if  v3==0  then  (u3,u1,u2)
                   else
                     case  quotRem u3 v3  
                     of
                       (q,r) -> g (v1,v2,v3) (u1-q*v1, u2-q*v2, r)

