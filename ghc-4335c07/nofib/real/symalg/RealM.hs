module RealM (RealT, evalReal, int2Real, addReal, subReal, mulReal, 
              divReal, sqrtReal) where

data Tree a = TreeC a (Tree a) (Tree a)
 
data RealT = RealC (Tree Integer) deriving ()


treeFrom :: Integer -> Tree Integer
treeFrom n = TreeC n (treeFrom (2*n-1)) (treeFrom (2*n-2))
-------------------------------------------------------------------------------


numberTree :: Tree Integer
numberTree = treeFrom 0
-------------------------------------------------------------------------------


-- multiply an integer, x, by 10^(-n).
scale :: Integer -> Integer -> Integer
scale x n | n <= 0 = (x * 10^(1-n) + 5) `quot` 10
scale x n | n > 0  = (x `quot` 10^(n-1) + 5) `quot` 10
-------------------------------------------------------------------------------


-- return the position of the most significant digit of a real. The second
-- argument is an accumulator in which the value is collected. This should be 0
-- in the initial call.
msd :: Tree Integer -> Integer -> Integer
msd x k = if (xk >= 1) && (xk < 10)
          then k
          else if (xk == 10) then (k+1)
               else if xk >= 1
                    then (msd x (k+1))
                    else (msd x (k-1))

          where xk = abs (evalReal (RealC x) k)
-------------------------------------------------------------------------------


-- evaluate a real  with a tolerance of tol.
evalReal :: RealT -> Integer -> Integer
evalReal (RealC tree) tol | tol <= 0 = lookupTree tree tol
evalReal (RealC tree) tol | tol > 0  = scale (lookupTree tree 0) tol
-------------------------------------------------------------------------------


lookupTree :: Tree Integer -> Integer -> Integer
lookupTree tree n = followPath (getPath (1-n) []) tree
                    where
                        getPath 1 p          = p
                        getPath n p | even n = getPath (n `quot` 2) (0:p)
                        getPath n p | odd n  = getPath (n `quot` 2) (1:p)

                        followPath [] (TreeC x _ _)      = x
                        followPath (0:ps) (TreeC x lc _) = followPath ps lc
                        followPath (1:ps) (TreeC x _ rc) = followPath ps rc
-------------------------------------------------------------------------------


mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (TreeC x lc rc) = TreeC (f x) (mapTree f lc) (mapTree f rc)
-------------------------------------------------------------------------------


-- convert an integer to a real.
int2Real :: Integer -> RealT
int2Real x = RealC (mapTree (\n -> scale x n) numberTree)
-------------------------------------------------------------------------------


-- add two real numbers.
addReal :: RealT -> RealT -> RealT
addReal x y = 
    RealC (mapTree (\n -> ((evalReal x (n-1)) + 
                           (evalReal y (n-1)) + 5) `quot` 10) 
                   numberTree)
-------------------------------------------------------------------------------


-- subtract two real numbers.
subReal :: RealT -> RealT -> RealT
subReal x y =
    RealC (mapTree (\n -> ((evalReal x (n-1)) - 
                           (evalReal y (n-1)) + 5) `quot` 10)
          numberTree)
-------------------------------------------------------------------------------


-- multiply two real numbers.
mulReal :: RealT -> RealT -> RealT
mulReal (xr@(RealC x)) (yr@(RealC y)) =
    RealC (mapTree (\n -> let
                              m  = if n >= 0 then (n+1) `quot` 2
                                             else (n-1) `quot` 2
                              x0 = evalReal xr m
                              y0 = evalReal yr m

                              mulAux x y = if y1 == 0
                                           then 0
                                           else scale (x1 * y1)
                                                      (4 + (msd x 0) +
                                                       (msd y 0) - n)

                                           where
                                               y1 = (evalReal yr
                                                         (n - (msd x 0) - 2))
                                               x1 = (evalReal xr
                                                         (n - (msd y 0) - 2))
                          in if (x0 == 0) && (y0 == 0) 
                             then 0
                              else if x0 == 0
                                   then mulAux y x 
                                   else mulAux x y)
          numberTree)
-------------------------------------------------------------------------------


-- divide two real numbers.
divReal :: RealT -> RealT -> RealT
divReal x y = 
    mulReal x (reciprocalReal y)
-------------------------------------------------------------------------------


-- find the reciprocal of a real number.
reciprocalReal :: RealT -> RealT
reciprocalReal xr@(RealC x) =
    RealC (mapTree 
            (\n -> ((f (dx + n)) + 5) `quot` 10)
            numberTree)
    where
        dx = msd x 0

        f m = if m >= 0
              then 0
              else if m == -1
                   then 10000 `quot` (evalReal xr (dx - 2))
                   else (scale ((fm) * ((scale 2 (m + hm - 2)) -
                                        (evalReal xr (dx + m - 1)) * (fm)))
                               (2 - 2*hm))

                   where hm = (m-1) `quot` 2
                         fm = f hm
-------------------------------------------------------------------------------


-- find the square of a real number.
sqrReal :: RealT -> RealT
sqrReal (xr@(RealC x)) =
    RealC (mapTree (\n -> let
                              m  = if n >= 0 then (n+1) `quot` 2
                                             else (n-1) `quot` 2
                              x0 = evalReal xr m
                              x1 = evalReal xr (n - (msd x 0) - 2)
                          in if (x0 == 0) 
                             then 0
                             else scale (x1 * x1) (4 + 2*(msd x 0) - n))
           numberTree)
-------------------------------------------------------------------------------


-- find the square root of a real number.
sqrtReal :: RealT -> RealT
sqrtReal xr@(RealC x) =
    RealC (mapTree
            (\n -> if (evalReal xr (2*n)) == 0 
                    then 0
                    else f n (2*hdx+2) xr (-2*hdx-2+n))
            numberTree)

    where
        dx  = msd x 0
        hdx = if dx >= 0 then (dx+1) `quot` 2 else dx `quot` 2

	f n dx xr m = if m >= 0
              then 0
              else if m == -1
                   then scale ((round . sqrt . fromInteger) 

					(evalReal xr (dx - 10)))
			      (5-hx+n)
                   else (fm + xv `quot` fm) `quot` 2
              where hm = (m-1) `quot` 2
                    fm = (f n dx xr hm)
                    xv = evalReal xr (n+n)
		    hx = dx `quot` 2
 
-------------------------------------------------------------------------------


{-
-- representation for pi. This representation is grossly inefficient.
pi :: RealT
pi = RealC (mapTree
            (\n -> if n > 0 then 0 else evalReal f n 
            where
                f = iter (int2Real 1)
                         (sqrtReal (RealC (mapTree (\n -> scale 5 (n+1))
                                                   numberTree)))
                         (RealC (mapTree (\n -> scale 25 (n+2)) numberTree))
                         (int2Real 1)
                iter a b t x = if evalReal (subReal a b) (n-1) <= 1   
                               then divReal (sqrReal a) t
                               else iter y
                                         (sqrtReal (mulReal a b))
                                         (subReal t (mulReal x (sqrReal d)))
                                         (mulReal x (int2Real 2))
                               where y = divReal (addReal a b) (int2Real 2)
                                     d = subReal y a)
            numberTree)
-------------------------------------------------------------------------------
-}
