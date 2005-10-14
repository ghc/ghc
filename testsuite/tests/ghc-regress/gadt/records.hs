{-# OPTIONS -fglasgow-exts #-}

-- Tests record syntax for GADTs

module Main where

data T a where
  T1 { x :: a, y :: b } :: T (a,b)
  T2 { x :: a } :: T (a,b)
  T3 { z :: Int } :: T Bool

f xv yv = T1 { x = xv, y = yv }

g :: T a -> T a
g (T1 {x=xv, y=yv}) = T2 { x = xv }

-- h :: Num a => T a any -> a
h v = x v + 1

main = do { let t1 = T1 { y = "foo", x = 4 }
	    	t2 = g t1
	  ; print (h (f 8 undefined))
	  ; print (h t2)
	}
	