{-# OPTIONS -fglasgow-exts -funbox-strict-fields #-}

-- Tests record selectors with unboxed fields for GADTs

module Main where

data T a where
  T1 { w :: !(Int, Int), x :: a, y :: b } :: T (a,b)
  T2 { w :: !(Int, Int), x :: a } :: T (a,b)
  T3 { z :: Int } :: T Bool

f xv yv = T1 { w = (0,0), x = xv, y = yv }

g :: T a -> T a
g (T1 {x=xv, y=yv}) = T2 { w = (0,0), x = xv }

-- h :: Num a => T a any -> a
h v = x v + 1

i v = let (x,y) = w v in x + y

main = do { let t1 = T1 { w = (0,0), y = "foo", x = 4 }
	    	t2 = g t1
	  ; print (h (f 8 undefined))
	  ; print (h t2)
          ; print (i t1)
	}
