module Main where

-- A test for loss of sharing.  GHC 6.4.1 did a bogus preInlineUnconditionally

import Control.Monad ( guard )

expensive 0 = True
expensive 1 = False
expensive n = expensive (n-2)

f g = if expensive (10*(fst g)) then odd else even

---------------------------------------------------------
-- The key point is that the (c g) call should not get pushed inside the \x,
-- as happened in 6.4.1.  Doing so loses laziness, and this test shows up 
-- the difference in performance
gen_sucW grow c g 
  = \ x -> grow g x >>= \ y -> do guard $ check y; return y
  where 
    check = c g

sucW = gen_sucW (\ g x -> map (+x) [fst g..snd g]) f (11,500000)

main = print (sum $ sucW 11,sum $ sucW 12)

-- Becuase this version uses a case expression, the bug 
-- doesn't happen and execution is much faster
gen_sucC grow c g = case c g of 
           check -> \ x -> grow g x >>= \ y -> do guard $ check y; return y

sucC = gen_sucC (\ g x -> map (+x) [fst g..snd g]) f (11,500000)

mainC = print (sum $ sucC 11,sum $ sucC 12)

