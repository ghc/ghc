
-- !!! A rules test
-- At one time the rule got too specialised a type:
--
--  _R "ffoo" forall {@ a1 v :: (a1, ((), ()))}
--            fst @ a1 @ () (sndSnd @ a1 @ () @ () v) = fst @ a1 @ ((), ()) v


module Main where

import System.IO
import System.IO.Unsafe ( unsafePerformIO )

{-# NOINLINE [0] sndSnd #-}
-- Don't inline till last, to give the rule a chance
sndSnd (a,(b,c)) = (a,c)

{-# NOINLINE [2] myFst #-}
-- Don't inline till last, to give the rule a chance
myFst (a,b) = a

trace x y = unsafePerformIO (hPutStr stderr x >> hPutStr stderr "\n" >> return y)

{-# RULES "foo" forall v .  myFst (sndSnd v) = trace "Yes" (fst v) #-}

main :: IO ()
main = print (myFst (sndSnd (True, (False,True))))
