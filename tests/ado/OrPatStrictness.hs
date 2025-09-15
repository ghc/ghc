-- {-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
-- {-# OPTIONS_GHC -O2 -fforce-recomp #-}
{-# LANGUAGE OrPatterns, ApplicativeDo #-}

import Data.Functor.Identity
import Debug.Trace

data T = M1 | M2 | M3 deriving Show

instance MonadFail Identity where fail = error "never happens"

blah :: Identity T -> Identity T -> Identity T -> Identity T
blah m1 m2 m3 = do
  (~M1; M2)     <- m1 -- a lazy pattern
  (M1; M2; M3)  <- m2 -- a strict pattern
  z <- m3
  return z
{-# NOINLINE blah #-}

main = print (blah (trace "m1" (Identity M1))
                   (trace "m2" (Identity M2))
                   (trace "m3" (Identity M3)))
