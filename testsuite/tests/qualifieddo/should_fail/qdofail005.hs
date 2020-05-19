{-# LANGUAGE Arrows #-}
{-# LANGUAGE QualifiedDo #-}

import Control.Arrow
import Prelude as P

main = runKleisli kleisliIO 1

-- Tests the error message when a qualified do
-- is used in a command.
kleisliIO = proc x -> P.do
    y <- arr id -< x+1
    Kleisli print -< 2*y
    let z = x+y
    t <- arr id -< x*z
    returnA -< t+z
