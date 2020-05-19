{-# LANGUAGE QualifiedDo #-}
-- Tests that QualfiedDo works for a linear monad.

import Monad.Linear as Linear


main = do
  let r = runTM (Linear.do
        t0 <- newT
        t1 <- increaseT t0
        (t2, ur) <- extractT t1
        deleteT t2
        Linear.return ur)
  print r
  print r
