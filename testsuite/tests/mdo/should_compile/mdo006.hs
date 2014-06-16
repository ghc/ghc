{-# OPTIONS -XRecursiveDo #-}

-- This test, from Iavor Diatchki, made GHC 6.2 loop (testLoop) 
-- or panic (testPanic); there was a Lint error.
-- The reason was a missing bindInstsOfLocalFuns in tcStmtAndThen

module ShouldCompile where

import Control.Monad.Fix

testLoop _  = mdo x <- mapM undefined (f x)
                  let f _ = []
                  return (f x)

testPanic _  = mdo x <- f x
                   let f _ = return ()
                   f x
