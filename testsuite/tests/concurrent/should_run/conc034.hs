{-# LANGUAGE CPP #-}
import Control.Concurrent
import Control.Exception
import Foreign

import System.IO (hFlush,stdout)

#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif

-- !!! Try to get two threads into a knot depending on each other.

-- This should result in the main thread being sent a NonTermination
-- exception (in GHC 5.02, the program is terminated with "no threads
-- to run" instead).

main = do
  Foreign.newStablePtr stdout
	-- HACK, because when these two threads get blocked on each other,
	-- there's nothing keeping stdout alive so it will get finalized.
	-- SDM 12/3/2004
  let a = last ([1..10000] ++ [b])
      b = last ([2..10000] ++ [a])
	-- we have to be careful to ensure that the strictness analyser
	-- can't see that a and b are both bottom, otherwise the
	-- simplifier will go to town here, resulting in something like
	-- a = a and b = a.
  forkIO (print a `catch` \NonTermination -> return ())
	-- we need to catch in the child thread too, because it might 
	-- get sent the NonTermination exception first.
  r <- Control.Exception.try (print b)
  print (r :: Either NonTermination ())

