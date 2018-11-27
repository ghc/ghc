-- Make sure @with#@ holds on to its argument, as promised, keeping it from
-- being garbage-collected.

{-#LANGUAGE MagicHash #-}

import System.Mem
import System.Mem.Weak
import GHC.Base
import GHC.IO
import GHC.Prim
import Control.Concurrent
import Control.Monad

main = do
  do
    -- For reasons that are unclear to me, we have to nest the @let@ binding in
    -- another @do@ block in order to make its scope smaller. If we scope @a@
    -- on the entire body of 'main', then the finalizer doesn't seem to run
    -- at all.
    let a = 2
    mkWeakPtr a (Just $ putStrLn "finalize")
    with a $ do
      putStrLn "with"
      performMajorGC
      threadDelay 10000
      putStrLn "without"
    performMajorGC
    threadDelay 10000
    putStrLn "going"

  performMajorGC
  threadDelay 10000
  putStrLn "gone"

-- | A simple wrapper for 'with#', making it more palatable to normal 'IO'
-- code.
with :: a -> IO () -> IO ()
with thing action = IO (with# thing $ unIO action)
