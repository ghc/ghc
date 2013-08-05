{-# Language MagicHash, UnboxedTuples  #-}

-- | Note: extensive testing of atomic operations is performed in the
-- "atomic-primops" library.  Only extremely rudimentary tests appear
-- here.

import GHC.IO
import GHC.IORef
import GHC.ST
import GHC.STRef
import GHC.Prim
import GHC.Base
import Data.Primitive.Array
import Data.IORef
import Control.Monad

------------------------------------------------------------------------

casArrayST :: MutableArray s a -> Int -> a -> a -> ST s (Bool, a)
casArrayST (MutableArray arr#) (I# i#) old new = ST$ \s1# ->
  case casArray# arr# i# old new s1# of 
    (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

casSTRef :: STRef s a -- ^ The 'STRef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> ST s (Bool, a) 
casSTRef (STRef var#) old new = ST $ \s1# ->
   -- The primop treats the boolean as a sort of error code.
   -- Zero means the CAS worked, one that it didn't.
   -- We flip that here:
    case casMutVar# var# old new s1# of
      (# s2#, x#, res #) -> (# s2#, (x# ==# 0#, res) #)

-- | Performs a machine-level compare and swap operation on an
-- 'IORef'. Returns a tuple containing a 'Bool' which is 'True' when a
-- swap is performed, along with the 'current' value from the 'IORef'.
-- 
-- Note \"compare\" here means pointer equality in the sense of
-- 'GHC.Prim.reallyUnsafePtrEquality#'.
casIORef :: IORef a -- ^ The 'IORef' containing a value 'current'
         -> a -- ^ The 'old' value to compare
         -> a -- ^ The 'new' value to replace 'current' if @old == current@
         -> IO (Bool, a) 
casIORef (IORef var) old new = stToIO (casSTRef var old new)


------------------------------------------------------------------------
-- Make sure this Int corresponds to a single object in memory (NOINLINE):
{-# NOINLINE mynum #-}
mynum :: Int
mynum = 33

main = do
  putStrLn "Perform a CAS within an IORef"
  ref  <- newIORef mynum
  res  <- casIORef ref mynum 44
  res2 <- casIORef ref mynum 44
  putStrLn$ "  1st try should succeed: "++show res
  putStrLn$ "  2nd should fail: "++show res2

  ------------------------------------------------------------
  putStrLn "Perform a CAS within a MutableArray#"
  arr <- newArray 5 mynum

  res  <- stToIO$ casArrayST arr 3 mynum 44
  res2 <- stToIO$ casArrayST arr 3 mynum 44
  putStrLn$ "  1st try should succeed: "++show res
  putStrLn$ "  2nd should fail: "++show res2

  putStrLn "Printing array:"
  forM_ [0..4] $ \ i -> do
    x <- readArray arr i 
    putStr ("  "++show x)
  putStrLn ""

  ------------------------------------------------------------
  putStrLn "Done."
