import Foreign
import Monad
import Addr
import System
import IO

import IOExts
global :: a -> IORef a
global a = unsafePerformIO (newIORef a)
{-# NOINLINE global #-}
v_NumCmps = global 0 :: IORef Int
{-# NOINLINE v_NumCmps #-}

newtype XPtr a = XPtr Addr
unXPtr (XPtr (A# x)) = x

type CInt  = Int32
type CSize = Word32

foreign export dynamic 
   mkComparator :: (XPtr Int -> XPtr Int -> IO CInt) 
		-> IO (XPtr (XPtr Int -> XPtr Int -> IO CInt))

foreign import 
   qsort :: Ptr Int -> CSize -> CSize -> XPtr (XPtr Int -> XPtr Int -> IO CInt) 
	 -> IO ()

compareInts :: XPtr Int -> XPtr Int -> IO CInt
compareInts a1 a2 = do
   num_cmps <- readIORef v_NumCmps
   if num_cmps < 100
    then 
     do writeIORef v_NumCmps (num_cmps+1)
        i1 <- peek (Ptr (unXPtr a1))
        i2 <- peek (Ptr (unXPtr a2))
        return (fromIntegral (i1 - i2 :: Int))
    else
     do hPutStrLn stderr 
                  "compareInts: 100 comparisons exceeded; something's wrong"
        exitWith (ExitFailure 1)

main :: IO ()
main = do
   let values = [ 12, 56, 90, 34, 78 ] :: [Int]
       n      = length values
   buf <- mallocArray n
   zipWithM_ (pokeElemOff buf) [ 0 .. ] values
   c <- mkComparator compareInts
   qsort buf (fromIntegral n) (fromIntegral (sizeOf (head values))) c
   mapM (peekElemOff buf) [ 0 .. n-1 ] >>= (print :: [Int] -> IO ())
