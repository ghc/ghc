import Foreign
import Monad
import Addr

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
   i1 <- peek (Ptr (unXPtr a1))
   i2 <- peek (Ptr (unXPtr a2))
   return (fromIntegral (i1 - i2 :: Int))

main :: IO ()
main = do
   let values = [ 12, 56, 90, 34, 78 ] :: [Int]
       n      = length values
   buf <- mallocArray n
   zipWithM_ (pokeElemOff buf) [ 0 .. ] values
   c <- mkComparator compareInts
   qsort buf (fromIntegral n) (fromIntegral (sizeOf (head values))) c
   mapM (peekElemOff buf) [ 0 .. n-1 ] >>= (print :: [Int] -> IO ())
