import Foreign
import Monad

type CInt  = Int32
type CSize = Word32

foreign export dynamic mkComparator :: (Addr -> Addr -> IO CInt) -> IO Addr
foreign import qsort :: Addr -> CSize -> CSize -> Addr -> IO ()

compareInts :: Addr -> Addr -> IO CInt
compareInts a1 a2 = do
   i1 <- peek a1
   i2 <- peek a2
   return (fromIntegral (i1 - i2 :: Int))

main :: IO ()
main = do
   let values = [ 12, 56, 90, 34, 78 ] :: [Int]
       n      = length values
   buf <- mallocElems (head values) n
   zipWithM_ (pokeElemOff buf) [ 0 .. ] values
   c <- mkComparator compareInts
   qsort buf (fromIntegral n) (fromIntegral (sizeOf (head values))) c
   mapM (peekElemOff buf) [ 0 .. n-1 ] >>= (print :: [Int] -> IO ())
