import Foreign
import Foreign.C.Types

foreign import ccall "wrapper"
   mkComparator :: (Ptr Int -> Ptr Int -> IO CInt) 
		-> IO (FunPtr (Ptr Int -> Ptr Int -> IO CInt))

foreign import ccall
   qsort :: Ptr Int -> CSize -> CSize -> FunPtr (Ptr Int -> Ptr Int -> IO CInt) 
	 -> IO ()

compareInts :: Ptr Int -> Ptr Int -> IO CInt
compareInts a1 a2 = do
   i1 <- peek a1
   i2 <- peek a2
   return (fromIntegral (i1 - i2 :: Int))

main :: IO ()
main = do
   c <- mkComparator compareInts
   let values = [ 12, 56, 90, 34, 78 ] :: [Int]
       n      = length values
   withArray values $ \ buf -> do
      qsort buf (fromIntegral n) (fromIntegral (sizeOf (head values))) c
      values' <- peekArray n buf
      print values'
