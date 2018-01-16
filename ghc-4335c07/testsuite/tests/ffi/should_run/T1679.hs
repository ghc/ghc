{-# LANGUAGE ForeignFunctionInterface #-}
import System.Mem
import Foreign
import Control.Exception

-- Test for #1679.  If there's a GC during a foreign call, the
-- interpreter could sometimes crash, because it was using the old
-- pointer to the byte code instructions, which has now moved.  The
-- tricky bit is allocating enough so that the old instructions are
-- overwritten, hence performGC followed by sum [1..100000].

foreign import ccall "wrapper" mkF :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "dynamic" call_F :: FunPtr (IO ()) -> IO ()

main = do
  fun <- mkF (do performGC
                 print (sum [1..100000]))
  call_F fun
  putStrLn "ok"
