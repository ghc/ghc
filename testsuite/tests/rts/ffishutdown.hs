import Control.Concurrent
import Control.Monad
import Foreign.Ptr

main = do
  fptr <- wrap $ return . (*4)
  forkIO $ forever $ do
    _ <- dyn fptr 4
    return ()
  threadDelay $ 100 * 1000

foreign import ccall "wrapper" wrap :: (Double -> IO Double) -> IO (FunPtr (Double -> IO Double))
foreign import ccall "dynamic" dyn :: FunPtr (Double -> IO Double) -> Double -> IO Double
