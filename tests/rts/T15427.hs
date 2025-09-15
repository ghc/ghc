import Control.Concurrent.MVar
import Control.Monad
import Foreign.C.Types (CInt(..))
import Foreign.StablePtr (StablePtr)
import GHC.Conc

foreign import ccall unsafe hs_try_putmvar :: CInt -> StablePtr PrimMVar -> IO ()

main = do
  mvs <- forM [0..numCapabilities] (\idx -> do
                                         a <- newEmptyMVar
                                         b <- newEmptyMVar
                                         return $ (idx, a, b))
  forM_ [mvs, reverse mvs] $ \mvars -> do
    forM_ mvars $ (\(cap,a,b) -> forkOn cap $ do
                      takeMVar a
                      putMVar b ())
    forM_ mvars $ \(cap, a, _) -> do
      sp <- newStablePtrPrimMVar a
      hs_try_putmvar (fromIntegral cap) sp
    forM_ mvars $ \(_,_,b) -> takeMVar b
