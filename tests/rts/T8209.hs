import Control.Concurrent
import Control.Monad
import GHC.Conc

main = do
  mainTid <- myThreadId
  labelThread mainTid "main"
  forM_ [0..0] $ \i -> forkIO $ do
    subTid <- myThreadId
    labelThread subTid $ "sub " ++ show i
    forM_ [0..100000000] $ \j -> putStrLn $ "sub " ++ show i ++ ": " ++ show j
  yield
  setNumCapabilities 2
