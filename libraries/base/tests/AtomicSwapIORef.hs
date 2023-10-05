import Data.IORef
import GHC.IORef
import Data.Word

main :: IO ()
main = do
    r <- newIORef 42 :: IO (IORef Int)
    mapM (atomicSwapIORef r) [0..1000] >>= print
    mapM (atomicSwapIORef r) [0..1000000] >>= print . sum . map (fromIntegral :: Int -> Integer)
    readIORef r >>= print
