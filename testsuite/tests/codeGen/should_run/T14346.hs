module Main where

import Control.Concurrent
import Control.Monad
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Storable
import Numeric
import GHC.Ptr

main :: IO ()
main = do
    replicateM_ 49 $ threadDelay 1
    _ <- forkIO $ do
       allocaBytes 4 $ \p -> do
         forever $ do
           poke p (0xDEADBEEF :: Word32)
           threadDelay 10
           x <- peek p
           unless (x == 0xDEADBEEF) $ putStrLn (showHex x "")
    threadDelay 1000000
