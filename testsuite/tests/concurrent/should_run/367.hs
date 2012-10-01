{-# LANGUAGE MagicHash #-}

import Control.Concurrent
import qualified Data.Vector as U

main = do
    t <- forkIO (U.sum (U.enumFromTo 1 (1000000000 :: Int)) `seq` return ())
    threadDelay 10
    killThread t
    putStrLn "Done"
