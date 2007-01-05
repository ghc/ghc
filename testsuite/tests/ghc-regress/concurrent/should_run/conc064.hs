-- test for bug #1067

import Control.Concurrent
import Control.Exception

main = do
         master <- myThreadId
         test master 10

test tid 0 = return ()
test tid n = handle (const (test tid (n-1))) $ do
                 sequence $ replicate 3 $
                         forkIO $ do t <- myThreadId
                                     --putStrLn ("Start " ++ show t)
                                     threadDelay one_second
                                     --putStrLn ("End " ++ show t)
                                     throwTo tid NonTermination
                                     --putStrLn ("Thrown " ++ show t)
                 threadDelay (10 * one_second)

one_second :: Int
one_second = 100000
