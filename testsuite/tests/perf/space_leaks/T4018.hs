module Main (main) where

 import Foreign
 import Control.Concurrent

 -- always space-leaks with 6.12.x and -fno-state-hack, see #4018
 {-
 always :: Monad m => m a -> m b
 always a = a >> always a
 -}
 always :: Monad m => m a -> m b
 always a = do
     _ <- a
     always a

 spawner :: IO ()
 spawner = always $ do
     forkIO $ always $ allocaBytes 4 $ \p -> return ()
     -- putStrLn "Delaying"
     threadDelay 1000000

 main :: IO ()
 main = do
     -- putStrLn "Spawning"
     forkIO spawner
     -- putStrLn "Delaying main"
     threadDelay 3000000
