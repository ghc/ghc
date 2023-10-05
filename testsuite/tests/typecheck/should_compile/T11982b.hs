{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where
import Control.Concurrent.MVar

type Locker = forall a. IO a -> IO a

main :: IO ()
main = do
    line <- getLine
    lock <- newMVar ()
    let locker :: Locker
        locker = withMVar lock . const
    f line locker

f :: String -> Locker -> IO ()
f line locker = locker $ putStrLn line
