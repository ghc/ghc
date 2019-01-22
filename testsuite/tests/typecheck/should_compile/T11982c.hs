-- This is similar to T11982b but 'locker' inlined which allows the module to
-- compile.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where
import Control.Concurrent.MVar

type Locker = forall a. IO a -> IO a

main :: IO ()
main = do
    line <- getLine
    lock <- newMVar ()
    f line $ withMVar lock . const

f :: String -> Locker -> IO ()
f line locker = locker $ putStrLn line
