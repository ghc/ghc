{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main where

import Data.IORef
import NativeRetainedJoinProvider

readStored :: Bool -> String
readStored b = case store b of
    Stored True n -> show n
    Stored False c -> [c]

main :: IO ()
main = do
    ref <- newIORef True
    first <- readStored <$> readIORef ref
    writeIORef ref False
    second <- readStored <$> readIORef ref
    print (first, second)
