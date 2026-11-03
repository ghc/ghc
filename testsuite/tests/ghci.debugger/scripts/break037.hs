module Main where

import Control.Exception

f :: () -> IO ()
f () = throwIO (ErrorCall "boom") `finally` g ()

g :: () -> IO ()
g () = putStrLn "cleanup"

main :: IO ()
main = f ()
