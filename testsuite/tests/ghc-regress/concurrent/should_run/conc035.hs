module Main where

import Concurrent
import qualified Exception as E

trapHandler :: MVar Int -> IO ()
trapHandler inVar =
  (do { trapMsg <- takeMVar inVar
      ; putStrLn ("Handler got: " ++ show trapMsg)
      ; trapHandler inVar
      }
  )
  `E.catch`
  (trapExc inVar)

trapExc :: MVar Int -> E.Exception -> IO ()
trapExc inVar e =
  do { putStrLn ("Exception: " ++ show e)
     ; trapHandler inVar
     }

main :: IO ()
main =
  do { inVar <- newEmptyMVar
     ; tid <- forkIO (trapHandler inVar)
     ; yield
     ; putMVar inVar 1
     ; threadDelay 1000
     ; throwTo tid (E.ErrorCall "1st")
     ; threadDelay 1000
     ; putMVar inVar 2
     ; threadDelay 1000
     ; throwTo tid (E.ErrorCall "2nd")
     ; threadDelay 1000
     ; putStrLn "All done"
     }
