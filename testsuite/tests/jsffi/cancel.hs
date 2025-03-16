module Test where

import Control.Exception

foreign import javascript safe "new Promise(res => setTimeout(res, $1))"
  js_setTimeout :: Int -> IO ()

setTimeout :: Int -> IO ()
setTimeout t = evaluate =<< js_setTimeout t

foreign export javascript "setTimeout"
  setTimeout :: Int -> IO ()
