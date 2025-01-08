{-# LANGUAGE MultilineStrings #-}
module Main where

import GHC.Prim
import GHC.JS.Prim
import Foreign.C
import System.IO

foreign import javascript
  """
  ((x) => x)
  """
  toJSDouble :: Double -> JSVal

foreign import javascript 
  """
  (function (x) { 
    console.log(x); 
  })
  """
  multiLog :: JSVal -> IO ()

foreign import javascript
  """
  ((x) => 
    x + ""
  )
  """
  jsToString :: JSVal -> JSVal

foreign import ccall 
  """
  cos
  """ mycos :: CDouble -> CDouble

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  multiLog $ toJSInt 5
  multiLog $ toJSString "Hello"
  putStrLn $ fromJSString $ jsToString $ toJSInt (- 5)
  multiLog $ jsToString $ toJSDouble 3.0
  print $ mycos 0 == 1 