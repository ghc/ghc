{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (handle, evaluate, TypeError, displayException)
import Data.Proxy (Proxy(Proxy))

import T19966 (ex1, ex2, ex3)

check :: a -> IO ()
check e =
  handle (putStrLn . displayException @TypeError)
         (evaluate (e `seq` ()))

main :: IO ()
main = do
  check ex1
  check ex2
  check (ex3 Proxy)