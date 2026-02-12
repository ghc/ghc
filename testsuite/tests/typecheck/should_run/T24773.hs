{-# LANGUAGE StaticPointers #-}
{-# OPTIONS_GHC -O1 #-}

module Main where

import GHC.StaticPtr ( StaticPtr )

{-# NOINLINE uhoh #-}
uhoh :: () -> StaticPtr ()
uhoh =
  let ptr :: StaticPtr ()
      ptr = static ()
  in \_ -> ptr

main :: IO ()
main = putStrLn "Hello, Haskell!"
