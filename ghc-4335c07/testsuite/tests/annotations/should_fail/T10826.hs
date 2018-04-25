{-# LANGUAGE Safe #-}
module Test (hook) where

import System.IO.Unsafe

{-# ANN hook (unsafePerformIO (putStrLn "Woops.")) #-}
hook = undefined
