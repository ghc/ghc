module Main where

import Control.Concurrent

foreign import ccall "showControlBits" checkfpu :: IO ()

main
 = do checkfpu
      forkOS checkfpu
