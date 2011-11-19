
module Main where

import Data.IORef

loop r 0 = return ()
loop r c = loop r (c-1) >> writeIORef r 42

main = newIORef 0 >>= \r -> loop r 1000000 >> putStrLn "done"
