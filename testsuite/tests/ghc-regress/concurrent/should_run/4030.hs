module Main where

import Control.Concurrent ( forkIO, killThread )
import Control.Exception  ( block )

main :: IO ()
main = do tid <- block $ forkIO $ let x = x in x
          killThread tid
