module Main where

import Paths_PathsModule (getBinDir)

main :: IO ()
main = do
    _ <- getBinDir
    return ()
