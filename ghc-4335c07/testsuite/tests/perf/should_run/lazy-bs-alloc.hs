module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
    (file : _) <- getArgs
    L.readFile file >>= L.putStr
