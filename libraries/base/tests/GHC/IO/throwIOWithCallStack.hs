module Main where

import Control.Exception
import GHC.Exception
import GHC.IO

data CustomException = CustomException deriving (Show)

instance Exception CustomException

main :: IO ()
main =
  catch
    (throwIOWithCallStack CustomException)
    printBacktraces
  where
    printBacktraces = putStr . pprBacktraces
