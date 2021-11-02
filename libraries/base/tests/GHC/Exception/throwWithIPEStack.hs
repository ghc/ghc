module Main where

import Control.Exception
import GHC.Exception

data CustomException = CustomException deriving (Show)

instance Exception CustomException

main :: IO ()
main = do
  catch
    (throwExceptionInScrutinee 1 >> pure ())
    printBacktraces
  where
    printBacktraces = putStr . pprBacktraces

-- Force creation of Stg stack return frames for IPE based backtraces.
throwExceptionInScrutinee :: Int -> IO Int
throwExceptionInScrutinee deepness = case scrutinee deepness of
  -- Due the the thrown exception, the cases below are never hit!
  0 -> pure 42
  i -> pure i
  where
    scrutinee :: Int -> Int
    scrutinee 0 = throwWithIPEStack CustomException
    scrutinee n = scrutinee $ n - 1
