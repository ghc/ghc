module Main where

import Control.Exception
import GHC.Exception
import GHC.Exception.Backtrace

data CustomException = CustomException deriving (Show)

instance Exception CustomException

main :: IO ()
main = do
  print "=== Test Show instances ==="
  print "SomeExceptionWithBacktrace:"
  catch
    (throw CustomException)
    (\(e :: SomeExceptionWithBacktrace) -> print e)

  print "SomeException:"
  catch
    (throw CustomException)
    (\(e :: SomeException) -> print e)

  print "CustomException:"
  catch
    (throw CustomException)
    (\(e :: CustomException) -> print e)
