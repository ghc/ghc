module Main where

import Control.Exception
import GHC.Exception
import GHC.Exception.Backtrace
import System.Exit

data CustomException = CustomException deriving (Show)

instance Exception CustomException

-- When an exception is thrown more than one times, it keeps the first backtrace.
-- This test shares almost all code with GHC/Exception/throw_rethrowException.hs.
-- Unfortunately, the test function cannot be parameterized over the throw* function,
-- because that would change the backtrace in an unsuable manner.
main :: IO ()
main = do
  setDefaultBacktraceMechanisms [HasCallStackBacktraceMech]
  catch
    ( catch
        -- Throw for the first time.
        (throwIO CustomException)
        -- Throw for the second time.
        (\(e :: SomeExceptionWithLocation) -> throwIO e)
    )
    ( \(e :: SomeExceptionWithLocation) -> case e of
        (SomeExceptionWithLocation _ bts) ->
          case head bts of
            -- Only print the most significant location; i.e. the location
            -- where throw was called.
            HasCallStackBacktrace cs -> print $ last $ getCallStack cs
            _ -> exitFailure
    )
