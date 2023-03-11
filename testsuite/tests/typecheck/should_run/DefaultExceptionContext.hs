module Main where

import GHC.Exception.Context
import GHC.Exception
import Control.Exception

data TestError = TestError
    deriving (Show)

instance Exception TestError

-- The implicit ExceptionContext constraint here should be defaulted with a warning
exc :: SomeException
exc = SomeException TestError

main :: IO ()
main = do
  case getAllExceptionAnnotations (exceptionContext exc) of
    [] -> return ()
    _  -> fail "unexpected ExceptionContext"

