module Main where

import Control.Exception.Context
import Control.Exception

data TestError = TestError
    deriving (Show)

instance Exception TestError

-- The implicit ExceptionContext constraint here should be defaulted
exc :: SomeException
exc = SomeException TestError

main :: IO ()
main = do
  case getAllExceptionAnnotations (someExceptionContext exc) of
    [] -> return ()
    _  -> fail "unexpected ExceptionContext"

