-- | Check that the demand signature of 'throw' doesn't suggest that it will
-- throw a precise exception. Specifically, `g` should have a `b` divergence
-- type in its demand signature.

module T25066 (g) where

import Control.Exception

data MyException = MyException
  deriving (Show)

instance Exception MyException

g :: a
g = throw MyException
