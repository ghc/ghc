{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (SomeException, handle)
import Test.HUnit (assertBool, assertEqual, assertFailure)
import qualified Data.ByteString as B
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

-- Try to generate arguments to concat that are big enough to cause an
-- Int to overflow.
concat_overflow :: IO ()
concat_overflow =
    handle (\(_::SomeException) -> return ()) $
    B.concat (replicate lsize (B.replicate bsize 0)) `seq`
    assertFailure "T.replicate should crash"
  where
    (lsize, bsize) | maxBound == (2147483647::Int) = (2^14, 2^18)
                   | otherwise                     = (2^34, 2^29)

tests :: [F.Test]
tests = [
    F.testCase "concat_overflow" concat_overflow
  ]

main = F.defaultMain tests
