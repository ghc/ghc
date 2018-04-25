module UnitTests.Distribution.Compat.CreatePipe (tests) where

import Distribution.Compat.CreatePipe
import System.IO (hClose, hGetContents, hPutStr, hSetEncoding, localeEncoding)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests = [testCase "Locale Encoding" case_Locale_Encoding]

case_Locale_Encoding :: Assertion
case_Locale_Encoding = assert $ do
    let str = "\0252"
    (r, w) <- createPipe
    hSetEncoding w localeEncoding
    out <- hGetContents r
    hPutStr w str
    hClose w
    return $! out == str
