module Main where

import Test.Hspec (describe, hspec, Spec)
import qualified ResponseFileSpec (spec)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ResponseFile" ResponseFileSpec.spec
