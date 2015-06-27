module Haddock.Backends.Hyperlinker.ParserSpec (main, spec) where


import Test.Hspec

import Haddock.Backends.Hyperlinker.Parser


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parse" parseSpec

parseSpec :: Spec
parseSpec = return ()
