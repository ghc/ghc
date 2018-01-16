import Test.Cabal.Prelude
main = cabalTest $ do
    cabal' "new-run" ["foo"] >>= assertOutputContains "Hello World"
