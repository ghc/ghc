import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "new-build" []
    cabal' "new-exec" ["foo"] >>= assertOutputContains "Hello World"

