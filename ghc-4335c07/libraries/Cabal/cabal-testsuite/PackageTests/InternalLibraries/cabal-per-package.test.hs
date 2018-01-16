import Test.Cabal.Prelude
main = cabalTest $ do
    fails $ cabal "new-build" ["--disable-per-component", "p"]
