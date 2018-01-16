import Test.Cabal.Prelude
-- Test legacy `build-tools` dependency on external package
-- We use one of the hard-coded names to accomplish this
main = cabalTest $ do
    cabal "new-build" ["client"]
