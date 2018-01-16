import Test.Cabal.Prelude
-- Test leacy `build-tools` dependency on internal library
main = cabalTest $ do
    cabal "new-build" ["foo", "hello-world"]
