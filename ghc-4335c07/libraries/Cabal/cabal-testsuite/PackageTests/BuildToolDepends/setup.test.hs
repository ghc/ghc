import Test.Cabal.Prelude
-- Test build-tool-depends between two packages
main = cabalTest $ do
    cabal "new-build" ["client"]
