import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        cabal_sandbox "add-source" ["p"]
        cabal "install" ["p"]
