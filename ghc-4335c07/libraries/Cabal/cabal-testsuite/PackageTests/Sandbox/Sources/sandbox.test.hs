import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        cabal_sandbox "add-source" ["p"]
        fails $ cabal_sandbox "delete-source" ["q"]
        cabal_sandbox "add-source" ["q"]
        cabal_sandbox "delete-source" ["p", "q"]
