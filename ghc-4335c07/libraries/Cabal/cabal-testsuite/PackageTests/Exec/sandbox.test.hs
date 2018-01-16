import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        fails $ cabal "exec" ["my-executable"]
        cabal "install" []
        cabal' "exec" ["my-executable"]
            >>= assertOutputContains "This is my-executable"
