import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        fails $ cabal "exec" ["my-executable"]
        cabal "install" []
        -- Execute indirectly via bash to ensure that we go through $PATH
        cabal' "exec" ["sh", "--", "-c", "my-executable"]
            >>= assertOutputContains "This is my-executable"
