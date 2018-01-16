import Test.Cabal.Prelude
main = cabalTest $ do
    withSandbox $ do
        cabal "install" ["./Cabal-99998"]
        cabal_sandbox "add-source" ["Cabal-99999"]
        r <- fails $ cabal' "install" ["custom-setup/"]
        assertOutputContains "This is Cabal-99999" r
