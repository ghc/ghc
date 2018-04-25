import Test.Cabal.Prelude
main = cabalTest $ do
    r <- fails $ cabal' "new-build" []
    assertOutputContains "Asdf" r
    assertOutputContains "Reexport2" r
