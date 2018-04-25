import Test.Cabal.Prelude
main = cabalTest $ do
    r <- cabal' "manpage" []
    assertOutputContains ".B cabal install" r
    assertOutputDoesNotContain ".B cabal manpage" r
