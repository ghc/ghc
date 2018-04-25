import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    r <- fails $ setup' "configure" []
    assertOutputContains "cycl" r -- match cyclic or cycle
    assertOutputContains "bar" r
    assertOutputContains "foo" r
