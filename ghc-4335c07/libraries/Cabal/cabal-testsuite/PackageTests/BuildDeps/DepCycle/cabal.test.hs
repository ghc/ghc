import Test.Cabal.Prelude
main = cabalTest $ do
    r <- fails $ cabal' "new-build" []
    assertOutputContains "cycl" r -- match cyclic or cycle
    assertOutputContains "bar" r
    assertOutputContains "foo" r
    assertOutputContains "DepCycle" r
