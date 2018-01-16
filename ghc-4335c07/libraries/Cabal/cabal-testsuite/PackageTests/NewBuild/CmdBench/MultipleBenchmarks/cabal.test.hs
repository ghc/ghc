import Test.Cabal.Prelude

main = cabalTest $ do
    res1 <- cabal' "new-bench" ["foo"]
    assertOutputContains "Hello Foo" res1
    assertOutputDoesNotContain "Hello Bar" res1
    res2 <- cabal' "new-bench" ["all"]
    assertOutputContains "Hello Foo" res2
    assertOutputContains "Hello Bar" res2

