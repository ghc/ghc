import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo" $ do
  cabal' "outdated" [] >>=
    (\out -> do
        assertOutputContains "base" out
        assertOutputContains "template-haskell" out)

  cabal' "outdated" ["--ignore=base"] >>=
    (\out -> do
        assertOutputDoesNotContain "base" out
        assertOutputContains "template-haskell" out)

  cabal' "outdated" ["--ignore=base,template-haskell"] >>=
    (\out -> do
        assertOutputDoesNotContain "base" out
        assertOutputDoesNotContain "template-haskell" out)

  cabal' "outdated" ["--minor=base"] >>=
    (\out -> do
        assertOutputDoesNotContain "base" out
        assertOutputContains "template-haskell" out)

  cabal' "outdated" ["--minor=base,template-haskell"] >>=
    (\out -> do
        assertOutputDoesNotContain "base" out
        assertOutputDoesNotContain "template-haskell" out)
