import Test.Cabal.Prelude
main = cabalTest $ withRepo "repo"
       $ forM_ ["--new-freeze-file", "--freeze-file"] $ \arg -> do

  cabal' "outdated" [arg] >>=
    (\out -> do
        assertOutputContains "base" out
        assertOutputContains "template-haskell" out)

  cabal' "outdated" [arg, "--ignore=base,template-haskell"] >>=
    (\out -> do
        assertOutputDoesNotContain "base" out
        assertOutputDoesNotContain "template-haskell" out)

  cabal' "outdated" [arg, "--minor=base,template-haskell"] >>=
    (\out -> do
        assertOutputDoesNotContain "base" out
        assertOutputContains "template-haskell" out)
