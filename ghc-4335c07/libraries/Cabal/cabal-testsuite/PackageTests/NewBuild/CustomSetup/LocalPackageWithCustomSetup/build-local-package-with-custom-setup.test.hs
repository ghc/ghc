import Test.Cabal.Prelude

-- The one local package, pkg, has a setup dependency on setup-dep-2.0, which is
-- in the repository.
main = cabalTest $ do
  skipUnless =<< hasNewBuildCompatBootCabal
  withRepo "repo" $ do
    r <- recordMode DoNotRecord $ cabal' "new-build" ["pkg"]
    -- pkg's setup script should print out a message that it imported from
    -- setup-dep:
    assertOutputContains "pkg Setup.hs: setup-dep-2.0" r
