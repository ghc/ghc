import Test.Cabal.Prelude

-- Test that unqualified command line constraints do not constrain setup
-- dependencies. cabal should be able to install the local time-99999 by
-- building its setup script with the installed time, even though the installed
-- time doesn't fit the constraint.
main = cabalTest $ withRepo "repo" $ do
  cabal "new-build" ["time", "--constraint=time==99999", "--dry-run"]

  -- Temporarily disabled recording here because output is not stable
  recordMode DoNotRecord $ do
      -- Constraining all uses of 'time' results in a cyclic dependency
      -- between 'Cabal' and the new 'time'.
      r <- fails $ cabal' "new-build" ["time", "--constraint=any.time==99999", "--dry-run"]
      assertOutputContains "cyclic dependencies; conflict set: time:setup.Cabal, time:setup.time" r
