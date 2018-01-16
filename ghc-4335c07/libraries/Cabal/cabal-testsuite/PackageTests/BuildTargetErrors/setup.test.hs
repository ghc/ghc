import Test.Cabal.Prelude
-- Test error message we report when a non-buildable target is
-- requested to be built
-- TODO: We can give a better error message here, see #3858.
-- NB: Do NOT test on cabal-install, as we fail differently
-- in that case
main = setupTest $ do
    setup "configure" []
    assertOutputContains "the component is marked as disabled"
        =<< fails (setup' "build" ["not-buildable-exe"])
