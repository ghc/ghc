import Test.Cabal.Prelude
main = cabalTest $ do
    -- NB: This variant seems to use the bootstrapped Cabal?
    skipUnless =<< hasCabalForGhc
    -- This test depends heavily on what packages are in the global
    -- database, don't record the output
    recordMode DoNotRecord $ do
        -- TODO: Hack, delete me
        withEnvFilter (/= "HOME") $ do
            cabal "new-build" ["all"]
