import Test.Cabal.Prelude
main = cabalTest $ do
    -- Regression test for #4393
    recordMode DoNotRecord $ do
        -- TODO: Hack; see also CustomDep/cabal.test.hs
        withEnvFilter (/= "HOME") $ do
            -- On -v2, we don't have vQuiet set, which suppressed
            -- the error
            cabal "new-build" ["-v1"]
