import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< hasCabalForGhc
    -- On Travis OSX, Cabal shipped with GHC 7.8 does not work
    -- with error "setup: /usr/bin/ar: permission denied"; see
    -- also https://github.com/haskell/cabal/issues/3938
    -- This is a hack to make the test not run in this case.
    skipIf =<< liftM2 (&&) isOSX (ghcVersionIs (< mkVersion [7,10]))
    setup' "configure" [] >>= assertOutputContains "ThisIsCustomYeah"
    setup' "build"     [] >>= assertOutputContains "ThisIsCustomYeah"
