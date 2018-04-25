import Test.Cabal.Prelude
main = cabalTest $ do
    osx <- isOSX
    -- On Travis OSX, Cabal shipped with GHC 7.8 does not work
    -- with error "setup: /usr/bin/ar: permission denied"; see
    -- also https://github.com/haskell/cabal/issues/3938
    -- This is a hack to make the test not run in this case.
    when osx $ skipUnless =<< ghcVersionIs (>= mkVersion [7,10])
    withSandbox $ do
        cabal_sandbox "add-source" ["custom"]
        cabal_sandbox "add-source" ["client"]
        -- NB: This test relies critically on the Setup script being
        -- built against GHC's bundled Cabal.  This means that the
        -- output we see may vary between tests, so we don't record this.
        recordMode DoNotRecord $
            cabal "install" ["client"]
