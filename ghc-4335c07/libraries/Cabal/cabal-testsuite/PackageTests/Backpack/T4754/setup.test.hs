import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    skipUnless =<< hasProfiledLibraries
    setup "configure" ["--enable-profiling"]
    setup "build" []
