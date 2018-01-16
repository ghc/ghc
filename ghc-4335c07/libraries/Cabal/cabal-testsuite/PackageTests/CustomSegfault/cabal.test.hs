import Test.Cabal.Prelude
main = cabalTest $ do
    -- TODO: this test ought to work on Windows too
    skipUnless =<< isLinux
    skipUnless =<< ghcVersionIs (>= mkVersion [7,8])
    fails $ cabal' "new-build" [] >>= assertOutputContains "SIGSEGV"
