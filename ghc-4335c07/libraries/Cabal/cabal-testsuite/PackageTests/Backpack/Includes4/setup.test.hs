import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      setup_install []
      runExe' "exe" [] >>= assertOutputContains "A (B (A (B"
