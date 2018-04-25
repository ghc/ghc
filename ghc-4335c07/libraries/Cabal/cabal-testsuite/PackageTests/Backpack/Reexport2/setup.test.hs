import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    fails (setup' "configure" [])
      >>= assertRegex "Expect problem with Asdf" "Asdf"
