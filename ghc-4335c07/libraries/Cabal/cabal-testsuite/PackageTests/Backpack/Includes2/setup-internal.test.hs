import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
        setup_install ["--cabal-file", "Includes2.cabal"]
        -- TODO: haddock for internal method doesn't work
        runExe "exe" []
