import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      withDirectory "sigs" $ setup_install []
      withDirectory "indef" $ setup_install []
      -- Forgot to build the instantiated versions!
      withDirectory "exe" $ do
        -- Missing package message includes a unit identifier,
        -- which wobbles when version numbers change
        r <- recordMode DoNotRecord . fails $ setup' "configure" []
        assertOutputContains "indef-0.1.0.0" r
        return ()
