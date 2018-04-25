import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      withDirectory "p" $ setup_install_with_docs []
      withDirectory "q" $ do
        setup_build []
        setup "haddock" []
