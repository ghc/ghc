import Test.Cabal.Prelude
-- NB: cabal-install doesn't understand --dependency
main = setupTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      withDirectory "sigs" $ setup_install_with_docs ["--cid", "sigs-0.1.0.0", "lib:sigs"]
      withDirectory "indef" $ setup_install_with_docs ["--cid", "indef-0.1.0.0", "--dependency=sigs=sigs-0.1.0.0", "lib:indef"]
