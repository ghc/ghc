import Test.Cabal.Prelude
-- Test if detailed-0.9 builds correctly
main = setupAndCabalTest $ do
    skipUnless =<< hasCabalForGhc
    setup_build ["--enable-tests"]
