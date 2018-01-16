import Test.Cabal.Prelude
-- Test if detailed-0.9 builds correctly and runs
-- when linked dynamically
-- See https://github.com/haskell/cabal/issues/4270
main = setupAndCabalTest $ do
  skipUnless =<< hasSharedLibraries
  skipUnless =<< hasCabalShared
  skipUnless =<< hasCabalForGhc
  setup_build ["--enable-tests", "--enable-executable-dynamic"]
  setup "test" []
