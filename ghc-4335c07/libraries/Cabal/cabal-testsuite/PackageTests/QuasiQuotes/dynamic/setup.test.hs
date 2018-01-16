import Test.Cabal.Prelude
-- Test building a dynamic library/executable which uses QuasiQuotes
main = setupAndCabalTest $ do
    skipUnless =<< hasSharedLibraries
    setup_build ["--enable-shared", "--enable-executable-dynamic"]
