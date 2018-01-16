import Test.Cabal.Prelude
-- Test building a profiled library/executable which uses Template Haskell
-- (setup has to build the non-profiled version first)
main = setupAndCabalTest $ do
    skipUnless =<< hasProfiledLibraries
    setup_build ["--enable-library-profiling",
                 "--enable-profiling"]
