import Test.Cabal.Prelude
-- Check that preprocessors (hsc2hs) are run
main = setupAndCabalTest $ setup_build ["--enable-tests", "--enable-benchmarks"]
