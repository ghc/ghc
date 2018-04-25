import Test.Cabal.Prelude
-- Check that preprocessors that generate extra C sources are handled
main = setupAndCabalTest $ setup_build ["--enable-tests", "--enable-benchmarks"]
