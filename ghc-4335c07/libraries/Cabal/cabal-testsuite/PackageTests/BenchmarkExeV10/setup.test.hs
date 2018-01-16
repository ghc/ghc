import Test.Cabal.Prelude
-- Test if exitcode-stdio-1.0 benchmark builds correctly
main = setupAndCabalTest $ setup_build ["--enable-benchmarks"]
