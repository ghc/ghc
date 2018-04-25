import Test.Cabal.Prelude
-- Test that we pick up include dirs from internal library
main = setupAndCabalTest $ setup_build []
