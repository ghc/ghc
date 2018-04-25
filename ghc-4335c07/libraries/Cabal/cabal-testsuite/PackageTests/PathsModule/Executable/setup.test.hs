import Test.Cabal.Prelude
-- Test that Paths module is generated and available for executables.
main = setupAndCabalTest $ setup_build []

