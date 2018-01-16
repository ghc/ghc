import Test.Cabal.Prelude
-- Test that Paths module is generated and available for libraries.
main = setupAndCabalTest $ setup_build []
