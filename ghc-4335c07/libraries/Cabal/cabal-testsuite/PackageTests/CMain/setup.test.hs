import Test.Cabal.Prelude
-- Test building an executable whose main() function is defined in a C
-- file
main = setupAndCabalTest $ setup_build []
