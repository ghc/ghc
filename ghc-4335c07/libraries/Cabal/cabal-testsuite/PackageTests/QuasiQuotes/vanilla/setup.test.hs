import Test.Cabal.Prelude
-- Test building a vanilla library/executable which uses QuasiQuotes
main = setupAndCabalTest $ setup_build []
