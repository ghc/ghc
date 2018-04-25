import Test.Cabal.Prelude
-- Test building a vanilla library/executable which uses Template Haskell
main = setupAndCabalTest $ setup_build []
