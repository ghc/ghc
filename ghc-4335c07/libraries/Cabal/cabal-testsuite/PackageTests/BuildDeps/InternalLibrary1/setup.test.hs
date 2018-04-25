import Test.Cabal.Prelude
-- Test executable depends on internal library.
main = setupAndCabalTest $ setup_build []

