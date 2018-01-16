import Test.Cabal.Prelude
-- Test build when the library is empty, for #1241
main = setupAndCabalTest $ withDirectory "empty" $ setup_build []
