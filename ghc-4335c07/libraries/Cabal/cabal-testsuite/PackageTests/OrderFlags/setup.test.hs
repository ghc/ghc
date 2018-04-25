import Test.Cabal.Prelude
-- Test that setup properly orders GHC flags passed to GHC (when
-- there are multiple ghc-options fields.)
main = setupAndCabalTest $ setup_build []
