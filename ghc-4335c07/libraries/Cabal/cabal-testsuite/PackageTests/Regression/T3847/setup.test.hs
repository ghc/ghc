import Test.Cabal.Prelude
-- Test that other-extensions of disabled component do not
-- effect configure step.
main = setupAndCabalTest $ setup "configure" ["--disable-tests"]
