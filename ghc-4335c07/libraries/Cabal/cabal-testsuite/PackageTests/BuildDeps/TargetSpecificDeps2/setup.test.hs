import Test.Cabal.Prelude
-- This is a control on ../TargetSpecificDeps1/setup.test.hs; it should
-- succeed.
main = setupAndCabalTest $ setup_build []
