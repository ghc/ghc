import Test.Cabal.Prelude
-- Test "old build-dep behavior", where we should get the
-- same package dependencies on all targets if setup-version
-- is sufficiently old.
main = setupAndCabalTest $ setup_build []

