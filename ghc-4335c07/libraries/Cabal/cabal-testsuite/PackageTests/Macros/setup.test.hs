import Test.Cabal.Prelude
-- Test to ensure that setup_macros.h are computed per-component.
main = setupAndCabalTest $ do
    setup_build []
    runExe "macros-a" []
    runExe "macros-b" []

