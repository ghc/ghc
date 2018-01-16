import Test.Cabal.Prelude
-- Test that setup can choose flags to disable building a component when that
-- component's dependencies are unavailable. The build should succeed without
-- requiring the component's dependencies or imports.
main = setupAndCabalTest $ do
    r <- setup' "configure" ["-v"]
    assertOutputContains "Flags chosen: build-exe=False" r
    setup "build" []
