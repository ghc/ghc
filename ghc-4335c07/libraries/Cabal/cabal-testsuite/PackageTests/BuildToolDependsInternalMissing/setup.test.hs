import Test.Cabal.Prelude
-- Test missing internal build-tool-depends does indeed fail
main = setupAndCabalTest $ do
    assertOutputContains "missing internal executable"
        =<< fails (setup' "configure" [])
