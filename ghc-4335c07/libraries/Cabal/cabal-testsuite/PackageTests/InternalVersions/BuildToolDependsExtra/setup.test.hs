import Test.Cabal.Prelude
-- Test unneed version bound on internal build-tools deps
main = setupAndCabalTest $ do
    setup' "configure" []
    assertOutputContains "extraneous version range"
        =<< setup' "sdist" []
