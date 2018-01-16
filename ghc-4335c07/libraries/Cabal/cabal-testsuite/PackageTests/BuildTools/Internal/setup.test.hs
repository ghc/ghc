import Test.Cabal.Prelude
-- Test PATH-munging
main = setupAndCabalTest $ do
    setup_build []
    runExe' "hello-world" []
        >>= assertOutputContains "1111"
