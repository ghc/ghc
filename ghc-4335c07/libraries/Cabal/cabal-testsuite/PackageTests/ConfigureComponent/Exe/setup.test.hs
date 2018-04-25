import Test.Cabal.Prelude
main = setupAndCabalTest $ do
    withPackageDb $ do
        setup_install ["goodexe"]
        runExe' "goodexe" [] >>= assertOutputContains "OK"
