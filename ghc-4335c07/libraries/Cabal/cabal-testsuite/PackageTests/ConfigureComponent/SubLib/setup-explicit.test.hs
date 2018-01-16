import Test.Cabal.Prelude
-- NB: The --dependency flag is not supported by cabal-install
main = setupTest $ do
    withPackageDb $ do
        base_id <- getIPID "base"
        setup_install ["sublib", "--cid", "sublib-0.1-abc"]
        setup_install [ "exe", "--exact-configuration"
                      , "--dependency", "sublib=sublib-0.1-abc"
                      , "--dependency", "base=" ++ base_id ]
        runExe' "exe" [] >>= assertOutputContains "OK"
