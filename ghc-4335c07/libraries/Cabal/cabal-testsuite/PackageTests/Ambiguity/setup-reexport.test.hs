import Test.Cabal.Prelude
-- Test that we can resolve a module name ambiguity when reexporting
-- by explicitly specifying what package we want.
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [7,9])
    withPackageDb $ do
        withDirectory "p" $ setup_install []
        withDirectory "q" $ setup_install []
        withDirectory "reexport" $ setup_install []
        withDirectory "reexport-test" $ do
            setup_build []
            runExe' "reexport-test" [] >>= assertOutputContains "p q"
