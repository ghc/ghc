import Test.Cabal.Prelude
-- Test that module name ambiguity can be resolved using package
-- qualified imports.  (Paper Backpack doesn't natively support
-- this but we must!)
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "p" $ setup_install []
        withDirectory "q" $ setup_install []
        withDirectory "package-import" $ do
            setup_build []
            runExe' "package-import" [] >>= assertOutputContains "p q"
