import Test.Cabal.Prelude
-- Basic test for internal libraries (in p); package q is to make
-- sure that the internal library correctly is used, not the
-- external library.
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "q" $ setup_install []
        withDirectory "p" $ do
            setup_install []
            setup "clean" []
            r <- runInstalledExe' "foo" []
            assertOutputContains "I AM THE ONE" r
