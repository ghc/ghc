import Test.Cabal.Prelude
-- Test that we don't pick up include-dirs from libraries
-- we didn't actually depend on.
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "p" $ setup_install []
        withDirectory "q" $ do
            setup "configure" []
            assertOutputContains "T2971test.h"
                =<< fails (setup' "build" [])
