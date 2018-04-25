import Test.Cabal.Prelude
-- Test that per-component copy works, when only building one executable
main = setupAndCabalTest $ do
    withPackageDb $ do
        setup "configure" []
        setup "build" ["myprog"]
        setup "copy" ["myprog"]
