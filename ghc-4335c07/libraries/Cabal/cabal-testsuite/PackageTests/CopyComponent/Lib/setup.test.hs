import Test.Cabal.Prelude
-- Test that per-component copy works, when only building library
main = setupAndCabalTest $ do
    withPackageDb $ do
        setup "configure" []
        setup "build" ["lib:p"]
        setup "copy" ["lib:p"]
