import Test.Cabal.Prelude
-- Test --test-option(s) flags on ./Setup test
main = setupAndCabalTest $ do
    setup_build ["--enable-tests"]
    setup "test" ["--test-options=1 2 3"]
    setup "test" [ "--test-option=1"
                 , "--test-option=2"
                 , "--test-option=3"
                 ]
