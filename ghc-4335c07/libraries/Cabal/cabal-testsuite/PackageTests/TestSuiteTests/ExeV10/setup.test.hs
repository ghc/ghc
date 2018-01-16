import Test.Cabal.Prelude

main = setupAndCabalTest $ do
    setup_build ["--enable-tests"]
    -- This one runs both tests, including the very LONG Foo
    -- test which prints a lot of output
    setup "test" ["--show-details=direct"]
