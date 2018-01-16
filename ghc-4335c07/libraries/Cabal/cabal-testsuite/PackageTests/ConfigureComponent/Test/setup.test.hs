import Test.Cabal.Prelude
-- NB: This doesn't work with cabal-install, because the
-- dependency solver doesn't know how to solve for only
-- a single component of a package.
main = setupTest $ do
    withPackageDb $ do
        setup_install ["test-for-cabal"]
        withDirectory "testlib" $ setup_install []
        setup "configure" ["testsuite"]
        setup "build" []
        setup "test" []
