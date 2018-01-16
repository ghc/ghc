import Test.Cabal.Prelude
main = cabalTest $ do
    -- 8.0 and up come with sufficiently recent versions of
    -- Cabal which don't have this bug.
    skipUnless =<< ghcVersionIs (< mkVersion [8,0])
    withSandbox $ do
        cabal_sandbox "add-source" ["Cabal"]
        cabal "install"
            -- Ignore the Cabal library that is under test
            ["--package-db=clear", "--package-db=global"
            ,"--only-dep", "--dry-run"]
