import Test.Cabal.Prelude
main = cabalTest $ do
    skipUnless =<< hasNewBuildCompatBootCabal
    -- Custom Setups inconsistently report output depending
    -- on your boot GHC.
    recordMode DoNotRecord $ cabal "new-build" ["foo"]
    recordMode DoNotRecord $ cabal "new-build" ["dep"]
