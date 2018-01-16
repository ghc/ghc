import Test.Cabal.Prelude

main = cabalTest $
    withSourceCopy $ do
        r <- cabal' "new-configure" []
        assertOutputContains "Now overwriting it" r
