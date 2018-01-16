import Test.Cabal.Prelude
main = cabalTest $ do
    r <- fails $ cabal' "new-build" []
    assertRegex "Should not have been able to import Cabal"
                "(Could not find module|Failed to load interface for).*Distribution\\.Simple" r
    {-
    -- TODO: With GHC 8.2, this no longer is displayed
    -- When using --with-ghc, this message is not necessarily output
    has_cabal <- hasCabalForGhc
    when has_cabal $
        assertRegex "It is a member of the hidden package .*Cabal-"
                    "It is a member of the hidden package" r
    -}
