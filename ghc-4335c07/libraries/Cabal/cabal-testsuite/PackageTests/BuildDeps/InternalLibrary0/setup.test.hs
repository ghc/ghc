import Test.Cabal.Prelude
-- Test attempt to have executable depend on internal
-- library, but setup-version is too old.
main = setupAndCabalTest $ do
    r <- fails $ setup' "configure" []
    -- Should tell you how to enable the desired behavior
    let sb = "library which is defined within the same package."
    assertOutputContains sb r
