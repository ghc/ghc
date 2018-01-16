import Test.Cabal.Prelude
-- Test that Haddock with a newline in synopsis works correctly, #3004
main = setupAndCabalTest $ do
    setup "configure" []
    setup "haddock" []
