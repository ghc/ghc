import Test.Cabal.Prelude
-- Test "new build-dep behavior", where each target gets
-- separate dependencies.  This tests that an library
-- dep does not leak into the executable.
main = setupAndCabalTest $ do
    setup "configure" []
    r <- fails $ setup' "build" []
    assertRegex "error should be in lemon.hs" "^lemon.hs:" r
    assertRegex
      "error should be \"Could not find module `Text\\.PrettyPrint\""
      "(Could not find module|Failed to load interface for).*Text\\.PrettyPrint" r
