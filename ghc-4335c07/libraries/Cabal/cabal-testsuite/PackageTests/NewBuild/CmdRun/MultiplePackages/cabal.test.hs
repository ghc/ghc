import Test.Cabal.Prelude

main = cabalTest $ do
    -- some ways of specifying an exe without ambiguities
    cabal' "new-run" ["bar-exe"]     >>= assertOutputContains "Hello bar:bar-exe"
    cabal' "new-run" ["bar:bar-exe"] >>= assertOutputContains "Hello bar:bar-exe"
    cabal' "new-run" ["foo:foo-exe"] >>= assertOutputContains "Hello foo:foo-exe"
    cabal' "new-run" ["bar:foo-exe"] >>= assertOutputContains "Hello bar:foo-exe"
    -- there are multiple exes ...
    fails (cabal' "new-run" [])              >>= assertOutputDoesNotContain "Hello" -- in the same project
    fails (cabal' "new-run" ["bar"])         >>= assertOutputDoesNotContain "Hello" -- in the same package
    fails (cabal' "new-run" ["foo-exe"])     >>= assertOutputDoesNotContain "Hello" -- with the same name
    -- invalid exes
    fails (cabal' "new-run" ["foo:bar-exe"]) >>= assertOutputDoesNotContain "Hello" -- does not exist

