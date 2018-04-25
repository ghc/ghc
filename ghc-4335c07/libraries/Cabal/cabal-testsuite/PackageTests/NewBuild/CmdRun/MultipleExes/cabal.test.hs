import Test.Cabal.Prelude

main = cabalTest $ do
    -- some ways of explicitly specifying an exe
    cabal' "new-run" ["foo"] >>= assertOutputContains "Hello Foo"
    cabal' "new-run" ["exe:bar"] >>= assertOutputContains "Hello Bar"
    cabal' "new-run" ["MultipleExes:foo"] >>= assertOutputContains "Hello Foo"
    -- there are multiple exes in ...
    fails (cabal' "new-run" []) >>= assertOutputDoesNotContain "Hello" -- in the same project
    fails (cabal' "new-run" ["MultipleExes"]) >>= assertOutputDoesNotContain "Hello" -- in the same package

