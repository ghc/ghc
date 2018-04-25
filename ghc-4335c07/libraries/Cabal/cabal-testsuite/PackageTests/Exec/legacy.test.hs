import Test.Cabal.Prelude
main = cabalTest $ do
    cabal "configure" []
    cabal' "exec" ["echo", "find_me_in_output"]
        >>= assertOutputContains "find_me_in_output"
