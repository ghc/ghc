import Test.Cabal.Prelude
import Control.Monad ( (>=>) )
main = cabalTest $ do
    -- Some different ways of calling an executable that should all work
    -- on a single-exe single-package project
    mapM_ (cabal' "new-run" >=> assertOutputContains "Hello World")
         [ ["foo"]
         , ["Single"]
         , []
         , ["Single:foo"]
         , ["exe:foo"] ]
    -- non-existent exe
    fails (cabal' "new-run" ["bar"]) >>= assertOutputDoesNotContain "Hello World"

