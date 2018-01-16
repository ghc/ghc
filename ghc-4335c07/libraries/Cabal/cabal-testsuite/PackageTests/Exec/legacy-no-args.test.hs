import Test.Cabal.Prelude
main = cabalTest $ fails (cabal' "exec" []) >>= assertOutputContains "Please specify an executable to run"
