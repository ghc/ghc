import Test.Cabal.Prelude
main = cabalTest $ do
    withSourceCopy . withDelay . withDirectory "p" . withSandbox $ do
        cabal_sandbox "add-source" ["../q"]
        cabal "install" ["--only-dependencies"]
        recordMode RecordAll $ cabal "run" ["p", "-v0"]
        delay
        copySourceFileTo "../q/Q.hs.in2" "../q/Q.hs"
        recordMode RecordAll $ cabal "run" ["p", "-v0"]
