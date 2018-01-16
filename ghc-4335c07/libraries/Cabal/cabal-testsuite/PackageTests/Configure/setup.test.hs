import Test.Cabal.Prelude
-- Test for 'build-type: Configure' example from the setup manual.
-- Disabled on Windows since MingW doesn't ship with autoreconf by
-- default.
main = setupAndCabalTest $ do
    skipIf =<< isWindows
    _ <- shell "autoreconf" ["-i"]
    setup_build []
