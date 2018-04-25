import Test.Cabal.Prelude
-- Test that we don't accidentally add the inplace directory to
-- an executable RPATH.  Don't test on Windows, which doesn't
-- support RPATH.
main = setupAndCabalTest $ do
    skipIf =<< isWindows
    setup "configure" ["--enable-executable-dynamic"]
    setup "build" []
    -- This should fail as it we should NOT be able to find the
    -- dynamic library for the internal library (since we didn't
    -- install it).  If we incorrectly encoded our local dist
    -- dir in the RPATH, this will succeed.
    recordMode DoNotRecord . fails $ runExe "exe" []
