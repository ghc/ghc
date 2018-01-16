import Test.Cabal.Prelude
-- Test that "./Setup haddock" works correctly
main = setupAndCabalTest $ do
    env <- getTestEnv
    let haddocksDir = testDistDir env </> "doc" </> "html" </> "Haddock"
    setup "configure" []
    setup "haddock" []
    let docFiles
            = map (haddocksDir </>)
                  ["CPP.html", "Literate.html", "NoCPP.html", "Simple.html"]
    mapM_ (assertFindInFile "For hiding needles.") docFiles
