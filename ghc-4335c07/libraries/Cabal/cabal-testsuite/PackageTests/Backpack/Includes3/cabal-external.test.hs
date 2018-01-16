import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withProjectFile "cabal.external.project" $ do
        cabal "new-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "exe" "exe" []
            assertOutputContains "fromList [(0,2),(2,4)]" r
