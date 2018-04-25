import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withProjectFile "cabal.internal.project" $ do
        cabal "new-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "Includes3" "exe" []
            assertOutputContains "fromList [(0,2),(2,4)]" r
