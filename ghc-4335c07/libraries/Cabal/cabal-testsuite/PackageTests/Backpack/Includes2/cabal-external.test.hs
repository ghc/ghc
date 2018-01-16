import Test.Cabal.Prelude

main = cabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withProjectFile "cabal.external.project" $ do
        cabal "new-build" ["exe"]
        withPlan $ do
            r <- runPlanExe' "exe" "exe" []
            assertOutputContains "minemysql minepostgresql" r
