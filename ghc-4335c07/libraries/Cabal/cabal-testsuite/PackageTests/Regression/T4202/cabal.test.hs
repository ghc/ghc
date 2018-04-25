import Test.Cabal.Prelude
main = cabalTest $
    withSourceCopy . withDelay $ do
        writeSourceFile ("p/P.hs") "module P where\np = \"AAA\""
        cabal "new-build" ["p","q"]
        delay
        writeSourceFile ("p/P.hs") "module P where\np = \"BBB\""
        cabal "new-build" ["p"]
        cabal "new-build" ["q"]
        withPlan $
            runPlanExe' "q" "qexe" []
                >>= assertOutputContains "BBB"
