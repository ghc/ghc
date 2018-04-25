import HackageBenchmark
import Statistics.Types (mkPValue)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assert, testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests" [

    testGroup "isSignificantTimeDifference" [

        testCase "detect increase in distribution" $ assert $
            isSignificantTimeDifference (mkPValue 0.05) [1,2..7] [4,5..10]

      , testCase "detect decrease in distribution" $ assert $
            isSignificantTimeDifference (mkPValue 0.05) [1,2..7] [-2,-1..4]

      , testCase "ignore same data" $ assert $
            not $ isSignificantTimeDifference (mkPValue 0.05) [1,2..10] [1,2..10]

      , testCase "same data with high p-value is significant" $ assert $
            isSignificantTimeDifference (mkPValue 0.9) [1,2..10] [1,2..10]

      , testCase "ignore outlier" $ assert $
            not $ isSignificantTimeDifference (mkPValue 0.05) [1, 2, 1, 1, 1] [2, 1, 50, 1, 1]
      ]

  , testGroup "combineTrialResults" [

        testCase "convert unexpected difference to Unknown" $
            combineTrialResults [NoInstallPlan, BackjumpLimit] @?= Unknown

      , testCase "return one of identical errors" $
            combineTrialResults [NoInstallPlan, NoInstallPlan] @?= NoInstallPlan

      , testCase "return one of identical successes" $
            combineTrialResults [Solution, Solution] @?= Solution

      , testCase "timeout overrides other results" $
            combineTrialResults [Solution, Timeout, Solution] @?= Timeout

      , testCase "convert unexpected difference to Unknown, even with timeout" $
            combineTrialResults [Solution, Timeout, NoInstallPlan] @?= Unknown
    ]

  , testGroup "isSignificantResult" [

        testCase "different results are significant" $ assert $
            isSignificantResult NoInstallPlan BackjumpLimit

      , testCase "unknown result is significant" $ assert $
            isSignificantResult Unknown Unknown

      , testCase "PkgNotFound is significant" $ assert $
            isSignificantResult PkgNotFound PkgNotFound

      , testCase "same expected error is not significant" $ assert $
            not $ isSignificantResult NoInstallPlan NoInstallPlan

      , testCase "success is not significant" $ assert $
            not $ isSignificantResult Solution Solution
    ]

  , testGroup "shouldContinueAfterFirstTrial" [

        testCase "rerun when min difference is zero" $ assert $
                  shouldContinueAfterFirstTrial 0 1.0 1.0 Solution Solution

      , testCase "rerun when min difference is zero, even with timeout" $ assert $
                  shouldContinueAfterFirstTrial 0 1.0 1.0 Timeout Timeout

      , testCase "treat timeouts as the same time" $ assert $
            not $ shouldContinueAfterFirstTrial 0.000001 89.9 92.0 Timeout Timeout

      , testCase "skip when times are too close - 1" $ assert $
            not $ shouldContinueAfterFirstTrial 10 1.0 0.91  Solution Solution

      , testCase "skip when times are too close - 2" $ assert $
            not $ shouldContinueAfterFirstTrial 10 1.0 1.09  Solution Solution

      , testCase "rerun when times aren't too close - 1" $ assert $
                  shouldContinueAfterFirstTrial 10 1.0 0.905 Solution Solution

      , testCase "rerun when times aren't too close - 2" $ assert $
                  shouldContinueAfterFirstTrial 10 1.0 1.1   Solution Solution
    ]
  ]
