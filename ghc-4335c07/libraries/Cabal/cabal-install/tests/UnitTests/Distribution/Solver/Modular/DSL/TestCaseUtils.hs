{-# LANGUAGE RecordWildCards #-}
-- | Utilities for creating HUnit test cases with the solver DSL.
module UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils (
    SolverTest
  , SolverResult(..)
  , independentGoals
  , allowBootLibInstalls
  , disableBackjumping
  , goalOrder
  , constraints
  , preferences
  , enableAllTests
  , solverSuccess
  , solverFailure
  , anySolverFailure
  , mkTest
  , mkTestExts
  , mkTestLangs
  , mkTestPCDepends
  , mkTestExtLangPC
  , runTest
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Data.List (elemIndex)
import Data.Ord (comparing)

-- test-framework
import Test.Tasty as TF
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

-- Cabal
import qualified Distribution.PackageDescription as C
import qualified Distribution.Types.PackageName as C
import Language.Haskell.Extension (Extension(..), Language(..))

-- cabal-install
import qualified Distribution.Solver.Types.PackagePath as P
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb, pkgConfigDbFromList)
import Distribution.Solver.Types.Settings
import Distribution.Solver.Types.Variable
import Distribution.Client.Dependency (foldProgress)
import UnitTests.Distribution.Solver.Modular.DSL
import UnitTests.Options

-- | Combinator to turn on --independent-goals behavior, i.e. solve
-- for the goals as if we were solving for each goal independently.
independentGoals :: SolverTest -> SolverTest
independentGoals test = test { testIndepGoals = IndependentGoals True }

allowBootLibInstalls :: SolverTest -> SolverTest
allowBootLibInstalls test =
    test { testAllowBootLibInstalls = AllowBootLibInstalls True }

disableBackjumping :: SolverTest -> SolverTest
disableBackjumping test =
    test { testEnableBackjumping = EnableBackjumping False }

goalOrder :: [ExampleVar] -> SolverTest -> SolverTest
goalOrder order test = test { testGoalOrder = Just order }

constraints :: [ExConstraint] -> SolverTest -> SolverTest
constraints cs test = test { testConstraints = cs }

preferences :: [ExPreference] -> SolverTest -> SolverTest
preferences prefs test = test { testSoftConstraints = prefs }

enableAllTests :: SolverTest -> SolverTest
enableAllTests test = test { testEnableAllTests = EnableAllTests True }

{-------------------------------------------------------------------------------
  Solver tests
-------------------------------------------------------------------------------}

data SolverTest = SolverTest {
    testLabel          :: String
  , testTargets        :: [String]
  , testResult         :: SolverResult
  , testIndepGoals     :: IndependentGoals
  , testAllowBootLibInstalls :: AllowBootLibInstalls
  , testEnableBackjumping :: EnableBackjumping
  , testGoalOrder      :: Maybe [ExampleVar]
  , testConstraints    :: [ExConstraint]
  , testSoftConstraints :: [ExPreference]
  , testDb             :: ExampleDb
  , testSupportedExts  :: Maybe [Extension]
  , testSupportedLangs :: Maybe [Language]
  , testPkgConfigDb    :: PkgConfigDb
  , testEnableAllTests :: EnableAllTests
  }

-- | Expected result of a solver test.
data SolverResult = SolverResult {
    -- | The solver's log should satisfy this predicate. Note that we also print
    -- the log, so evaluating a large log here can cause a space leak.
    resultLogPredicate            :: [String] -> Bool,

    -- | Fails with an error message satisfying the predicate, or succeeds with
    -- the given plan.
    resultErrorMsgPredicateOrPlan :: Either (String -> Bool) [(String, Int)]
  }

solverSuccess :: [(String, Int)] -> SolverResult
solverSuccess = SolverResult (const True) . Right

solverFailure :: (String -> Bool) -> SolverResult
solverFailure = SolverResult (const True) . Left

-- | Can be used for test cases where we just want to verify that
-- they fail, but do not care about the error message.
anySolverFailure :: SolverResult
anySolverFailure = solverFailure (const True)

-- | Makes a solver test case, consisting of the following components:
--
--      1. An 'ExampleDb', representing the package database (both
--         installed and remote) we are doing dependency solving over,
--      2. A 'String' name for the test,
--      3. A list '[String]' of package names to solve for
--      4. The expected result, either 'Nothing' if there is no
--         satisfying solution, or a list '[(String, Int)]' of
--         packages to install, at which versions.
--
-- See 'UnitTests.Distribution.Solver.Modular.DSL' for how
-- to construct an 'ExampleDb', as well as definitions of 'db1' etc.
-- in this file.
mkTest :: ExampleDb
       -> String
       -> [String]
       -> SolverResult
       -> SolverTest
mkTest = mkTestExtLangPC Nothing Nothing []

mkTestExts :: [Extension]
           -> ExampleDb
           -> String
           -> [String]
           -> SolverResult
           -> SolverTest
mkTestExts exts = mkTestExtLangPC (Just exts) Nothing []

mkTestLangs :: [Language]
            -> ExampleDb
            -> String
            -> [String]
            -> SolverResult
            -> SolverTest
mkTestLangs langs = mkTestExtLangPC Nothing (Just langs) []

mkTestPCDepends :: [(String, String)]
                -> ExampleDb
                -> String
                -> [String]
                -> SolverResult
                -> SolverTest
mkTestPCDepends pkgConfigDb = mkTestExtLangPC Nothing Nothing pkgConfigDb

mkTestExtLangPC :: Maybe [Extension]
                -> Maybe [Language]
                -> [(String, String)]
                -> ExampleDb
                -> String
                -> [String]
                -> SolverResult
                -> SolverTest
mkTestExtLangPC exts langs pkgConfigDb db label targets result = SolverTest {
    testLabel          = label
  , testTargets        = targets
  , testResult         = result
  , testIndepGoals     = IndependentGoals False
  , testAllowBootLibInstalls = AllowBootLibInstalls False
  , testEnableBackjumping = EnableBackjumping True
  , testGoalOrder      = Nothing
  , testConstraints    = []
  , testSoftConstraints = []
  , testDb             = db
  , testSupportedExts  = exts
  , testSupportedLangs = langs
  , testPkgConfigDb    = pkgConfigDbFromList pkgConfigDb
  , testEnableAllTests = EnableAllTests False
  }

runTest :: SolverTest -> TF.TestTree
runTest SolverTest{..} = askOption $ \(OptionShowSolverLog showSolverLog) ->
    testCase testLabel $ do
      let progress = exResolve testDb testSupportedExts
                     testSupportedLangs testPkgConfigDb testTargets
                     Nothing (CountConflicts True) testIndepGoals
                     (ReorderGoals False) testAllowBootLibInstalls
                     testEnableBackjumping (sortGoals <$> testGoalOrder)
                     testConstraints testSoftConstraints testEnableAllTests
          printMsg msg = when showSolverLog $ putStrLn msg
          msgs = foldProgress (:) (const []) (const []) progress
      assertBool ("Unexpected solver log:\n" ++ unlines msgs) $
                 resultLogPredicate testResult $ concatMap lines msgs
      result <- foldProgress ((>>) . printMsg) (return . Left) (return . Right) progress
      case result of
        Left  err  -> assertBool ("Unexpected error:\n" ++ err)
                                 (checkErrorMsg testResult err)
        Right plan -> assertEqual "" (toMaybe testResult) (Just (extractInstallPlan plan))
  where
    toMaybe :: SolverResult -> Maybe [(String, Int)]
    toMaybe = either (const Nothing) Just . resultErrorMsgPredicateOrPlan

    checkErrorMsg :: SolverResult -> String -> Bool
    checkErrorMsg result msg =
        case resultErrorMsgPredicateOrPlan result of
          Left f  -> f msg
          Right _ -> False

    sortGoals :: [ExampleVar]
              -> Variable P.QPN -> Variable P.QPN -> Ordering
    sortGoals = orderFromList . map toVariable

    -- Sort elements in the list ahead of elements not in the list. Otherwise,
    -- follow the order in the list.
    orderFromList :: Eq a => [a] -> a -> a -> Ordering
    orderFromList xs =
        comparing $ \x -> let i = elemIndex x xs in (isNothing i, i)

    toVariable :: ExampleVar -> Variable P.QPN
    toVariable (P q pn)        = PackageVar (toQPN q pn)
    toVariable (F q pn fn)     = FlagVar    (toQPN q pn) (C.mkFlagName fn)
    toVariable (S q pn stanza) = StanzaVar  (toQPN q pn) stanza

    toQPN :: ExampleQualifier -> ExamplePkgName -> P.QPN
    toQPN q pn = P.Q pp (C.mkPackageName pn)
      where
        pp = case q of
               None           -> P.PackagePath P.DefaultNamespace P.QualToplevel
               Indep p        -> P.PackagePath (P.Independent $ C.mkPackageName p)
                                               P.QualToplevel
               Setup s        -> P.PackagePath P.DefaultNamespace
                                               (P.QualSetup (C.mkPackageName s))
               IndepSetup p s -> P.PackagePath (P.Independent $ C.mkPackageName p)
                                               (P.QualSetup (C.mkPackageName s))
