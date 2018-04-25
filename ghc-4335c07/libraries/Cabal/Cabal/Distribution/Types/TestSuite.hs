{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.TestSuite (
    TestSuite(..),
    emptyTestSuite,
    testType,
    testModules,
    testModulesAutogen
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.BuildInfo
import Distribution.Types.TestType
import Distribution.Types.TestSuiteInterface
import Distribution.Types.UnqualComponentName

import Distribution.ModuleName

import qualified Distribution.Types.BuildInfo.Lens as L

-- | A \"test-suite\" stanza in a cabal file.
--
data TestSuite = TestSuite {
        testName      :: UnqualComponentName,
        testInterface :: TestSuiteInterface,
        testBuildInfo :: BuildInfo
    }
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance L.HasBuildInfo TestSuite where
    buildInfo f l = (\x -> l { testBuildInfo = x }) <$> f (testBuildInfo l)

instance Binary TestSuite

instance Monoid TestSuite where
    mempty = TestSuite {
        testName      = mempty,
        testInterface = mempty,
        testBuildInfo = mempty
    }
    mappend = (<>)

instance Semigroup TestSuite where
    a <> b = TestSuite {
        testName      = combine' testName,
        testInterface = combine  testInterface,
        testBuildInfo = combine  testBuildInfo
    }
        where combine  field = field a `mappend` field b
              combine' field = case ( unUnqualComponentName $ field a
                                    , unUnqualComponentName $ field b) of
                        ("", _) -> field b
                        (_, "") -> field a
                        (x, y) -> error $ "Ambiguous values for test field: '"
                            ++ x ++ "' and '" ++ y ++ "'"

emptyTestSuite :: TestSuite
emptyTestSuite = mempty


testType :: TestSuite -> TestType
testType test = case testInterface test of
  TestSuiteExeV10 ver _         -> TestTypeExe ver
  TestSuiteLibV09 ver _         -> TestTypeLib ver
  TestSuiteUnsupported testtype -> testtype

-- | Get all the module names from a test suite.
testModules :: TestSuite -> [ModuleName]
testModules test = (case testInterface test of
                     TestSuiteLibV09 _ m -> [m]
                     _                   -> [])
                ++ otherModules (testBuildInfo test)

-- | Get all the auto generated module names from a test suite.
-- This are a subset of 'testModules'.
testModulesAutogen :: TestSuite -> [ModuleName]
testModulesAutogen test = autogenModules (testBuildInfo test)
