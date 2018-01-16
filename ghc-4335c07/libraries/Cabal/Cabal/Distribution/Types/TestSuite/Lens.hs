module Distribution.Types.TestSuite.Lens (
    TestSuite,
    module Distribution.Types.TestSuite.Lens,
    ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.BuildInfo           (BuildInfo)
import Distribution.Types.TestSuite           (TestSuite)
import Distribution.Types.TestSuiteInterface  (TestSuiteInterface)
import Distribution.Types.UnqualComponentName (UnqualComponentName)

import qualified Distribution.Types.TestSuite as T

testName :: Lens' TestSuite UnqualComponentName
testName f s = fmap (\x -> s { T.testName = x }) (f (T.testName s))
{-# INLINE testName #-}

testInterface :: Lens' TestSuite TestSuiteInterface
testInterface f s = fmap (\x -> s { T.testInterface = x }) (f (T.testInterface s))
{-# INLINE testInterface #-}

testBuildInfo :: Lens' TestSuite BuildInfo
testBuildInfo f s = fmap (\x -> s { T.testBuildInfo = x }) (f (T.testBuildInfo s))
{-# INLINE testBuildInfo #-}
