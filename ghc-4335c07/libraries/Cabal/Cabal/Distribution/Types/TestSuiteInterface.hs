{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.TestSuiteInterface (
    TestSuiteInterface(..),
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.TestType
import Distribution.ModuleName
import Distribution.Version

-- | The test suite interfaces that are currently defined. Each test suite must
-- specify which interface it supports.
--
-- More interfaces may be defined in future, either new revisions or totally
-- new interfaces.
--
data TestSuiteInterface =

     -- | Test interface \"exitcode-stdio-1.0\". The test-suite takes the form
     -- of an executable. It returns a zero exit code for success, non-zero for
     -- failure. The stdout and stderr channels may be logged. It takes no
     -- command line parameters and nothing on stdin.
     --
     TestSuiteExeV10 Version FilePath

     -- | Test interface \"detailed-0.9\". The test-suite takes the form of a
     -- library containing a designated module that exports \"tests :: [Test]\".
     --
   | TestSuiteLibV09 Version ModuleName

     -- | A test suite that does not conform to one of the above interfaces for
     -- the given reason (e.g. unknown test type).
     --
   | TestSuiteUnsupported TestType
   deriving (Eq, Generic, Read, Show, Typeable, Data)

instance Binary TestSuiteInterface


instance Monoid TestSuiteInterface where
    mempty  =  TestSuiteUnsupported (TestTypeUnknown mempty nullVersion)
    mappend = (<>)

instance Semigroup TestSuiteInterface where
    a <> (TestSuiteUnsupported _) = a
    _ <> b                        = b
