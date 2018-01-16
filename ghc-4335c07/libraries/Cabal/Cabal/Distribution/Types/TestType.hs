{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.TestType (
    TestType(..),
    knownTestTypes,
) where

import Distribution.Compat.Prelude
import Distribution.Version
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text
import Text.PrettyPrint          (char, text)

-- | The \"test-type\" field in the test suite stanza.
--
data TestType = TestTypeExe Version     -- ^ \"type: exitcode-stdio-x.y\"
              | TestTypeLib Version     -- ^ \"type: detailed-x.y\"
              | TestTypeUnknown String Version -- ^ Some unknown test type e.g. \"type: foo\"
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary TestType

knownTestTypes :: [TestType]
knownTestTypes = [ TestTypeExe (mkVersion [1,0])
                 , TestTypeLib (mkVersion [0,9]) ]

instance Pretty TestType where
  pretty (TestTypeExe ver)          = text "exitcode-stdio-" <<>> pretty ver
  pretty (TestTypeLib ver)          = text "detailed-"       <<>> pretty ver
  pretty (TestTypeUnknown name ver) = text name <<>> char '-' <<>> pretty ver

instance Parsec TestType where
  parsec = parsecStandard $ \ver name -> case name of
      "exitcode-stdio" -> TestTypeExe ver
      "detailed"       -> TestTypeLib ver
      _                -> TestTypeUnknown name ver

instance Text TestType where
  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> TestTypeExe ver
    "detailed"       -> TestTypeLib ver
    _                -> TestTypeUnknown name ver
