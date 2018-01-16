{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Distribution.Types.BenchmarkType (
    BenchmarkType(..),
    knownBenchmarkTypes,
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text
import Distribution.Version
import Text.PrettyPrint          (char, text)

-- | The \"benchmark-type\" field in the benchmark stanza.
--
data BenchmarkType = BenchmarkTypeExe Version
                     -- ^ \"type: exitcode-stdio-x.y\"
                   | BenchmarkTypeUnknown String Version
                     -- ^ Some unknown benchmark type e.g. \"type: foo\"
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary BenchmarkType

knownBenchmarkTypes :: [BenchmarkType]
knownBenchmarkTypes = [ BenchmarkTypeExe (mkVersion [1,0]) ]

instance Pretty BenchmarkType where
  pretty (BenchmarkTypeExe ver)          = text "exitcode-stdio-" <<>> pretty ver
  pretty (BenchmarkTypeUnknown name ver) = text name <<>> char '-' <<>> pretty ver

instance Parsec BenchmarkType where
    parsec = parsecStandard $ \ver name -> case name of
       "exitcode-stdio" -> BenchmarkTypeExe ver
       _                -> BenchmarkTypeUnknown name ver

instance Text BenchmarkType where
  parse = stdParse $ \ver name -> case name of
    "exitcode-stdio" -> BenchmarkTypeExe ver
    _                -> BenchmarkTypeUnknown name ver


