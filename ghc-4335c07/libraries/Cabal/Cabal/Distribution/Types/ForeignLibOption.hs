{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ForeignLibOption(
    ForeignLibOption(..)
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

data ForeignLibOption =
     -- | Merge in all dependent libraries (i.e., use
     -- @ghc -shared -static@ rather than just record
     -- the dependencies, ala @ghc -shared -dynamic@).
     -- This option is compulsory on Windows and unsupported
     -- on other platforms.
     ForeignLibStandalone
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Pretty ForeignLibOption where
  pretty ForeignLibStandalone = Disp.text "standalone"

instance Parsec ForeignLibOption where
  parsec = do
    name <- P.munch1 (\c -> isAlphaNum c || c == '-')
    case name of
      "standalone" -> return ForeignLibStandalone
      _            -> fail "unrecognized foreign-library option"

instance Text ForeignLibOption where
  parse = Parse.choice [
      do _ <- Parse.string "standalone" ; return ForeignLibStandalone
    ]

instance Binary ForeignLibOption
