{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ForeignLibType(
    ForeignLibType(..),
    knownForeignLibTypes,
    foreignLibTypeIsShared,
) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.PackageDescription.Utils

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

-- | What kind of foreign library is to be built?
data ForeignLibType =
      -- | A native shared library (@.so@ on Linux, @.dylib@ on OSX, or
      -- @.dll@ on Windows).
      ForeignLibNativeShared
      -- | A native static library (not currently supported.)
    | ForeignLibNativeStatic
      -- TODO: Maybe this should record a string?
    | ForeignLibTypeUnknown
    deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Pretty ForeignLibType where
  pretty ForeignLibNativeShared = Disp.text "native-shared"
  pretty ForeignLibNativeStatic = Disp.text "native-static"
  pretty ForeignLibTypeUnknown  = Disp.text "unknown"

instance Parsec ForeignLibType where
  parsec = do
    name <- P.munch1 (\c -> isAlphaNum c || c == '-')
    return $ case name of
      "native-shared" -> ForeignLibNativeShared
      "native-static" -> ForeignLibNativeStatic
      _               -> ForeignLibTypeUnknown

instance Text ForeignLibType where
  parse = Parse.choice [
      do _ <- Parse.string "native-shared" ; return ForeignLibNativeShared
    , do _ <- Parse.string "native-static" ; return ForeignLibNativeStatic
    ]

instance Binary ForeignLibType

instance Semigroup ForeignLibType where
  ForeignLibTypeUnknown <> b = b
  a <> ForeignLibTypeUnknown = a
  _ <> _ = error "Ambiguous foreign library type"

instance Monoid ForeignLibType where
  mempty = ForeignLibTypeUnknown
  mappend = (<>)

knownForeignLibTypes :: [ForeignLibType]
knownForeignLibTypes = [
      ForeignLibNativeShared
    , ForeignLibNativeStatic
    ]

foreignLibTypeIsShared :: ForeignLibType -> Bool
foreignLibTypeIsShared t =
    case t of
      ForeignLibNativeShared -> True
      ForeignLibNativeStatic -> False
      ForeignLibTypeUnknown  -> cabalBug "Unknown foreign library type"
