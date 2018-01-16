{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.BuildType (
    BuildType(..),
    knownBuildTypes,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Pretty
import Distribution.Parsec.Class
import Distribution.Text

import qualified Distribution.Compat.Parsec as P
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

-- | The type of build system used by this package.
data BuildType
  = Simple      -- ^ calls @Distribution.Simple.defaultMain@
  | Configure   -- ^ calls @Distribution.Simple.defaultMainWithHooks defaultUserHooks@,
                -- which invokes @configure@ to generate additional build
                -- information used by later phases.
  | Make        -- ^ calls @Distribution.Make.defaultMain@
  | Custom      -- ^ uses user-supplied @Setup.hs@ or @Setup.lhs@ (default)
  | UnknownBuildType String
                -- ^ a package that uses an unknown build type cannot actually
                --   be built. Doing it this way rather than just giving a
                --   parse error means we get better error messages and allows
                --   you to inspect the rest of the package description.
                deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary BuildType

knownBuildTypes :: [BuildType]
knownBuildTypes = [Simple, Configure, Make, Custom]

instance Pretty BuildType where
  pretty (UnknownBuildType other) = Disp.text other
  pretty other                    = Disp.text (show other)

instance Parsec BuildType where
  parsec = do
    name <- P.munch1 isAlphaNum
    return $ case name of
      "Simple"    -> Simple
      "Configure" -> Configure
      "Custom"    -> Custom
      "Make"      -> Make
      _           -> UnknownBuildType name

instance Text BuildType where
  parse = do
    name <- Parse.munch1 isAlphaNum
    return $ case name of
      "Simple"    -> Simple
      "Configure" -> Configure
      "Custom"    -> Custom
      "Make"      -> Make
      _           -> UnknownBuildType name
