{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Distribution.Types.GenericPackageDescription (
    GenericPackageDescription(..),
    Flag(..),
    emptyFlag,
    FlagName,
    mkFlagName,
    unFlagName,
    FlagAssignment,
    mkFlagAssignment,
    unFlagAssignment,
    lookupFlagAssignment,
    insertFlagAssignment,
    diffFlagAssignment,
    nullFlagAssignment,
    showFlagValue,
    dispFlagAssignment,
    parseFlagAssignment,
    parsecFlagAssignment,
    ConfVar(..),
) where

import Prelude ()
import Data.List ((\\))
import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Distribution.Utils.Generic (lowercase)
import qualified Text.PrettyPrint as Disp
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.Parsec as P
import Distribution.Compat.ReadP ((+++))

import Distribution.Types.PackageDescription

import Distribution.Types.Dependency
import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.Types.Executable
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark
import Distribution.Types.UnqualComponentName
import Distribution.Types.CondTree

import Distribution.Package
import Distribution.Version
import Distribution.Compiler
import Distribution.System
import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

-- ---------------------------------------------------------------------------
-- The GenericPackageDescription type

data GenericPackageDescription =
    GenericPackageDescription {
        packageDescription :: PackageDescription,
        genPackageFlags    :: [Flag],
        condLibrary        :: Maybe (CondTree ConfVar [Dependency] Library),
        condSubLibraries   :: [(UnqualComponentName, CondTree ConfVar [Dependency] Library)],
        condForeignLibs    :: [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)],
        condExecutables    :: [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)],
        condTestSuites     :: [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)],
        condBenchmarks     :: [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)]
      }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Package GenericPackageDescription where
  packageId = packageId . packageDescription

instance Binary GenericPackageDescription

-- | A flag can represent a feature to be included, or a way of linking
--   a target against its dependencies, or in fact whatever you can think of.
data Flag = MkFlag
    { flagName        :: FlagName
    , flagDescription :: String
    , flagDefault     :: Bool
    , flagManual      :: Bool
    }
    deriving (Show, Eq, Typeable, Data, Generic)

instance Binary Flag

-- | A 'Flag' initialized with default parameters.
emptyFlag :: FlagName -> Flag
emptyFlag name = MkFlag
    { flagName        = name
    , flagDescription = ""
    , flagDefault     = True
    , flagManual      = False
    }

-- | A 'FlagName' is the name of a user-defined configuration flag
--
-- Use 'mkFlagName' and 'unFlagName' to convert from/to a 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
newtype FlagName = FlagName ShortText
    deriving (Eq, Generic, Ord, Show, Read, Typeable, Data)

-- | Construct a 'FlagName' from a 'String'
--
-- 'mkFlagName' is the inverse to 'unFlagName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'FlagName' is valid
--
-- @since 2.0.0.2
mkFlagName :: String -> FlagName
mkFlagName = FlagName . toShortText

-- | 'mkFlagName'
--
-- @since 2.0.0.2
instance IsString FlagName where
    fromString = mkFlagName

-- | Convert 'FlagName' to 'String'
--
-- @since 2.0.0.2
unFlagName :: FlagName -> String
unFlagName (FlagName s) = fromShortText s

instance Binary FlagName

instance Pretty FlagName where
    pretty = Disp.text . unFlagName

instance Parsec FlagName where
    parsec = mkFlagName . lowercase <$> parsec'
      where
        parsec' = (:) <$> lead <*> rest
        lead = P.satisfy (\c ->  isAlphaNum c || c == '_')
        rest = P.munch (\c -> isAlphaNum c ||  c == '_' || c == '-')

instance Text FlagName where
    -- Note:  we don't check that FlagName doesn't have leading dash,
    -- cabal check will do that.
    parse = mkFlagName . lowercase <$> parse'
      where
        parse' = (:) <$> lead <*> rest
        lead = Parse.satisfy (\c ->  isAlphaNum c || c == '_')
        rest = Parse.munch (\c -> isAlphaNum c ||  c == '_' || c == '-')

-- | A 'FlagAssignment' is a total or partial mapping of 'FlagName's to
-- 'Bool' flag values. It represents the flags chosen by the user or
-- discovered during configuration. For example @--flags=foo --flags=-bar@
-- becomes @[("foo", True), ("bar", False)]@
--
newtype FlagAssignment = FlagAssignment [(FlagName, Bool)]
    deriving (Binary,Eq,Ord,Semigroup,Monoid)

-- TODO: the Semigroup/Monoid/Ord/Eq instances would benefit from
-- [(FlagName,Bool)] being in a normal form, i.e. sorted. We could
-- e.g.  switch to a `Data.Map.Map` representation, but see duplicates
-- check in `configuredPackageProblems`.
--
-- Also, the 'Semigroup' instance currently is left-biased as entries
-- in the left-hand 'FlagAssignment' shadow those occuring in the
-- right-hand side 'FlagAssignment' for the same flagnames.

-- | Construct a 'FlagAssignment' from a list of flag/value pairs.
--
-- @since 2.2.0
mkFlagAssignment :: [(FlagName, Bool)] -> FlagAssignment
mkFlagAssignment = FlagAssignment

-- | Deconstruct a 'FlagAssignment' into a list of flag/value pairs.
--
-- @ ('mkFlagAssignment' . 'unFlagAssignment') fa == fa @
--
-- @since 2.2.0
unFlagAssignment :: FlagAssignment -> [(FlagName, Bool)]
unFlagAssignment (FlagAssignment xs) = xs

-- | Test whether 'FlagAssignment' is empty.
--
-- @since 2.2.0
nullFlagAssignment :: FlagAssignment -> Bool
nullFlagAssignment (FlagAssignment []) = True
nullFlagAssignment _                   = False

-- | Lookup the value for a flag
--
-- Returns 'Nothing' if the flag isn't contained in the 'FlagAssignment'.
--
-- @since 2.2.0
lookupFlagAssignment :: FlagName -> FlagAssignment -> Maybe Bool
lookupFlagAssignment fn = lookup fn . unFlagAssignment

-- | Insert or update the boolean value of a flag.
--
-- @since 2.2.0
insertFlagAssignment :: FlagName -> Bool -> FlagAssignment -> FlagAssignment
-- TODO: this currently just shadows prior values for an existing flag;
-- rather than enforcing uniqueness at construction, it's verified lateron via
-- `D.C.Dependency.configuredPackageProblems`
insertFlagAssignment flag val = mkFlagAssignment . ((flag,val):) . unFlagAssignment

-- | Remove all flag-assignments from the first 'FlagAssignment' that
-- are contained in the second 'FlagAssignment'
--
-- NB/TODO: This currently only removes flag assignments which also
-- match the value assignment! We should review the code which uses
-- this operation to figure out if this it's not enough to only
-- compare the flagnames without the values.
--
-- @since 2.2.0
diffFlagAssignment :: FlagAssignment -> FlagAssignment -> FlagAssignment
diffFlagAssignment fa1 fa2 = mkFlagAssignment (unFlagAssignment fa1 \\ unFlagAssignment fa2)

-- | @since 2.2.0
instance Read FlagAssignment where
    readsPrec p s = [ (FlagAssignment x, rest) | (x,rest) <- readsPrec p s ]

-- | @since 2.2.0
instance Show FlagAssignment where
    showsPrec p (FlagAssignment xs) = showsPrec p xs

-- | String representation of a flag-value pair.
showFlagValue :: (FlagName, Bool) -> String
showFlagValue (f, True)   = '+' : unFlagName f
showFlagValue (f, False)  = '-' : unFlagName f

-- | Pretty-prints a flag assignment.
dispFlagAssignment :: FlagAssignment -> Disp.Doc
dispFlagAssignment = Disp.hsep . map (Disp.text . showFlagValue) . unFlagAssignment

-- | Parses a flag assignment.
parsecFlagAssignment :: ParsecParser FlagAssignment
parsecFlagAssignment = FlagAssignment <$> P.sepBy (onFlag <|> offFlag) P.skipSpaces1
  where
    onFlag = do
        P.optional (P.char '+')
        f <- parsec
        return (f, True)
    offFlag = do
        _ <- P.char '-'
        f <- parsec
        return (f, False)

-- | Parses a flag assignment.
parseFlagAssignment :: Parse.ReadP r FlagAssignment
parseFlagAssignment = FlagAssignment <$> Parse.sepBy parseFlagValue Parse.skipSpaces1
  where
    parseFlagValue =
          (do Parse.optional (Parse.char '+')
              f <- parse
              return (f, True))
      +++ (do _ <- Parse.char '-'
              f <- parse
              return (f, False))
-- {-# DEPRECATED parseFlagAssignment "Use parsecFlagAssignment" #-}

-- | A @ConfVar@ represents the variable type used.
data ConfVar = OS OS
             | Arch Arch
             | Flag FlagName
             | Impl CompilerFlavor VersionRange
    deriving (Eq, Show, Typeable, Data, Generic)

instance Binary ConfVar
