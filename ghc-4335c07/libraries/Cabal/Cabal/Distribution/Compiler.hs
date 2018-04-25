{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Compiler
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This has an enumeration of the various compilers that Cabal knows about. It
-- also specifies the default compiler. Sadly you'll often see code that does
-- case analysis on this compiler flavour enumeration like:
--
-- > case compilerFlavor comp of
-- >   GHC -> GHC.getInstalledPackages verbosity packageDb progdb
-- >   JHC -> JHC.getInstalledPackages verbosity packageDb progdb
--
-- Obviously it would be better to use the proper 'Compiler' abstraction
-- because that would keep all the compiler-specific code together.
-- Unfortunately we cannot make this change yet without breaking the
-- 'UserHooks' api, which would break all custom @Setup.hs@ files, so for the
-- moment we just have to live with this deficiency. If you're interested, see
-- ticket #57.

module Distribution.Compiler (
  -- * Compiler flavor
  CompilerFlavor(..),
  buildCompilerId,
  buildCompilerFlavor,
  defaultCompilerFlavor,
  parseCompilerFlavorCompat,
  classifyCompilerFlavor,

  -- * Compiler id
  CompilerId(..),

  -- * Compiler info
  CompilerInfo(..),
  unknownCompilerInfo,
  AbiTag(..), abiTagString
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Language.Haskell.Extension

import Distribution.Version (Version, mkVersion', nullVersion)

import qualified System.Info (compilerName, compilerVersion)
import Distribution.Parsec.Class (Parsec (..))
import Distribution.Pretty (Pretty (..))
import Distribution.Text (Text(..), display)
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.Compat.Parsec as P
import qualified Text.PrettyPrint as Disp

data CompilerFlavor =
  GHC | GHCJS | NHC | YHC | Hugs | HBC | Helium | JHC | LHC | UHC
  | HaskellSuite String -- string is the id of the actual compiler
  | OtherCompiler String
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary CompilerFlavor

knownCompilerFlavors :: [CompilerFlavor]
knownCompilerFlavors = [GHC, GHCJS, NHC, YHC, Hugs, HBC, Helium, JHC, LHC, UHC]

instance Pretty CompilerFlavor where
  pretty (OtherCompiler name) = Disp.text name
  pretty (HaskellSuite name)  = Disp.text name
  pretty NHC                  = Disp.text "nhc98"
  pretty other                = Disp.text (lowercase (show other))

instance Parsec CompilerFlavor where
    parsec = classifyCompilerFlavor <$> component
      where
        component = do
          cs <- P.munch1 isAlphaNum
          if all isDigit cs then fail "all digits compiler name" else return cs

instance Text CompilerFlavor where
  parse = do
    comp <- Parse.munch1 isAlphaNum
    when (all isDigit comp) Parse.pfail
    return (classifyCompilerFlavor comp)

classifyCompilerFlavor :: String -> CompilerFlavor
classifyCompilerFlavor s =
  fromMaybe (OtherCompiler s) $ lookup (lowercase s) compilerMap
  where
    compilerMap = [ (lowercase (display compiler), compiler)
                  | compiler <- knownCompilerFlavors ]


--TODO: In some future release, remove 'parseCompilerFlavorCompat' and use
-- ordinary 'parse'. Also add ("nhc", NHC) to the above 'compilerMap'.

-- | Like 'classifyCompilerFlavor' but compatible with the old ReadS parser.
--
-- It is compatible in the sense that it accepts only the same strings,
-- eg "GHC" but not "ghc". However other strings get mapped to 'OtherCompiler'.
-- The point of this is that we do not allow extra valid values that would
-- upset older Cabal versions that had a stricter parser however we cope with
-- new values more gracefully so that we'll be able to introduce new value in
-- future without breaking things so much.
--
parseCompilerFlavorCompat :: Parse.ReadP r CompilerFlavor
parseCompilerFlavorCompat = do
  comp <- Parse.munch1 isAlphaNum
  when (all isDigit comp) Parse.pfail
  case lookup comp compilerMap of
    Just compiler -> return compiler
    Nothing       -> return (OtherCompiler comp)
  where
    compilerMap = [ (show compiler, compiler)
                  | compiler <- knownCompilerFlavors
                  , compiler /= YHC ]

buildCompilerFlavor :: CompilerFlavor
buildCompilerFlavor = classifyCompilerFlavor System.Info.compilerName

buildCompilerVersion :: Version
buildCompilerVersion = mkVersion' System.Info.compilerVersion

buildCompilerId :: CompilerId
buildCompilerId = CompilerId buildCompilerFlavor buildCompilerVersion

-- | The default compiler flavour to pick when compiling stuff. This defaults
-- to the compiler used to build the Cabal lib.
--
-- However if it's not a recognised compiler then it's 'Nothing' and the user
-- will have to specify which compiler they want.
--
defaultCompilerFlavor :: Maybe CompilerFlavor
defaultCompilerFlavor = case buildCompilerFlavor of
  OtherCompiler _ -> Nothing
  _               -> Just buildCompilerFlavor

-- ------------------------------------------------------------
-- * Compiler Id
-- ------------------------------------------------------------

data CompilerId = CompilerId CompilerFlavor Version
  deriving (Eq, Generic, Ord, Read, Show)

instance Binary CompilerId

instance Text CompilerId where
  disp (CompilerId f v)
    | v == nullVersion = disp f
    | otherwise        = disp f <<>> Disp.char '-' <<>> disp v

  parse = do
    flavour <- parse
    version <- (Parse.char '-' >> parse) Parse.<++ return nullVersion
    return (CompilerId flavour version)

lowercase :: String -> String
lowercase = map toLower

-- ------------------------------------------------------------
-- * Compiler Info
-- ------------------------------------------------------------

-- | Compiler information used for resolving configurations. Some
--   fields can be set to Nothing to indicate that the information is
--   unknown.

data CompilerInfo = CompilerInfo {
         compilerInfoId         :: CompilerId,
         -- ^ Compiler flavour and version.
         compilerInfoAbiTag     :: AbiTag,
         -- ^ Tag for distinguishing incompatible ABI's on the same
         -- architecture/os.
         compilerInfoCompat     :: Maybe [CompilerId],
         -- ^ Other implementations that this compiler claims to be
         -- compatible with, if known.
         compilerInfoLanguages  :: Maybe [Language],
         -- ^ Supported language standards, if known.
         compilerInfoExtensions :: Maybe [Extension]
         -- ^ Supported extensions, if known.
     }
     deriving (Generic, Show, Read)

instance Binary CompilerInfo

data AbiTag
  = NoAbiTag
  | AbiTag String
  deriving (Eq, Generic, Show, Read)

instance Binary AbiTag

instance Text AbiTag where
  disp NoAbiTag     = Disp.empty
  disp (AbiTag tag) = Disp.text tag

  parse = do
    tag <- Parse.munch (\c -> isAlphaNum c || c == '_')
    if null tag then return NoAbiTag else return (AbiTag tag)

abiTagString :: AbiTag -> String
abiTagString NoAbiTag     = ""
abiTagString (AbiTag tag) = tag

-- | Make a CompilerInfo of which only the known information is its CompilerId,
--   its AbiTag and that it does not claim to be compatible with other
--   compiler id's.
unknownCompilerInfo :: CompilerId -> AbiTag -> CompilerInfo
unknownCompilerInfo compilerId abiTag =
  CompilerInfo compilerId abiTag (Just []) Nothing Nothing
