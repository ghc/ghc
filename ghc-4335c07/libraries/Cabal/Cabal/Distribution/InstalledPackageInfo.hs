{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.InstalledPackageInfo
-- Copyright   :  (c) The University of Glasgow 2004
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- This is the information about an /installed/ package that
-- is communicated to the @ghc-pkg@ program in order to register
-- a package.  @ghc-pkg@ now consumes this package format (as of version
-- 6.4). This is specific to GHC at the moment.
--
-- The @.cabal@ file format is for describing a package that is not yet
-- installed. It has a lot of flexibility, like conditionals and dependency
-- ranges. As such, that format is not at all suitable for describing a package
-- that has already been built and installed. By the time we get to that stage,
-- we have resolved all conditionals and resolved dependency version
-- constraints to exact versions of dependent packages. So, this module defines
-- the 'InstalledPackageInfo' data structure that contains all the info we keep
-- about an installed package. There is a parser and pretty printer. The
-- textual format is rather simpler than the @.cabal@ format: there are no
-- sections, for example.

-- This module is meant to be local-only to Distribution...

module Distribution.InstalledPackageInfo (
        InstalledPackageInfo(..),
        installedPackageId,
        installedComponentId,
        installedOpenUnitId,
        sourceComponentName,
        requiredSignatures,
        ExposedModule(..),
        AbiDependency(..),
        ParseResult(..), PError(..), PWarning,
        emptyInstalledPackageInfo,
        parseInstalledPackageInfo,
        showInstalledPackageInfo,
        showInstalledPackageInfoField,
        showSimpleInstalledPackageInfoField,
        fieldsInstalledPackageInfo,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.ParseUtils
import Distribution.License
import Distribution.Package hiding (installedUnitId, installedPackageId)
import Distribution.Backpack
import qualified Distribution.Package as Package
import Distribution.ModuleName
import Distribution.Version
import Distribution.Text
import qualified Distribution.Compat.ReadP as Parse
import Distribution.Compat.Graph
import Distribution.Types.MungedPackageId
import Distribution.Types.ComponentName
import Distribution.Types.MungedPackageName
import Distribution.Types.UnqualComponentName

import Text.PrettyPrint as Disp
import qualified Data.Char as Char
import qualified Data.Map as Map
import Data.Set (Set)

-- -----------------------------------------------------------------------------
-- The InstalledPackageInfo type

-- For BC reasons, we continue to name this record an InstalledPackageInfo;
-- but it would more accurately be called an InstalledUnitInfo with Backpack
data InstalledPackageInfo
   = InstalledPackageInfo {
        -- these parts are exactly the same as PackageDescription
        sourcePackageId   :: PackageId,
        installedUnitId   :: UnitId,
        installedComponentId_ :: ComponentId,
        -- INVARIANT: if this package is definite, OpenModule's
        -- OpenUnitId directly records UnitId.  If it is
        -- indefinite, OpenModule is always an OpenModuleVar
        -- with the same ModuleName as the key.
        instantiatedWith  :: [(ModuleName, OpenModule)],
        sourceLibName     :: Maybe UnqualComponentName,
        compatPackageKey  :: String,
        license           :: License,
        copyright         :: String,
        maintainer        :: String,
        author            :: String,
        stability         :: String,
        homepage          :: String,
        pkgUrl            :: String,
        synopsis          :: String,
        description       :: String,
        category          :: String,
        -- these parts are required by an installed package only:
        abiHash           :: AbiHash,
        indefinite        :: Bool,
        exposed           :: Bool,
        -- INVARIANT: if the package is definite, OpenModule's
        -- OpenUnitId directly records UnitId.
        exposedModules    :: [ExposedModule],
        hiddenModules     :: [ModuleName],
        trusted           :: Bool,
        importDirs        :: [FilePath],
        libraryDirs       :: [FilePath],
        libraryDynDirs    :: [FilePath],  -- ^ overrides 'libraryDirs'
        dataDir           :: FilePath,
        hsLibraries       :: [String],
        extraLibraries    :: [String],
        extraGHCiLibraries:: [String],    -- overrides extraLibraries for GHCi
        includeDirs       :: [FilePath],
        includes          :: [String],
        -- INVARIANT: if the package is definite, UnitId is NOT
        -- a ComponentId of an indefinite package
        depends           :: [UnitId],
        abiDepends        :: [AbiDependency],
        ccOptions         :: [String],
        ldOptions         :: [String],
        frameworkDirs     :: [FilePath],
        frameworks        :: [String],
        haddockInterfaces :: [FilePath],
        haddockHTMLs      :: [FilePath],
        pkgRoot           :: Maybe FilePath
    }
    deriving (Eq, Generic, Typeable, Read, Show)

installedComponentId :: InstalledPackageInfo -> ComponentId
installedComponentId ipi =
    case unComponentId (installedComponentId_ ipi) of
        "" -> mkComponentId (unUnitId (installedUnitId ipi))
        _  -> installedComponentId_ ipi

-- | Get the indefinite unit identity representing this package.
-- This IS NOT guaranteed to give you a substitution; for
-- instantiated packages you will get @DefiniteUnitId (installedUnitId ipi)@.
-- For indefinite libraries, however, you will correctly get
-- an @OpenUnitId@ with the appropriate 'OpenModuleSubst'.
installedOpenUnitId :: InstalledPackageInfo -> OpenUnitId
installedOpenUnitId ipi
    = mkOpenUnitId (installedUnitId ipi) (installedComponentId ipi) (Map.fromList (instantiatedWith ipi))

-- | Returns the set of module names which need to be filled for
-- an indefinite package, or the empty set if the package is definite.
requiredSignatures :: InstalledPackageInfo -> Set ModuleName
requiredSignatures ipi = openModuleSubstFreeHoles (Map.fromList (instantiatedWith ipi))

{-# DEPRECATED installedPackageId "Use installedUnitId instead" #-}
-- | Backwards compatibility with Cabal pre-1.24.
--
-- This type synonym is slightly awful because in cabal-install
-- we define an 'InstalledPackageId' but it's a ComponentId,
-- not a UnitId!
installedPackageId :: InstalledPackageInfo -> UnitId
installedPackageId = installedUnitId

instance Binary InstalledPackageInfo

instance Package.HasMungedPackageId InstalledPackageInfo where
   mungedId = mungedPackageId

instance Package.Package InstalledPackageInfo where
   packageId = sourcePackageId

instance Package.HasUnitId InstalledPackageInfo where
   installedUnitId = installedUnitId

instance Package.PackageInstalled InstalledPackageInfo where
   installedDepends = depends

instance IsNode InstalledPackageInfo where
    type Key InstalledPackageInfo = UnitId
    nodeKey       = installedUnitId
    nodeNeighbors = depends

emptyInstalledPackageInfo :: InstalledPackageInfo
emptyInstalledPackageInfo
   = InstalledPackageInfo {
        sourcePackageId   = PackageIdentifier (mkPackageName "") nullVersion,
        installedUnitId   = mkUnitId "",
        installedComponentId_ = mkComponentId "",
        instantiatedWith  = [],
        sourceLibName     = Nothing,
        compatPackageKey  = "",
        license           = UnspecifiedLicense,
        copyright         = "",
        maintainer        = "",
        author            = "",
        stability         = "",
        homepage          = "",
        pkgUrl            = "",
        synopsis          = "",
        description       = "",
        category          = "",
        abiHash           = mkAbiHash "",
        indefinite        = False,
        exposed           = False,
        exposedModules    = [],
        hiddenModules     = [],
        trusted           = False,
        importDirs        = [],
        libraryDirs       = [],
        libraryDynDirs    = [],
        dataDir           = "",
        hsLibraries       = [],
        extraLibraries    = [],
        extraGHCiLibraries= [],
        includeDirs       = [],
        includes          = [],
        depends           = [],
        abiDepends        = [],
        ccOptions         = [],
        ldOptions         = [],
        frameworkDirs     = [],
        frameworks        = [],
        haddockInterfaces = [],
        haddockHTMLs      = [],
        pkgRoot           = Nothing
    }

-- -----------------------------------------------------------------------------
-- Exposed modules

data ExposedModule
   = ExposedModule {
       exposedName      :: ModuleName,
       exposedReexport  :: Maybe OpenModule
     }
  deriving (Eq, Generic, Read, Show)

instance Text ExposedModule where
    disp (ExposedModule m reexport) =
        Disp.hsep [ disp m
                  , case reexport of
                     Just m' -> Disp.hsep [Disp.text "from", disp m']
                     Nothing -> Disp.empty
                  ]
    parse = do
        m <- parseModuleNameQ
        Parse.skipSpaces
        reexport <- Parse.option Nothing $ do
            _ <- Parse.string "from"
            Parse.skipSpaces
            fmap Just parse
        return (ExposedModule m reexport)

instance Binary ExposedModule

-- To maintain backwards-compatibility, we accept both comma/non-comma
-- separated variants of this field.  You SHOULD use the comma syntax if you
-- use any new functions, although actually it's unambiguous due to a quirk
-- of the fact that modules must start with capital letters.

showExposedModules :: [ExposedModule] -> Disp.Doc
showExposedModules xs
    | all isExposedModule xs = fsep (map disp xs)
    | otherwise = fsep (Disp.punctuate comma (map disp xs))
    where isExposedModule (ExposedModule _ Nothing) = True
          isExposedModule _ = False

parseExposedModules :: Parse.ReadP r [ExposedModule]
parseExposedModules = parseOptCommaList parse

dispMaybe :: Text a => Maybe a -> Disp.Doc
dispMaybe Nothing = Disp.empty
dispMaybe (Just x) = disp x

parseMaybe :: Text a => Parse.ReadP r (Maybe a)
parseMaybe = fmap Just parse Parse.<++ return Nothing

-- -----------------------------------------------------------------------------
-- ABI dependency

-- | An ABI dependency is a dependency on a library which also
-- records the ABI hash ('abiHash') of the library it depends
-- on.
--
-- The primary utility of this is to enable an extra sanity when
-- GHC loads libraries: it can check if the dependency has a matching
-- ABI and if not, refuse to load this library.  This information
-- is critical if we are shadowing libraries; differences in the
-- ABI hash let us know what packages get shadowed by the new version
-- of a package.
data AbiDependency = AbiDependency {
        depUnitId  :: UnitId,
        depAbiHash :: AbiHash
    }
  deriving (Eq, Generic, Read, Show)

instance Text AbiDependency where
    disp (AbiDependency uid abi) =
        disp uid <<>> Disp.char '=' <<>> disp abi
    parse = do
        uid <- parse
        _ <- Parse.char '='
        abi <- parse
        return (AbiDependency uid abi)

instance Binary AbiDependency

-- -----------------------------------------------------------------------------
-- Munging

sourceComponentName :: InstalledPackageInfo -> ComponentName
sourceComponentName ipi =
    case sourceLibName ipi of
        Nothing -> CLibName
        Just qn -> CSubLibName qn

-- | Returns @Just@ if the @name@ field of the IPI record would not contain
-- the package name verbatim.  This helps us avoid writing @package-name@
-- when it's redundant.
maybePackageName :: InstalledPackageInfo -> Maybe PackageName
maybePackageName ipi =
    case sourceLibName ipi of
        Nothing -> Nothing
        Just _ -> Just (packageName ipi)

-- | Setter for the @package-name@ field.  It should be acceptable for this
-- to be a no-op.
setMaybePackageName :: Maybe PackageName -> InstalledPackageInfo -> InstalledPackageInfo
setMaybePackageName Nothing ipi = ipi
setMaybePackageName (Just pn) ipi = ipi {
        sourcePackageId=(sourcePackageId ipi){pkgName=pn}
    }

-- | Returns the munged package name, which we write into @name@ for
-- compatibility with old versions of GHC.
mungedPackageName :: InstalledPackageInfo -> MungedPackageName
mungedPackageName ipi =
    computeCompatPackageName
        (packageName ipi)
        (sourceLibName ipi)

setMungedPackageName :: MungedPackageName -> InstalledPackageInfo -> InstalledPackageInfo
setMungedPackageName mpn ipi =
    let (pn, mb_uqn) = decodeCompatPackageName mpn
    in ipi {
            sourcePackageId = (sourcePackageId ipi) {pkgName=pn},
            sourceLibName   = mb_uqn
        }

mungedPackageId :: InstalledPackageInfo -> MungedPackageId
mungedPackageId ipi =
    MungedPackageId (mungedPackageName ipi) (packageVersion ipi)

-- -----------------------------------------------------------------------------
-- Parsing

parseInstalledPackageInfo :: String -> ParseResult InstalledPackageInfo
parseInstalledPackageInfo =
    parseFieldsFlat (fieldsInstalledPackageInfo ++ deprecatedFieldDescrs)
    emptyInstalledPackageInfo

-- -----------------------------------------------------------------------------
-- Pretty-printing

showInstalledPackageInfo :: InstalledPackageInfo -> String
showInstalledPackageInfo = showFields fieldsInstalledPackageInfo

showInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showInstalledPackageInfoField = showSingleNamedField fieldsInstalledPackageInfo

showSimpleInstalledPackageInfoField :: String -> Maybe (InstalledPackageInfo -> String)
showSimpleInstalledPackageInfoField = showSimpleSingleNamedField fieldsInstalledPackageInfo

dispCompatPackageKey :: String -> Doc
dispCompatPackageKey = text

parseCompatPackageKey :: Parse.ReadP r String
parseCompatPackageKey = Parse.munch1 uid_char
    where uid_char c = Char.isAlphaNum c || c `elem` "-_.=[],:<>+"

-- -----------------------------------------------------------------------------
-- Description of the fields, for parsing/printing

fieldsInstalledPackageInfo :: [FieldDescr InstalledPackageInfo]
fieldsInstalledPackageInfo = basicFieldDescrs ++ installedFieldDescrs

basicFieldDescrs :: [FieldDescr InstalledPackageInfo]
basicFieldDescrs =
 [ simpleField "name"
                           disp                   (parseMaybeQuoted parse)
                           mungedPackageName      setMungedPackageName
 , simpleField "version"
                           disp                   parseOptVersion
                           packageVersion         (\ver pkg -> pkg{sourcePackageId=(sourcePackageId pkg){pkgVersion=ver}})
 , simpleField "id"
                           disp                   parse
                           installedUnitId        (\pk pkg -> pkg{installedUnitId=pk})
 , simpleField "instantiated-with"
        (dispOpenModuleSubst . Map.fromList)    (fmap Map.toList parseOpenModuleSubst)
        instantiatedWith   (\iw    pkg -> pkg{instantiatedWith=iw})
 , simpleField "package-name"
                           dispMaybe              parseMaybe
                           maybePackageName       setMaybePackageName
 , simpleField "lib-name"
                           dispMaybe              parseMaybe
                           sourceLibName          (\n pkg -> pkg{sourceLibName=n})
 , simpleField "key"
                           dispCompatPackageKey   parseCompatPackageKey
                           compatPackageKey       (\pk pkg -> pkg{compatPackageKey=pk})
 , simpleField "license"
                           disp                   parseLicenseQ
                           license                (\l pkg -> pkg{license=l})
 , simpleField "copyright"
                           showFreeText           parseFreeText
                           copyright              (\val pkg -> pkg{copyright=val})
 , simpleField "maintainer"
                           showFreeText           parseFreeText
                           maintainer             (\val pkg -> pkg{maintainer=val})
 , simpleField "stability"
                           showFreeText           parseFreeText
                           stability              (\val pkg -> pkg{stability=val})
 , simpleField "homepage"
                           showFreeText           parseFreeText
                           homepage               (\val pkg -> pkg{homepage=val})
 , simpleField "package-url"
                           showFreeText           parseFreeText
                           pkgUrl                 (\val pkg -> pkg{pkgUrl=val})
 , simpleField "synopsis"
                           showFreeText           parseFreeText
                           synopsis               (\val pkg -> pkg{synopsis=val})
 , simpleField "description"
                           showFreeText           parseFreeText
                           description            (\val pkg -> pkg{description=val})
 , simpleField "category"
                           showFreeText           parseFreeText
                           category               (\val pkg -> pkg{category=val})
 , simpleField "author"
                           showFreeText           parseFreeText
                           author                 (\val pkg -> pkg{author=val})
 ]

installedFieldDescrs :: [FieldDescr InstalledPackageInfo]
installedFieldDescrs = [
   boolField "exposed"
        exposed            (\val pkg -> pkg{exposed=val})
 , boolField "indefinite"
        indefinite         (\val pkg -> pkg{indefinite=val})
 , simpleField "exposed-modules"
        showExposedModules parseExposedModules
        exposedModules     (\xs    pkg -> pkg{exposedModules=xs})
 , listField   "hidden-modules"
        disp               parseModuleNameQ
        hiddenModules      (\xs    pkg -> pkg{hiddenModules=xs})
 , simpleField "abi"
        disp               parse
        abiHash            (\abi    pkg -> pkg{abiHash=abi})
 , boolField   "trusted"
        trusted            (\val pkg -> pkg{trusted=val})
 , listField   "import-dirs"
        showFilePath       parseFilePathQ
        importDirs         (\xs pkg -> pkg{importDirs=xs})
 , listField   "library-dirs"
        showFilePath       parseFilePathQ
        libraryDirs        (\xs pkg -> pkg{libraryDirs=xs})
 , listField   "dynamic-library-dirs"
        showFilePath       parseFilePathQ
        libraryDynDirs     (\xs pkg -> pkg{libraryDynDirs=xs})
 , simpleField "data-dir"
        showFilePath       (parseFilePathQ Parse.<++ return "")
        dataDir            (\val pkg -> pkg{dataDir=val})
 , listField   "hs-libraries"
        showFilePath       parseTokenQ
        hsLibraries        (\xs pkg -> pkg{hsLibraries=xs})
 , listField   "extra-libraries"
        showToken          parseTokenQ
        extraLibraries     (\xs pkg -> pkg{extraLibraries=xs})
 , listField   "extra-ghci-libraries"
        showToken          parseTokenQ
        extraGHCiLibraries (\xs pkg -> pkg{extraGHCiLibraries=xs})
 , listField   "include-dirs"
        showFilePath       parseFilePathQ
        includeDirs        (\xs pkg -> pkg{includeDirs=xs})
 , listField   "includes"
        showFilePath       parseFilePathQ
        includes           (\xs pkg -> pkg{includes=xs})
 , listField   "depends"
        disp               parse
        depends            (\xs pkg -> pkg{depends=xs})
 , listField   "abi-depends"
        disp               parse
        abiDepends         (\xs pkg -> pkg{abiDepends=xs})
 , listField   "cc-options"
        showToken          parseTokenQ
        ccOptions          (\path  pkg -> pkg{ccOptions=path})
 , listField   "ld-options"
        showToken          parseTokenQ
        ldOptions          (\path  pkg -> pkg{ldOptions=path})
 , listField   "framework-dirs"
        showFilePath       parseFilePathQ
        frameworkDirs      (\xs pkg -> pkg{frameworkDirs=xs})
 , listField   "frameworks"
        showToken          parseTokenQ
        frameworks         (\xs pkg -> pkg{frameworks=xs})
 , listField   "haddock-interfaces"
        showFilePath       parseFilePathQ
        haddockInterfaces  (\xs pkg -> pkg{haddockInterfaces=xs})
 , listField   "haddock-html"
        showFilePath       parseFilePathQ
        haddockHTMLs       (\xs pkg -> pkg{haddockHTMLs=xs})
 , simpleField "pkgroot"
        (const Disp.empty)        parseFilePathQ
        (fromMaybe "" . pkgRoot)  (\xs pkg -> pkg{pkgRoot=Just xs})
 ]

deprecatedFieldDescrs :: [FieldDescr InstalledPackageInfo]
deprecatedFieldDescrs = [
   listField   "hugs-options"
        showToken          parseTokenQ
        (const [])        (const id)
  ]
