{-
(c) The University of Glasgow, 2004-2006


Module
~~~~~~~~~~
Simply the name of a module, represented as a FastString.
These are Uniquable, hence we can build Maps with Modules as
the keys.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GHC.Types.Module
    (
        -- * The ModuleName type
        ModuleName,
        pprModuleName,
        moduleNameFS,
        moduleNameString,
        moduleNameSlashes, moduleNameColons,
        moduleStableString,
        moduleFreeHoles,
        moduleIsDefinite,
        mkModuleName,
        mkModuleNameFS,
        stableModuleNameCmp,

        -- * The UnitId type
        ComponentId(..),
        ComponentDetails(..),
        UnitId(..),
        unitIdFS,
        unitIdKey,
        IndefUnitId(..),
        IndefModule(..),
        indefUnitIdToUnitId,
        indefModuleToModule,
        InstalledUnitId(..),
        toInstalledUnitId,
        ShHoleSubst,

        unitIdIsDefinite,
        unitIdString,
        unitIdFreeHoles,

        newUnitId,
        newIndefUnitId,
        newSimpleUnitId,
        hashUnitId,
        fsToUnitId,
        stringToUnitId,
        stableUnitIdCmp,

        -- * HOLE renaming
        renameHoleUnitId,
        renameHoleModule,
        renameHoleUnitId',
        renameHoleModule',

        -- * Generalization
        splitModuleInsts,
        splitUnitIdInsts,
        generalizeIndefUnitId,
        generalizeIndefModule,

        -- * Parsers
        parseModuleName,
        parseUnitId,
        parseComponentId,
        parseModuleId,
        parseModSubst,

        -- * Wired-in UnitIds
        -- $wired_in_packages
        primUnitId,
        integerUnitId,
        baseUnitId,
        rtsUnitId,
        thUnitId,
        mainUnitId,
        thisGhcUnitId,
        isHoleModule,
        interactiveUnitId, isInteractiveModule,
        wiredInUnitIds,

        -- * The Module type
        Module(Module),
        moduleUnitId, moduleName,
        pprModule,
        mkModule,
        mkHoleModule,
        stableModuleCmp,
        HasModule(..),
        ContainsModule(..),

        -- * Installed unit ids and modules
        InstalledModule(..),
        InstalledModuleEnv,
        installedModuleEq,
        installedUnitIdEq,
        installedUnitIdString,
        fsToInstalledUnitId,
        componentIdToInstalledUnitId,
        stringToInstalledUnitId,
        emptyInstalledModuleEnv,
        lookupInstalledModuleEnv,
        extendInstalledModuleEnv,
        filterInstalledModuleEnv,
        delInstalledModuleEnv,
        DefUnitId(..),

        -- * The ModuleLocation type
        ModLocation(..),
        addBootSuffix, addBootSuffix_maybe,
        addBootSuffixLocn, addBootSuffixLocnOut,

        -- * Module mappings
        ModuleEnv,
        elemModuleEnv, extendModuleEnv, extendModuleEnvList,
        extendModuleEnvList_C, plusModuleEnv_C,
        delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv,
        lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv,
        moduleEnvKeys, moduleEnvElts, moduleEnvToList,
        unitModuleEnv, isEmptyModuleEnv,
        extendModuleEnvWith, filterModuleEnv,

        -- * ModuleName mappings
        ModuleNameEnv, DModuleNameEnv,

        -- * Sets of Modules
        ModuleSet,
        emptyModuleSet, mkModuleSet, moduleSetElts,
        extendModuleSet, extendModuleSetList, delModuleSet,
        elemModuleSet, intersectModuleSet, minusModuleSet, unionModuleSet,
        unitModuleSet
    ) where

import GhcPrelude

import Outputable
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Types.Unique.DSet
import FastString
import Binary
import Util
import Data.List (sortBy, sort)
import Data.Ord
import Data.Version
import GHC.PackageDb
import Fingerprint

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Encoding

import qualified Text.ParserCombinators.ReadP as Parse
import Text.ParserCombinators.ReadP (ReadP, (<++))
import Data.Char (isAlphaNum)
import Control.DeepSeq
import Data.Coerce
import Data.Data
import Data.Function
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified FiniteMap as Map
import System.FilePath

import {-# SOURCE #-} GHC.Driver.Session (DynFlags)
import {-# SOURCE #-} GHC.Driver.Packages (improveUnitId, componentIdString, UnitInfoMap, getUnitInfoMap, displayInstalledUnitId, getPackageState)

-- Note [The identifier lexicon]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Unit IDs, installed package IDs, ABI hashes, package names,
-- versions, there are a *lot* of different identifiers for closely
-- related things.  What do they all mean? Here's what.  (See also
-- https://gitlab.haskell.org/ghc/ghc/wikis/commentary/packages/concepts )
--
-- THE IMPORTANT ONES
--
-- ComponentId: An opaque identifier provided by Cabal, which should
-- uniquely identify such things as the package name, the package
-- version, the name of the component, the hash of the source code
-- tarball, the selected Cabal flags, GHC flags, direct dependencies of
-- the component.  These are very similar to InstalledPackageId, but
-- an 'InstalledPackageId' implies that it identifies a package, while
-- a package may install multiple components with different
-- 'ComponentId's.
--      - Same as Distribution.Package.ComponentId
--
-- UnitId/InstalledUnitId: A ComponentId + a mapping from hole names
-- (ModuleName) to Modules.  This is how the compiler identifies instantiated
-- components, and also is the main identifier by which GHC identifies things.
--      - When Backpack is not being used, UnitId = ComponentId.
--        this means a useful fiction for end-users is that there are
--        only ever ComponentIds, and some ComponentIds happen to have
--        more information (UnitIds).
--      - Same as Language.Haskell.TH.Syntax:PkgName, see
--          https://gitlab.haskell.org/ghc/ghc/issues/10279
--      - The same as PackageKey in GHC 7.10 (we renamed it because
--        they don't necessarily identify packages anymore.)
--      - Same as -this-package-key/-package-name flags
--      - An InstalledUnitId corresponds to an actual package which
--        we have installed on disk.  It could be definite or indefinite,
--        but if it's indefinite, it has nothing instantiated (we
--        never install partially instantiated units.)
--
-- Module/InstalledModule: A UnitId/InstalledUnitId + ModuleName. This is how
-- the compiler identifies modules (e.g. a Name is a Module + OccName)
--      - Same as Language.Haskell.TH.Syntax:Module
--
-- THE LESS IMPORTANT ONES
--
-- PackageName: The "name" field in a Cabal file, something like "lens".
--      - Same as Distribution.Package.PackageName
--      - DIFFERENT FROM Language.Haskell.TH.Syntax:PkgName, see
--          https://gitlab.haskell.org/ghc/ghc/issues/10279
--      - DIFFERENT FROM -package-name flag
--      - DIFFERENT FROM the 'name' field in an installed package
--        information.  This field could more accurately be described
--        as a munged package name: when it's for the main library
--        it is the same as the package name, but if it's an internal
--        library it's a munged combination of the package name and
--        the component name.
--
-- LEGACY ONES
--
-- InstalledPackageId: This is what we used to call ComponentId.
-- It's a still pretty useful concept for packages that have only
-- one library; in that case the logical InstalledPackageId =
-- ComponentId.  Also, the Cabal nix-local-build continues to
-- compute an InstalledPackageId which is then forcibly used
-- for all components in a package.  This means that if a dependency
-- from one component in a package changes, the InstalledPackageId
-- changes: you don't get as fine-grained dependency tracking,
-- but it means your builds are hermetic.  Eventually, Cabal will
-- deal completely in components and we can get rid of this.
--
-- PackageKey: This is what we used to call UnitId.  We ditched
-- "Package" from the name when we realized that you might want to
-- assign different "PackageKeys" to components from the same package.
-- (For a brief, non-released period of time, we also called these
-- UnitKeys).

{-
************************************************************************
*                                                                      *
\subsection{Module locations}
*                                                                      *
************************************************************************
-}

-- | Module Location
--
-- Where a module lives on the file system: the actual locations
-- of the .hs, .hi and .o files, if we have them
data ModLocation
   = ModLocation {
        ml_hs_file   :: Maybe FilePath,
                -- The source file, if we have one.  Package modules
                -- probably don't have source files.

        ml_hi_file   :: FilePath,
                -- Where the .hi file is, whether or not it exists
                -- yet.  Always of form foo.hi, even if there is an
                -- hi-boot file (we add the -boot suffix later)

        ml_obj_file  :: FilePath,
                -- Where the .o file is, whether or not it exists yet.
                -- (might not exist either because the module hasn't
                -- been compiled yet, or because it is part of a
                -- package with a .a file)
        ml_hie_file  :: FilePath
  } deriving Show

instance Outputable ModLocation where
   ppr = text . show

{-
For a module in another package, the hs_file and obj_file
components of ModLocation are undefined.

The locations specified by a ModLocation may or may not
correspond to actual files yet: for example, even if the object
file doesn't exist, the ModLocation still contains the path to
where the object file will reside if/when it is created.
-}

addBootSuffix :: FilePath -> FilePath
-- ^ Add the @-boot@ suffix to .hs, .hi and .o files
addBootSuffix path = path ++ "-boot"

addBootSuffix_maybe :: Bool -> FilePath -> FilePath
-- ^ Add the @-boot@ suffix if the @Bool@ argument is @True@
addBootSuffix_maybe is_boot path
 | is_boot   = addBootSuffix path
 | otherwise = path

addBootSuffixLocn :: ModLocation -> ModLocation
-- ^ Add the @-boot@ suffix to all file paths associated with the module
addBootSuffixLocn locn
  = locn { ml_hs_file  = fmap addBootSuffix (ml_hs_file locn)
         , ml_hi_file  = addBootSuffix (ml_hi_file locn)
         , ml_obj_file = addBootSuffix (ml_obj_file locn)
         , ml_hie_file = addBootSuffix (ml_hie_file locn) }

addBootSuffixLocnOut :: ModLocation -> ModLocation
-- ^ Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut locn
  = locn { ml_hi_file  = addBootSuffix (ml_hi_file locn)
         , ml_obj_file = addBootSuffix (ml_obj_file locn)
         , ml_hie_file = addBootSuffix (ml_hie_file locn) }

{-
************************************************************************
*                                                                      *
\subsection{The name of a module}
*                                                                      *
************************************************************************
-}

-- | A ModuleName is essentially a simple string, e.g. @Data.List@.
newtype ModuleName = ModuleName FastString

instance Uniquable ModuleName where
  getUnique (ModuleName nm) = getUnique nm

instance Eq ModuleName where
  nm1 == nm2 = getUnique nm1 == getUnique nm2

instance Ord ModuleName where
  nm1 `compare` nm2 = stableModuleNameCmp nm1 nm2

instance Outputable ModuleName where
  ppr = pprModuleName

instance Binary ModuleName where
  put_ bh (ModuleName fs) = put_ bh fs
  get bh = do fs <- get bh; return (ModuleName fs)

instance BinaryStringRep ModuleName where
  fromStringRep = mkModuleNameFS . mkFastStringByteString
  toStringRep   = bytesFS . moduleNameFS

instance Data ModuleName where
  -- don't traverse?
  toConstr _   = abstractConstr "ModuleName"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "ModuleName"

instance NFData ModuleName where
  rnf x = x `seq` ()

stableModuleNameCmp :: ModuleName -> ModuleName -> Ordering
-- ^ Compares module names lexically, rather than by their 'Unique's
stableModuleNameCmp n1 n2 = moduleNameFS n1 `compare` moduleNameFS n2

pprModuleName :: ModuleName -> SDoc
pprModuleName (ModuleName nm) =
    getPprStyle $ \ sty ->
    if codeStyle sty
        then ztext (zEncodeFS nm)
        else ftext nm

moduleNameFS :: ModuleName -> FastString
moduleNameFS (ModuleName mod) = mod

moduleNameString :: ModuleName -> String
moduleNameString (ModuleName mod) = unpackFS mod

-- | Get a string representation of a 'Module' that's unique and stable
-- across recompilations.
-- eg. "$aeson_70dylHtv1FFGeai1IoxcQr$Data.Aeson.Types.Internal"
moduleStableString :: Module -> String
moduleStableString Module{..} =
  "$" ++ unitIdString moduleUnitId ++ "$" ++ moduleNameString moduleName

mkModuleName :: String -> ModuleName
mkModuleName s = ModuleName (mkFastString s)

mkModuleNameFS :: FastString -> ModuleName
mkModuleNameFS s = ModuleName s

-- |Returns the string version of the module name, with dots replaced by slashes.
--
moduleNameSlashes :: ModuleName -> String
moduleNameSlashes = dots_to_slashes . moduleNameString
  where dots_to_slashes = map (\c -> if c == '.' then pathSeparator else c)

-- |Returns the string version of the module name, with dots replaced by colons.
--
moduleNameColons :: ModuleName -> String
moduleNameColons = dots_to_colons . moduleNameString
  where dots_to_colons = map (\c -> if c == '.' then ':' else c)

{-
************************************************************************
*                                                                      *
\subsection{A fully qualified module}
*                                                                      *
************************************************************************
-}

-- | A Module is a pair of a 'UnitId' and a 'ModuleName'.
--
-- Module variables (i.e. @<H>@) which can be instantiated to a
-- specific module at some later point in time are represented
-- with 'moduleUnitId' set to 'holeUnitId' (this allows us to
-- avoid having to make 'moduleUnitId' a partial operation.)
--
data Module = Module {
   moduleUnitId :: !UnitId,  -- pkg-1.0
   moduleName :: !ModuleName  -- A.B.C
  }
  deriving (Eq, Ord)

-- | Calculate the free holes of a 'Module'.  If this set is non-empty,
-- this module was defined in an indefinite library that had required
-- signatures.
--
-- If a module has free holes, that means that substitutions can operate on it;
-- if it has no free holes, substituting over a module has no effect.
moduleFreeHoles :: Module -> UniqDSet ModuleName
moduleFreeHoles m
    | isHoleModule m = unitUniqDSet (moduleName m)
    | otherwise = unitIdFreeHoles (moduleUnitId m)

-- | A 'Module' is definite if it has no free holes.
moduleIsDefinite :: Module -> Bool
moduleIsDefinite = isEmptyUniqDSet . moduleFreeHoles

-- | Create a module variable at some 'ModuleName'.
-- See Note [Representation of module/name variables]
mkHoleModule :: ModuleName -> Module
mkHoleModule = mkModule holeUnitId

instance Uniquable Module where
  getUnique (Module p n) = getUnique (unitIdFS p `appendFS` moduleNameFS n)

instance Outputable Module where
  ppr = pprModule

instance Binary Module where
  put_ bh (Module p n) = put_ bh p >> put_ bh n
  get bh = do p <- get bh; n <- get bh; return (Module p n)

instance Data Module where
  -- don't traverse?
  toConstr _   = abstractConstr "Module"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Module"

instance NFData Module where
  rnf x = x `seq` ()

-- | This gives a stable ordering, as opposed to the Ord instance which
-- gives an ordering based on the 'Unique's of the components, which may
-- not be stable from run to run of the compiler.
stableModuleCmp :: Module -> Module -> Ordering
stableModuleCmp (Module p1 n1) (Module p2 n2)
   = (p1 `stableUnitIdCmp`  p2) `thenCmp`
     (n1 `stableModuleNameCmp` n2)

mkModule :: UnitId -> ModuleName -> Module
mkModule = Module

pprModule :: Module -> SDoc
pprModule mod@(Module p n)  = getPprStyle doc
 where
  doc sty
    | codeStyle sty =
        (if p == mainUnitId
                then empty -- never qualify the main package in code
                else ztext (zEncodeFS (unitIdFS p)) <> char '_')
            <> pprModuleName n
    | qualModule sty mod =
        if isHoleModule mod
            then angleBrackets (pprModuleName n)
            else ppr (moduleUnitId mod) <> char ':' <> pprModuleName n
    | otherwise =
        pprModuleName n

class ContainsModule t where
    extractModule :: t -> Module

class HasModule m where
    getModule :: m Module

instance DbUnitIdModuleRep InstalledUnitId ComponentId UnitId ModuleName Module where
  fromDbModule (DbModule uid mod_name)  = mkModule uid mod_name
  fromDbModule (DbModuleVar mod_name)   = mkHoleModule mod_name
  fromDbUnitId (DbUnitId cid insts)     = newUnitId cid insts
  fromDbUnitId (DbInstalledUnitId iuid) = DefiniteUnitId (DefUnitId iuid)
  -- GHC never writes to the database, so it's not needed
  toDbModule = error "toDbModule: not implemented"
  toDbUnitId = error "toDbUnitId: not implemented"

{-
************************************************************************
*                                                                      *
\subsection{ComponentId}
*                                                                      *
************************************************************************
-}

-- | A 'ComponentId' consists of the package name, package version, component
-- ID, the transitive dependencies of the component, and other information to
-- uniquely identify the source code and build configuration of a component.
--
-- This used to be known as an 'InstalledPackageId', but a package can contain
-- multiple components and a 'ComponentId' uniquely identifies a component
-- within a package.  When a package only has one component, the 'ComponentId'
-- coincides with the 'InstalledPackageId'
data ComponentId = ComponentId
   { componentIdRaw     :: FastString             -- ^ Raw
   , componentIdDetails :: Maybe ComponentDetails -- ^ Cache of component details retrieved from the DB
   }

instance Eq ComponentId where
   a == b = componentIdRaw a == componentIdRaw b

instance Ord ComponentId where
   compare a b = compare (componentIdRaw a) (componentIdRaw b)

data ComponentDetails = ComponentDetails
   { componentPackageName    :: String
   , componentPackageVersion :: Version
   , componentName           :: Maybe String
   , componentSourcePkdId    :: String
   }

instance BinaryStringRep ComponentId where
  fromStringRep bs = ComponentId (mkFastStringByteString bs) Nothing
  toStringRep (ComponentId s _) = bytesFS s

instance Uniquable ComponentId where
  getUnique (ComponentId n _) = getUnique n

instance Outputable ComponentId where
  ppr cid@(ComponentId fs _) =
    getPprStyle $ \sty ->
      if debugStyle sty
         then ftext fs
         else text (componentIdString cid)



{-
************************************************************************
*                                                                      *
\subsection{UnitId}
*                                                                      *
************************************************************************
-}

-- | A unit identifier identifies a (possibly partially) instantiated
-- library.  It is primarily used as part of 'Module', which in turn
-- is used in 'Name', which is used to give names to entities when
-- typechecking.
--
-- There are two possible forms for a 'UnitId'.  It can be a
-- 'DefiniteUnitId', in which case we just have a string that uniquely
-- identifies some fully compiled, installed library we have on disk.
-- However, when we are typechecking a library with missing holes,
-- we may need to instantiate a library on the fly (in which case
-- we don't have any on-disk representation.)  In that case, you
-- have an 'IndefiniteUnitId', which explicitly records the
-- instantiation, so that we can substitute over it.
data UnitId
    = IndefiniteUnitId {-# UNPACK #-} !IndefUnitId
    |   DefiniteUnitId {-# UNPACK #-} !DefUnitId

unitIdFS :: UnitId -> FastString
unitIdFS (IndefiniteUnitId x) = indefUnitIdFS x
unitIdFS (DefiniteUnitId (DefUnitId x)) = installedUnitIdFS x

unitIdKey :: UnitId -> Unique
unitIdKey (IndefiniteUnitId x) = indefUnitIdKey x
unitIdKey (DefiniteUnitId (DefUnitId x)) = installedUnitIdKey x

-- | A unit identifier which identifies an indefinite
-- library (with holes) that has been *on-the-fly* instantiated
-- with a substitution 'indefUnitIdInsts'.  In fact, an indefinite
-- unit identifier could have no holes, but we haven't gotten
-- around to compiling the actual library yet.
--
-- An indefinite unit identifier pretty-prints to something like
-- @p[H=<H>,A=aimpl:A>]@ (@p@ is the 'ComponentId', and the
-- brackets enclose the module substitution).
data IndefUnitId
    = IndefUnitId {
        -- | A private, uniquely identifying representation of
        -- a UnitId.  This string is completely private to GHC
        -- and is just used to get a unique; in particular, we don't use it for
        -- symbols (indefinite libraries are not compiled).
        indefUnitIdFS :: FastString,
        -- | Cached unique of 'unitIdFS'.
        indefUnitIdKey :: Unique,
        -- | The component identity of the indefinite library that
        -- is being instantiated.
        indefUnitIdComponentId :: !ComponentId,
        -- | The sorted (by 'ModuleName') instantiations of this library.
        indefUnitIdInsts :: ![(ModuleName, Module)],
        -- | A cache of the free module variables of 'unitIdInsts'.
        -- This lets us efficiently tell if a 'UnitId' has been
        -- fully instantiated (free module variables are empty)
        -- and whether or not a substitution can have any effect.
        indefUnitIdFreeHoles :: UniqDSet ModuleName
    }

instance Eq IndefUnitId where
  u1 == u2 = indefUnitIdKey u1 == indefUnitIdKey u2

instance Ord IndefUnitId where
  u1 `compare` u2 = indefUnitIdFS u1 `compare` indefUnitIdFS u2

instance Binary IndefUnitId where
  put_ bh indef = do
    put_ bh (indefUnitIdComponentId indef)
    put_ bh (indefUnitIdInsts indef)
  get bh = do
    cid   <- get bh
    insts <- get bh
    let fs = hashUnitId cid insts
    return IndefUnitId {
            indefUnitIdComponentId = cid,
            indefUnitIdInsts = insts,
            indefUnitIdFreeHoles = unionManyUniqDSets (map (moduleFreeHoles.snd) insts),
            indefUnitIdFS = fs,
            indefUnitIdKey = getUnique fs
           }

-- | Create a new 'IndefUnitId' given an explicit module substitution.
newIndefUnitId :: ComponentId -> [(ModuleName, Module)] -> IndefUnitId
newIndefUnitId cid insts =
    IndefUnitId {
        indefUnitIdComponentId = cid,
        indefUnitIdInsts = sorted_insts,
        indefUnitIdFreeHoles = unionManyUniqDSets (map (moduleFreeHoles.snd) insts),
        indefUnitIdFS = fs,
        indefUnitIdKey = getUnique fs
    }
  where
     fs = hashUnitId cid sorted_insts
     sorted_insts = sortBy (stableModuleNameCmp `on` fst) insts

-- | Injects an 'IndefUnitId' (indefinite library which
-- was on-the-fly instantiated) to a 'UnitId' (either
-- an indefinite or definite library).
indefUnitIdToUnitId :: DynFlags -> IndefUnitId -> UnitId
indefUnitIdToUnitId dflags iuid =
    -- NB: suppose that we want to compare the indefinite
    -- unit id p[H=impl:H] against p+abcd (where p+abcd
    -- happens to be the existing, installed version of
    -- p[H=impl:H].  If we *only* wrap in p[H=impl:H]
    -- IndefiniteUnitId, they won't compare equal; only
    -- after improvement will the equality hold.
    improveUnitId (getUnitInfoMap dflags) $
        IndefiniteUnitId iuid

data IndefModule = IndefModule {
        indefModuleUnitId :: IndefUnitId,
        indefModuleName   :: ModuleName
    } deriving (Eq, Ord)

instance Outputable IndefModule where
  ppr (IndefModule uid m) =
    ppr uid <> char ':' <> ppr m

-- | Injects an 'IndefModule' to 'Module' (see also
-- 'indefUnitIdToUnitId'.
indefModuleToModule :: DynFlags -> IndefModule -> Module
indefModuleToModule dflags (IndefModule iuid mod_name) =
    mkModule (indefUnitIdToUnitId dflags iuid) mod_name

-- | An installed unit identifier identifies a library which has
-- been installed to the package database.  These strings are
-- provided to us via the @-this-unit-id@ flag.  The library
-- in question may be definite or indefinite; if it is indefinite,
-- none of the holes have been filled (we never install partially
-- instantiated libraries.)  Put another way, an installed unit id
-- is either fully instantiated, or not instantiated at all.
--
-- Installed unit identifiers look something like @p+af23SAj2dZ219@,
-- or maybe just @p@ if they don't use Backpack.
newtype InstalledUnitId =
    InstalledUnitId {
      -- | The full hashed unit identifier, including the component id
      -- and the hash.
      installedUnitIdFS :: FastString
    }

instance Binary InstalledUnitId where
  put_ bh (InstalledUnitId fs) = put_ bh fs
  get bh = do fs <- get bh; return (InstalledUnitId fs)

instance BinaryStringRep InstalledUnitId where
  fromStringRep bs = InstalledUnitId (mkFastStringByteString bs)
  -- GHC doesn't write to database
  toStringRep   = error "BinaryStringRep InstalledUnitId: not implemented"

instance Eq InstalledUnitId where
    uid1 == uid2 = installedUnitIdKey uid1 == installedUnitIdKey uid2

instance Ord InstalledUnitId where
    u1 `compare` u2 = installedUnitIdFS u1 `compare` installedUnitIdFS u2

instance Uniquable InstalledUnitId where
    getUnique = installedUnitIdKey

instance Outputable InstalledUnitId where
    ppr uid@(InstalledUnitId fs) =
        getPprStyle $ \sty ->
        sdocWithDynFlags $ \dflags ->
          case displayInstalledUnitId (getPackageState dflags) uid of
            Just str | not (debugStyle sty) -> text str
            _ -> ftext fs

installedUnitIdKey :: InstalledUnitId -> Unique
installedUnitIdKey = getUnique . installedUnitIdFS

-- | Lossy conversion to the on-disk 'InstalledUnitId' for a component.
toInstalledUnitId :: UnitId -> InstalledUnitId
toInstalledUnitId (DefiniteUnitId (DefUnitId iuid)) = iuid
toInstalledUnitId (IndefiniteUnitId indef) =
    componentIdToInstalledUnitId (indefUnitIdComponentId indef)

installedUnitIdString :: InstalledUnitId -> String
installedUnitIdString = unpackFS . installedUnitIdFS

instance Outputable IndefUnitId where
    ppr uid =
      -- getPprStyle $ \sty ->
      ppr cid <>
        (if not (null insts) -- pprIf
          then
            brackets (hcat
                (punctuate comma $
                    [ ppr modname <> text "=" <> ppr m
                    | (modname, m) <- insts]))
          else empty)
     where
      cid   = indefUnitIdComponentId uid
      insts = indefUnitIdInsts uid

-- | A 'InstalledModule' is a 'Module' which contains a 'InstalledUnitId'.
data InstalledModule = InstalledModule {
   installedModuleUnitId :: !InstalledUnitId,
   installedModuleName :: !ModuleName
  }
  deriving (Eq, Ord)

instance Outputable InstalledModule where
  ppr (InstalledModule p n) =
    ppr p <> char ':' <> pprModuleName n

fsToInstalledUnitId :: FastString -> InstalledUnitId
fsToInstalledUnitId fs = InstalledUnitId fs

componentIdToInstalledUnitId :: ComponentId -> InstalledUnitId
componentIdToInstalledUnitId (ComponentId fs _) = fsToInstalledUnitId fs

stringToInstalledUnitId :: String -> InstalledUnitId
stringToInstalledUnitId = fsToInstalledUnitId . mkFastString

-- | Test if a 'Module' corresponds to a given 'InstalledModule',
-- modulo instantiation.
installedModuleEq :: InstalledModule -> Module -> Bool
installedModuleEq imod mod =
    fst (splitModuleInsts mod) == imod

-- | Test if a 'UnitId' corresponds to a given 'InstalledUnitId',
-- modulo instantiation.
installedUnitIdEq :: InstalledUnitId -> UnitId -> Bool
installedUnitIdEq iuid uid =
    fst (splitUnitIdInsts uid) == iuid

-- | A 'DefUnitId' is an 'InstalledUnitId' with the invariant that
-- it only refers to a definite library; i.e., one we have generated
-- code for.
newtype DefUnitId = DefUnitId { unDefUnitId :: InstalledUnitId }
    deriving (Eq, Ord)

instance Outputable DefUnitId where
    ppr (DefUnitId uid) = ppr uid

instance Binary DefUnitId where
    put_ bh (DefUnitId uid) = put_ bh uid
    get bh = do uid <- get bh; return (DefUnitId uid)

-- | A map keyed off of 'InstalledModule'
newtype InstalledModuleEnv elt = InstalledModuleEnv (Map InstalledModule elt)

emptyInstalledModuleEnv :: InstalledModuleEnv a
emptyInstalledModuleEnv = InstalledModuleEnv Map.empty

lookupInstalledModuleEnv :: InstalledModuleEnv a -> InstalledModule -> Maybe a
lookupInstalledModuleEnv (InstalledModuleEnv e) m = Map.lookup m e

extendInstalledModuleEnv :: InstalledModuleEnv a -> InstalledModule -> a -> InstalledModuleEnv a
extendInstalledModuleEnv (InstalledModuleEnv e) m x = InstalledModuleEnv (Map.insert m x e)

filterInstalledModuleEnv :: (InstalledModule -> a -> Bool) -> InstalledModuleEnv a -> InstalledModuleEnv a
filterInstalledModuleEnv f (InstalledModuleEnv e) =
  InstalledModuleEnv (Map.filterWithKey f e)

delInstalledModuleEnv :: InstalledModuleEnv a -> InstalledModule -> InstalledModuleEnv a
delInstalledModuleEnv (InstalledModuleEnv e) m = InstalledModuleEnv (Map.delete m e)

-- Note [UnitId to InstalledUnitId improvement]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Just because a UnitId is definite (has no holes) doesn't
-- mean it's necessarily a InstalledUnitId; it could just be
-- that over the course of renaming UnitIds on the fly
-- while typechecking an indefinite library, we
-- ended up with a fully instantiated unit id with no hash,
-- since we haven't built it yet.  This is fine.
--
-- However, if there is a hashed unit id for this instantiation
-- in the package database, we *better use it*, because
-- that hashed unit id may be lurking in another interface,
-- and chaos will ensue if we attempt to compare the two
-- (the unitIdFS for a UnitId never corresponds to a Cabal-provided
-- hash of a compiled instantiated library).
--
-- There is one last niggle: improvement based on the package database means
-- that we might end up developing on a package that is not transitively
-- depended upon by the packages the user specified directly via command line
-- flags.  This could lead to strange and difficult to understand bugs if those
-- instantiations are out of date.  The solution is to only improve a
-- unit id if the new unit id is part of the 'preloadClosure'; i.e., the
-- closure of all the packages which were explicitly specified.

-- | Retrieve the set of free holes of a 'UnitId'.
unitIdFreeHoles :: UnitId -> UniqDSet ModuleName
unitIdFreeHoles (IndefiniteUnitId x) = indefUnitIdFreeHoles x
-- Hashed unit ids are always fully instantiated
unitIdFreeHoles (DefiniteUnitId _) = emptyUniqDSet

instance Show UnitId where
    show = unitIdString

-- | A 'UnitId' is definite if it has no free holes.
unitIdIsDefinite :: UnitId -> Bool
unitIdIsDefinite = isEmptyUniqDSet . unitIdFreeHoles

-- | Generate a uniquely identifying 'FastString' for a unit
-- identifier.  This is a one-way function.  You can rely on one special
-- property: if a unit identifier is in most general form, its 'FastString'
-- coincides with its 'ComponentId'.  This hash is completely internal
-- to GHC and is not used for symbol names or file paths.
hashUnitId :: ComponentId -> [(ModuleName, Module)] -> FastString
hashUnitId cid sorted_holes =
    mkFastStringByteString
  . fingerprintUnitId (toStringRep cid)
  $ rawHashUnitId sorted_holes

-- | Generate a hash for a sorted module substitution.
rawHashUnitId :: [(ModuleName, Module)] -> Fingerprint
rawHashUnitId sorted_holes =
    fingerprintByteString
  . BS.concat $ do
        (m, b) <- sorted_holes
        [ toStringRep m,                BS.Char8.singleton ' ',
          bytesFS (unitIdFS (moduleUnitId b)), BS.Char8.singleton ':',
          toStringRep (moduleName b),   BS.Char8.singleton '\n']

fingerprintUnitId :: BS.ByteString -> Fingerprint -> BS.ByteString
fingerprintUnitId prefix (Fingerprint a b)
    = BS.concat
    $ [ prefix
      , BS.Char8.singleton '-'
      , BS.Char8.pack (toBase62Padded a)
      , BS.Char8.pack (toBase62Padded b) ]

-- | Create a new, un-hashed unit identifier.
newUnitId :: ComponentId -> [(ModuleName, Module)] -> UnitId
newUnitId cid [] = newSimpleUnitId cid -- TODO: this indicates some latent bug...
newUnitId cid insts = IndefiniteUnitId $ newIndefUnitId cid insts

pprUnitId :: UnitId -> SDoc
pprUnitId (DefiniteUnitId uid) = ppr uid
pprUnitId (IndefiniteUnitId uid) = ppr uid

instance Eq UnitId where
  uid1 == uid2 = unitIdKey uid1 == unitIdKey uid2

instance Uniquable UnitId where
  getUnique = unitIdKey

instance Ord UnitId where
  nm1 `compare` nm2 = stableUnitIdCmp nm1 nm2

instance Data UnitId where
  -- don't traverse?
  toConstr _   = abstractConstr "UnitId"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "UnitId"

instance NFData UnitId where
  rnf x = x `seq` ()

stableUnitIdCmp :: UnitId -> UnitId -> Ordering
-- ^ Compares package ids lexically, rather than by their 'Unique's
stableUnitIdCmp p1 p2 = unitIdFS p1 `compare` unitIdFS p2

instance Outputable UnitId where
   ppr pk = pprUnitId pk

-- Performance: would prefer to have a NameCache like thing
instance Binary UnitId where
  put_ bh (DefiniteUnitId def_uid) = do
    putByte bh 0
    put_ bh def_uid
  put_ bh (IndefiniteUnitId indef_uid) = do
    putByte bh 1
    put_ bh indef_uid
  get bh = do b <- getByte bh
              case b of
                0 -> fmap DefiniteUnitId   (get bh)
                _ -> fmap IndefiniteUnitId (get bh)

instance Binary ComponentId where
  put_ bh (ComponentId fs _) = put_ bh fs
  get bh = do { fs <- get bh; return (ComponentId fs Nothing) }

-- | Create a new simple unit identifier (no holes) from a 'ComponentId'.
newSimpleUnitId :: ComponentId -> UnitId
newSimpleUnitId (ComponentId fs _) = fsToUnitId fs

-- | Create a new simple unit identifier from a 'FastString'.  Internally,
-- this is primarily used to specify wired-in unit identifiers.
fsToUnitId :: FastString -> UnitId
fsToUnitId = DefiniteUnitId . DefUnitId . InstalledUnitId

stringToUnitId :: String -> UnitId
stringToUnitId = fsToUnitId . mkFastString

unitIdString :: UnitId -> String
unitIdString = unpackFS . unitIdFS

{-
************************************************************************
*                                                                      *
                        Hole substitutions
*                                                                      *
************************************************************************
-}

-- | Substitution on module variables, mapping module names to module
-- identifiers.
type ShHoleSubst = ModuleNameEnv Module

-- | Substitutes holes in a 'Module'.  NOT suitable for being called
-- directly on a 'nameModule', see Note [Representation of module/name variable].
-- @p[A=<A>]:B@ maps to @p[A=q():A]:B@ with @A=q():A@;
-- similarly, @<A>@ maps to @q():A@.
renameHoleModule :: DynFlags -> ShHoleSubst -> Module -> Module
renameHoleModule dflags = renameHoleModule' (getUnitInfoMap dflags)

-- | Substitutes holes in a 'UnitId', suitable for renaming when
-- an include occurs; see Note [Representation of module/name variable].
--
-- @p[A=<A>]@ maps to @p[A=<B>]@ with @A=<B>@.
renameHoleUnitId :: DynFlags -> ShHoleSubst -> UnitId -> UnitId
renameHoleUnitId dflags = renameHoleUnitId' (getUnitInfoMap dflags)

-- | Like 'renameHoleModule', but requires only 'UnitInfoMap'
-- so it can be used by "Packages".
renameHoleModule' :: UnitInfoMap -> ShHoleSubst -> Module -> Module
renameHoleModule' pkg_map env m
  | not (isHoleModule m) =
        let uid = renameHoleUnitId' pkg_map env (moduleUnitId m)
        in mkModule uid (moduleName m)
  | Just m' <- lookupUFM env (moduleName m) = m'
  -- NB m = <Blah>, that's what's in scope.
  | otherwise = m

-- | Like 'renameHoleUnitId, but requires only 'UnitInfoMap'
-- so it can be used by "Packages".
renameHoleUnitId' :: UnitInfoMap -> ShHoleSubst -> UnitId -> UnitId
renameHoleUnitId' pkg_map env uid =
    case uid of
      (IndefiniteUnitId
        IndefUnitId{ indefUnitIdComponentId = cid
                   , indefUnitIdInsts       = insts
                   , indefUnitIdFreeHoles   = fh })
          -> if disjointUdfmUfm (getUniqDSet fh) env
                then uid
                -- Functorially apply the substitution to the instantiation,
                -- then check the 'UnitInfoMap' to see if there is
                -- a compiled version of this 'UnitId' we can improve to.
                -- See Note [UnitId to InstalledUnitId] improvement
                else improveUnitId pkg_map $
                        newUnitId cid
                            (map (\(k,v) -> (k, renameHoleModule' pkg_map env v)) insts)
      _ -> uid

-- | Given a possibly on-the-fly instantiated module, split it into
-- a 'Module' that we definitely can find on-disk, as well as an
-- instantiation if we need to instantiate it on the fly.  If the
-- instantiation is @Nothing@ no on-the-fly renaming is needed.
splitModuleInsts :: Module -> (InstalledModule, Maybe IndefModule)
splitModuleInsts m =
    let (uid, mb_iuid) = splitUnitIdInsts (moduleUnitId m)
    in (InstalledModule uid (moduleName m),
        fmap (\iuid -> IndefModule iuid (moduleName m)) mb_iuid)

-- | See 'splitModuleInsts'.
splitUnitIdInsts :: UnitId -> (InstalledUnitId, Maybe IndefUnitId)
splitUnitIdInsts (IndefiniteUnitId iuid) =
    (componentIdToInstalledUnitId (indefUnitIdComponentId iuid), Just iuid)
splitUnitIdInsts (DefiniteUnitId (DefUnitId uid)) = (uid, Nothing)

generalizeIndefUnitId :: IndefUnitId -> IndefUnitId
generalizeIndefUnitId IndefUnitId{ indefUnitIdComponentId = cid
                                 , indefUnitIdInsts = insts } =
    newIndefUnitId cid (map (\(m,_) -> (m, mkHoleModule m)) insts)

generalizeIndefModule :: IndefModule -> IndefModule
generalizeIndefModule (IndefModule uid n) = IndefModule (generalizeIndefUnitId uid) n

parseModuleName :: ReadP ModuleName
parseModuleName = fmap mkModuleName
                $ Parse.munch1 (\c -> isAlphaNum c || c `elem` "_.")

parseUnitId :: ReadP UnitId
parseUnitId = parseFullUnitId <++ parseDefiniteUnitId <++ parseSimpleUnitId
  where
    parseFullUnitId = do
        cid <- parseComponentId
        insts <- parseModSubst
        return (newUnitId cid insts)
    parseDefiniteUnitId = do
        s <- Parse.munch1 (\c -> isAlphaNum c || c `elem` "-_.+")
        return (stringToUnitId s)
    parseSimpleUnitId = do
        cid <- parseComponentId
        return (newSimpleUnitId cid)

parseComponentId :: ReadP ComponentId
parseComponentId = (flip ComponentId Nothing . mkFastString)  `fmap` Parse.munch1 abi_char
   where abi_char c = isAlphaNum c || c `elem` "-_."

parseModuleId :: ReadP Module
parseModuleId = parseModuleVar <++ parseModule
    where
      parseModuleVar = do
        _ <- Parse.char '<'
        modname <- parseModuleName
        _ <- Parse.char '>'
        return (mkHoleModule modname)
      parseModule = do
        uid <- parseUnitId
        _ <- Parse.char ':'
        modname <- parseModuleName
        return (mkModule uid modname)

parseModSubst :: ReadP [(ModuleName, Module)]
parseModSubst = Parse.between (Parse.char '[') (Parse.char ']')
      . flip Parse.sepBy (Parse.char ',')
      $ do k <- parseModuleName
           _ <- Parse.char '='
           v <- parseModuleId
           return (k, v)


{-
Note [Wired-in packages]
~~~~~~~~~~~~~~~~~~~~~~~~

Certain packages are known to the compiler, in that we know about certain
entities that reside in these packages, and the compiler needs to
declare static Modules and Names that refer to these packages.  Hence
the wired-in packages can't include version numbers in their package UnitId,
since we don't want to bake the version numbers of these packages into GHC.

So here's the plan.  Wired-in packages are still versioned as
normal in the packages database, and you can still have multiple
versions of them installed. To the user, everything looks normal.

However, for each invocation of GHC, only a single instance of each wired-in
package will be recognised (the desired one is selected via
@-package@\/@-hide-package@), and GHC will internally pretend that it has the
*unversioned* 'UnitId', including in .hi files and object file symbols.

Unselected versions of wired-in packages will be ignored, as will any other
package that depends directly or indirectly on it (much as if you
had used @-ignore-package@).

The affected packages are compiled with, e.g., @-this-unit-id base@, so that
the symbols in the object files have the unversioned unit id in their name.

Make sure you change 'Packages.findWiredInPackages' if you add an entry here.

For `integer-gmp`/`integer-simple` we also change the base name to
`integer-wired-in`, but this is fundamentally no different.
See Note [The integer library] in PrelNames.
-}

integerUnitId, primUnitId,
  baseUnitId, rtsUnitId,
  thUnitId, mainUnitId, thisGhcUnitId, interactiveUnitId  :: UnitId
primUnitId        = fsToUnitId (fsLit "ghc-prim")
integerUnitId     = fsToUnitId (fsLit "integer-wired-in")
   -- See Note [The integer library] in PrelNames
baseUnitId        = fsToUnitId (fsLit "base")
rtsUnitId         = fsToUnitId (fsLit "rts")
thUnitId          = fsToUnitId (fsLit "template-haskell")
thisGhcUnitId     = fsToUnitId (fsLit "ghc")
interactiveUnitId = fsToUnitId (fsLit "interactive")

-- | This is the package Id for the current program.  It is the default
-- package Id if you don't specify a package name.  We don't add this prefix
-- to symbol names, since there can be only one main package per program.
mainUnitId      = fsToUnitId (fsLit "main")

-- | This is a fake package id used to provide identities to any un-implemented
-- signatures.  The set of hole identities is global over an entire compilation.
-- Don't use this directly: use 'mkHoleModule' or 'isHoleModule' instead.
-- See Note [Representation of module/name variables]
holeUnitId :: UnitId
holeUnitId      = fsToUnitId (fsLit "hole")

isInteractiveModule :: Module -> Bool
isInteractiveModule mod = moduleUnitId mod == interactiveUnitId

-- Note [Representation of module/name variables]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In our ICFP'16, we use <A> to represent module holes, and {A.T} to represent
-- name holes.  This could have been represented by adding some new cases
-- to the core data types, but this would have made the existing 'nameModule'
-- and 'moduleUnitId' partial, which would have required a lot of modifications
-- to existing code.
--
-- Instead, we adopted the following encoding scheme:
--
--      <A>   ===> hole:A
--      {A.T} ===> hole:A.T
--
-- This encoding is quite convenient, but it is also a bit dangerous too,
-- because if you have a 'hole:A' you need to know if it's actually a
-- 'Module' or just a module stored in a 'Name'; these two cases must be
-- treated differently when doing substitutions.  'renameHoleModule'
-- and 'renameHoleUnitId' assume they are NOT operating on a
-- 'Name'; 'NameShape' handles name substitutions exclusively.

isHoleModule :: Module -> Bool
isHoleModule mod = moduleUnitId mod == holeUnitId

wiredInUnitIds :: [UnitId]
wiredInUnitIds = [ primUnitId,
                       integerUnitId,
                       baseUnitId,
                       rtsUnitId,
                       thUnitId,
                       thisGhcUnitId ]

{-
************************************************************************
*                                                                      *
\subsection{@ModuleEnv@s}
*                                                                      *
************************************************************************
-}

-- | A map keyed off of 'Module's
newtype ModuleEnv elt = ModuleEnv (Map NDModule elt)

{-
Note [ModuleEnv performance and determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To prevent accidental reintroduction of nondeterminism the Ord instance
for Module was changed to not depend on Unique ordering and to use the
lexicographic order. This is potentially expensive, but when measured
there was no difference in performance.

To be on the safe side and not pessimize ModuleEnv uses nondeterministic
ordering on Module and normalizes by doing the lexicographic sort when
turning the env to a list.
See Note [Unique Determinism] for more information about the source of
nondeterminismand and Note [Deterministic UniqFM] for explanation of why
it matters for maps.
-}

newtype NDModule = NDModule { unNDModule :: Module }
  deriving Eq
  -- A wrapper for Module with faster nondeterministic Ord.
  -- Don't export, See [ModuleEnv performance and determinism]

instance Ord NDModule where
  compare (NDModule (Module p1 n1)) (NDModule (Module p2 n2)) =
    (getUnique p1 `nonDetCmpUnique` getUnique p2) `thenCmp`
    (getUnique n1 `nonDetCmpUnique` getUnique n2)

filterModuleEnv :: (Module -> a -> Bool) -> ModuleEnv a -> ModuleEnv a
filterModuleEnv f (ModuleEnv e) =
  ModuleEnv (Map.filterWithKey (f . unNDModule) e)

elemModuleEnv :: Module -> ModuleEnv a -> Bool
elemModuleEnv m (ModuleEnv e) = Map.member (NDModule m) e

extendModuleEnv :: ModuleEnv a -> Module -> a -> ModuleEnv a
extendModuleEnv (ModuleEnv e) m x = ModuleEnv (Map.insert (NDModule m) x e)

extendModuleEnvWith :: (a -> a -> a) -> ModuleEnv a -> Module -> a
                    -> ModuleEnv a
extendModuleEnvWith f (ModuleEnv e) m x =
  ModuleEnv (Map.insertWith f (NDModule m) x e)

extendModuleEnvList :: ModuleEnv a -> [(Module, a)] -> ModuleEnv a
extendModuleEnvList (ModuleEnv e) xs =
  ModuleEnv (Map.insertList [(NDModule k, v) | (k,v) <- xs] e)

extendModuleEnvList_C :: (a -> a -> a) -> ModuleEnv a -> [(Module, a)]
                      -> ModuleEnv a
extendModuleEnvList_C f (ModuleEnv e) xs =
  ModuleEnv (Map.insertListWith f [(NDModule k, v) | (k,v) <- xs] e)

plusModuleEnv_C :: (a -> a -> a) -> ModuleEnv a -> ModuleEnv a -> ModuleEnv a
plusModuleEnv_C f (ModuleEnv e1) (ModuleEnv e2) =
  ModuleEnv (Map.unionWith f e1 e2)

delModuleEnvList :: ModuleEnv a -> [Module] -> ModuleEnv a
delModuleEnvList (ModuleEnv e) ms =
  ModuleEnv (Map.deleteList (map NDModule ms) e)

delModuleEnv :: ModuleEnv a -> Module -> ModuleEnv a
delModuleEnv (ModuleEnv e) m = ModuleEnv (Map.delete (NDModule m) e)

plusModuleEnv :: ModuleEnv a -> ModuleEnv a -> ModuleEnv a
plusModuleEnv (ModuleEnv e1) (ModuleEnv e2) = ModuleEnv (Map.union e1 e2)

lookupModuleEnv :: ModuleEnv a -> Module -> Maybe a
lookupModuleEnv (ModuleEnv e) m = Map.lookup (NDModule m) e

lookupWithDefaultModuleEnv :: ModuleEnv a -> a -> Module -> a
lookupWithDefaultModuleEnv (ModuleEnv e) x m =
  Map.findWithDefault x (NDModule m) e

mapModuleEnv :: (a -> b) -> ModuleEnv a -> ModuleEnv b
mapModuleEnv f (ModuleEnv e) = ModuleEnv (Map.mapWithKey (\_ v -> f v) e)

mkModuleEnv :: [(Module, a)] -> ModuleEnv a
mkModuleEnv xs = ModuleEnv (Map.fromList [(NDModule k, v) | (k,v) <- xs])

emptyModuleEnv :: ModuleEnv a
emptyModuleEnv = ModuleEnv Map.empty

moduleEnvKeys :: ModuleEnv a -> [Module]
moduleEnvKeys (ModuleEnv e) = sort $ map unNDModule $ Map.keys e
  -- See Note [ModuleEnv performance and determinism]

moduleEnvElts :: ModuleEnv a -> [a]
moduleEnvElts e = map snd $ moduleEnvToList e
  -- See Note [ModuleEnv performance and determinism]

moduleEnvToList :: ModuleEnv a -> [(Module, a)]
moduleEnvToList (ModuleEnv e) =
  sortBy (comparing fst) [(m, v) | (NDModule m, v) <- Map.toList e]
  -- See Note [ModuleEnv performance and determinism]

unitModuleEnv :: Module -> a -> ModuleEnv a
unitModuleEnv m x = ModuleEnv (Map.singleton (NDModule m) x)

isEmptyModuleEnv :: ModuleEnv a -> Bool
isEmptyModuleEnv (ModuleEnv e) = Map.null e

-- | A set of 'Module's
type ModuleSet = Set NDModule

mkModuleSet :: [Module] -> ModuleSet
mkModuleSet = Set.fromList . coerce

extendModuleSet :: ModuleSet -> Module -> ModuleSet
extendModuleSet s m = Set.insert (NDModule m) s

extendModuleSetList :: ModuleSet -> [Module] -> ModuleSet
extendModuleSetList s ms = foldl' (coerce . flip Set.insert) s ms

emptyModuleSet :: ModuleSet
emptyModuleSet = Set.empty

moduleSetElts :: ModuleSet -> [Module]
moduleSetElts = sort . coerce . Set.toList

elemModuleSet :: Module -> ModuleSet -> Bool
elemModuleSet = Set.member . coerce

intersectModuleSet :: ModuleSet -> ModuleSet -> ModuleSet
intersectModuleSet = coerce Set.intersection

minusModuleSet :: ModuleSet -> ModuleSet -> ModuleSet
minusModuleSet = coerce Set.difference

delModuleSet :: ModuleSet -> Module -> ModuleSet
delModuleSet = coerce (flip Set.delete)

unionModuleSet :: ModuleSet -> ModuleSet -> ModuleSet
unionModuleSet = coerce Set.union

unitModuleSet :: Module -> ModuleSet
unitModuleSet = coerce Set.singleton

{-
A ModuleName has a Unique, so we can build mappings of these using
UniqFM.
-}

-- | A map keyed off of 'ModuleName's (actually, their 'Unique's)
type ModuleNameEnv elt = UniqFM elt


-- | A map keyed off of 'ModuleName's (actually, their 'Unique's)
-- Has deterministic folds and can be deterministically converted to a list
type DModuleNameEnv elt = UniqDFM elt
