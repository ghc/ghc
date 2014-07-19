%
% (c) The University of Glasgow, 2004-2006
%

Module
~~~~~~~~~~
Simply the name of a module, represented as a FastString.
These are Uniquable, hence we can build Maps with Modules as
the keys.

\begin{code}
{-# LANGUAGE DeriveDataTypeable #-}

module Module
    (
        -- * The ModuleName type
        ModuleName,
        pprModuleName,
        moduleNameFS,
        moduleNameString,
        moduleNameSlashes, moduleNameColons,
        mkModuleName,
        mkModuleNameFS,
        stableModuleNameCmp,

        -- * The PackageId type
        PackageId,
        fsToPackageId,
        packageIdFS,
        stringToPackageId,
        packageIdString,
        stablePackageIdCmp,

        -- * Wired-in PackageIds
        -- $wired_in_packages
        primPackageId,
        integerPackageId,
        basePackageId,
        rtsPackageId,
        thPackageId,
        dphSeqPackageId,
        dphParPackageId,
        mainPackageId,
        thisGhcPackageId,
        interactivePackageId, isInteractiveModule,

        -- * The Module type
        Module,
        modulePackageId, moduleName,
        pprModule,
        mkModule,
        stableModuleCmp,
        HasModule(..),
        ContainsModule(..),

        -- * The ModuleLocation type
        ModLocation(..),
        addBootSuffix, addBootSuffix_maybe, addBootSuffixLocn,

        -- * Module mappings
        ModuleEnv,
        elemModuleEnv, extendModuleEnv, extendModuleEnvList,
        extendModuleEnvList_C, plusModuleEnv_C,
        delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv,
        lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv,
        moduleEnvKeys, moduleEnvElts, moduleEnvToList,
        unitModuleEnv, isEmptyModuleEnv,
        foldModuleEnv, extendModuleEnvWith, filterModuleEnv,

        -- * ModuleName mappings
        ModuleNameEnv,

        -- * Sets of Modules
        ModuleSet,
        emptyModuleSet, mkModuleSet, moduleSetElts, extendModuleSet, elemModuleSet
    ) where

import Config
import Outputable
import Unique
import UniqFM
import FastString
import Binary
import Util

import Data.Data
import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map
import System.FilePath
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Module locations}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Where a module lives on the file system: the actual locations
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

        ml_obj_file  :: FilePath
                -- Where the .o file is, whether or not it exists yet.
                -- (might not exist either because the module hasn't
                -- been compiled yet, or because it is part of a
                -- package with a .a file)
  } deriving Show

instance Outputable ModLocation where
   ppr = text . show
\end{code}

For a module in another package, the hs_file and obj_file
components of ModLocation are undefined.

The locations specified by a ModLocation may or may not
correspond to actual files yet: for example, even if the object
file doesn't exist, the ModLocation still contains the path to
where the object file will reside if/when it is created.

\begin{code}
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
         , ml_obj_file = addBootSuffix (ml_obj_file locn) }
\end{code}


%************************************************************************
%*                                                                      *
\subsection{The name of a module}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A ModuleName is essentially a simple string, e.g. @Data.List@.
newtype ModuleName = ModuleName FastString
    deriving Typeable

instance Uniquable ModuleName where
  getUnique (ModuleName nm) = getUnique nm

instance Eq ModuleName where
  nm1 == nm2 = getUnique nm1 == getUnique nm2

-- Warning: gives an ordering relation based on the uniques of the
-- FastStrings which are the (encoded) module names.  This is _not_
-- a lexicographical ordering.
instance Ord ModuleName where
  nm1 `compare` nm2 = getUnique nm1 `compare` getUnique nm2

instance Outputable ModuleName where
  ppr = pprModuleName

instance Binary ModuleName where
  put_ bh (ModuleName fs) = put_ bh fs
  get bh = do fs <- get bh; return (ModuleName fs)

instance Data ModuleName where
  -- don't traverse?
  toConstr _   = abstractConstr "ModuleName"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "ModuleName"

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

mkModuleName :: String -> ModuleName
mkModuleName s = ModuleName (mkFastString s)

mkModuleNameFS :: FastString -> ModuleName
mkModuleNameFS s = ModuleName s

-- |Returns the string version of the module name, with dots replaced by slashes.
--
moduleNameSlashes :: ModuleName -> String
moduleNameSlashes = dots_to_slashes . moduleNameString
  where dots_to_slashes = map (\c -> if c == '.' then pathSeparator else c)

-- |Returns the string version of the module name, with dots replaced by underscores.
--
moduleNameColons :: ModuleName -> String
moduleNameColons = dots_to_colons . moduleNameString
  where dots_to_colons = map (\c -> if c == '.' then ':' else c)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{A fully qualified module}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A Module is a pair of a 'PackageId' and a 'ModuleName'.
data Module = Module {
   modulePackageId :: !PackageId,  -- pkg-1.0
   moduleName      :: !ModuleName  -- A.B.C
  }
  deriving (Eq, Ord, Typeable)

instance Uniquable Module where
  getUnique (Module p n) = getUnique (packageIdFS p `appendFS` moduleNameFS n)

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

-- | This gives a stable ordering, as opposed to the Ord instance which
-- gives an ordering based on the 'Unique's of the components, which may
-- not be stable from run to run of the compiler.
stableModuleCmp :: Module -> Module -> Ordering
stableModuleCmp (Module p1 n1) (Module p2 n2)
   = (p1 `stablePackageIdCmp`  p2) `thenCmp`
     (n1 `stableModuleNameCmp` n2)

mkModule :: PackageId -> ModuleName -> Module
mkModule = Module

pprModule :: Module -> SDoc
pprModule mod@(Module p n)  =
  pprPackagePrefix p mod <> pprModuleName n

pprPackagePrefix :: PackageId -> Module -> SDoc
pprPackagePrefix p mod = getPprStyle doc
 where
   doc sty
       | codeStyle sty =
          if p == mainPackageId
                then empty -- never qualify the main package in code
                else ztext (zEncodeFS (packageIdFS p)) <> char '_'
       | qualModule sty mod = ftext (packageIdFS (modulePackageId mod)) <> char ':'
                -- the PrintUnqualified tells us which modules have to
                -- be qualified with package names
       | otherwise = empty

class ContainsModule t where
    extractModule :: t -> Module

class HasModule m where
    getModule :: m Module
\end{code}

%************************************************************************
%*                                                                      *
\subsection{PackageId}
%*                                                                      *
%************************************************************************

\begin{code}
-- | Essentially just a string identifying a package, including the version: e.g. parsec-1.0
newtype PackageId = PId FastString deriving( Eq, Typeable )
    -- here to avoid module loops with PackageConfig

instance Uniquable PackageId where
 getUnique pid = getUnique (packageIdFS pid)

-- Note: *not* a stable lexicographic ordering, a faster unique-based
-- ordering.
instance Ord PackageId where
  nm1 `compare` nm2 = getUnique nm1 `compare` getUnique nm2

instance Data PackageId where
  -- don't traverse?
  toConstr _   = abstractConstr "PackageId"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "PackageId"

stablePackageIdCmp :: PackageId -> PackageId -> Ordering
-- ^ Compares package ids lexically, rather than by their 'Unique's
stablePackageIdCmp p1 p2 = packageIdFS p1 `compare` packageIdFS p2

instance Outputable PackageId where
   ppr pid = text (packageIdString pid)

instance Binary PackageId where
  put_ bh pid = put_ bh (packageIdFS pid)
  get bh = do { fs <- get bh; return (fsToPackageId fs) }

fsToPackageId :: FastString -> PackageId
fsToPackageId = PId

packageIdFS :: PackageId -> FastString
packageIdFS (PId fs) = fs

stringToPackageId :: String -> PackageId
stringToPackageId = fsToPackageId . mkFastString

packageIdString :: PackageId -> String
packageIdString = unpackFS . packageIdFS


-- -----------------------------------------------------------------------------
-- $wired_in_packages
-- Certain packages are known to the compiler, in that we know about certain
-- entities that reside in these packages, and the compiler needs to
-- declare static Modules and Names that refer to these packages.  Hence
-- the wired-in packages can't include version numbers, since we don't want
-- to bake the version numbers of these packages into GHC.
--
-- So here's the plan.  Wired-in packages are still versioned as
-- normal in the packages database, and you can still have multiple
-- versions of them installed.  However, for each invocation of GHC,
-- only a single instance of each wired-in package will be recognised
-- (the desired one is selected via @-package@\/@-hide-package@), and GHC
-- will use the unversioned 'PackageId' below when referring to it,
-- including in .hi files and object file symbols.  Unselected
-- versions of wired-in packages will be ignored, as will any other
-- package that depends directly or indirectly on it (much as if you
-- had used @-ignore-package@).

-- Make sure you change 'Packages.findWiredInPackages' if you add an entry here

integerPackageId, primPackageId,
  basePackageId, rtsPackageId,
  thPackageId, dphSeqPackageId, dphParPackageId,
  mainPackageId, thisGhcPackageId, interactivePackageId  :: PackageId
primPackageId        = fsToPackageId (fsLit "ghc-prim")
integerPackageId     = fsToPackageId (fsLit cIntegerLibrary)
basePackageId        = fsToPackageId (fsLit "base")
rtsPackageId         = fsToPackageId (fsLit "rts")
thPackageId          = fsToPackageId (fsLit "template-haskell")
dphSeqPackageId      = fsToPackageId (fsLit "dph-seq")
dphParPackageId      = fsToPackageId (fsLit "dph-par")
thisGhcPackageId     = fsToPackageId (fsLit "ghc")
interactivePackageId = fsToPackageId (fsLit "interactive")

-- | This is the package Id for the current program.  It is the default
-- package Id if you don't specify a package name.  We don't add this prefix
-- to symbol names, since there can be only one main package per program.
mainPackageId      = fsToPackageId (fsLit "main")

isInteractiveModule :: Module -> Bool
isInteractiveModule mod = modulePackageId mod == interactivePackageId
\end{code}

%************************************************************************
%*                                                                      *
\subsection{@ModuleEnv@s}
%*                                                                      *
%************************************************************************

\begin{code}
-- | A map keyed off of 'Module's
newtype ModuleEnv elt = ModuleEnv (Map Module elt)

filterModuleEnv :: (Module -> a -> Bool) -> ModuleEnv a -> ModuleEnv a
filterModuleEnv f (ModuleEnv e) = ModuleEnv (Map.filterWithKey f e)

elemModuleEnv :: Module -> ModuleEnv a -> Bool
elemModuleEnv m (ModuleEnv e) = Map.member m e

extendModuleEnv :: ModuleEnv a -> Module -> a -> ModuleEnv a
extendModuleEnv (ModuleEnv e) m x = ModuleEnv (Map.insert m x e)

extendModuleEnvWith :: (a -> a -> a) -> ModuleEnv a -> Module -> a -> ModuleEnv a
extendModuleEnvWith f (ModuleEnv e) m x = ModuleEnv (Map.insertWith f m x e)

extendModuleEnvList :: ModuleEnv a -> [(Module, a)] -> ModuleEnv a
extendModuleEnvList (ModuleEnv e) xs = ModuleEnv (Map.insertList xs e)

extendModuleEnvList_C :: (a -> a -> a) -> ModuleEnv a -> [(Module, a)]
                      -> ModuleEnv a
extendModuleEnvList_C f (ModuleEnv e) xs = ModuleEnv (Map.insertListWith f xs e)

plusModuleEnv_C :: (a -> a -> a) -> ModuleEnv a -> ModuleEnv a -> ModuleEnv a
plusModuleEnv_C f (ModuleEnv e1) (ModuleEnv e2) = ModuleEnv (Map.unionWith f e1 e2)

delModuleEnvList :: ModuleEnv a -> [Module] -> ModuleEnv a
delModuleEnvList (ModuleEnv e) ms = ModuleEnv (Map.deleteList ms e)

delModuleEnv :: ModuleEnv a -> Module -> ModuleEnv a
delModuleEnv (ModuleEnv e) m = ModuleEnv (Map.delete m e)

plusModuleEnv :: ModuleEnv a -> ModuleEnv a -> ModuleEnv a
plusModuleEnv (ModuleEnv e1) (ModuleEnv e2) = ModuleEnv (Map.union e1 e2)

lookupModuleEnv :: ModuleEnv a -> Module -> Maybe a
lookupModuleEnv (ModuleEnv e) m = Map.lookup m e

lookupWithDefaultModuleEnv :: ModuleEnv a -> a -> Module -> a
lookupWithDefaultModuleEnv (ModuleEnv e) x m = Map.findWithDefault x m e

mapModuleEnv :: (a -> b) -> ModuleEnv a -> ModuleEnv b
mapModuleEnv f (ModuleEnv e) = ModuleEnv (Map.mapWithKey (\_ v -> f v) e)

mkModuleEnv :: [(Module, a)] -> ModuleEnv a
mkModuleEnv xs = ModuleEnv (Map.fromList xs)

emptyModuleEnv :: ModuleEnv a
emptyModuleEnv = ModuleEnv Map.empty

moduleEnvKeys :: ModuleEnv a -> [Module]
moduleEnvKeys (ModuleEnv e) = Map.keys e

moduleEnvElts :: ModuleEnv a -> [a]
moduleEnvElts (ModuleEnv e) = Map.elems e

moduleEnvToList :: ModuleEnv a -> [(Module, a)]
moduleEnvToList (ModuleEnv e) = Map.toList e

unitModuleEnv :: Module -> a -> ModuleEnv a
unitModuleEnv m x = ModuleEnv (Map.singleton m x)

isEmptyModuleEnv :: ModuleEnv a -> Bool
isEmptyModuleEnv (ModuleEnv e) = Map.null e

foldModuleEnv :: (a -> b -> b) -> b -> ModuleEnv a -> b
foldModuleEnv f x (ModuleEnv e) = Map.foldRightWithKey (\_ v -> f v) x e
\end{code}

\begin{code}
-- | A set of 'Module's
type ModuleSet = Map Module ()

mkModuleSet     :: [Module] -> ModuleSet
extendModuleSet :: ModuleSet -> Module -> ModuleSet
emptyModuleSet  :: ModuleSet
moduleSetElts   :: ModuleSet -> [Module]
elemModuleSet   :: Module -> ModuleSet -> Bool

emptyModuleSet    = Map.empty
mkModuleSet ms    = Map.fromList [(m,()) | m <- ms ]
extendModuleSet s m = Map.insert m () s
moduleSetElts     = Map.keys
elemModuleSet     = Map.member
\end{code}

A ModuleName has a Unique, so we can build mappings of these using
UniqFM.

\begin{code}
-- | A map keyed off of 'ModuleName's (actually, their 'Unique's)
type ModuleNameEnv elt = UniqFM elt
\end{code}

