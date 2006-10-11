%
% (c) The University of Glasgow, 2004-2006
%

Module
~~~~~~~~~~
Simply the name of a module, represented as a FastString.
These are Uniquable, hence we can build FiniteMaps with Modules as
the keys.

\begin{code}
module Module 
    (
	-- * The ModuleName type
	ModuleName,
	pprModuleName,
	moduleNameFS,
	moduleNameString,
	mkModuleName,
	mkModuleNameFS,

	-- * The Module type
	Module,
	modulePackageId, moduleName,
	pprModule,
	mkModule,

	-- * The ModuleLocation type
	ModLocation(..),
	addBootSuffix, addBootSuffix_maybe, addBootSuffixLocn,

	-- * Module mappings
    	ModuleEnv,
	elemModuleEnv, extendModuleEnv, extendModuleEnvList, 
	extendModuleEnvList_C, plusModuleEnv_C,
	delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv,
	lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv,
	moduleEnvElts, unitModuleEnv, isEmptyModuleEnv, foldModuleEnv,
	extendModuleEnv_C, filterModuleEnv,

	-- * ModuleName mappings
	ModuleNameEnv,

	-- * Sets of modules
	ModuleSet, emptyModuleSet, mkModuleSet, moduleSetElts, extendModuleSet,
	elemModuleSet
    ) where

#include "HsVersions.h"
import Outputable
import Unique
import FiniteMap
import UniqFM
import PackageConfig
import FastString
import Binary
\end{code}

%************************************************************************
%*									*
\subsection{Module locations}
%*									*
%************************************************************************

\begin{code}
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
-- Add the "-boot" suffix to .hs, .hi and .o files
addBootSuffix path = path ++ "-boot"

addBootSuffix_maybe :: Bool -> FilePath -> FilePath
addBootSuffix_maybe is_boot path
 | is_boot   = addBootSuffix path
 | otherwise = path

addBootSuffixLocn :: ModLocation -> ModLocation
addBootSuffixLocn locn
  = locn { ml_hs_file  = fmap addBootSuffix (ml_hs_file locn)
	 , ml_hi_file  = addBootSuffix (ml_hi_file locn)
	 , ml_obj_file = addBootSuffix (ml_obj_file locn) }
\end{code}


%************************************************************************
%*									*
\subsection{The name of a module}
%*									*
%************************************************************************

\begin{code}
-- | A ModuleName is a simple string, eg. @Data.List@.
newtype ModuleName = ModuleName FastString

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

pprModuleName :: ModuleName -> SDoc
pprModuleName (ModuleName nm) = 
    getPprStyle $ \ sty ->
    if codeStyle sty 
	then ftext (zEncodeFS nm)
	else ftext nm

moduleNameFS :: ModuleName -> FastString
moduleNameFS (ModuleName mod) = mod

moduleNameString :: ModuleName -> String
moduleNameString (ModuleName mod) = unpackFS mod

mkModuleName :: String -> ModuleName
mkModuleName s = ModuleName (mkFastString s)

mkModuleNameFS :: FastString -> ModuleName
mkModuleNameFS s = ModuleName s
\end{code}

%************************************************************************
%*									*
\subsection{A fully qualified module}
%*									*
%************************************************************************

\begin{code}
-- | A Module is a pair of a 'PackageId' and a 'ModuleName'.
data Module = Module {
   modulePackageId :: !PackageId,  -- pkg-1.0
   moduleName      :: !ModuleName  -- A.B.C
  }
  deriving (Eq, Ord)

instance Outputable Module where
  ppr = pprModule

instance Binary Module where
  put_ bh (Module p n) = put_ bh p >> put_ bh n
  get bh = do p <- get bh; n <- get bh; return (Module p n)

mkModule :: PackageId -> ModuleName -> Module
mkModule = Module

pprModule :: Module -> SDoc
pprModule mod@(Module p n)  = pprPackagePrefix p mod <> pprModuleName n

pprPackagePrefix p mod = getPprStyle doc
 where
   doc sty
       | codeStyle sty = 
          if p == mainPackageId 
                then empty -- never qualify the main package in code
                else ftext (zEncodeFS (packageIdFS p)) <> char '_'
       | Just pkg <- qualModule sty mod = ftext (packageIdFS pkg) <> char ':'
                -- the PrintUnqualified tells us which modules have to
                -- be qualified with package names
       | otherwise = empty
\end{code}

%************************************************************************
%*                                                                      *
\subsection{@ModuleEnv@s}
%*                                                                      *
%************************************************************************

\begin{code}
type ModuleEnv elt = FiniteMap Module elt

emptyModuleEnv       :: ModuleEnv a
mkModuleEnv          :: [(Module, a)] -> ModuleEnv a
unitModuleEnv        :: Module -> a -> ModuleEnv a
extendModuleEnv      :: ModuleEnv a -> Module -> a -> ModuleEnv a
extendModuleEnv_C    :: (a->a->a) -> ModuleEnv a -> Module -> a -> ModuleEnv a
plusModuleEnv        :: ModuleEnv a -> ModuleEnv a -> ModuleEnv a
extendModuleEnvList  :: ModuleEnv a -> [(Module, a)] -> ModuleEnv a
extendModuleEnvList_C  :: (a->a->a) -> ModuleEnv a -> [(Module, a)] -> ModuleEnv a
                  
delModuleEnvList     :: ModuleEnv a -> [Module] -> ModuleEnv a
delModuleEnv         :: ModuleEnv a -> Module -> ModuleEnv a
plusModuleEnv_C      :: (a -> a -> a) -> ModuleEnv a -> ModuleEnv a -> ModuleEnv a
mapModuleEnv         :: (a -> b) -> ModuleEnv a -> ModuleEnv b
moduleEnvElts        :: ModuleEnv a -> [a]
                  
isEmptyModuleEnv     :: ModuleEnv a -> Bool
lookupModuleEnv      :: ModuleEnv a -> Module     -> Maybe a
lookupWithDefaultModuleEnv :: ModuleEnv a -> a -> Module -> a
elemModuleEnv        :: Module -> ModuleEnv a -> Bool
foldModuleEnv        :: (a -> b -> b) -> b -> ModuleEnv a -> b
filterModuleEnv      :: (a -> Bool) -> ModuleEnv a -> ModuleEnv a

filterModuleEnv f   = filterFM (\_ v -> f v)
elemModuleEnv       = elemFM
extendModuleEnv     = addToFM
extendModuleEnv_C   = addToFM_C
extendModuleEnvList = addListToFM
extendModuleEnvList_C = addListToFM_C
plusModuleEnv_C     = plusFM_C
delModuleEnvList    = delListFromFM
delModuleEnv        = delFromFM
plusModuleEnv       = plusFM
lookupModuleEnv     = lookupFM
lookupWithDefaultModuleEnv = lookupWithDefaultFM
mapModuleEnv f      = mapFM (\_ v -> f v)
mkModuleEnv         = listToFM
emptyModuleEnv      = emptyFM
moduleEnvElts       = eltsFM
unitModuleEnv       = unitFM
isEmptyModuleEnv    = isEmptyFM
foldModuleEnv f     = foldFM (\_ v -> f v)
\end{code}

\begin{code}
type ModuleSet = FiniteMap Module ()
mkModuleSet	:: [Module] -> ModuleSet
extendModuleSet :: ModuleSet -> Module -> ModuleSet
emptyModuleSet  :: ModuleSet
moduleSetElts   :: ModuleSet -> [Module]
elemModuleSet   :: Module -> ModuleSet -> Bool

emptyModuleSet    = emptyFM
mkModuleSet ms    = listToFM [(m,()) | m <- ms ]
extendModuleSet s m = addToFM s m ()
moduleSetElts     = keysFM
elemModuleSet     = elemFM
\end{code}

A ModuleName has a Unique, so we can build mappings of these using
UniqFM.

\begin{code}
type ModuleNameEnv elt = UniqFM elt
\end{code}
