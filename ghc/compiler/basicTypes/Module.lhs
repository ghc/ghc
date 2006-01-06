%
% (c) The University of Glasgow, 2004
%

Module
~~~~~~~~~~
Simply the name of a module, represented as a FastString.
These are Uniquable, hence we can build FiniteMaps with ModuleNames as
the keys.

\begin{code}
module Module 
    (
      Module 		   	-- Abstract, instance of Eq, Ord, Outputable
    , pprModule			-- :: ModuleName -> SDoc

    , ModLocation(..)
    , addBootSuffix, addBootSuffix_maybe, addBootSuffixLocn

    , moduleString		-- :: ModuleName -> String
    , moduleFS			-- :: ModuleName -> FastString

    , mkModule			-- :: String -> ModuleName
    , mkModuleFS		-- :: FastString -> ModuleName
 
    , ModuleEnv
    , elemModuleEnv, extendModuleEnv, extendModuleEnvList, plusModuleEnv_C
    , delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv
    , lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv
    , moduleEnvElts, unitModuleEnv, isEmptyModuleEnv, foldModuleEnv
    , extendModuleEnv_C, filterModuleEnv

    , ModuleSet, emptyModuleSet, mkModuleSet, moduleSetElts, extendModuleSet, elemModuleSet

    ) where

#include "HsVersions.h"
import OccName
import Outputable
import Unique		( Uniquable(..) )
import UniqFM
import UniqSet
import Binary
import FastString
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
newtype Module = Module FastString
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them

instance Binary Module where
   put_ bh (Module m) = put_ bh m
   get bh = do m <- get bh; return (Module m)

instance Uniquable Module where
  getUnique (Module nm) = getUnique nm

instance Eq Module where
  nm1 == nm2 = getUnique nm1 == getUnique nm2

-- Warning: gives an ordering relation based on the uniques of the
-- FastStrings which are the (encoded) module names.  This is _not_
-- a lexicographical ordering.
instance Ord Module where
  nm1 `compare` nm2 = getUnique nm1 `compare` getUnique nm2

instance Outputable Module where
  ppr = pprModule

pprModule :: Module -> SDoc
pprModule (Module nm) = 
    getPprStyle $ \ sty ->
    if codeStyle sty 
	then ftext (zEncodeFS nm)
	else ftext nm

moduleFS :: Module -> FastString
moduleFS (Module mod) = mod

moduleString :: Module -> String
moduleString (Module mod) = unpackFS mod

-- used to be called mkSrcModule
mkModule :: String -> Module
mkModule s = Module (mkFastString s)

-- used to be called mkSrcModuleFS
mkModuleFS :: FastString -> Module
mkModuleFS s = Module s
\end{code}

%************************************************************************
%*                                                                      *
\subsection{@ModuleEnv@s}
%*                                                                      *
%************************************************************************

\begin{code}
type ModuleEnv elt = UniqFM elt

emptyModuleEnv       :: ModuleEnv a
mkModuleEnv          :: [(Module, a)] -> ModuleEnv a
unitModuleEnv        :: Module -> a -> ModuleEnv a
extendModuleEnv      :: ModuleEnv a -> Module -> a -> ModuleEnv a
extendModuleEnv_C    :: (a->a->a) -> ModuleEnv a -> Module -> a -> ModuleEnv a
plusModuleEnv        :: ModuleEnv a -> ModuleEnv a -> ModuleEnv a
extendModuleEnvList  :: ModuleEnv a -> [(Module, a)] -> ModuleEnv a
                  
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

filterModuleEnv	    = filterUFM
elemModuleEnv       = elemUFM
extendModuleEnv     = addToUFM
extendModuleEnv_C   = addToUFM_C
extendModuleEnvList = addListToUFM
plusModuleEnv_C     = plusUFM_C
delModuleEnvList    = delListFromUFM
delModuleEnv        = delFromUFM
plusModuleEnv       = plusUFM
lookupModuleEnv     = lookupUFM
lookupWithDefaultModuleEnv = lookupWithDefaultUFM
mapModuleEnv        = mapUFM
mkModuleEnv         = listToUFM
emptyModuleEnv      = emptyUFM
moduleEnvElts       = eltsUFM
unitModuleEnv       = unitUFM
isEmptyModuleEnv    = isNullUFM
foldModuleEnv       = foldUFM
\end{code}

\begin{code}
type ModuleSet = UniqSet Module
mkModuleSet	:: [Module] -> ModuleSet
extendModuleSet :: ModuleSet -> Module -> ModuleSet
emptyModuleSet  :: ModuleSet
moduleSetElts   :: ModuleSet -> [Module]
elemModuleSet   :: Module -> ModuleSet -> Bool

emptyModuleSet  = emptyUniqSet
mkModuleSet     = mkUniqSet
extendModuleSet = addOneToUniqSet
moduleSetElts   = uniqSetToList
elemModuleSet   = elementOfUniqSet
\end{code}
