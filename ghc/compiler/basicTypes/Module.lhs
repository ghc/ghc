%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-2002
%

ModuleName
~~~~~~~~~~
Simply the name of a module, represented as a Z-encoded FastString.
These are Uniquable, hence we can build FiniteMaps with ModuleNames as
the keys.

Module
~~~~~~

A ModuleName with some additional information, namely whether the
module resides in the Home package or in a different package.  We need
to know this for two reasons: 
  
  * generating cross-DLL calls is different from intra-DLL calls 
    (see below).
  * we don't record version information in interface files for entities
    in a different package.

The unique of a Module is identical to the unique of a ModuleName, so
it is safe to look up in a Module map using a ModuleName and vice
versa.

Notes on DLLs
~~~~~~~~~~~~~
When compiling module A, which imports module B, we need to 
know whether B will be in the same DLL as A.  
	If it's in the same DLL, we refer to B_f_closure
	If it isn't, we refer to _imp__B_f_closure
When compiling A, we record in B's Module value whether it's
in a different DLL, by setting the DLL flag.




\begin{code}
module Module 
    (
      Module, 		   	-- Abstract, instance of Eq, Ord, Outputable

    , ModLocation(..),
    , showModMsg

    , ModuleName
    , pprModuleName		-- :: ModuleName -> SDoc
    , printModulePrefix

    , moduleName		-- :: Module -> ModuleName 
    , moduleNameString		-- :: ModuleName -> EncodedString
    , moduleNameUserString	-- :: ModuleName -> UserString
    , moduleNameFS		-- :: ModuleName -> EncodedFS

    , moduleString		-- :: Module -> EncodedString
    , moduleUserString		-- :: Module -> UserString

    , mkBasePkgModule		-- :: UserString -> Module
    , mkThPkgModule		-- :: UserString -> Module
    , mkHomeModule		-- :: ModuleName -> Module
    , isHomeModule		-- :: Module -> Bool
    , mkPackageModule		-- :: ModuleName -> Module

    , mkModuleName		-- :: UserString -> ModuleName
    , mkModuleNameFS		-- :: UserFS    -> ModuleName
    , mkSysModuleNameFS		-- :: EncodedFS -> ModuleName

    , pprModule,
 
    , ModuleEnv,
    , elemModuleEnv, extendModuleEnv, extendModuleEnvList, plusModuleEnv_C
    , delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv
    , lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv
    , moduleEnvElts, unitModuleEnv, isEmptyModuleEnv, foldModuleEnv
    , extendModuleEnv_C
    , lookupModuleEnvByName, extendModuleEnvByName, unitModuleEnvByName

    , ModuleSet, emptyModuleSet, mkModuleSet, moduleSetElts, extendModuleSet, elemModuleSet

    ) where

#include "HsVersions.h"
import OccName
import Outputable
import Packages		( PackageName, basePackage, thPackage )
import CmdLineOpts	( opt_InPackage )
import FastString	( FastString )
import Unique		( Uniquable(..) )
import Maybes		( expectJust )
import UniqFM
import UniqSet
import Binary
import FastString
\end{code}


%************************************************************************
%*									*
\subsection{Interface file flavour}
%*									*
%************************************************************************

A further twist to the tale is the support for dynamically linked
libraries under Win32. Here, dealing with the use of global variables
that's residing in a DLL requires special handling at the point of use
(there's an extra level of indirection, i.e., (**v) to get at v's
value, rather than just (*v) .) When slurping in an interface file we
then record whether it's coming from a .hi corresponding to a module
that's packaged up in a DLL or not, so that we later can emit the
appropriate code.

The logic for how an interface file is marked as corresponding to a
module that's hiding in a DLL is explained elsewhere (ToDo: give
renamer href here.)

\begin{code}
data Module = Module ModuleName !PackageInfo

data PackageInfo
  = ThisPackage				-- A module from the same package 
					-- as the one being compiled
  | AnotherPackage			-- A module from a different package

packageInfoPackage :: PackageInfo -> PackageName
packageInfoPackage ThisPackage        = opt_InPackage
packageInfoPackage AnotherPackage     = FSLIT("<pkg>")

instance Outputable PackageInfo where
	-- Just used in debug prints of lex tokens and in debug modde
   ppr pkg_info = ppr (packageInfoPackage pkg_info)
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

        ml_hspp_file :: Maybe FilePath, -- Path of preprocessed source

        ml_hi_file   :: FilePath,	-- Where the .hi file is, whether or not it exists
					-- Always of form foo.hi, even if there is an hi-boot
					-- file (we add the -boot suffix later)

        ml_obj_file  :: FilePath	-- Where the .o file is, whether or not it exists
					-- (might not exist either because the module
					--  hasn't been compiled yet, or because
					--  it is part of a package with a .a file)
     }
     deriving Show

instance Outputable ModLocation where
   ppr = text . show

-- Rather a gruesome function to have in Module

showModMsg :: Bool -> Module -> ModLocation -> String
showModMsg use_object mod location =
    mod_str ++ replicate (max 0 (16 - length mod_str)) ' '
    ++" ( " ++ expectJust "showModMsg" (ml_hs_file location) ++ ", "
    ++ (if use_object
	  then ml_obj_file location
	  else "interpreted")
    ++ " )"
 where mod_str = moduleUserString mod
\end{code}

For a module in another package, the hs_file and obj_file
components of ModLocation are undefined.  

The locations specified by a ModLocation may or may not
correspond to actual files yet: for example, even if the object
file doesn't exist, the ModLocation still contains the path to
where the object file will reside if/when it is created.


%************************************************************************
%*									*
\subsection{The name of a module}
%*									*
%************************************************************************

\begin{code}
newtype ModuleName = ModuleName EncodedFS
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them

instance Binary ModuleName where
   put_ bh (ModuleName m) = put_ bh m
   get bh = do m <- get bh; return (ModuleName m)

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


pprModuleName :: ModuleName -> SDoc
pprModuleName (ModuleName nm) = pprEncodedFS nm

moduleNameFS :: ModuleName -> EncodedFS
moduleNameFS (ModuleName mod) = mod

moduleNameString :: ModuleName -> EncodedString
moduleNameString (ModuleName mod) = unpackFS mod

moduleNameUserString :: ModuleName -> UserString
moduleNameUserString (ModuleName mod) = decode (unpackFS mod)

-- used to be called mkSrcModule
mkModuleName :: UserString -> ModuleName
mkModuleName s = ModuleName (mkFastString (encode s))

-- used to be called mkSrcModuleFS
mkModuleNameFS :: UserFS -> ModuleName
mkModuleNameFS s = ModuleName (encodeFS s)

-- used to be called mkSysModuleFS
mkSysModuleNameFS :: EncodedFS -> ModuleName
mkSysModuleNameFS s = ModuleName s 
\end{code}

\begin{code}
instance Outputable Module where
  ppr = pprModule

instance Uniquable Module where
  getUnique (Module nm _) = getUnique nm

-- Same if they have the same name.
instance Eq Module where
  m1 == m2 = getUnique m1 == getUnique m2

-- Warning: gives an ordering relation based on the uniques of the
-- FastStrings which are the (encoded) module names.  This is _not_
-- a lexicographical ordering.
instance Ord Module where
  m1 `compare` m2 = getUnique m1 `compare` getUnique m2
\end{code}


\begin{code}
pprModule :: Module -> SDoc
pprModule (Module mod p) = getPprStyle $ \ sty ->
			   if debugStyle sty then
				-- Print the package too
				-- Don't use '.' because it gets confused
				-- 	with module names
				brackets (ppr p) <> pprModuleName mod
			   else
				pprModuleName mod
\end{code}


\begin{code}
mkBasePkgModule :: ModuleName -> Module
mkBasePkgModule mod_nm
  = Module mod_nm pack_info
  where
    pack_info
      | opt_InPackage == basePackage = ThisPackage
      | otherwise		     = AnotherPackage

mkThPkgModule :: ModuleName -> Module
mkThPkgModule mod_nm
  = Module mod_nm pack_info
  where
    pack_info
      | opt_InPackage == thPackage = ThisPackage
      | otherwise		   = AnotherPackage

mkHomeModule :: ModuleName -> Module
mkHomeModule mod_nm = Module mod_nm ThisPackage

isHomeModule :: Module -> Bool
isHomeModule (Module nm ThisPackage) = True
isHomeModule _                       = False

mkPackageModule :: ModuleName -> Module
mkPackageModule mod_nm = Module mod_nm AnotherPackage

moduleString :: Module -> EncodedString
moduleString (Module (ModuleName fs) _) = unpackFS fs

moduleName :: Module -> ModuleName
moduleName (Module mod pkg_info) = mod

moduleUserString :: Module -> UserString
moduleUserString (Module mod _) = moduleNameUserString mod

printModulePrefix :: Module -> Bool
  -- When printing, say M.x
printModulePrefix (Module nm ThisPackage) = False
printModulePrefix _                       = True
\end{code}


%************************************************************************
%*                                                                      *
\subsection{@ModuleEnv@s}
%*                                                                      *
%************************************************************************

\begin{code}
type ModuleEnv elt = UniqFM elt
-- A ModuleName and Module have the same Unique,
-- so both will work as keys.  
-- The 'ByName' variants work on ModuleNames

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

-- The ByName variants
lookupModuleEnvByName :: ModuleEnv a -> ModuleName -> Maybe a
unitModuleEnvByName   :: ModuleName -> a -> ModuleEnv a
extendModuleEnvByName :: ModuleEnv a -> ModuleName -> a -> ModuleEnv a

elemModuleEnv       = elemUFM
extendModuleEnv     = addToUFM
extendModuleEnvByName = addToUFM
extendModuleEnv_C   = addToUFM_C
extendModuleEnvList = addListToUFM
plusModuleEnv_C     = plusUFM_C
delModuleEnvList    = delListFromUFM
delModuleEnv        = delFromUFM
plusModuleEnv       = plusUFM
lookupModuleEnv     = lookupUFM
lookupModuleEnvByName = lookupUFM
lookupWithDefaultModuleEnv = lookupWithDefaultUFM
mapModuleEnv        = mapUFM
mkModuleEnv         = listToUFM
emptyModuleEnv      = emptyUFM
moduleEnvElts       = eltsUFM
unitModuleEnv       = unitUFM
unitModuleEnvByName = unitUFM
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
