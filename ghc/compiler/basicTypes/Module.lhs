%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Module]{The @Module@ module.}

Representing modules and their flavours.


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
      Module, moduleName
			    -- abstract, instance of Eq, Ord, Outputable
    , ModuleName
    , isModuleInThisPackage

    , moduleNameString		-- :: ModuleName -> EncodedString
    , moduleNameUserString	-- :: ModuleName -> UserString
    , moduleNameFS		-- :: ModuleName -> EncodedFS

    , moduleString		-- :: Module -> EncodedString
    , moduleUserString		-- :: Module -> UserString
    , moduleName		-- :: Module -> ModuleName

    , mkVanillaModule	        -- :: ModuleName -> Module
    , mkPrelModule		-- :: UserString -> Module
    , mkModule			-- :: ModuleName -> PackageName -> Module
    , mkHomeModule		-- :: ModuleName -> Module

--    , mkSrcModule

    , mkModuleName		-- :: UserString -> ModuleName
    , mkModuleNameFS		-- :: UserFS    -> ModuleName
    , mkSysModuleNameFS		-- :: EncodedFS -> ModuleName

    , pprModule,
 
    , PackageName

	-- Where to find a .hi file
    , WhereFrom(..)

    , ModuleEnv,
    , elemModuleEnv, extendModuleEnv, extendModuleEnvList, plusModuleEnv_C
    , delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv
    , lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv
    , rngModuleEnv, unitModuleEnv, isEmptyModuleEnv, foldModuleEnv

    ) where

#include "HsVersions.h"
import OccName
import Outputable
import CmdLineOpts	( opt_InPackage )
import FastString	( FastString, uniqueOfFS )
import Unique		( Uniquable(..), mkUniqueGrimily )
import UniqFM
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
data Module = Module ModuleName PackageInfo

data PackageInfo 
  = ThisPackage				-- A module from the same package 
					-- as the one being compiled
  | AnotherPackage PackageName		-- A module from a different package

type PackageName = FastString		-- No encoding at all

preludePackage :: PackageName
preludePackage = SLIT("std")

instance Outputable PackageInfo where
	-- Just used in debug prints of lex tokens and in debug modde
   ppr ThisPackage        = ptext SLIT("<THIS>")
   ppr (AnotherPackage p) = ptext p
\end{code}


%************************************************************************
%*									*
\subsection{Where from}
%*									*
%************************************************************************

The @WhereFrom@ type controls where the renamer looks for an interface file

\begin{code}
data WhereFrom = ImportByUser		-- Ordinary user import: look for M.hi
	       | ImportByUserSource	-- User {- SOURCE -}: look for M.hi-boot
	       | ImportBySystem		-- Non user import.  Look for M.hi if M is in
					-- the module this module depends on, or is a system-ish module; 
					-- M.hi-boot otherwise

instance Outputable WhereFrom where
  ppr ImportByUser       = empty
  ppr ImportByUserSource = ptext SLIT("{- SOURCE -}")
  ppr ImportBySystem     = ptext SLIT("{- SYSTEM IMPORT -}")
\end{code}


%************************************************************************
%*									*
\subsection{The name of a module}
%*									*
%************************************************************************

\begin{code}
newtype ModuleName = ModuleName EncodedFS
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them

instance Uniquable ModuleName where
  getUnique (ModuleName nm) = mkUniqueGrimily (uniqueOfFS nm)

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
moduleNameString (ModuleName mod) = _UNPK_ mod

moduleNameUserString :: ModuleName -> UserString
moduleNameUserString (ModuleName mod) = decode (_UNPK_ mod)

-- used to be called mkSrcModule
mkModuleName :: UserString -> ModuleName
mkModuleName s = ModuleName (_PK_ (encode s))

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
				ppr p <> dot <> pprModuleName mod
			   else
				pprModuleName mod
\end{code}


\begin{code}
mkModule :: ModuleName	-- Name of the module
	 -> PackageName
	 -> Module
mkModule mod_nm pack_name
  = Module mod_nm pack_info
  where
    pack_info | pack_name == opt_InPackage = ThisPackage
	      | otherwise		   = AnotherPackage pack_name

mkHomeModule :: ModuleName -> Module
mkHomeModule mod_nm = Module mod_nm ThisPackage

-- Used temporarily when we first come across Foo.x in an interface
-- file, but before we've opened Foo.hi.
-- (Until we've opened Foo.hi we don't know what the PackageInfo is.)
mkVanillaModule :: ModuleName -> Module
mkVanillaModule name = mkModule name (panic "mkVanillaModule:unknown mod_kind field")

mkPrelModule :: ModuleName -> Module
mkPrelModule name = mkModule name preludePackage

moduleString :: Module -> EncodedString
moduleString (Module (ModuleName fs) _) = _UNPK_ fs

moduleName :: Module -> ModuleName
moduleName (Module mod _) = mod

moduleUserString :: Module -> UserString
moduleUserString (Module mod _) = moduleNameUserString mod

isModuleInThisPackage :: Module -> Bool
isModuleInThisPackage (Module nm ThisPackage) = True
isModuleInThisPackage _                       = False
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
plusModuleEnv        :: ModuleEnv a -> ModuleEnv a -> ModuleEnv a
extendModuleEnvList  :: ModuleEnv a -> [(Module, a)] -> ModuleEnv a
                  
delModuleEnvList     :: ModuleEnv a -> [Module] -> ModuleEnv a
delModuleEnv         :: ModuleEnv a -> Module -> ModuleEnv a
plusModuleEnv_C      :: (a -> a -> a) -> ModuleEnv a -> ModuleEnv a -> ModuleEnv a
mapModuleEnv         :: (a -> b) -> ModuleEnv a -> ModuleEnv b
rngModuleEnv         :: ModuleEnv a -> [a]
                  
isEmptyModuleEnv     :: ModuleEnv a -> Bool
lookupModuleEnv      :: ModuleEnv a -> Module -> Maybe a
lookupWithDefaultModuleEnv :: ModuleEnv a -> a -> Module -> a
elemModuleEnv        :: Module -> ModuleEnv a -> Bool
foldModuleEnv        :: (a -> b -> b) -> b -> ModuleEnv a -> b

elemModuleEnv       = elemUFM
extendModuleEnv     = addToUFM
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
rngModuleEnv        = eltsUFM
unitModuleEnv       = unitUFM
isEmptyModuleEnv    = isNullUFM
foldModuleEnv       = foldUFM
\end{code}
