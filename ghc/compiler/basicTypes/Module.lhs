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
      Module		    -- abstract, instance of Eq, Ord, Outputable
    , ModuleName

    , moduleNameString		-- :: ModuleName -> EncodedString
    , moduleNameUserString	-- :: ModuleName -> UserString

    , moduleString          -- :: Module -> EncodedString
    , moduleUserString      -- :: Module -> UserString
    , moduleName	    -- :: Module -> ModuleName

    , mkVanillaModule	    -- :: ModuleName -> Module
    , mkThisModule	    -- :: ModuleName -> Module
    , mkPrelModule          -- :: UserString -> Module
    , mkModule		    -- :: ModuleName -> PackageName -> Module
    
    , isLocalModule       -- :: Module -> Bool

    , mkSrcModule

    , mkSrcModuleFS         -- :: UserFS    -> ModuleName
    , mkSysModuleFS         -- :: EncodedFS -> ModuleName

    , pprModule, pprModuleName
 
    , PackageName

	-- Where to find a .hi file
    , WhereFrom(..)

    ) where

#include "HsVersions.h"
import OccName
import Outputable
import CmdLineOpts	( opt_InPackage )
import FastString	( FastString )
\end{code}


%************************************************************************
%*									*
\subsection{Interface file flavour}
%*									*
%************************************************************************

A further twist to the tale is the support for dynamically linked libraries under
Win32. Here, dealing with the use of global variables that's residing in a DLL
requires special handling at the point of use (there's an extra level of indirection,
i.e., (**v) to get at v's value, rather than just (*v) .) When slurping in an
interface file we then record whether it's coming from a .hi corresponding to a
module that's packaged up in a DLL or not, so that we later can emit the
appropriate code.

The logic for how an interface file is marked as corresponding to a module that's
hiding in a DLL is explained elsewhere (ToDo: give renamer href here.)

\begin{code}
data PackageInfo = ThisPackage 			-- A module from the same package 
						-- as the one being compiled
		 | AnotherPackage PackageName	-- A module from a different package

type PackageName = FastString		-- No encoding at all

preludePackage :: PackageName
preludePackage = SLIT("std")

instance Show PackageInfo where	-- Just used in debug prints of lex tokens
				-- and in debug modde
  showsPrec n ThisPackage        s = "<THIS>"   ++ s
  showsPrec n (AnotherPackage p) s = (_UNPK_ p) ++ s
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
type ModuleName = EncodedFS
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them

pprModuleName :: ModuleName -> SDoc
pprModuleName nm = pprEncodedFS nm

moduleNameString :: ModuleName -> EncodedString
moduleNameString mod = _UNPK_ mod

moduleNameUserString :: ModuleName -> UserString
moduleNameUserString mod = decode (_UNPK_ mod)

mkSrcModule :: UserString -> ModuleName
mkSrcModule s = _PK_ (encode s)

mkSrcModuleFS :: UserFS -> ModuleName
mkSrcModuleFS s = encodeFS s

mkSysModuleFS :: EncodedFS -> ModuleName
mkSysModuleFS s = s 
\end{code}

\begin{code}
data Module = Module ModuleName PackageInfo
\end{code}

\begin{code}
instance Outputable Module where
  ppr = pprModule

instance Eq Module where
  (Module m1 _) == (Module m2 _) = m1 == m2

instance Ord Module where
  (Module m1 _) `compare` (Module m2 _) = m1 `compare` m2
\end{code}


\begin{code}
pprModule :: Module -> SDoc
pprModule (Module mod p) = getPprStyle $ \ sty ->
			   if debugStyle sty then
				-- Print the package too
				text (show p) <> dot <> pprModuleName mod
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


mkVanillaModule :: ModuleName -> Module
mkVanillaModule name = Module name ThisPackage
	-- Used temporarily when we first come across Foo.x in an interface
	-- file, but before we've opened Foo.hi.
	-- (Until we've opened Foo.hi we don't know what the PackageInfo is.)

mkThisModule :: ModuleName -> Module	-- The module being compiled
mkThisModule name = Module name ThisPackage

mkPrelModule :: ModuleName -> Module
mkPrelModule name = mkModule name preludePackage

moduleString :: Module -> EncodedString
moduleString (Module mod _) = _UNPK_ mod

moduleName :: Module -> ModuleName
moduleName (Module mod _) = mod

moduleUserString :: Module -> UserString
moduleUserString (Module mod _) = moduleNameUserString mod
\end{code}

\begin{code}
isLocalModule :: Module -> Bool
isLocalModule (Module _ ThisPackage) = True
isLocalModule _		      	     = False
\end{code}
