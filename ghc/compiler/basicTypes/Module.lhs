%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Module]{The @Module@ module.}

Representing modules and their flavours.

\begin{code}
module Module 
    (
      Module		    -- abstract, instance of Eq, Ord, Outputable
    , moduleString          -- :: Module -> EncodedString
    , moduleUserString      -- :: Module -> UserString
    , moduleIfaceFlavour    -- :: Module -> IfaceFlavour
    , moduleFS		    -- :: Module -> EncodedFS

    , mkBootModule          -- :: Module -> Module
    , setModuleFlavour	    -- :: IfaceFlavour -> Module -> Module

    , mkDynamicModule       -- :: Module -> Module
    , isDynamicModule       -- :: Module -> Bool

    , mkSrcModule
    , mkPrelModule          -- :: UserString -> Module

    , mkSrcModuleFS         -- :: UserFS -> Module
    , mkSysModuleFS         -- :: EncodedFS -> IfaceFlavour -> Module
    , mkImportModuleFS      -- :: UserFS -> IfaceFlavour -> Module

    , pprModule
    , pprModuleSep
    , pprModuleBoot
 
      -- IfaceFlavour
    , IfaceFlavour
    , hiFile
    , hiBootFile	    -- :: IfaceFlavour
    , mkDynFlavour	    -- :: Bool -> IfaceFlavour -> IfaceFlavour

    , bootFlavour           -- :: IfaceFlavour -> Bool

    ) where

#include "HsVersions.h"
import OccName
import Outputable
import CmdLineOpts ( opt_Static, opt_CompilingPrelude )

\end{code}


%************************************************************************
%*									*
\subsection{Interface file flavour}
%*									*
%************************************************************************

The IfaceFlavour type is used mainly in an imported Name's Provenance
to say whether the name comes from a regular .hi file, or whether it comes
from a hand-written .hi-boot file.  This is important, because it has to be 
propagated.  Suppose

	C.hs imports B
	B.hs imports A
	A.hs imports C {-# SOURCE -#} ( f )

Then in A.hi we may mention C.f, in an inlining.  When compiling B we *must not* 
read C.f's details from C.hi, even if the latter happens to exist from an earlier
compilation run.  So we use the name "C!f" in A.hi, and when looking for an interface
file with details of C!f we look in C.hi-boot.  The "!" stuff is recorded in the
IfaceFlavour in the Module of C.f in A. 

Not particularly beautiful, but it works.

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
data IfaceFlavour = HiFile		-- The thing comes from a standard interface file
					-- or from the source file itself
		  | HiBootFile		-- ... or from a handwritten "hi-boot" interface file

		  | HiDllFile           -- The thing comes from a standard interface file, but
		                        -- it's corresponding object code is residing in a DLL.
					-- (see above.)
		  deriving( Eq )

hiFile     = HiFile
hiDllFile  = HiDllFile
hiBootFile = HiBootFile

-- badly named, isn't clear whether the boolean deals with
-- the 'bootedness' or the 'DLLedness'. ToDo: improve.
mkDynFlavour :: Bool{-is really dyn?-} -> IfaceFlavour -> IfaceFlavour
mkDynFlavour True HiFile = HiDllFile
mkDynFlavour _	  x      = x

instance Text IfaceFlavour where	-- Just used in debug prints of lex tokens
  showsPrec n HiBootFile s = "!" ++ s
  showsPrec n HiFile     s = s
  showsPrec n HiDllFile  s = s

bootFlavour :: IfaceFlavour -> Bool
bootFlavour HiBootFile = True
bootFlavour HiFile     = False
bootFlavour HiDllFile  = False
\end{code}


%************************************************************************
%*									*
\subsection[Module]{The name of a module}
%*									*
%************************************************************************

\begin{code}
data Module = Module
		EncodedFS
		IfaceFlavour
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them
\end{code}

\begin{code}
instance Outputable Module where
  ppr = pprModule

-- Ignore the IfaceFlavour when comparing modules
instance Eq Module where
  (Module m1 _) == (Module m2 _) = m1 == m2

instance Ord Module where
  (Module m1 _) `compare` (Module m2 _) = m1 `compare` m2
\end{code}


\begin{code}
pprModule :: Module -> SDoc
pprModule (Module mod _) = pprEncodedFS mod

pprModuleSep, pprModuleBoot :: Module -> SDoc
pprModuleSep (Module mod HiFile)     = dot
pprModuleSep (Module mod HiDllFile)  = dot
pprModuleSep (Module mod HiBootFile) = char '!'

pprModuleBoot (Module mod HiFile)     = empty
pprModuleBoot (Module mod HiDllFile)  = empty
pprModuleBoot (Module mod HiBootFile) = char '!'
\end{code}


\begin{code}
mkSrcModule :: UserString -> Module
mkSrcModule s = Module (_PK_ (encode s)) HiFile

mkPrelModule :: UserString -> Module
mkPrelModule s = Module (_PK_ (encode s)) ilk
 where 
  ilk
   | opt_Static || opt_CompilingPrelude = HiFile
   | otherwise				= HiDllFile

mkSrcModuleFS :: UserFS -> Module
mkSrcModuleFS s = Module (encodeFS s) HiFile

mkImportModuleFS :: UserFS -> IfaceFlavour -> Module
mkImportModuleFS s hif = Module (encodeFS s) hif

mkSysModuleFS :: EncodedFS -> IfaceFlavour -> Module
mkSysModuleFS s hif = Module s hif

mkBootModule :: Module -> Module
mkBootModule (Module s _) = Module s HiBootFile

mkDynamicModule :: Module -> Module
mkDynamicModule (Module s HiFile) = Module s HiDllFile
mkDynamicModule m = m

setModuleFlavour :: IfaceFlavour -> Module -> Module
setModuleFlavour hif (Module n _) = Module n hif

moduleString :: Module -> EncodedString
moduleString (Module mod _) = _UNPK_ mod

moduleFS :: Module -> EncodedFS
moduleFS (Module mod _) = mod

moduleUserString :: Module -> UserString
moduleUserString (Module mod _) = decode (_UNPK_ mod)

moduleIfaceFlavour :: Module -> IfaceFlavour
moduleIfaceFlavour (Module _ hif) = hif
\end{code}

\begin{code}
isDynamicModule :: Module -> Bool
isDynamicModule (Module _ HiDllFile)  = True
isDynamicModule _		      = False
\end{code}
