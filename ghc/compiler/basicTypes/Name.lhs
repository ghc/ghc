%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
module Name (
	-- Re-export the OccName stuff
	module OccName,

	-- The Name type
	Name,					-- Abstract
	mkLocalName, mkSysLocalName, mkCCallName,
	mkIPName,
	mkGlobalName, mkKnownKeyGlobal, mkWiredInName,

	nameUnique, setNameUnique,
	nameOccName, nameModule, nameModule_maybe,
	setNameOcc, nameRdrName, setNameModuleAndLoc, 
	toRdrName, hashName, 
	globaliseName, localiseName,

	nameSrcLoc, 

	isSystemName, isLocalName, isGlobalName, isExternallyVisibleName,
	isTyVarName, isDllName, 
	nameIsLocalOrFrom, isHomePackageName,
	
	-- Environment
	NameEnv, mkNameEnv,
	emptyNameEnv, unitNameEnv, nameEnvElts, 
	extendNameEnv_C, extendNameEnv, foldNameEnv, filterNameEnv,
	plusNameEnv, plusNameEnv_C, extendNameEnv, extendNameEnvList,
	lookupNameEnv, lookupNameEnv_NF, delFromNameEnv, elemNameEnv, 


	-- Class NamedThing and overloaded friends
	NamedThing(..),
	getSrcLoc, getOccString, toRdrName
    ) where

#include "HsVersions.h"

import OccName		-- All of it
import Module		( Module, moduleName, mkVanillaModule, isHomeModule )
import RdrName		( RdrName, mkRdrOrig, mkRdrUnqual, rdrNameOcc, rdrNameModule )
import CmdLineOpts	( opt_Static )
import SrcLoc		( builtinSrcLoc, noSrcLoc, SrcLoc )
import Unique		( Unique, Uniquable(..), u2i, pprUnique, pprUnique10 )
import FastTypes
import Maybes		( expectJust )
import UniqFM
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection[Name-datatype]{The @Name@ datatype, and name construction}
%*									*
%************************************************************************
 
\begin{code}
data Name = Name {
		n_sort :: NameSort,	-- What sort of name it is
		n_occ  :: OccName,	-- Its occurrence name
		n_uniq :: Unique,
		n_loc  :: SrcLoc	-- Definition site
	    }

data NameSort
  = Global Module	-- (a) TyCon, Class, their derived Ids, dfun Id
			-- (b) Imported Id
			-- (c) Top-level Id in the original source, even if
			--	locally defined

  | Local		-- A user-defined Id or TyVar
			-- defined in the module being compiled

  | System		-- A system-defined Id or TyVar.  Typically the
			-- OccName is very uninformative (like 's')
\end{code}

Notes about the NameSorts:

1.  Initially, top-level Ids (including locally-defined ones) get Global names, 
    and all other local Ids get Local names

2.  Things with a @Global@ name are given C static labels, so they finally
    appear in the .o file's symbol table.  They appear in the symbol table
    in the form M.n.  If originally-local things have this property they
    must be made @Global@ first.

3.  In the tidy-core phase, a Global that is not visible to an importer
    is changed to Local, and a Local that is visible is changed to Global

4.  A System Name differs in the following ways:
	a) has unique attached when printing dumps
	b) unifier eliminates sys tyvars in favour of user provs where possible

    Before anything gets printed in interface files or output code, it's
    fed through a 'tidy' processor, which zaps the OccNames to have
    unique names; and converts all sys-locals to user locals
    If any desugarer sys-locals have survived that far, they get changed to
    "ds1", "ds2", etc.

\begin{code}
nameUnique		:: Name -> Unique
nameOccName		:: Name -> OccName 
nameModule		:: Name -> Module
nameSrcLoc		:: Name -> SrcLoc

nameUnique  name = n_uniq name
nameOccName name = n_occ  name
nameSrcLoc  name = n_loc  name

nameModule (Name { n_sort = Global mod }) = mod
nameModule name				  = pprPanic "nameModule" (ppr name)

nameModule_maybe (Name { n_sort = Global mod }) = Just mod
nameModule_maybe name				= Nothing
\end{code}

\begin{code}
nameIsLocalOrFrom	:: Module -> Name -> Bool
isLocalName		:: Name -> Bool		-- Not globals
isGlobalName		:: Name -> Bool
isSystemName		:: Name -> Bool
isExternallyVisibleName :: Name -> Bool
isHomePackageName	:: Name -> Bool

isGlobalName (Name {n_sort = Global _}) = True
isGlobalName other	                = False

isLocalName name = not (isGlobalName name)

nameIsLocalOrFrom from (Name {n_sort = Global mod}) = mod == from
nameIsLocalOrFrom from other			    = True

isHomePackageName (Name {n_sort = Global mod}) = isHomeModule mod
isHomePackageName other			       = True 	-- Local and system names

isDllName :: Name -> Bool	-- Does this name refer to something in a different DLL?
isDllName nm = not opt_Static && not (isHomePackageName nm)

isTyVarName :: Name -> Bool
isTyVarName name = isTvOcc (nameOccName name)

-- Global names are by definition those that are visible
-- outside the module, *as seen by the linker*.  Externally visible
-- does not mean visible at the source level
isExternallyVisibleName name = isGlobalName name

isSystemName (Name {n_sort = System}) = True
isSystemName other		      = False
\end{code}


%************************************************************************
%*									*
\subsection{Making names}
%*									*
%************************************************************************

\begin{code}
mkLocalName :: Unique -> OccName -> SrcLoc -> Name
mkLocalName uniq occ loc = Name { n_uniq = uniq, n_sort = Local, n_occ = occ, n_loc = loc }
	-- NB: You might worry that after lots of huffing and
	-- puffing we might end up with two local names with distinct
	-- uniques, but the same OccName.  Indeed we can, but that's ok
	--	* the insides of the compiler don't care: they use the Unique
	--	* when printing for -ddump-xxx you can switch on -dppr-debug to get the
	--	  uniques if you get confused
	--	* for interface files we tidyCore first, which puts the uniques
	--	  into the print name (see setNameVisibility below)

mkGlobalName :: Unique -> Module -> OccName -> SrcLoc -> Name
mkGlobalName uniq mod occ loc = Name { n_uniq = uniq, n_sort = Global mod,
				       n_occ = occ, n_loc = loc }

mkKnownKeyGlobal :: RdrName -> Unique -> Name
mkKnownKeyGlobal rdr_name uniq
  = mkGlobalName uniq (mkVanillaModule (rdrNameModule rdr_name))
		      (rdrNameOcc rdr_name)
		      builtinSrcLoc

mkWiredInName :: Module -> OccName -> Unique -> Name
mkWiredInName mod occ uniq = mkGlobalName uniq mod occ builtinSrcLoc

mkSysLocalName :: Unique -> UserFS -> Name
mkSysLocalName uniq fs = Name { n_uniq = uniq, n_sort = System, 
				n_occ = mkVarOcc fs, n_loc = noSrcLoc }

mkCCallName :: Unique -> EncodedString -> Name
	-- The encoded string completely describes the ccall
mkCCallName uniq str =  Name { n_uniq = uniq, n_sort = Local, 
			       n_occ = mkCCallOcc str, n_loc = noSrcLoc }

mkIPName :: Unique -> OccName -> Name
mkIPName uniq occ
  = Name { n_uniq = uniq,
	   n_sort = Local,
	   n_occ  = occ,
	   n_loc = noSrcLoc }
\end{code}

\begin{code}
-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique name uniq = name {n_uniq = uniq}

setNameOcc :: Name -> OccName -> Name
setNameOcc name occ = name {n_occ = occ}

globaliseName :: Name -> Module -> Name
globaliseName n mod = n { n_sort = Global mod }
				
localiseName :: Name -> Name
localiseName n = n { n_sort = Local }
				
setNameModuleAndLoc :: Name -> Module -> SrcLoc -> Name
setNameModuleAndLoc name mod loc = name {n_sort = set (n_sort name), n_loc = loc}
		       where
			 set (Global _) = Global mod
\end{code}


%************************************************************************
%*									*
\subsection{Predicates and selectors}
%*									*
%************************************************************************

\begin{code}
hashName :: Name -> Int
hashName name = iBox (u2i (nameUnique name))


nameRdrName :: Name -> RdrName
-- Makes a qualified name for top-level (Global) names, whether locally defined or not
-- and an unqualified name just for Locals
nameRdrName (Name { n_occ = occ, n_sort = Global mod }) = mkRdrOrig (moduleName mod) occ
nameRdrName (Name { n_occ = occ })			= mkRdrUnqual occ
\end{code}


%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = n_uniq n1 `compare` n_uniq n2
\end{code}

\begin{code}
instance Eq Name where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord Name where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpName a b

instance Uniquable Name where
    getUnique = nameUnique

instance NamedThing Name where
    getName n = n
\end{code}


%************************************************************************
%*									*
\subsection{Name environment}
%*									*
%************************************************************************

\begin{code}
type NameEnv a = UniqFM a	-- Domain is Name

emptyNameEnv   	 :: NameEnv a
mkNameEnv	 :: [(Name,a)] -> NameEnv a
nameEnvElts    	 :: NameEnv a -> [a]
extendNameEnv_C  :: (a->a->a) -> NameEnv a -> Name -> a -> NameEnv a
extendNameEnv  	 :: NameEnv a -> Name -> a -> NameEnv a
plusNameEnv    	 :: NameEnv a -> NameEnv a -> NameEnv a
plusNameEnv_C  	 :: (a->a->a) -> NameEnv a -> NameEnv a -> NameEnv a
extendNameEnvList:: NameEnv a -> [(Name,a)] -> NameEnv a
delFromNameEnv 	 :: NameEnv a -> Name -> NameEnv a
elemNameEnv    	 :: Name -> NameEnv a -> Bool
unitNameEnv    	 :: Name -> a -> NameEnv a
lookupNameEnv  	 :: NameEnv a -> Name -> Maybe a
lookupNameEnv_NF :: NameEnv a -> Name -> a
mapNameEnv	 :: (a->b) -> NameEnv a -> NameEnv b
foldNameEnv	 :: (a -> b -> b) -> b -> NameEnv a -> b
filterNameEnv	 :: (elt -> Bool) -> NameEnv elt -> NameEnv elt

emptyNameEnv   	 = emptyUFM
foldNameEnv	 = foldUFM
mkNameEnv	 = listToUFM
nameEnvElts    	 = eltsUFM
extendNameEnv_C  = addToUFM_C
extendNameEnv  	 = addToUFM
plusNameEnv    	 = plusUFM
plusNameEnv_C  	 = plusUFM_C
extendNameEnvList= addListToUFM
delFromNameEnv 	 = delFromUFM
elemNameEnv    	 = elemUFM
mapNameEnv	 = mapUFM
unitNameEnv    	 = unitUFM
filterNameEnv	 = filterUFM

lookupNameEnv  	       = lookupUFM
lookupNameEnv_NF env n = expectJust "lookupNameEnv_NF" (lookupUFM env n)
\end{code}


%************************************************************************
%*									*
\subsection{Pretty printing}
%*									*
%************************************************************************

\begin{code}
instance Outputable Name where
	-- When printing interfaces, all Locals have been given nice print-names
    ppr name = pprName name

pprName name@(Name {n_sort = sort, n_uniq = uniq, n_occ = occ})
  = getPprStyle $ \ sty ->
    case sort of
      Global mod -> pprGlobal sty name uniq mod occ
      System     -> pprSysLocal sty uniq occ
      Local      -> pprLocal sty uniq occ

pprGlobal sty name uniq mod occ
  | codeStyle sty        = ppr (moduleName mod) <> char '_' <> pprOccName occ

  | debugStyle sty       = ppr (moduleName mod) <> dot <> pprOccName occ <> 
			    text "{-" <> pprUnique uniq <> text "-}"

  | unqualStyle sty name = pprOccName occ
  | otherwise		 = ppr (moduleName mod) <> dot <> pprOccName occ

pprLocal sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | debugStyle sty = pprOccName occ <> 
		     text "{-" <> pprUnique10 uniq <> text "-}"
  | otherwise      = pprOccName occ	-- User and Iface styles

-- Like Local, except that we only omit the unique in Iface style
pprSysLocal sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | ifaceStyle sty = pprOccName occ	-- The tidy phase has ensured that OccNames
					-- are enough
  | otherwise	   = pprOccName occ <> char '_' <> pprUnique uniq
				-- If the tidy phase hasn't run, the OccName
				-- is unlikely to be informative (like 's'),
				-- so print the unique
\end{code}

%************************************************************************
%*									*
\subsection{Overloaded functions related to Names}
%*									*
%************************************************************************

\begin{code}
class NamedThing a where
    getOccName :: a -> OccName
    getName    :: a -> Name

    getOccName n = nameOccName (getName n)	-- Default method
\end{code}

\begin{code}
getSrcLoc	    :: NamedThing a => a -> SrcLoc
getOccString	    :: NamedThing a => a -> String
toRdrName	    :: NamedThing a => a -> RdrName

getSrcLoc	    = nameSrcLoc	   . getName
getOccString 	    = occNameString	   . getOccName
toRdrName	    = nameRdrName	   . getName
\end{code}

