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
	mkTopName, mkIPName,
	mkDerivedName, mkGlobalName, mkKnownKeyGlobal, mkWiredInName,

	nameUnique, setNameUnique,
	tidyTopName, 
	nameOccName, nameModule, nameModule_maybe,
	setNameOcc, nameRdrName, setNameModuleAndLoc, 
	toRdrName, hashName,

	nameSrcLoc, nameIsLocallyDefined, isDllName, nameIsFrom, nameIsLocalOrFrom,

	isSystemName, isLocalName, isGlobalName, isExternallyVisibleName,
	isTyVarName,
	
	-- Environment
	NameEnv, mkNameEnv,
	emptyNameEnv, unitNameEnv, nameEnvElts, 
	extendNameEnv_C, extendNameEnv, foldNameEnv,
	plusNameEnv, plusNameEnv_C, extendNameEnv, extendNameEnvList,
	lookupNameEnv, lookupNameEnv_NF, delFromNameEnv, elemNameEnv, 


	-- Class NamedThing and overloaded friends
	NamedThing(..),
	getSrcLoc, getOccString, toRdrName,
	isFrom, isLocalOrFrom
    ) where

#include "HsVersions.h"

import OccName		-- All of it
import Module		( Module, moduleName, mkVanillaModule, isHomeModule )
import RdrName		( RdrName, mkRdrOrig, mkRdrUnqual, rdrNameOcc, rdrNameModule )
import CmdLineOpts	( opt_Static, opt_OmitInterfacePragmas, opt_EnsureSplittableC )
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
nameIsLocallyDefined	:: Name -> Bool
nameIsFrom		:: Module -> Name -> Bool
nameIsLocalOrFrom	:: Module -> Name -> Bool
isLocalName		:: Name -> Bool		-- Not globals
isGlobalName		:: Name -> Bool
isSystemName		:: Name -> Bool
isExternallyVisibleName :: Name -> Bool

isGlobalName (Name {n_sort = Global _}) = True
isGlobalName other	                = False

isLocalName name = not (isGlobalName name)

nameIsLocallyDefined name = isLocalName name

nameIsLocalOrFrom from (Name {n_sort = Global mod}) = mod == from
nameIsLocalOrFrom from other			    = True

nameIsFrom from (Name {n_sort = Global mod}) = mod == from
nameIsFrom from other			     = pprPanic "nameIsFrom" (ppr other)

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

---------------------------------------------------------------------
mkDerivedName :: (OccName -> OccName)
	      -> Name		-- Base name
	      -> Unique		-- New unique
	      -> Name		-- Result is always a value name

mkDerivedName f name uniq = name {n_uniq = uniq, n_occ = f (n_occ name)}
\end{code}

\begin{code}
-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique name uniq = name {n_uniq = uniq}

setNameOcc :: Name -> OccName -> Name
	-- Give the thing a new OccName, *and*
	-- record that it's no longer a sys-local
	-- This is used by the tidy-up pass
setNameOcc name occ = name {n_occ = occ}

setNameModuleAndLoc :: Name -> Module -> SrcLoc -> Name
setNameModuleAndLoc name mod loc = name {n_sort = set (n_sort name), n_loc = loc}
		       where
			 set (Global _) = Global mod
\end{code}


%************************************************************************
%*									*
\subsection{Tidying a name}
%*									*
%************************************************************************

tidyTopName is applied to top-level names in the final program

For top-level things, 
	it globalises Local names 
		(if all top-level things should be visible)
	and localises non-exported Global names
		 (if only exported things should be visible)

In all cases except an exported global, it gives it a new occurrence name.

The "visibility" here concerns whether the .o file's symbol table
mentions the thing; if so, it needs a module name in its symbol.
The Global things are "visible" and the Local ones are not

Why should things be "visible"?  Certainly they must be if they
are exported.  But also:

(a) In certain (prelude only) modules we split up the .hc file into
    lots of separate little files, which are separately compiled by the C
    compiler.  That gives lots of little .o files.  The idea is that if
    you happen to mention one of them you don't necessarily pull them all
    in.  (Pulling in a piece you don't need can be v bad, because it may
    mention other pieces you don't need either, and so on.)
    
    Sadly, splitting up .hc files means that local names (like s234) are
    now globally visible, which can lead to clashes between two .hc
    files. So unlocaliseWhatnot goes through making all the local things
    into global things, essentially by giving them full names so when they
    are printed they'll have their module name too.  Pretty revolting
    really.

(b) When optimisation is on we want to make all the internal
    top-level defns externally visible

\begin{code}
tidyTopName :: Module -> TidyOccEnv -> Bool -> Name -> (TidyOccEnv, Name)
tidyTopName mod env is_exported
	    name@(Name { n_occ = occ, n_sort = sort, n_uniq = uniq, n_loc = loc })
  = case sort of
	Global _ | is_exported -> (env, name)
		 | otherwise   -> (env, name { n_sort = new_sort })
		-- Don't change the occurrnce names of globals, because many of them
		-- are bound by either a class declaration or a data declaration
		-- or an explicit user export.

	other    | is_exported -> (env', name { n_sort = Global mod, n_occ = occ' })
		 | otherwise   -> (env', name { n_sort = new_sort,   n_occ = occ' })
  where
    (env', occ') = tidyOccName env occ
    new_sort     = mkLocalTopSort mod

mkTopName :: Unique -> Module -> FAST_STRING -> Name
	-- Make a top-level name; make it Global if top-level
	-- things should be externally visible; Local otherwise
	-- This chap is only used *after* the tidyCore phase
	-- Notably, it is used during STG lambda lifting
	--
	-- We have to make sure that the name is globally unique
	-- and we don't have tidyCore to help us. So we append
	-- the unique.  Hack!  Hack!
	-- (Used only by the STG lambda lifter.)
mkTopName uniq mod fs
  = Name { n_uniq = uniq, 
	   n_sort = mkLocalTopSort mod,
	   n_occ  = mkVarOcc (_PK_ ((_UNPK_ fs) ++ show uniq)),
	   n_loc = noSrcLoc }

mkLocalTopSort :: Module -> NameSort
mkLocalTopSort mod
  | all_toplev_ids_visible = Global mod
  | otherwise		   = Local

all_toplev_ids_visible
  = not opt_OmitInterfacePragmas ||  -- Pragmas can make them visible
    opt_EnsureSplittableC            -- Splitting requires visiblilty
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

isDllName :: Name -> Bool
	-- Does this name refer to something in a different DLL?
isDllName nm = not opt_Static &&
	       not (isLocalName nm) &&				-- isLocalName test needed 'cos
	       not (isHomeModule (nameModule nm))	-- nameModule won't work on local names



isTyVarName :: Name -> Bool
isTyVarName name = isTvOcc (nameOccName name)

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

pprLocal sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | debugStyle sty = pprOccName occ <> 
		     text "{-" <> pprUnique10 uniq <> text "-}"
  | otherwise      = pprOccName occ

pprGlobal sty name uniq mod occ
  | codeStyle sty        = ppr (moduleName mod) <> char '_' <> pprOccName occ

  | debugStyle sty       = ppr (moduleName mod) <> dot <> pprOccName occ <> 
			    text "{-" <> pprUnique10 uniq <> text "-}"

  | unqualStyle sty name = pprOccName occ
  | otherwise		 = ppr (moduleName mod) <> dot <> pprOccName occ

pprSysLocal sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | otherwise	   = pprOccName occ <> char '_' <> pprUnique uniq
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
isFrom		    :: NamedThing a => Module -> a -> Bool
isLocalOrFrom	    :: NamedThing a => Module -> a -> Bool

getSrcLoc	    = nameSrcLoc	   . getName
getOccString 	    = occNameString	   . getOccName
toRdrName	    = nameRdrName	   . getName
isFrom mod x	    = nameIsFrom mod (getName x)
isLocalOrFrom mod x = nameIsLocalOrFrom mod ( getName x)
\end{code}

