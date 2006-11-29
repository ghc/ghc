%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
module Name (
	-- Re-export the OccName stuff
	module OccName,

	-- The Name type
	Name,					-- Abstract
	BuiltInSyntax(..), 
	mkInternalName, mkSystemName,
	mkSystemVarName, mkSysTvName, 
	mkFCallName, mkIPName,
        mkTickBoxOpName,
	mkExternalName, mkWiredInName,

	nameUnique, setNameUnique,
	nameOccName, nameModule, nameModule_maybe,
	tidyNameOcc, 
	hashName, localiseName,

	nameSrcLoc,

	isSystemName, isInternalName, isExternalName,
	isTyVarName, isTyConName, isWiredInName, isBuiltInSyntax,
	wiredInNameTyThing_maybe, 
	nameIsLocalOrFrom,
	
	-- Class NamedThing and overloaded friends
	NamedThing(..),
	getSrcLoc, getOccString
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TypeRep( TyThing )

import OccName
import Module
import SrcLoc
import UniqFM
import Unique
import Maybes
import Binary
import FastMutInt
import FastString
import Outputable

import Data.IORef
import GHC.Exts
import Data.Array
\end{code}

%************************************************************************
%*									*
\subsection[Name-datatype]{The @Name@ datatype, and name construction}
%*									*
%************************************************************************
 
\begin{code}
data Name = Name {
		n_sort :: NameSort,	-- What sort of name it is
		n_occ  :: !OccName,	-- Its occurrence name
		n_uniq :: Int#,         -- UNPACK doesn't work, recursive type
		n_loc  :: !SrcLoc	-- Definition site
	    }

-- NOTE: we make the n_loc field strict to eliminate some potential
-- (and real!) space leaks, due to the fact that we don't look at
-- the SrcLoc in a Name all that often.

data NameSort
  = External Module
 
  | WiredIn Module TyThing BuiltInSyntax
	-- A variant of External, for wired-in things

  | Internal		-- A user-defined Id or TyVar
			-- defined in the module being compiled

  | System		-- A system-defined Id or TyVar.  Typically the
			-- OccName is very uninformative (like 's')

data BuiltInSyntax = BuiltInSyntax | UserSyntax
-- BuiltInSyntax is for things like (:), [], tuples etc, 
-- which have special syntactic forms.  They aren't "in scope"
-- as such.
\end{code}

Notes about the NameSorts:

1.  Initially, top-level Ids (including locally-defined ones) get External names, 
    and all other local Ids get Internal names

2.  Things with a External name are given C static labels, so they finally
    appear in the .o file's symbol table.  They appear in the symbol table
    in the form M.n.  If originally-local things have this property they
    must be made @External@ first.

3.  In the tidy-core phase, a External that is not visible to an importer
    is changed to Internal, and a Internal that is visible is changed to External

4.  A System Name differs in the following ways:
	a) has unique attached when printing dumps
	b) unifier eliminates sys tyvars in favour of user provs where possible

    Before anything gets printed in interface files or output code, it's
    fed through a 'tidy' processor, which zaps the OccNames to have
    unique names; and converts all sys-locals to user locals
    If any desugarer sys-locals have survived that far, they get changed to
    "ds1", "ds2", etc.

Built-in syntax => It's a syntactic form, not "in scope" (e.g. [])

Wired-in thing  => The thing (Id, TyCon) is fully known to the compiler, 
		   not read from an interface file. 
		   E.g. Bool, True, Int, Float, and many others

All built-in syntax is for wired-in things.

\begin{code}
nameUnique		:: Name -> Unique
nameOccName		:: Name -> OccName 
nameModule		:: Name -> Module
nameSrcLoc		:: Name -> SrcLoc

nameUnique  name = mkUniqueGrimily (I# (n_uniq name))
nameOccName name = n_occ  name
nameSrcLoc  name = n_loc  name
\end{code}

\begin{code}
nameIsLocalOrFrom :: Module -> Name -> Bool
isInternalName	  :: Name -> Bool
isExternalName	  :: Name -> Bool
isSystemName	  :: Name -> Bool
isWiredInName	  :: Name -> Bool

isWiredInName (Name {n_sort = WiredIn _ _ _}) = True
isWiredInName other			      = False

wiredInNameTyThing_maybe :: Name -> Maybe TyThing
wiredInNameTyThing_maybe (Name {n_sort = WiredIn _ thing _}) = Just thing
wiredInNameTyThing_maybe other				     = Nothing

isBuiltInSyntax (Name {n_sort = WiredIn _ _ BuiltInSyntax}) = True
isBuiltInSyntax other			 	            = False

isExternalName (Name {n_sort = External _})    = True
isExternalName (Name {n_sort = WiredIn _ _ _}) = True
isExternalName other                           = False

isInternalName name = not (isExternalName name)

nameModule name = nameModule_maybe name `orElse` pprPanic "nameModule" (ppr name)
nameModule_maybe (Name { n_sort = External mod})    = Just mod
nameModule_maybe (Name { n_sort = WiredIn mod _ _}) = Just mod
nameModule_maybe name				    = Nothing

nameIsLocalOrFrom from name
  | isExternalName name = from == nameModule name
  | otherwise		= True

isTyVarName :: Name -> Bool
isTyVarName name = isTvOcc (nameOccName name)

isTyConName :: Name -> Bool
isTyConName name = isTcOcc (nameOccName name)

isSystemName (Name {n_sort = System}) = True
isSystemName other		      = False
\end{code}


%************************************************************************
%*									*
\subsection{Making names}
%*									*
%************************************************************************

\begin{code}
mkInternalName :: Unique -> OccName -> SrcLoc -> Name
mkInternalName uniq occ loc = Name { n_uniq = getKey# uniq, n_sort = Internal, n_occ = occ, n_loc = loc }
	-- NB: You might worry that after lots of huffing and
	-- puffing we might end up with two local names with distinct
	-- uniques, but the same OccName.  Indeed we can, but that's ok
	--	* the insides of the compiler don't care: they use the Unique
	--	* when printing for -ddump-xxx you can switch on -dppr-debug to get the
	--	  uniques if you get confused
	--	* for interface files we tidyCore first, which puts the uniques
	--	  into the print name (see setNameVisibility below)

mkExternalName :: Unique -> Module -> OccName -> SrcLoc -> Name
mkExternalName uniq mod occ loc 
  = Name { n_uniq = getKey# uniq, n_sort = External mod,
           n_occ = occ, n_loc = loc }

mkWiredInName :: Module -> OccName -> Unique -> TyThing -> BuiltInSyntax
        -> Name
mkWiredInName mod occ uniq thing built_in
  = Name { n_uniq = getKey# uniq,
	   n_sort = WiredIn mod thing built_in,
	   n_occ = occ, n_loc = wiredInSrcLoc }

mkSystemName :: Unique -> OccName -> Name
mkSystemName uniq occ = Name { n_uniq = getKey# uniq, n_sort = System, 
			       n_occ = occ, n_loc = noSrcLoc }

mkSystemVarName :: Unique -> FastString -> Name
mkSystemVarName uniq fs = mkSystemName uniq (mkVarOccFS fs)

mkSysTvName :: Unique -> FastString -> Name
mkSysTvName uniq fs = mkSystemName uniq (mkOccNameFS tvName fs) 

mkFCallName :: Unique -> String -> Name
	-- The encoded string completely describes the ccall
mkFCallName uniq str =  Name { n_uniq = getKey# uniq, n_sort = Internal, 
			       n_occ = mkVarOcc str, n_loc = noSrcLoc }

mkTickBoxOpName :: Unique -> String -> Name
mkTickBoxOpName uniq str 
   = Name { n_uniq = getKey# uniq, n_sort = Internal, 
	    n_occ = mkVarOcc str, n_loc = noSrcLoc }

mkIPName :: Unique -> OccName -> Name
mkIPName uniq occ
  = Name { n_uniq = getKey# uniq,
	   n_sort = Internal,
	   n_occ  = occ,
	   n_loc = noSrcLoc }
\end{code}

\begin{code}
-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique :: Name -> Unique -> Name
setNameUnique name uniq = name {n_uniq = getKey# uniq}

tidyNameOcc :: Name -> OccName -> Name
-- We set the OccName of a Name when tidying
-- In doing so, we change System --> Internal, so that when we print
-- it we don't get the unique by default.  It's tidy now!
tidyNameOcc name@(Name { n_sort = System }) occ = name { n_occ = occ, n_sort = Internal}
tidyNameOcc name 			    occ = name { n_occ = occ }

localiseName :: Name -> Name
localiseName n = n { n_sort = Internal }
\end{code}


%************************************************************************
%*									*
\subsection{Predicates and selectors}
%*									*
%************************************************************************

\begin{code}
hashName :: Name -> Int
hashName name = getKey (nameUnique name)
\end{code}


%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = I# (n_uniq n1) `compare` I# (n_uniq n2)
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
\subsection{Binary}
%*									*
%************************************************************************

\begin{code}
instance Binary Name where
   put_ bh name = do
      case getUserData bh of { 
        UserData { ud_symtab_map = symtab_map_ref,
                   ud_symtab_next = symtab_next } -> do
         symtab_map <- readIORef symtab_map_ref
         case lookupUFM symtab_map name of
           Just (off,_) -> put_ bh off
           Nothing -> do
              off <- readFastMutInt symtab_next
              writeFastMutInt symtab_next (off+1)
              writeIORef symtab_map_ref
                  $! addToUFM symtab_map name (off,name)
              put_ bh off          
     }

   get bh = do
        i <- get bh
        return $! (ud_symtab (getUserData bh) ! i)
\end{code}

%************************************************************************
%*									*
\subsection{Pretty printing}
%*									*
%************************************************************************

\begin{code}
instance Outputable Name where
    ppr name = pprName name

instance OutputableBndr Name where
    pprBndr _ name = pprName name

pprName name@(Name {n_sort = sort, n_uniq = u#, n_occ = occ})
  = getPprStyle $ \ sty ->
    case sort of
      WiredIn mod _ builtin   -> pprExternal sty uniq mod occ True  builtin
      External mod  	      -> pprExternal sty uniq mod occ False UserSyntax
      System   		      -> pprSystem sty uniq occ
      Internal    	      -> pprInternal sty uniq occ
  where uniq = mkUniqueGrimily (I# u#)

pprExternal sty uniq mod occ is_wired is_builtin
  | codeStyle sty        = ppr mod <> char '_' <> ppr_z_occ_name occ
	-- In code style, always qualify
	-- ToDo: maybe we could print all wired-in things unqualified
	-- 	 in code style, to reduce symbol table bloat?
 | debugStyle sty       = ppr mod <> dot <> ppr_occ_name occ
		<> braces (hsep [if is_wired then ptext SLIT("(w)") else empty,
				 pprNameSpaceBrief (occNameSpace occ), 
		 		 pprUnique uniq])
  | BuiltInSyntax <- is_builtin  = ppr_occ_name occ
	-- never qualify builtin syntax
  | Just mod <- qualName sty mod occ = ppr mod <> dot <> ppr_occ_name occ
        -- the PrintUnqualified tells us how to qualify this Name, if at all
  | otherwise		          = ppr_occ_name occ

pprInternal sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | debugStyle sty = ppr_occ_name occ <> braces (hsep [pprNameSpaceBrief (occNameSpace occ), 
				 		       pprUnique uniq])
  | dumpStyle sty  = ppr_occ_name occ <> char '_' <> pprUnique uniq
			-- For debug dumps, we're not necessarily dumping
			-- tidied code, so we need to print the uniques.
  | otherwise      = ppr_occ_name occ	-- User style

-- Like Internal, except that we only omit the unique in Iface style
pprSystem sty uniq occ
  | codeStyle sty  = pprUnique uniq
  | debugStyle sty = ppr_occ_name occ <> char '_' <> pprUnique uniq
		     <> braces (pprNameSpaceBrief (occNameSpace occ))
  | otherwise	   = ppr_occ_name occ <> char '_' <> pprUnique uniq
				-- If the tidy phase hasn't run, the OccName
				-- is unlikely to be informative (like 's'),
				-- so print the unique

ppr_occ_name occ = ftext (occNameFS occ)
	-- Don't use pprOccName; instead, just print the string of the OccName; 
	-- we print the namespace in the debug stuff above

-- In code style, we Z-encode the strings.  The results of Z-encoding each FastString are
-- cached behind the scenes in the FastString implementation.
ppr_z_occ_name occ = ftext (zEncodeFS (occNameFS occ))
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

getSrcLoc	    = nameSrcLoc	   . getName
getOccString 	    = occNameString	   . getOccName
\end{code}

