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
	mkLocalName, mkSysLocalName, mkTopName,
	mkDerivedName, mkGlobalName,
	mkWiredInIdName,   mkWiredInTyConName,
	maybeWiredInIdName, maybeWiredInTyConName,
	isWiredInName,

	nameUnique, setNameUnique, setNameProvenance, getNameProvenance,
	tidyTopName, mkNameVisible,
	nameOccName, nameModule, setNameOcc,

	isExportedName,	nameSrcLoc,
	isLocallyDefinedName,

	isSysLocalName, isLocalName, isGlobalName, isExternallyVisibleName,

        pprNameProvenance,

	-- Misc
	Provenance(..), ImportReason(..), pprProvenance,
	ExportFlag(..), PrintUnqualified,

	-- Class NamedThing and overloaded friends
	NamedThing(..),
	modAndOcc, isExported, 
	getSrcLoc, isLocallyDefined, getOccString
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Var   ( Id )
import {-# SOURCE #-} TyCon ( TyCon )

import OccName		-- All of it
import CmdLineOpts	( opt_PprStyle_NoPrags, opt_OmitInterfacePragmas, opt_EnsureSplittableC )
import BasicTypes	( IfaceFlavour(..) )

import SrcLoc		( noSrcLoc, mkBuiltinSrcLoc, SrcLoc )
import Unique		( pprUnique, Unique, Uniquable(..) )
import Outputable
import GlaExts
\end{code}


%************************************************************************
%*									*
\subsection[Name-datatype]{The @Name@ datatype, and name construction}
%*									*
%************************************************************************
 
\begin{code}
data Name
  = Local    Unique
	     OccName		-- How to print it
	     Bool		-- True <=> this is a "sys-local"
				-- see notes just below


  | Global   Unique
	     Module		-- The defining module
	     OccName		-- Its name in that module
             Provenance		-- How it was defined
\end{code}

Sys-locals are only used internally.  When the compiler generates (say)
a fresh desguar variable it always calls it "ds", and of course it gets
a fresh unique.  But when printing -ddump-xx dumps, we must print it with
its unique, because there'll be a lot of "ds" variables.  That debug
printing issue is the ONLY way in which sys-locals are different.  I think.

Before anything gets printed in interface files or output code, it's
fed through a 'tidy' processor, which zaps the OccNames to have
unique names; and converts all sys-locals to ordinary locals
If any desugarer sys-locals have survived that far, they get changed to
"ds1", "ds2", etc.

Things with a @Global@ name are given C static labels, so they finally
appear in the .o file's symbol table.  They appear in the symbol table
in the form M.n.  If originally-local things have this property they
must be made @Global@ first.


\begin{code}
mkLocalName    :: Unique -> OccName -> Name
mkLocalName uniq occ = Local uniq occ False
	-- NB: You might worry that after lots of huffing and
	-- puffing we might end up with two local names with distinct
	-- uniques, but the same OccName.  Indeed we can, but that's ok
	--	* the insides of the compiler don't care: they use the Unique
	--	* when printing for -ddump-xxx you can switch on -dppr-debug to get the
	--	  uniques if you get confused
	--	* for interface files we tidyCore first, which puts the uniques
	--	  into the print name (see setNameVisibility below)

mkGlobalName :: Unique -> Module -> OccName -> Provenance -> Name
mkGlobalName = Global

mkSysLocalName :: Unique -> FAST_STRING -> Name
mkSysLocalName uniq fs = Local uniq (varOcc fs) True

mkTopName :: Unique -> Module -> FAST_STRING -> Name
	-- Make a top-level name; make it Global if top-level
	-- things should be externally visible; Local otherwise
	-- This chap is only used *after* the tidyCore phase
	-- Notably, it is used during STG lambda lifting
	--
	-- We have to make sure that the name is globally unique
	-- and we don't have tidyCore to help us. So we append
	-- the unique.  Hack!  Hack!
mkTopName uniq mod fs 
  | all_toplev_ids_visible = Global uniq mod occ (LocalDef noSrcLoc NotExported)
  | otherwise		   = Local uniq occ False
  where
    occ = varOcc (_PK_ ((_UNPK_ fs) ++ show uniq))

mkWiredInIdName :: Unique -> Module -> OccName -> Id -> Name
mkWiredInIdName uniq mod occ id = Global uniq mod occ (WiredInId id)

-- mkWiredInTyConName takes a FAST_STRING instead of
-- an OccName, which is a bit yukky but that's what the 
-- clients find easiest.
mkWiredInTyConName :: Unique -> Module -> FAST_STRING -> TyCon -> Name
mkWiredInTyConName uniq mod occ tycon
  = Global uniq mod (tcOcc occ) (WiredInTyCon tycon)

mkDerivedName :: (OccName -> OccName)
	      -> Name		-- Base name
	      -> Unique		-- New unique
	      -> Name		-- Result is always a value name

mkDerivedName f (Global _ mod occ prov) uniq = Global uniq mod (f occ) prov
mkDerivedName f (Local _ occ sys)       uniq = Local uniq (f occ) sys

-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique (Local _ occ sys)        u = Local u occ sys
setNameUnique (Global  _ mod occ prov) u = Global u mod occ prov

setNameOcc :: Name -> OccName -> Name
	-- Give the thing a new OccName, *and*
	-- record that it's no longer a sys-local
	-- This is used by the tidy-up pass
setNameOcc (Global uniq mod _ prov) occ = Global uniq mod occ prov
setNameOcc (Local uniq _ sys)	    occ = Local uniq occ False
\end{code}


%************************************************************************
%*									*
\subsection{Setting provenance and visibility
%*									*
%************************************************************************

tidyTopName is applied to top-level names in the final program

For top-level things, it globalises Local names 
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
tidyTopName :: Module -> TidyOccEnv -> Name -> (TidyOccEnv, Name)
tidyTopName mod env name
  | isExported name = (env, name)	-- Don't fiddle with an exported name
					-- It should be in the TidyOccEnv already
  | otherwise       = (env', name')
  where
    prov	 = getNameProvenance name
    uniq         = nameUnique name
    (env', occ') = tidyOccName env (nameOccName name)

    name' | all_toplev_ids_visible = Global uniq mod occ' prov
	  | otherwise		   = Local uniq occ' False

all_toplev_ids_visible = 
	not opt_OmitInterfacePragmas ||  -- Pragmas can make them visible
	opt_EnsureSplittableC            -- Splitting requires visiblilty
\end{code}

\begin{code}
setNameProvenance :: Name -> Provenance -> Name	
	-- setNameProvenance used to only change the provenance of 
	-- Implicit-provenance things, but that gives bad error messages 
	-- for names defined twice in the same module, so I changed it to 
	-- set the provenance of *any* global (SLPJ Jun 97)
setNameProvenance (Global uniq mod occ _) prov = Global uniq mod occ prov
setNameProvenance other_name 		  prov = other_name

getNameProvenance :: Name -> Provenance
getNameProvenance (Global uniq mod occ prov) = prov
getNameProvenance (Local _ _ _)              = LocalDef noSrcLoc NotExported
\end{code}

\begin{code}
-- make the Name globally visible regardless.
mkNameVisible :: Module -> Unique -> Name -> Name
mkNameVisible mod occ_uniq nm@(Global _ _ _ _) = nm
mkNameVisible mod occ_uniq nm@(Local uniq occ _)
 = Global uniq mod occ (LocalDef noSrcLoc Exported)
\end{code}


%************************************************************************
%*									*
\subsection{Provenance and export info}
%*									*
%************************************************************************

\begin{code}
data Provenance
  = NoProvenance 

  | LocalDef			-- Defined locally
	SrcLoc 			-- Defn site
	ExportFlag		-- Whether it's exported

  | NonLocalDef  		-- Defined non-locally
	ImportReason
	IfaceFlavour		-- Whether the defn site is an .hi-boot file
	PrintUnqualified

  | WiredInTyCon TyCon			-- There's a wired-in version
  | WiredInId    Id			-- ...ditto...

data ImportReason
  = UserImport Module SrcLoc Bool	-- Imported from module M on line L
					-- Note the M may well not be the defining module
					-- for this thing!
	-- The Bool is true iff the thing was named *explicitly* in the import spec,
	-- rather than being imported as part of a group; e.g.
	--	import B
	--	import C( T(..) )
	-- Here, everything imported by B, and the constructors of T
	-- are not named explicitly; only T is named explicitly.
	-- This info is used when warning of unused names.

  | ImplicitImport			-- Imported implicitly for some other reason
			

type PrintUnqualified = Bool	-- True <=> the unqualified name of this thing is
				-- in scope in this module, so print it 
				-- unqualified in error messages

data ExportFlag = Exported  | NotExported
\end{code}

Something is "Exported" if it may be mentioned by another module without
warning.  The crucial thing about Exported things is that they must
never be dropped as dead code, even if they aren't used in this module.
Furthermore, being Exported means that we can't see all call sites of the thing.

Exported things include:

	- explicitly exported Ids, including data constructors, 
	  class method selectors

	- dfuns from instance decls

Being Exported is *not* the same as finally appearing in the .o file's 
symbol table.  For example, a local Id may be mentioned in an Exported
Id's unfolding in the interface file, in which case the local Id goes
out too.


\begin{code}
-- pprNameProvenance is used in error messages to say where a name came from
pprNameProvenance :: Name -> SDoc
pprNameProvenance name = pprProvenance (getNameProvenance name)

pprProvenance :: Provenance -> SDoc
pprProvenance NoProvenance	     = ptext SLIT("No provenance")
pprProvenance (LocalDef loc _)       = ptext SLIT("defined at")    <+> ppr loc
pprProvenance (WiredInTyCon tc)      = ptext SLIT("Wired-in tycon")
pprProvenance (WiredInId id)         = ptext SLIT("Wired-in id")
pprProvenance (NonLocalDef ImplicitImport _ _)
  = ptext SLIT("implicitly imported")
pprProvenance (NonLocalDef (UserImport mod loc _) _ _) 
  =  ptext SLIT("imported from") <+> ppr mod <+> ptext SLIT("at") <+> ppr loc
\end{code}


%************************************************************************
%*									*
\subsection{Predicates and selectors}
%*									*
%************************************************************************

\begin{code}
nameUnique		:: Name -> Unique
nameModAndOcc		:: Name -> (Module, OccName)	-- Globals only
nameOccName		:: Name -> OccName 
nameModule		:: Name -> Module
nameSrcLoc		:: Name -> SrcLoc
isLocallyDefinedName	:: Name -> Bool
isExportedName		:: Name -> Bool
isWiredInName		:: Name -> Bool
isLocalName		:: Name -> Bool
isGlobalName		:: Name -> Bool
isExternallyVisibleName :: Name -> Bool



nameUnique (Local  u _ _)   = u
nameUnique (Global u _ _ _) = u

nameOccName (Local _ occ _)    = occ
nameOccName (Global _ _ occ _) = occ

nameModule (Global _ mod occ _) = mod

nameModAndOcc (Global _ mod occ _) = (mod,occ)

isExportedName (Global _ _ _ (LocalDef _ Exported)) = True
isExportedName other				    = False

nameSrcLoc (Global _ _ _ (LocalDef loc _))         		 = loc        
nameSrcLoc (Global _ _ _ (NonLocalDef (UserImport _ loc _) _ _)) = loc
nameSrcLoc (Global _ _ _ (WiredInTyCon _))         		 = mkBuiltinSrcLoc
nameSrcLoc (Global _ _ _ (WiredInId _))            		 = mkBuiltinSrcLoc
nameSrcLoc other			           		 = noSrcLoc   
  
isLocallyDefinedName (Local  _ _ _)	     	   = True
isLocallyDefinedName (Global _ _ _ (LocalDef _ _)) = True
isLocallyDefinedName other		           = False

-- Things the compiler "knows about" are in some sense
-- "imported".  When we are compiling the module where
-- the entities are defined, we need to be able to pick
-- them out, often in combination with isLocallyDefined.
isWiredInName (Global _ _ _ (WiredInTyCon _)) = True
isWiredInName (Global _ _ _ (WiredInId    _)) = True
isWiredInName _				      = False

maybeWiredInIdName :: Name -> Maybe Id
maybeWiredInIdName (Global _ _ _ (WiredInId id)) = Just id
maybeWiredInIdName other			 = Nothing

maybeWiredInTyConName :: Name -> Maybe TyCon
maybeWiredInTyConName (Global _ _ _ (WiredInTyCon tc)) = Just tc
maybeWiredInTyConName other			       = Nothing


isLocalName (Local _ _ _) = True
isLocalName _ 		  = False

isSysLocalName (Local _ _ sys) = sys
isSysLocalName other	       = False

isGlobalName (Global _ _ _ _) = True
isGlobalName other	      = False

-- Global names are by definition those that are visible
-- outside the module, *as seen by the linker*.  Externally visible
-- does not mean visible at the source level (that's isExported).
isExternallyVisibleName name = isGlobalName name
\end{code}


%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = c n1 n2
  where
    c (Local  u1 _ _)   (Local  u2 _ _)   = compare u1 u2
    c (Local   _ _ _)   _		  = LT
    c (Global u1 _ _ _) (Global u2 _ _ _) = compare u1 u2
    c (Global  _ _ _ _) _		  = GT
\end{code}

\begin{code}
instance Eq Name where
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord Name where
    a <= b = case (a `compare` b) of { LT -> True;	EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;	EQ -> False; GT -> False }
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
\subsection{Pretty printing}
%*									*
%************************************************************************

\begin{code}
instance Outputable Name where
	-- When printing interfaces, all Locals have been given nice print-names
    ppr name = pprName name

pprName (Local uniq occ sys_local)
  = getPprStyle $ \ sty ->
    if codeStyle sty then
	pprUnique uniq		-- When printing in code we required all names to 
				-- be globally unique; for example, we use this identifier
				-- for the closure name.  So we just print the unique alone.
    else
	pprOccName occ <> pp_local_extra sty uniq
  where
    pp_local_extra sty uniq
	| sys_local      = underscore <> pprUnique uniq 	-- Must print uniques for sys_locals
	| debugStyle sty = text "{-" <> pprUnique uniq <> text "-}"
	| otherwise	 = empty


pprName (Global uniq mod occ prov)
  = getPprStyle $ \ sty ->
    if codeStyle sty then
	ppr mod <> underscore <> ppr occ
    else
    	pp_mod_dot sty <> ppr occ <> pp_global_debug sty uniq prov
  where
    pp_mod_dot sty
      = case prov of   -- Omit home module qualifier if in scope 
    	   LocalDef _ _           -> pp_qual dot (user_sty || iface_sty)
    	   NonLocalDef _ hif omit -> pp_qual (pp_hif hif) (omit && user_sty)
    			 -- Hack: omit qualifers on wired in things
    			 -- in user style only
    	   WiredInTyCon _ 	-> pp_qual dot user_sty
    	   WiredInId _		-> pp_qual dot user_sty
    	   NoProvenance    	-> pp_qual dot False
      where
        user_sty  = userStyle sty
        iface_sty = ifaceStyle sty
    
    pp_qual sep omit_qual
        | omit_qual  = empty
        | otherwise	 = pprModule mod <> sep
    
    pp_hif HiFile     = dot	 -- Vanilla case
    pp_hif HiBootFile = text "!"  -- M!t indicates a name imported from a .hi-boot interface
   
    pp_global_debug sty uniq prov
      | debugStyle sty = hcat [text "{-", pprUnique uniq, prov_p prov, text "-}"]
      | otherwise      = empty

    prov_p prov | opt_PprStyle_NoPrags = empty
	        | otherwise	       = comma <> pp_prov prov

pp_prov (LocalDef _ Exported)    	 = char 'x'
pp_prov (LocalDef _ NotExported) 	 = char 'l'
pp_prov (NonLocalDef ImplicitImport _ _) = char 'i'
pp_prov (NonLocalDef explicitimport _ _) = char 'I'
pp_prov (WiredInTyCon _)         	 = char 'W'
pp_prov (WiredInId _)      	 	 = char 'w'
pp_prov NoProvenance     	 	 = char '?'
\end{code}


%************************************************************************
%*									*
\subsection{Overloaded functions related to Names}
%*									*
%************************************************************************

\begin{code}
class NamedThing a where
    getOccName :: a -> OccName		-- Even RdrNames can do this!
    getName    :: a -> Name

    getOccName n = nameOccName (getName n)	-- Default method
\end{code}

\begin{code}
modAndOcc	    :: NamedThing a => a -> (Module, OccName)
getSrcLoc	    :: NamedThing a => a -> SrcLoc
isLocallyDefined    :: NamedThing a => a -> Bool
isExported	    :: NamedThing a => a -> Bool
getOccString	    :: NamedThing a => a -> String

modAndOcc	    = nameModAndOcc	   . getName
isExported	    = isExportedName 	   . getName
getSrcLoc	    = nameSrcLoc	   . getName
isLocallyDefined    = isLocallyDefinedName . getName
getOccString x	    = occNameString (getOccName x)
\end{code}

\begin{code}
{-# SPECIALIZE isLocallyDefined :: Name -> Bool #-}
\end{code}
