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

	nameUnique, setNameUnique, setNameProvenance, getNameProvenance, setNameImportReason,
	tidyTopName, 
	nameOccName, nameModule, setNameOcc, nameRdrName, setNameModule,

	isExportedName,	nameSrcLoc,
	isLocallyDefinedName,

	isSystemName, isLocalName, isGlobalName, isExternallyVisibleName,


	-- Provenance
	Provenance(..), ImportReason(..), pprProvenance,
	ExportFlag(..), PrintUnqualified,
        pprNameProvenance, systemProvenance,

	-- Class NamedThing and overloaded friends
	NamedThing(..),
	isExported, 
	getSrcLoc, isLocallyDefined, getOccString
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Var   ( Id, setIdName )
import {-# SOURCE #-} TyCon ( TyCon, setTyConName )

import OccName		-- All of it
import RdrName		( RdrName, mkRdrQual, mkRdrUnqual )
import CmdLineOpts	( opt_PprStyle_NoPrags, opt_OmitInterfacePragmas, opt_EnsureSplittableC )

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
data Name = Name {
		n_sort :: NameSort,	-- What sort of name it is
		n_uniq :: Unique,
		n_occ  :: OccName,	-- Its occurrence name
		n_prov :: Provenance	-- How it was made
	    }

data NameSort
  = Local
  | Global Module
  | WiredInId Module Id
  | WiredInTyCon Module TyCon
\end{code}

Things with a @Global@ name are given C static labels, so they finally
appear in the .o file's symbol table.  They appear in the symbol table
in the form M.n.  If originally-local things have this property they
must be made @Global@ first.

\begin{code}
mkLocalName :: Unique -> OccName -> SrcLoc -> Name
mkLocalName uniq occ loc = Name { n_uniq = uniq, n_sort = Local, n_occ = occ, 
				  n_prov = LocalDef loc NotExported }
	-- NB: You might worry that after lots of huffing and
	-- puffing we might end up with two local names with distinct
	-- uniques, but the same OccName.  Indeed we can, but that's ok
	--	* the insides of the compiler don't care: they use the Unique
	--	* when printing for -ddump-xxx you can switch on -dppr-debug to get the
	--	  uniques if you get confused
	--	* for interface files we tidyCore first, which puts the uniques
	--	  into the print name (see setNameVisibility below)

mkGlobalName :: Unique -> Module -> OccName -> Provenance -> Name
mkGlobalName uniq mod occ prov = Name { n_uniq = uniq, n_sort = Global mod,
					n_occ = occ, n_prov = prov }
				

mkSysLocalName :: Unique -> FAST_STRING -> Name
mkSysLocalName uniq fs = Name { n_uniq = uniq, n_sort = Local, 
				n_occ = mkSrcVarOcc fs, n_prov = SystemProv }

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
  = Name { n_uniq = uniq, 
	   n_sort = mk_top_sort mod,
	   n_occ  = mkSrcVarOcc (_PK_ ((_UNPK_ fs) ++ show uniq)),
	   n_prov = LocalDef noSrcLoc NotExported }

------------------------- Wired in names -------------------------

mkWiredInIdName :: Unique -> Module -> OccName -> Id -> Name
mkWiredInIdName uniq mod occ id = Name { n_uniq = uniq, n_sort = WiredInId mod id,
					 n_occ = occ, n_prov = SystemProv }

-- mkWiredInTyConName takes a FAST_STRING instead of
-- an OccName, which is a bit yukky but that's what the 
-- clients find easiest.
mkWiredInTyConName :: Unique -> Module -> FAST_STRING -> TyCon -> Name
mkWiredInTyConName uniq mod fs tycon
  = Name { n_uniq = uniq, n_sort = WiredInTyCon mod tycon,
	   n_occ = mkSrcOccFS tcName fs, n_prov = SystemProv }

fixupSystemName :: Name -> Module -> Provenance -> Name
	-- Give the SystemProv name an appropriate provenance, and
	-- perhaps change the Moulde too (so that its HiFlag is right)
	-- There is a painful hack in that we want to push this
	-- better name into an WiredInId/TyCon so that it prints
	-- nicely in error messages
fixupSystemName name@(Name {n_sort = Global _}) mod' prov'
  = name {n_sort = Global mod', n_prov = prov'}

fixupSystemName name@(Name {n_sort = WiredInId _ id}) mod' prov'
  = name'
  where
    name' = name {n_sort = WiredInId mod' id', n_prov = prov'}
    id'   = setIdName id name'

fixupSystemName name@(Name {n_sort = WiredInTyCon _ tc}) mod' prov'
  = name'
  where
    name' = name {n_sort = WiredInTyCon mod' tc', n_prov = prov'}
    tc'   = setTyConName tc name'

---------------------------------------------------------------------
mkDerivedName :: (OccName -> OccName)
	      -> Name		-- Base name
	      -> Unique		-- New unique
	      -> Name		-- Result is always a value name

mkDerivedName f name uniq = name {n_uniq = uniq, n_occ = f (n_occ name)}

-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
setNameUnique name uniq = name {n_uniq = uniq}

setNameOcc :: Name -> OccName -> Name
	-- Give the thing a new OccName, *and*
	-- record that it's no longer a sys-local
	-- This is used by the tidy-up pass
setNameOcc name occ = name {n_occ = occ}

setNameModule :: Name -> Module -> Name
setNameModule name mod = name {n_sort = set (n_sort name)}
		       where
			 set (Global _)             = Global mod
			 set (WiredInId _ id)       = WiredInId mod id
			 set (WiredInTyCon _ tycon) = WiredInTyCon mod tycon
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
    (env', occ') = tidyOccName env (n_occ name)

    name'        = Name { n_uniq = n_uniq name, n_sort = mk_top_sort mod,
			  n_occ = occ', n_prov = LocalDef noSrcLoc NotExported }

mk_top_sort mod | all_toplev_ids_visible = Global mod
		| otherwise		 = Local

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
setNameProvenance name prov = name {n_prov = prov}

getNameProvenance :: Name -> Provenance
getNameProvenance name = n_prov name

setNameImportReason :: Name -> ImportReason -> Name
setNameImportReason name reason
  = name { n_prov = new_prov }
  where
	-- It's important that we don't do the pattern matching
	-- in the top-level clause, else we get a black hole in 
	-- the renamer.  Rather a yukky constraint.  There's only
	-- one call, in RnNames
    old_prov = n_prov name
    new_prov = case old_prov of
		  NonLocalDef _ omit -> NonLocalDef reason omit
		  other		     -> old_prov
\end{code}


%************************************************************************
%*									*
\subsection{Provenance and export info}
%*									*
%************************************************************************

\begin{code}
data Provenance
  = LocalDef			-- Defined locally
	SrcLoc 			-- Defn site
	ExportFlag		-- Whether it's exported

  | NonLocalDef  		-- Defined non-locally
	ImportReason
	PrintUnqualified

  | SystemProv			-- Either (a) a system-generated local with 
				--	      a v short name OccName
				-- or     (b) a known-key global which should have a proper
				--	      provenance attached by the renamer
\end{code}

Sys-provs are only used internally.  When the compiler generates (say)
a fresh desguar variable it always calls it "ds", and of course it gets
a fresh unique.  But when printing -ddump-xx dumps, we must print it with
its unique, because there'll be a lot of "ds" variables.

Names with SystemProv differ in the following ways:
	a) locals have unique attached when printing dumps
	b) unifier eliminates sys tyvars in favour of user provs where possible
	c) renamer replaces SystemProv with a better one

Before anything gets printed in interface files or output code, it's
fed through a 'tidy' processor, which zaps the OccNames to have
unique names; and converts all sys-locals to user locals
If any desugarer sys-locals have survived that far, they get changed to
"ds1", "ds2", etc.

\begin{code}
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
systemProvenance :: Provenance
systemProvenance = SystemProv

-- pprNameProvenance is used in error messages to say where a name came from
pprNameProvenance :: Name -> SDoc
pprNameProvenance name = pprProvenance (getNameProvenance name)

pprProvenance :: Provenance -> SDoc
pprProvenance SystemProv	     = ptext SLIT("System")
pprProvenance (LocalDef loc _)       = ptext SLIT("defined at")    <+> ppr loc
pprProvenance (NonLocalDef ImplicitImport _)
  = ptext SLIT("implicitly imported")
pprProvenance (NonLocalDef (UserImport mod loc _) _) 
  =  ptext SLIT("imported from") <+> ppr mod <+> ptext SLIT("at") <+> ppr loc
\end{code}


%************************************************************************
%*									*
\subsection{Predicates and selectors}
%*									*
%************************************************************************

\begin{code}
nameUnique		:: Name -> Unique
nameOccName		:: Name -> OccName 
nameModule		:: Name -> Module
nameSrcLoc		:: Name -> SrcLoc
isLocallyDefinedName	:: Name -> Bool
isExportedName		:: Name -> Bool
isWiredInName		:: Name -> Bool
isLocalName		:: Name -> Bool
isGlobalName		:: Name -> Bool
isExternallyVisibleName :: Name -> Bool



nameUnique name = n_uniq name
nameOccName name = n_occ name

nameModule name = nameSortModule (n_sort name)

nameSortModule (Global       mod)   = mod
nameSortModule (WiredInId    mod _) = mod
nameSortModule (WiredInTyCon mod _) = mod

nameRdrName :: Name -> RdrName
nameRdrName (Name { n_sort = Local, n_occ = occ }) = mkRdrUnqual occ
nameRdrName (Name { n_sort = sort,  n_occ = occ }) = mkRdrQual (nameSortModule sort) occ

isExportedName (Name { n_prov = LocalDef _ Exported }) = True
isExportedName other				       = False

nameSrcLoc name = provSrcLoc (n_prov name)

provSrcLoc (LocalDef loc _)         	        = loc        
provSrcLoc (NonLocalDef (UserImport _ loc _) _) = loc
provSrcLoc SystemProv				= noSrcLoc   
  
isLocallyDefinedName (Name {n_sort = Local})        = True	-- Local (might have SystemProv)
isLocallyDefinedName (Name {n_prov = LocalDef _ _}) = True	-- Global, but defined here
isLocallyDefinedName other		            = False	-- Other

-- Things the compiler "knows about" are in some sense
-- "imported".  When we are compiling the module where
-- the entities are defined, we need to be able to pick
-- them out, often in combination with isLocallyDefined.
isWiredInName (Name {n_sort = WiredInTyCon _ _}) = True
isWiredInName (Name {n_sort = WiredInId    _ _}) = True
isWiredInName _				         = False

maybeWiredInIdName :: Name -> Maybe Id
maybeWiredInIdName (Name {n_sort = WiredInId _ id}) = Just id
maybeWiredInIdName other			    = Nothing

maybeWiredInTyConName :: Name -> Maybe TyCon
maybeWiredInTyConName (Name {n_sort = WiredInTyCon _ tc}) = Just tc
maybeWiredInTyConName other			          = Nothing


isLocalName (Name {n_sort = Local}) = True
isLocalName _ 		            = False

isGlobalName (Name {n_sort = Local}) = False
isGlobalName other	             = True

-- Global names are by definition those that are visible
-- outside the module, *as seen by the linker*.  Externally visible
-- does not mean visible at the source level (that's isExported).
isExternallyVisibleName name = isGlobalName name

isSystemName (Name {n_prov = SystemProv}) = True
isSystemName other			  = False
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

pprName (Name {n_sort = Local, n_uniq = uniq, n_occ = occ, n_prov = prov})
	-- Locals
  = getPprStyle $ \ sty ->
    if codeStyle sty then
	pprUnique uniq		-- When printing in code we required all names to 
				-- be globally unique; for example, we use this identifier
				-- for the closure name.  So we just print the unique alone.
    else
	pprOccName occ <> pp_local_extra sty uniq
  where
    sys_local = case prov of
		  SystemProv -> True
		  other	     -> False

    pp_local_extra sty uniq
	| sys_local      = underscore <> pprUnique uniq 	-- Must print uniques for sys_locals
	| debugStyle sty = text "{-" <> pprUnique uniq <> text "-}"
	| otherwise	 = empty


pprName (Name {n_sort = sort, n_uniq = uniq, n_occ = occ, n_prov = prov})
	-- Globals, and wired in things
  = getPprStyle $ \ sty ->
    if codeStyle sty then
	ppr mod <> underscore <> ppr occ
    else
    	pp_mod_dot sty <> ppr occ <> pp_global_debug sty uniq prov
  where
    mod = nameSortModule sort

    pp_mod_dot sty
      = case prov of
    	   SystemProv	  			     -> pp_qual mod  dot    user_sty
		-- Hack alert!  Omit the qualifier on SystemProv things, which I claim
		-- will also be WiredIn things. We can't get the omit flag right
		-- on wired in tycons etc (sigh) so we just leave it out in user style, 
		-- and hope that leaving it out isn't too consfusing.
		-- (e.g. if the programmer hides Bool and  redefines it.  If so, use -dppr-debug.)

    	   LocalDef _ _				     -> pp_qual mod  dot    (user_sty || iface_sty)

    	   NonLocalDef (UserImport imp_mod _ _) omit 
		| user_sty			     -> pp_qual imp_mod pp_sep omit
		| otherwise			     -> pp_qual mod     pp_sep False
    	   NonLocalDef ImplicitImport		omit -> pp_qual mod	pp_sep (user_sty && omit)
      where
        user_sty  = userStyle sty
        iface_sty = ifaceStyle sty
    
    pp_qual mod sep omit_qual
        | omit_qual  = empty
        | otherwise  = pprModule mod <> sep
    
    pp_sep | bootFlavour (moduleIfaceFlavour mod) = text "!"	-- M!t indicates a name imported 
								-- from a .hi-boot interface
	   | otherwise				  = dot		-- Vanilla case
   
    pp_global_debug sty uniq prov
      | debugStyle sty = hcat [text "{-", pprUnique uniq, prov_p prov, text "-}"]
      | otherwise      = empty

    prov_p prov | opt_PprStyle_NoPrags = empty
	        | otherwise	       = comma <> pp_prov prov

pp_prov (LocalDef _ Exported)          = char 'x'
pp_prov (LocalDef _ NotExported)       = char 'l'
pp_prov (NonLocalDef ImplicitImport _) = char 'j'
pp_prov (NonLocalDef (UserImport _ _ True ) _) = char 'I'	-- Imported by name
pp_prov (NonLocalDef (UserImport _ _ False) _) = char 'i'	-- Imported by ..
pp_prov SystemProv	     	       = char 's'
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
isLocallyDefined    :: NamedThing a => a -> Bool
isExported	    :: NamedThing a => a -> Bool
getOccString	    :: NamedThing a => a -> String

isExported	    = isExportedName 	   . getName
getSrcLoc	    = nameSrcLoc	   . getName
isLocallyDefined    = isLocallyDefinedName . getName
getOccString x	    = occNameString (getOccName x)
\end{code}

\begin{code}
{-# SPECIALIZE isLocallyDefined :: Name -> Bool #-}
\end{code}
