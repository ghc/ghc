%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
#include "HsVersions.h"

module Name (
	-- Re-export the Module type
	SYN_IE(Module),
	pprModule, moduleString,

	-- The OccName type
	OccName(..),
	pprOccName, occNameString, occNameFlavour, 
	isTvOcc, isTCOcc, isVarOcc, prefixOccName,
	uniqToOccName,

	-- The Name type
	Name,					-- Abstract
	mkLocalName, mkSysLocalName, 

	mkCompoundName, mkGlobalName, mkInstDeclName,

	mkWiredInIdName,   mkWiredInTyConName,
	maybeWiredInIdName, maybeWiredInTyConName,
	isWiredInName,

	nameUnique, changeUnique, setNameProvenance, getNameProvenance,
	setNameVisibility,
	nameOccName, nameString, nameModule,

	isExportedName,	nameSrcLoc,
	isLocallyDefinedName,

	isLocalName, 

        pprNameProvenance,

	-- Sets of Names
	SYN_IE(NameSet),
	emptyNameSet, unitNameSet, mkNameSet, unionNameSets, unionManyNameSets,
	minusNameSet, elemNameSet, nameSetToList, addOneToNameSet, addListToNameSet, isEmptyNameSet,

	-- Misc
	Provenance(..), pprProvenance,
	ExportFlag(..),

	-- Class NamedThing and overloaded friends
	NamedThing(..),
	modAndOcc, isExported, 
	getSrcLoc, isLocallyDefined, getOccString
    ) where

IMP_Ubiq()
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(TyLoop)	( GenId, Id(..), TyCon )			-- Used inside Names
#else
import {-# SOURCE #-} Id    ( Id )
import {-# SOURCE #-} TyCon ( TyCon )
#endif

import CStrings		( identToC, modnameToC, cSEP )
import CmdLineOpts	( opt_OmitInterfacePragmas, opt_EnsureSplittableC, all_toplev_ids_visible )
import BasicTypes	( SYN_IE(Module), IfaceFlavour(..), moduleString, pprModule )

import Outputable	( Outputable(..), PprStyle(..), codeStyle, ifaceStyle, userStyle )
import PrelMods		( gHC__ )
import Pretty
import Lex		( isLexSym, isLexConId )
import SrcLoc		( noSrcLoc, SrcLoc )
import Usage            ( SYN_IE(UVar), SYN_IE(Usage) )
import Unique		( pprUnique, showUnique, Unique, Uniquable(..) )
import UniqSet		( UniqSet(..), emptyUniqSet, unitUniqSet, unionUniqSets, uniqSetToList, isEmptyUniqSet,
		 	  unionManyUniqSets, minusUniqSet, mkUniqSet, elementOfUniqSet, addListToUniqSet, addOneToUniqSet )
import UniqFM		( UniqFM )
import Util		( Ord3(..), cmpPString, panic, assertPanic {-, pprTrace ToDo:rm-} )
\end{code}


%************************************************************************
%*									*
\subsection[Name-pieces-datatypes]{The @OccName@ datatypes}
%*									*
%************************************************************************

\begin{code}
data OccName  = VarOcc  FAST_STRING	-- Variables and data constructors
	      | TvOcc	FAST_STRING	-- Type variables
	      | TCOcc	FAST_STRING	-- Type constructors and classes

pprOccName :: PprStyle -> OccName -> Doc
pprOccName sty      n = if codeStyle sty 
			then identToC (occNameString n)
			else ptext (occNameString n)

occNameString :: OccName -> FAST_STRING
occNameString (VarOcc s)  = s
occNameString (TvOcc s)   = s
occNameString (TCOcc s)   = s

prefixOccName :: FAST_STRING -> OccName -> OccName
prefixOccName prefix (VarOcc s) = VarOcc (prefix _APPEND_ s)
prefixOccName prefix (TvOcc s)  = TvOcc (prefix _APPEND_ s)
prefixOccName prefix (TCOcc s) = TCOcc (prefix _APPEND_ s)

-- occNameFlavour is used only to generate good error messages, so it doesn't matter
-- that the VarOcc case isn't mega-efficient.  We could have different Occ constructors for
-- data constructors and values, but that makes everything else a bit more complicated.
occNameFlavour :: OccName -> String
occNameFlavour (VarOcc s) | isLexConId s = "Data constructor"
			  | otherwise    = "Value"
occNameFlavour (TvOcc s)  = "Type variable"
occNameFlavour (TCOcc s)  = "Type constructor or class"

isVarOcc, isTCOcc, isTvOcc :: OccName -> Bool
isVarOcc (VarOcc s) = True
isVarOcc other     = False

isTvOcc (TvOcc s) = True
isTvOcc other     = False

isTCOcc (TCOcc s) = True
isTCOcc other     = False

instance Eq OccName where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True }

instance Ord OccName where
    a <= b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }

instance Ord3 OccName where
    cmp = cmpOcc

(VarOcc s1) `cmpOcc` (VarOcc s2) = s1 `_CMP_STRING_` s2
(VarOcc s1) `cmpOcc` other2      = LT_

(TvOcc s1)  `cmpOcc` (VarOcc s2) = GT_
(TvOcc s1)  `cmpOcc` (TvOcc s2)  = s1 `_CMP_STRING_` s2
(TvOcc s1)  `cmpOcc` other	 = LT_

(TCOcc s1) `cmpOcc` (TCOcc s2) = s1 `_CMP_STRING_` s2
(TCOcc s1) `cmpOcc` other      = GT_

instance Outputable OccName where
  ppr = pprOccName
\end{code}


%************************************************************************
%*									*
\subsection[Name-datatype]{The @Name@ datatype, and name construction}
%*									*
%************************************************************************
 
\begin{code}
data Name
  = Local    Unique
             OccName
             SrcLoc

  | Global   Unique
	     Module		-- The defining module
	     OccName		-- Its name in that module
             Provenance		-- How it was defined
\end{code}

Things with a @Global@ name are given C static labels, so they finally
appear in the .o file's symbol table.  They appear in the symbol table
in the form M.n.  If originally-local things have this property they
must be made @Global@ first.

\begin{code}
data Provenance
  = LocalDef ExportFlag SrcLoc		-- Locally defined
  | Imported Module SrcLoc IfaceFlavour	-- Directly imported from M; 
					-- 		gives name of module in import statement
					--		and locn of import statement
  | Implicit IfaceFlavour		-- Implicitly imported
  | WiredInTyCon TyCon			-- There's a wired-in version
  | WiredInId    Id			-- ...ditto...
\end{code}

Something is "Exported" if it may be mentioned by another module without
warning.  The crucial thing about Exported things is that they must
never be dropped as dead code, even if they aren't used in this module.
Furthermore, being Exported means that we can't see all call sites of the thing.

Exported things include:
	- explicitly exported Ids, including data constructors, class method selectors
	- dfuns from instance decls

Being Exported is *not* the same as finally appearing in the .o file's 
symbol table.  For example, a local Id may be mentioned in an Exported
Id's unfolding in the interface file, in which case the local Id goes
out too.

\begin{code}
data ExportFlag = Exported  | NotExported
\end{code}

\begin{code}
mkLocalName    :: Unique -> OccName -> SrcLoc -> Name
mkLocalName = Local

mkGlobalName :: Unique -> Module -> OccName -> Provenance -> Name
mkGlobalName = Global

mkSysLocalName :: Unique -> FAST_STRING -> SrcLoc -> Name
mkSysLocalName uniq str loc = Local uniq (VarOcc str) loc

mkWiredInIdName :: Unique -> Module -> FAST_STRING -> Id -> Name
mkWiredInIdName uniq mod occ id 
  = Global uniq mod (VarOcc occ) (WiredInId id)

mkWiredInTyConName :: Unique -> Module -> FAST_STRING -> TyCon -> Name
mkWiredInTyConName uniq mod occ tycon
  = Global uniq mod (TCOcc occ) (WiredInTyCon tycon)


mkCompoundName :: (FAST_STRING -> FAST_STRING)	-- Occurrence-name modifier
	       -> Unique			-- New unique
	       -> Name				-- Base name (must be a Global)
	       -> Name		-- Result is always a value name

mkCompoundName str_fn uniq (Global _ mod occ prov)
  = Global uniq mod new_occ prov
  where    
    new_occ = VarOcc (str_fn (occNameString occ))		-- Always a VarOcc

mkCompoundName str_fn uniq (Local _ occ loc)
  = Local uniq (VarOcc (str_fn (occNameString occ))) loc

	-- Rather a wierd one that's used for names generated for instance decls
mkInstDeclName :: Unique -> Module -> OccName -> SrcLoc -> Bool -> Name
mkInstDeclName uniq mod occ loc from_here
  = Global uniq mod occ prov
  where
    prov | from_here = LocalDef Exported loc
         | otherwise = Implicit HiFile		-- Odd


setNameProvenance :: Name -> Provenance -> Name	
	-- setNameProvenance used to only change the provenance of Implicit-provenance things,
	-- but that gives bad error messages for names defined twice in the same
	-- module, so I changed it to set the proveance of *any* global (SLPJ Jun 97)
setNameProvenance (Global uniq mod occ _) prov = Global uniq mod occ prov
setNameProvenance other_name 		  prov = other_name

getNameProvenance :: Name -> Provenance
getNameProvenance (Global uniq mod occ prov) = prov
getNameProvenance (Local uniq occ locn)      = LocalDef NotExported locn

-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
changeUnique (Local      _ n l)  u = Local u n l
changeUnique (Global   _ mod occ  prov) u = Global u mod occ prov
\end{code}

setNameVisibility is applied to names in the final program

The Maybe Module argument is (Just mod) for top-level values,
and Nothing for all others (local values and type variables)

For top-level things, it globalises Local names 
				(if all top-level things should be visible)
			 and localises non-exported Global names
				 (if only exported things should be visible)

For nested things it localises Global names.

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
setNameVisibility :: Maybe Module -> Unique -> Name -> Name

setNameVisibility maybe_mod occ_uniq name@(Global uniq mod occ (LocalDef NotExported loc))
  | not all_toplev_ids_visible || not_top_level maybe_mod
  = Local uniq (uniqToOccName occ_uniq) loc	-- Localise Global name

setNameVisibility maybe_mod occ_uniq name@(Global _ _ _ _)
  = name					-- Otherwise don't fiddle with Global

setNameVisibility (Just mod) occ_uniq (Local uniq occ loc)
  | all_toplev_ids_visible
  = Global uniq mod	 			-- Globalise Local name
	   (uniqToOccName occ_uniq)
	   (LocalDef NotExported loc)

setNameVisibility maybe_mod occ_uniq (Local uniq occ loc)
  = Local uniq (uniqToOccName occ_uniq) loc	-- New OccName for Local

uniqToOccName uniq = VarOcc (_PK_ ('$':showUnique uniq))
	-- The "$" is to make sure that this OccName is distinct from all user-defined ones

not_top_level (Just m) = False
not_top_level Nothing  = True

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
nameString		:: Name -> FAST_STRING		-- A.b form
nameSrcLoc		:: Name -> SrcLoc
isLocallyDefinedName	:: Name -> Bool
isExportedName		:: Name -> Bool
isWiredInName		:: Name -> Bool
isLocalName		:: Name -> Bool



nameUnique (Local  u _ _)   = u
nameUnique (Global u _ _ _) = u

nameOccName (Local _ occ _)    = occ
nameOccName (Global _ _ occ _) = occ

nameModule (Global _ mod occ _) = mod

nameModAndOcc (Global _ mod occ _) = (mod,occ)

nameString (Local _ occ _)      = occNameString occ
nameString (Global _ mod occ _) = mod _APPEND_ SLIT(".") _APPEND_ occNameString occ

isExportedName (Global _ _ _ (LocalDef Exported _)) = True
isExportedName other				    = False

nameSrcLoc (Local _ _ loc)     = loc
nameSrcLoc (Global _ _ _ (LocalDef _ loc))   = loc
nameSrcLoc (Global _ _ _ (Imported _ loc _)) = loc
nameSrcLoc other			     = noSrcLoc
  
isLocallyDefinedName (Local  _ _ _)	     	     = True
isLocallyDefinedName (Global _ _ _ (LocalDef _ _)) = True
isLocallyDefinedName other		           = False

-- Things the compiler "knows about" are in some sense
-- "imported".  When we are compiling the module where
-- the entities are defined, we need to be able to pick
-- them out, often in combination with isLocallyDefined.
isWiredInName (Global _ _ _ (WiredInTyCon _)) = True
isWiredInName (Global _ _ _ (WiredInId    _)) = True
isWiredInName _				          = False

maybeWiredInIdName :: Name -> Maybe Id
maybeWiredInIdName (Global _ _ _ (WiredInId id)) = Just id
maybeWiredInIdName other			 = Nothing

maybeWiredInTyConName :: Name -> Maybe TyCon
maybeWiredInTyConName (Global _ _ _ (WiredInTyCon tc)) = Just tc
maybeWiredInTyConName other			       = Nothing


isLocalName (Local _ _ _) = True
isLocalName _ 		  = False
\end{code}


%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = c n1 n2
  where
    c (Local  u1 _ _)   (Local  u2 _ _)   = cmp u1 u2
    c (Local   _ _ _)	  _		  = LT_
    c (Global u1 _ _ _) (Global u2 _ _ _) = cmp u1 u2
    c (Global  _ _ _ _)   _		  = GT_
\end{code}

\begin{code}
instance Eq Name where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True }

instance Ord Name where
    a <= b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }

instance Ord3 Name where
    cmp = cmpName

instance Uniquable Name where
    uniqueOf = nameUnique

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
    ppr PprQuote name@(Local _ _ _)  = quotes (ppr (PprForUser 1) name)

	-- When printing interfaces, all Locals have been given nice print-names
    ppr (PprForUser _) (Local _ n _) = ptext (occNameString n)
    ppr PprInterface   (Local _ n _) = ptext (occNameString n)

    ppr sty (Local u n _) | codeStyle sty = pprUnique u

    ppr sty (Local u n _) = hcat [ptext (occNameString n), ptext SLIT("_"), pprUnique u]

    ppr PprQuote name@(Global _ _ _ _) = quotes (ppr (PprForUser 1) name)

    ppr sty name@(Global u m n _)
	| codeStyle sty
	= identToC (m _APPEND_ SLIT(".") _APPEND_ occNameString n)

    ppr sty name@(Global u m n prov)
	= hcat [pp_mod_dot, ptext (occNameString n), pp_debug sty name]
	where
	  pp_mod = pprModule (PprForUser 1) m 

	  pp_mod_dot | userStyle sty		-- Omit qualifier in user style
		     = empty
		     | otherwise
	  	     = case prov of		-- Omit home module qualifier
			LocalDef _ _     -> empty
			Imported _ _ hif -> pp_mod <> pp_dot hif
			Implicit hif     -> pp_mod <> pp_dot hif
			other		 -> pp_mod <> text "."

	  pp_dot HiFile     = text "."		-- Vanilla case
	  pp_dot HiBootFile = text "!"		-- M!t indicates a name imported from 
						-- a .hi-boot interface


pp_debug PprDebug (Global uniq m n prov) = hcat [text "{-", pprUnique uniq, char ',', 
						        pp_prov prov, text "-}"]
					where
						pp_prov (LocalDef Exported _)    = char 'x'
						pp_prov (LocalDef NotExported _) = char 'l'
						pp_prov (Imported _ _ _) = char 'i'
						pp_prov (Implicit _)     = char 'p'
						pp_prov (WiredInTyCon _) = char 'W'
						pp_prov (WiredInId _)    = char 'w'
pp_debug other    name 		        = empty

-- pprNameProvenance is used in error messages to say where a name came from
pprNameProvenance :: PprStyle -> Name -> Doc
pprNameProvenance sty (Local _ _ loc)     = pprProvenance sty (LocalDef NotExported loc)
pprNameProvenance sty (Global _ _ _ prov) = pprProvenance sty prov

pprProvenance :: PprStyle -> Provenance -> Doc
pprProvenance sty (Imported mod loc _)
  = sep [ptext SLIT("Imported from"), pprModule sty mod, ptext SLIT("at"), ppr sty loc]
pprProvenance sty (LocalDef _ loc)  = sep [ptext SLIT("Defined at"), ppr sty loc]
pprProvenance sty (Implicit _)      = panic "pprNameProvenance: Implicit"
pprProvenance sty (WiredInTyCon tc) = ptext SLIT("Wired-in tycon")
pprProvenance sty (WiredInId id)    = ptext SLIT("Wired-in id")
\end{code}


%************************************************************************
%*									*
\subsection[Sets of names}
%*									*
%************************************************************************

\begin{code}
type NameSet = UniqSet Name
emptyNameSet	  :: NameSet
unitNameSet	  :: Name -> NameSet
addListToNameSet  :: NameSet -> [Name] -> NameSet
addOneToNameSet   :: NameSet -> Name -> NameSet
mkNameSet         :: [Name] -> NameSet
unionNameSets	  :: NameSet -> NameSet -> NameSet
unionManyNameSets :: [NameSet] -> NameSet
minusNameSet 	  :: NameSet -> NameSet -> NameSet
elemNameSet	  :: Name -> NameSet -> Bool
nameSetToList	  :: NameSet -> [Name]
isEmptyNameSet	  :: NameSet -> Bool

isEmptyNameSet    = isEmptyUniqSet
emptyNameSet	  = emptyUniqSet
unitNameSet	  = unitUniqSet
mkNameSet         = mkUniqSet
addListToNameSet  = addListToUniqSet
addOneToNameSet	  = addOneToUniqSet
unionNameSets     = unionUniqSets
unionManyNameSets = unionManyUniqSets
minusNameSet	  = minusUniqSet
elemNameSet       = elementOfUniqSet
nameSetToList     = uniqSetToList
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
getModule	    :: NamedThing a => a -> Module
getSrcLoc	    :: NamedThing a => a -> SrcLoc
isLocallyDefined    :: NamedThing a => a -> Bool
isExported	    :: NamedThing a => a -> Bool
getOccString	    :: NamedThing a => a -> String

modAndOcc	    = nameModAndOcc	   . getName
getModule	    = nameModule	   . getName
isExported	    = isExportedName 	   . getName
getSrcLoc	    = nameSrcLoc	   . getName
isLocallyDefined    = isLocallyDefinedName . getName
getOccString x	    = _UNPK_ (occNameString (getOccName x))
\end{code}

\begin{code}
{-# SPECIALIZE isLocallyDefined
	:: Name	    -> Bool
  #-}
\end{code}
