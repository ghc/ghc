%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
module Name (
	-- Re-export the Module type
	Module,
	pprModule, moduleString,

	-- The basic form of names
	isLexCon, isLexVar, isLexId, isLexSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym,
	mkTupNameStr, mkUbxTupNameStr, isLowerISO, isUpperISO,

	-- The OccName type
	OccName(..), varOcc, 
	pprOccName, occNameString, occNameFlavour, 
	isTvOcc, isTCOcc, isVarOcc, prefixOccName,

	-- The Name type
	Name,					-- Abstract
	mkLocalName, mkSysLocalName, 

	mkCompoundName, mkGlobalName,

	mkWiredInIdName,   mkWiredInTyConName,
	maybeWiredInIdName, maybeWiredInTyConName,
	isWiredInName,

	nameUnique, changeUnique, setNameProvenance, getNameProvenance,
	setNameVisibility, mkNameVisible,
	nameOccName, nameModule,

	isExportedName,	nameSrcLoc,
	isLocallyDefinedName,

	isSysLocalName, isLocalName, isGlobalName, isExternallyVisibleName,

        pprNameProvenance,

	-- Special Names
	dictNamePrefix, mkSuperDictSelName, mkWorkerName,
	mkDefaultMethodName, mkClassTyConStr, mkClassDataConStr,

	-- Misc
	Provenance(..), pprProvenance,
	ExportFlag(..), 
	PrintUnqualified,

	-- Class NamedThing and overloaded friends
	NamedThing(..),
	modAndOcc, isExported, 
	getSrcLoc, isLocallyDefined, getOccString
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Var   ( Id )
import {-# SOURCE #-} TyCon ( TyCon )

import CStrings		( identToC )
import PrelMods		( pREL_BASE, pREL_TUP, pREL_GHC )
import CmdLineOpts	( opt_PprStyle_NoPrags, opt_OmitInterfacePragmas, opt_EnsureSplittableC )
import BasicTypes	( Module, IfaceFlavour(..), moduleString, pprModule )

import SrcLoc		( noSrcLoc, mkBuiltinSrcLoc, SrcLoc )
import Unique		( pprUnique, Unique, Uniquable(..) )
import Outputable
import Char		( isUpper, isLower, ord )
import Util		( nOfThem )
import GlaExts
\end{code}


%************************************************************************
%*									*
\subsection{Lexical categories}
%*									*
%************************************************************************

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.

\begin{code}
isLexCon, isLexVar, isLexId, isLexSym, isLexConId, isLexConSym,
 isLexVarId, isLexVarSym  :: FAST_STRING -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------

isLexConId cs
  | _NULL_ cs	     = False
  | cs == SLIT("[]") = True
  | c  == '('	     = True	-- (), (,), (,,), ...
  | otherwise	     = isUpper c || isUpperISO c
  where					
    c = _HEAD_ cs

isLexVarId cs
  | _NULL_ cs	 = False
  | otherwise    = isLower c || isLowerISO c
  where
    c = _HEAD_ cs

isLexConSym cs
  | _NULL_ cs	= False
  | otherwise	= c  == ':'
	       || cs == SLIT("->")
  where
    c = _HEAD_ cs

isLexVarSym cs
  | _NULL_ cs = False
  | otherwise = isSymbolASCII c
	     || isSymbolISO c
  where
    c = _HEAD_ cs

-------------
isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
isSymbolISO   c = ord c `elem` (0xd7 : 0xf7 : [0xa1 .. 0xbf])
isUpperISO    (C# c#) = c# `geChar#` '\xc0'# && c# `leChar#` '\xde'# && c# `neChar#` '\xd7'#
--0xc0 <= oc && oc <= 0xde && oc /= 0xd7 where oc = ord c
isLowerISO    (C# c#) = c# `geChar#` '\xdf'# && c# `leChar#` '\xff'# && c# `neChar#` '\xf7'#
--0xdf <= oc && oc <= 0xff && oc /= 0xf7 where oc = ord c
\end{code}

\begin{code}
mkTupNameStr 0 = (pREL_BASE, SLIT("()"))
mkTupNameStr 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr 2 = (pREL_TUP, _PK_ "(,)")   -- not strictly necessary
mkTupNameStr 3 = (pREL_TUP, _PK_ "(,,)")  -- ditto
mkTupNameStr 4 = (pREL_TUP, _PK_ "(,,,)") -- ditto
mkTupNameStr n = (pREL_TUP, _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")"))

mkUbxTupNameStr 0 = panic "Name.mkUbxTupNameStr: 0 ???"
mkUbxTupNameStr 1 = (pREL_GHC, _PK_ "(# #)") -- 1 and 0 both make sense!!!
mkUbxTupNameStr 2 = (pREL_GHC, _PK_ "(#,#)")
mkUbxTupNameStr 3 = (pREL_GHC, _PK_ "(#,,#)")
mkUbxTupNameStr 4 = (pREL_GHC, _PK_ "(#,,,#)")
mkUbxTupNameStr n = (pREL_GHC, _PK_ ("(#" ++ nOfThem (n-1) ',' ++ "#)"))
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

pprOccName :: OccName -> SDoc
pprOccName n = getPprStyle $ \ sty ->
	       if codeStyle sty 
	       then identToC (occNameString n)
	       else ptext (occNameString n)

varOcc :: FAST_STRING -> OccName
varOcc = VarOcc

occNameString :: OccName -> FAST_STRING
occNameString (VarOcc s)  = s
occNameString (TvOcc s)   = s
occNameString (TCOcc s)   = s

mapOccName :: (FAST_STRING -> FAST_STRING) -> OccName -> OccName
mapOccName f (VarOcc s) = VarOcc (f s)
mapOccName f (TvOcc s)  = TvOcc  (f s)
mapOccName f (TCOcc s)  = TCOcc  (f s)

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
    a == b = case (a `compare` b) of { EQ -> True;  _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False; _ -> True }

instance Ord OccName where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = cmpOcc a b

(VarOcc s1) `cmpOcc` (VarOcc s2) = s1 `compare` s2
(VarOcc s1) `cmpOcc` other2      = LT

(TvOcc s1)  `cmpOcc` (VarOcc s2) = GT
(TvOcc s1)  `cmpOcc` (TvOcc s2)  = s1 `compare` s2
(TvOcc s1)  `cmpOcc` other	 = LT

(TCOcc s1) `cmpOcc` (TCOcc s2) = s1 `compare` s2
(TCOcc s1) `cmpOcc` other      = GT

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
	     (Maybe OccName)	-- For ones that started life with a user name

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
  = NoProvenance

  | LocalDef			-- Defined locally
	SrcLoc 			-- Defn site
	ExportFlag		-- Whether it's exported

  | NonLocalDef  		-- Defined non-locally
	SrcLoc			-- Defined non-locally; src-loc gives defn site
	IfaceFlavour		-- Whether the defn site is an .hi-boot file
	PrintUnqualified

  | WiredInTyCon TyCon			-- There's a wired-in version
  | WiredInId    Id			-- ...ditto...

type PrintUnqualified = Bool	-- True <=> the unqualified name of this thing is
				-- in scope in this module, so print it 
				-- unqualified in error messages
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
data ExportFlag = Exported  | NotExported
\end{code}

\begin{code}
mkLocalName    :: Unique -> OccName -> Name
mkLocalName uniq occ = Local uniq (Just occ)

mkGlobalName :: Unique -> Module -> OccName -> Provenance -> Name
mkGlobalName = Global

mkSysLocalName :: Unique -> Name
mkSysLocalName uniq = Local uniq Nothing

mkWiredInIdName :: Unique -> Module -> FAST_STRING -> Id -> Name
mkWiredInIdName uniq mod occ id 
  = Global uniq mod (VarOcc occ) (WiredInId id)

mkWiredInTyConName :: Unique -> Module -> FAST_STRING -> TyCon -> Name
mkWiredInTyConName uniq mod occ tycon
  = Global uniq mod (TCOcc occ) (WiredInTyCon tycon)


mkCompoundName :: (OccName -> OccName)
	       -> Unique		-- New unique
	       -> Name			-- Base name
	       -> Name		-- Result is always a value name

mkCompoundName f uniq (Global _ mod occ prov)
  = Global uniq mod (f occ) prov

mkCompoundName f uniq (Local _ (Just occ))
  = Local uniq (Just (f occ))

mkCompoundName f uniq (Local _ Nothing)
  = Local uniq Nothing

setNameProvenance :: Name -> Provenance -> Name	
	-- setNameProvenance used to only change the provenance of 
	-- Implicit-provenance things, but that gives bad error messages 
	-- for names defined twice in the same module, so I changed it to 
	-- set the provenance of *any* global (SLPJ Jun 97)
setNameProvenance (Global uniq mod occ _) prov = Global uniq mod occ prov
setNameProvenance other_name 		  prov = other_name

getNameProvenance :: Name -> Provenance
getNameProvenance (Global uniq mod occ prov) = prov
getNameProvenance (Local uniq occ)           = LocalDef noSrcLoc NotExported

-- When we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
changeUnique (Local      _ n )          u = Local u n
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

setNameVisibility maybe_mod uniq name@(Global _ mod occ (LocalDef loc NotExported))
  | not all_toplev_ids_visible || not_top_level maybe_mod
  = Local uniq Nothing				-- Localise Global name

setNameVisibility maybe_mod uniq name@(Global _ _ _ _)
  = name					-- Otherwise don't fiddle with Global

setNameVisibility (Just mod) uniq (Local _ _)
  | all_toplev_ids_visible
  = Global uniq mod	 			-- Globalise Local name
	   (uniqToOccName uniq)
	   (LocalDef noSrcLoc NotExported)

setNameVisibility maybe_mod uniq (Local _ _)
  = Local uniq Nothing 			-- New unique for Local; zap its occ

-- make the Name globally visible regardless.
mkNameVisible :: Module -> Unique -> Name -> Name
mkNameVisible mod occ_uniq nm@(Global _ _ _ _) = nm
mkNameVisible mod occ_uniq nm@(Local uniq occ)
 = Global uniq mod (uniqToOccName occ_uniq) (LocalDef noSrcLoc Exported)

uniqToOccName uniq = VarOcc (_PK_ ('_':show uniq))
	-- The "_" is to make sure that this OccName is distinct from all user-defined ones

not_top_level (Just m) = False
not_top_level Nothing  = True

all_toplev_ids_visible = 
	not opt_OmitInterfacePragmas ||  -- Pragmas can make them visible
	opt_EnsureSplittableC            -- Splitting requires visiblilty
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



nameUnique (Local  u _)     = u
nameUnique (Global u _ _ _) = u

nameOccName (Local _ (Just occ)) = occ
nameOccName (Local uniq Nothing) = pprPanic "nameOccName" (ppr uniq)
nameOccName (Global _ _ occ _)   = occ

nameModule (Global _ mod occ _) = mod

nameModAndOcc (Global _ mod occ _) = (mod,occ)

isExportedName (Global _ _ _ (LocalDef _ Exported)) = True
isExportedName other				    = False

nameSrcLoc (Local _ _)   			= noSrcLoc
nameSrcLoc (Global _ _ _ (LocalDef loc _))      = loc
nameSrcLoc (Global _ _ _ (NonLocalDef loc _ _)) = loc
nameSrcLoc (Global _ _ _ (WiredInTyCon _))      = mkBuiltinSrcLoc
nameSrcLoc (Global _ _ _ (WiredInId _))         = mkBuiltinSrcLoc
nameSrcLoc other			        = noSrcLoc
  
isLocallyDefinedName (Local  _ _)	     	   = True
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


isLocalName (Local _ _) = True
isLocalName _ 		= False

isSysLocalName (Local _ Nothing) = True
isSysLocalName other		 = False

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
    c (Local  u1 _)   (Local  u2 _)       = compare u1 u2
    c (Local   _ _)	  _		  = LT
    c (Global u1 _ _ _) (Global u2 _ _ _) = compare u1 u2
    c (Global  _ _ _ _)   _		  = GT
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
\subsection[Special-Names]{Special Kinds of names}
%*									*
%************************************************************************

Here's our convention for splitting up the object file name space:

	_d...		dictionary identifiers
	_g...		externally visible (non-user visible) names

	_m...		default methods
	_n...		default methods (encoded symbols, eg. <= becomes _nle)

	_p...		superclass selectors

	_w...		workers
	_v...		workers (encoded symbols)

	_x...		local variables

	_u...		user-defined names that previously began with '_'

	_[A-Z]...	compiler-generated tycons/datacons (namely dictionary
			constructors)

	__....		keywords (__export, __letrec etc.)

This knowledge is encoded in the following functions.

\begin{code}
dictNamePrefix :: FAST_STRING
dictNamePrefix	= SLIT("_d")

mkSuperDictSelName :: Int -> OccName -> OccName
mkSuperDictSelName index = prefixOccName (_PK_ ("_p" ++ show index ++ "_"))

mkWorkerName :: OccName -> OccName
mkWorkerName nm
  | isLexSym nm_str = 
	prefixOccName SLIT("_v") (mapOccName trName nm)
  | otherwise 		     = 
	prefixOccName SLIT("_w") nm
  where nm_str = occNameString nm

mkDefaultMethodName :: OccName -> OccName
mkDefaultMethodName nm
  | isLexSym nm_str = 
	prefixOccName SLIT("_n") (mapOccName trName nm)
  | otherwise 		     = 
	prefixOccName SLIT("_m") nm
  where nm_str = occNameString nm

-- not used yet:
--mkRecordSelectorName     :: Name -> Name
--mkMethodSelectorName     :: Name -> Name

mkClassTyConStr, mkClassDataConStr :: FAST_STRING -> FAST_STRING

mkClassTyConStr   s = SLIT("_") _APPEND_ s
mkClassDataConStr s = SLIT("_") _APPEND_ s

-- translate a string such that it can occur as *part* of an identifer.  This
-- is used when we prefix identifiers to create new names, for example the
-- name of a default method.

trName :: FAST_STRING -> FAST_STRING
trName nm = _PK_ (foldr tran "" (_UNPK_ nm))
 where 
    tran c cs = case trChar c of
		   '\0' -> '_' : show (ord c) ++ cs
		   c'   -> c' : cs
    trChar '&'  = 'a'
    trChar '|'  = 'b'
    trChar ':'  = 'c'
    trChar '/'  = 'd'
    trChar '='  = 'e'
    trChar '>'  = 'g'
    trChar '#'  = 'h'
    trChar '@'  = 'i'
    trChar '<'  = 'l'
    trChar '-'  = 'm'
    trChar '!'  = 'n'
    trChar '+'  = 'p'
    trChar '\'' = 'q'
    trChar '$'  = 'r'
    trChar '?'  = 's'
    trChar '*'  = 't'
    trChar '_'  = 'u'
    trChar '.'  = 'v'
    trChar '\\' = 'w'
    trChar '%'  = 'x'
    trChar '~'  = 'y'
    trChar '^'  = 'z'
    trChar _    = '\0'
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

pprName name
  = getPprStyle $ \ sty ->
    let
       -- when printing local names for interface files, prepend the '_'
       -- to avoid clashes with user-defined names.  In fact, these names
       -- will always begin with 'g' for top-level ids and 'x' otherwise,
       -- because these are the unique supplies going into the tidy phase.
       ppr (Local u n) | codeStyle sty   = pprUnique u
		       | ifaceStyle sty  = char '_' <> pprUnique u

       ppr (Local u Nothing)	= pprUnique u
       ppr (Local u (Just occ))	| userStyle sty = ptext (occNameString occ)
				| otherwise	= ptext (occNameString occ) <> char '_' <> pprUnique u
   
       ppr name@(Global u m n prov)
	 | codeStyle sty
	 = identToC (m _APPEND_ SLIT(".") _APPEND_ occNameString n)
   
	 | otherwise  
	 = hcat [pp_mod_dot, ptext (occNameString n), pp_debug sty name]
	 where
	   pp_mod_dot 
	     = case prov of   -- Omit home module qualifier if in scope 
		   LocalDef _ _          -> pp_qual dot (user_sty || iface_sty)
		   NonLocalDef _ hif omit -> pp_qual (pp_hif hif) (omit && user_sty)
				 -- Hack: omit qualifers on wired in things
				 -- in user style only
		   WiredInTyCon _ 	-> pp_qual dot user_sty
		   WiredInId _		-> pp_qual dot user_sty
		   NoProvenance	    	-> pp_qual dot False
   
	   pp_qual sep omit_qual
	    | omit_qual  = empty
	    | otherwise	 = pprModule m <> sep

	   dot = text "."
	   pp_hif HiFile     = dot	 -- Vanilla case
	   pp_hif HiBootFile = text "!"  -- M!t indicates a name imported from a .hi-boot interface

	   user_sty  = userStyle sty
	   iface_sty = ifaceStyle sty
    in
    ppr name
   
   
pp_debug sty (Global uniq m n prov) 
  | debugStyle sty = hcat [text "{-", pprUnique uniq, prov_p, text "-}"]
  | otherwise	   = empty
	           where
		     prov_p | opt_PprStyle_NoPrags = empty
			    | otherwise		   = comma <> pp_prov prov

pp_prov (LocalDef _ Exported)    = char 'x'
pp_prov (LocalDef _ NotExported) = char 'l'
pp_prov (NonLocalDef _ _ _)    	 = char 'n'
pp_prov (WiredInTyCon _)   	 = char 'W'
pp_prov (WiredInId _)      	 = char 'w'
pp_prov NoProvenance     	 = char '?'

-- pprNameProvenance is used in error messages to say where a name came from
pprNameProvenance :: Name -> SDoc
pprNameProvenance (Local _ _)         = pprProvenance (LocalDef noSrcLoc NotExported)
pprNameProvenance (Global _ _ _ prov) = pprProvenance prov

pprProvenance :: Provenance -> SDoc
pprProvenance (LocalDef loc _)      = ptext SLIT("Locally defined at")     <+> ppr loc
pprProvenance (NonLocalDef loc _ _) = ptext SLIT("Non-locally defined at") <+> ppr loc
pprProvenance (WiredInTyCon tc)     = ptext SLIT("Wired-in tycon")
pprProvenance (WiredInId id)        = ptext SLIT("Wired-in id")
pprProvenance NoProvenance	    = ptext SLIT("No provenance")
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
getOccString x	    = _UNPK_ (occNameString (getOccName x))
\end{code}

\begin{code}
{-# SPECIALIZE isLocallyDefined
	:: Name	    -> Bool
  #-}
\end{code}
