%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
#include "HsVersions.h"

module Name (
	SYN_IE(Module),

	OrigName(..), -- glorified pair
	qualToOrigName, -- a Qual to an OrigName

	RdrName(..),
	preludeQual,
	moduleNamePair,
	isUnqual,
	isQual,
	isRdrLexCon, isRdrLexConOrSpecial,
	appendRdr,
	showRdr,
	cmpRdr,

	Name,
	Provenance,
	mkLocalName, isLocalName, 
	mkTopLevName, mkImportedName, oddlyImportedName,
	mkImplicitName,	isImplicitName,
	mkPrimitiveName, mkWiredInName,
	mkCompoundName, mkCompoundName2,

	mkFunTyConName, mkTupleDataConName, mkTupleTyConName,
	mkTupNameStr,

	NamedThing(..), -- class
	ExportFlag(..),
	isExported{-overloaded-}, exportFlagOn{-not-},

	nameUnique, changeUnique,
	nameOccName,
--	nameOrigName, : not exported
	nameExportFlag,
	nameSrcLoc,
	nameImpLocs,
	nameImportFlag,
	isLocallyDefinedName, isWiredInName,

	origName, moduleOf, nameOf,
	getOccName, getExportFlag,
	getSrcLoc, getImpLocs,
	isLocallyDefined,
	getLocalName,

	isSymLexeme, pprSym, pprNonSym,
	isLexCon, isLexVar, isLexId, isLexSym, isLexSpecialSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym
    ) where

IMP_Ubiq()
IMPORT_1_3(Char(isUpper,isLower))

import CmdLineOpts	( maybe_CompilingGhcInternals )
import CStrings		( identToC, modnameToC, cSEP )
import Outputable	( Outputable(..) )
import PprStyle		( PprStyle(..), codeStyle )
import PrelMods		( pRELUDE )
import Pretty
import SrcLoc		( mkBuiltinSrcLoc, mkUnknownSrcLoc, SrcLoc )
import Unique		( funTyConKey, mkTupleDataConUnique, mkTupleTyConUnique,
			  pprUnique, Unique
			)
import Util		( thenCmp, _CMP_STRING_, nOfThem, panic, assertPanic{-, pprTrace ToDo:rm-} )

#ifdef REALLY_HASKELL_1_3
ord = fromEnum :: Char -> Int
#endif
\end{code}

%************************************************************************
%*									*
\subsection[RdrName]{The @RdrName@ datatype; names read from files}
%*									*
%************************************************************************

\begin{code}
type Module = FAST_STRING

data OrigName = OrigName Module FAST_STRING

qualToOrigName (Qual m n) = OrigName m n

data RdrName
  = Unqual FAST_STRING
  | Qual   Module FAST_STRING

preludeQual n = Qual pRELUDE n

moduleNamePair (Qual m n) = (m, n)  -- we make *no* claim whether this
				    -- constitutes an original name or
				    -- an occurrence name, or anything else

isUnqual (Unqual _) = True
isUnqual (Qual _ _) = False

isQual (Unqual _) = False
isQual (Qual _ _) = True

isRdrLexCon (Unqual n) = isLexCon n
isRdrLexCon (Qual m n) = isLexCon n

isRdrLexConOrSpecial (Unqual n) = isLexCon n || isLexSpecialSym n
isRdrLexConOrSpecial (Qual m n) = isLexCon n || isLexSpecialSym n

appendRdr (Unqual n) str = Unqual (n _APPEND_ str)
appendRdr (Qual m n) str = Qual m (n _APPEND_ str)

cmpRdr (Unqual  n1) (Unqual  n2) = _CMP_STRING_ n1 n2
cmpRdr (Unqual  n1) (Qual m2 n2) = LT_
cmpRdr (Qual m1 n1) (Unqual  n2) = GT_
cmpRdr (Qual m1 n1) (Qual m2 n2) = _CMP_STRING_ n1 n2 `thenCmp` _CMP_STRING_ m1 m2
				   -- always compare module-names *second*

cmpOrig (OrigName m1 n1) (OrigName m2 n2)
  = _CMP_STRING_ n1 n2 `thenCmp` _CMP_STRING_ m1 m2 -- again; module-names *second*

instance Eq RdrName where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True }

instance Ord RdrName where
    a <= b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }

instance Ord3 RdrName where
    cmp = cmpRdr

instance NamedThing RdrName where
    -- We're sorta faking it here
    getName (Unqual n)
      = Local u n True locn
      where
	u    = panic "NamedThing.RdrName:Unique1"
	locn = panic "NamedThing.RdrName:locn"

    getName rdr_name@(Qual m n)
      = Global u m (Left n) prov ex [rdr_name]
      where
	u    = panic "NamedThing.RdrName:Unique"
	prov = panic "NamedThing.RdrName:Provenance"
	ex   = panic "NamedThing.RdrName:ExportFlag"

instance Outputable RdrName where
    ppr sty (Unqual n) = pp_name sty n
    ppr sty (Qual m n) = ppBeside (pp_mod sty m) (pp_name sty n)

pp_mod sty m
  = case sty of
      PprForC		-> pp_code
      PprForAsm False _ -> pp_code
      PprForAsm True  _ -> ppBeside (ppPStr cSEP) pp_code
      _			-> ppBeside (ppPStr m)    (ppChar '.')
  where
    pp_code = ppBeside (ppPStr (modnameToC m)) (ppPStr cSEP)

pp_name sty n = (if codeStyle sty then identToC else ppPStr) n

pp_name2 sty pieces
  = ppIntersperse sep (map pp_piece pieces)
  where
    sep = if codeStyle sty then ppPStr cSEP else ppChar '.'

    pp_piece (Left (OrigName m n)) = ppBeside (pp_mod sty m) (pp_name sty n)
    pp_piece (Right n)		   = pp_name sty n

showRdr sty rdr = ppShow 100 (ppr sty rdr)

-------------------------
instance Eq OrigName where
    a == b = case (a `cmp` b) of { EQ_ -> True;  _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False; _ -> True }

instance Ord OrigName where
    a <= b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;	 EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }

instance Ord3 OrigName where
    cmp = cmpOrig

instance NamedThing OrigName where -- faking it
    getName (OrigName m n) = getName (Qual m n)

instance Outputable OrigName where -- ditto
    ppr sty (OrigName m n) = ppr sty (Qual m n)
\end{code}

%************************************************************************
%*									*
\subsection[Name-datatype]{The @Name@ datatype}
%*									*
%************************************************************************

\begin{code}
data Name
  = Local    Unique
             FAST_STRING
	     Bool	-- True <=> emphasize Unique when
			-- printing; this is just an esthetic thing...
             SrcLoc

  | Global   Unique
             Module	-- original name
	     (Either
		FAST_STRING -- just an ordinary M.n name... or...
		([Either OrigName FAST_STRING]))
			    -- "dot" these bits of name together...
             Provenance -- where it came from
             ExportFlag -- is it exported?
             [RdrName]  -- ordered occurrence names (usually just one);
			-- first may be *un*qual.

data Provenance
  = LocalDef SrcLoc     -- locally defined; give its source location
			
  | Imported ExportFlag	-- how it was imported
	     SrcLoc     -- *original* source location
             [SrcLoc]   -- any import source location(s)

  | Implicit
  | Primitive		-- really and truly primitive thing (not
			-- definable in Haskell)
  | WiredIn  Bool	-- something defined in Haskell; True <=>
			-- definition is in the module in question;
			-- this probably comes from the -fcompiling-prelude=...
			-- flag.
\end{code}

\begin{code}
mkLocalName = Local

mkTopLevName   u (OrigName m n) locn exp occs = Global u m (Left n) (LocalDef locn) exp occs
mkImportedName u (OrigName m n) imp locn imp_locs exp occs = Global u m (Left n) (Imported imp locn imp_locs) exp occs

mkImplicitName :: Unique -> OrigName -> Name
mkImplicitName u (OrigName m n) = Global u m (Left n) Implicit NotExported []

mkPrimitiveName :: Unique -> OrigName -> Name
mkPrimitiveName u (OrigName m n)  = Global u m (Left n) Primitive NotExported []

mkWiredInName :: Unique -> OrigName -> ExportFlag -> Name
mkWiredInName u (OrigName m n) exp
  = Global u m (Left n) (WiredIn from_here) exp []
  where
    from_here
      = case maybe_CompilingGhcInternals of
          Nothing  -> False
	  Just mod -> mod == _UNPK_ m

mkCompoundName :: Unique
	       -> Module
	       -> FAST_STRING	-- indicates what kind of compound thing it is (e.g., "sdsel")
	       -> [Either OrigName FAST_STRING]	-- "dot" these names together
	       -> Name		-- from which we get provenance, etc....
	       -> Name		-- result!

mkCompoundName u m str ns (Local _ _ _ locn) -- these arise for workers...
  = Local u str True{-emph uniq-} locn

mkCompoundName u m str ns (Global _ _ _ prov exp _)
  = Global u m (Right (Right str : ns)) prov exp []

glue = glue1
glue1 (Left (OrigName m n):ns) = m : _CONS_ '.' n : glue2 ns
glue1 (Right n            :ns) = n 		  : glue2 ns
glue2 []		       = []
glue2 (Left (OrigName m n):ns) = _CONS_ '.' m : _CONS_ '.' n : glue2 ns
glue2 (Right n            :ns) = _CONS_ '.' n		     : glue2 ns

-- this ugly one is used for instance-y things
mkCompoundName2 :: Unique
		-> Module
		-> FAST_STRING	-- indicates what kind of compound thing it is
		-> [Either OrigName FAST_STRING] -- "dot" these names together
		-> Bool		-- True <=> defined in this module
		-> SrcLoc	
		-> Name		-- result!

mkCompoundName2 u m str ns from_here locn
  = Global u m (Right (Right str : ns))
	     (if from_here then LocalDef locn else Imported ExportAll locn [])
	     ExportAll{-instances-}
	     []

mkFunTyConName
  = mkPrimitiveName funTyConKey		       (OrigName pRELUDE SLIT("->"))
mkTupleDataConName arity
  = mkWiredInName (mkTupleDataConUnique arity) (OrigName pRELUDE (mkTupNameStr arity)) ExportAll
mkTupleTyConName   arity
  = mkWiredInName (mkTupleTyConUnique   arity) (OrigName pRELUDE (mkTupNameStr arity)) ExportAll

mkTupNameStr 0 = SLIT("()")
mkTupNameStr 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr 2 = _PK_ "(,)"   -- not strictly necessary
mkTupNameStr 3 = _PK_ "(,,)"  -- ditto
mkTupNameStr 4 = _PK_ "(,,,)" -- ditto
mkTupNameStr n
  = _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")")

	-- ToDo: what about module ???
	-- ToDo: exported when compiling builtin ???

isLocalName (Local _ _ _ _) = True
isLocalName _ 		    = False

-- things the compiler "knows about" are in some sense
-- "imported".  When we are compiling the module where
-- the entities are defined, we need to be able to pick
-- them out, often in combination with isLocallyDefined.
oddlyImportedName (Global _ _ _ Primitive   _ _) = True
oddlyImportedName (Global _ _ _ (WiredIn _) _ _) = True
oddlyImportedName _				 = False

isImplicitName (Global _ _ _ Implicit _ _) = True
isImplicitName _ 		           = False
\end{code}

%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = c n1 n2
  where
    c (Local  u1 _ _ _)     (Local  u2 _ _ _)     = cmp u1 u2
    c (Local   _ _ _ _)	    _			  = LT_
    c (Global u1 _ _ _ _ _) (Global u2 _ _ _ _ _) = cmp u1 u2
    c (Global  _ _ _ _ _ _) _			  = GT_
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

\begin{code}
nameUnique (Local  u _ _ _)     = u
nameUnique (Global u _ _ _ _ _) = u

-- when we renumber/rename things, we need to be
-- able to change a Name's Unique to match the cached
-- one in the thing it's the name of.  If you know what I mean.
changeUnique (Local      _ n b l)    u = Local u n b l
changeUnique (Global   _ m n p e os) u = Global u m n p e os

nameOrigName msg (Global _ m (Left  n) _ _ _) = OrigName m n
nameOrigName msg (Global _ m (Right n) _ _ _) = let str = _CONCAT_ (glue n) in
						--pprTrace ("nameOrigName:"++msg) (ppPStr str) $
						OrigName m str
#ifdef DEBUG
nameOrigName msg (Local  _ n _ _)     = panic ("nameOrigName:Local:"++msg++":"++ _UNPK_ n)
#endif

nameOccName (Local  _ n _ _)	     = Unqual n
nameOccName (Global _ m (Left  n) _ _ []  )  = Qual m n
nameOccName (Global _ m (Right n) _ _ []  )  =  let str = _CONCAT_ (glue n) in
						--pprTrace "nameOccName:" (ppPStr str) $
						Qual m str
nameOccName (Global _ m (Left  _) _ _ (o:_)) = o
nameOccName (Global _ m (Right _) _ _ (o:_)) = panic "nameOccName:compound name"

nameExportFlag (Local  _ _ _ _)       = NotExported
nameExportFlag (Global _ _ _ _ exp _) = exp

nameSrcLoc (Local  _ _ _ loc)	                 = loc
nameSrcLoc (Global _ _ _ (LocalDef loc)     _ _) = loc
nameSrcLoc (Global _ _ _ (Imported _ loc _) _ _) = loc
nameSrcLoc (Global _ _ _ Implicit           _ _) = mkUnknownSrcLoc
nameSrcLoc (Global _ _ _ Primitive          _ _) = mkBuiltinSrcLoc
nameSrcLoc (Global _ _ _ (WiredIn _)        _ _) = mkBuiltinSrcLoc
  
nameImpLocs (Global _ _ _ (Imported _ _ locs) _ _) = locs
nameImpLocs _ 					   = []

nameImportFlag (Local  _ _ _ _)                      = NotExported
nameImportFlag (Global _ _ _ (LocalDef _)       _ _) = ExportAll
nameImportFlag (Global _ _ _ (Imported exp _ _) _ _) = exp
nameImportFlag (Global _ _ _ Implicit           _ _) = ExportAll
nameImportFlag (Global _ _ _ Primitive          _ _) = ExportAll
nameImportFlag (Global _ _ _ (WiredIn _)        _ _) = ExportAll

isLocallyDefinedName (Local  _ _ _ _)	            	    = True
isLocallyDefinedName (Global _ _ _ (LocalDef _)        _ _) = True
isLocallyDefinedName (Global _ _ _ (Imported _ _ _)    _ _) = False
isLocallyDefinedName (Global _ _ _ Implicit            _ _) = False
isLocallyDefinedName (Global _ _ _ Primitive           _ _) = False
isLocallyDefinedName (Global _ _ _ (WiredIn from_here) _ _) = from_here

isWiredInName (Global _ _ _ (WiredIn _) _ _) = True
isWiredInName _				     = False
\end{code}

\begin{code}
instance Outputable Name where
    ppr sty (Local u n emph_uniq _)
      | codeStyle sty = pprUnique u
      | emph_uniq     = ppBesides [pprUnique u, ppStr "{-", ppPStr n, ppStr "-}"]
      | otherwise     = ppBesides [ppPStr n, ppStr "{-", pprUnique u, ppStr "-}"]

    ppr PprDebug   (Global   u m (Left  n) _ _ _) = ppBesides [pp_mod PprDebug m, pp_name  PprDebug n, ppStr "{-", pprUnique u, ppStr "-}"]
    ppr PprDebug   (Global   u m (Right n) _ _ _) = ppBesides [pp_mod PprDebug m, pp_name2 PprDebug n, ppStr "{-", pprUnique u, ppStr "-}"]

    ppr PprForUser (Global   u m (Left  n) _ _ []  ) = ppBeside (pp_mod PprForUser m) (pp_name  PprForUser n)
    ppr PprForUser (Global   u m (Right n) _ _ []  ) = ppBeside (pp_mod PprForUser m) (pp_name2 PprForUser n)
    ppr PprForUser (Global   u m (Left  _) _ _ occs) = ppr PprForUser (head occs)

-- LATER:?
--  ppr PprShowAll (Global   u m n prov exp occs) = pp_all (Qual m n) prov exp occs

    ppr sty (Global u m (Left  n) _ _ _) = ppBeside (pp_mod sty m) (pp_name  sty n)
    ppr sty (Global u m (Right n) _ _ _) = ppBeside (pp_mod sty m) (pp_name2 sty n)

pp_all orig prov exp occs
  = ppBesides [ppr PprShowAll orig, ppr PprShowAll occs, pp_prov prov, pp_exp exp]

pp_exp NotExported = ppNil
pp_exp ExportAll   = ppPStr SLIT("/EXP(..)")
pp_exp ExportAbs   = ppPStr SLIT("/EXP")

pp_prov Implicit    = ppPStr SLIT("/IMPLICIT")
pp_prov Primitive   = ppPStr SLIT("/PRIMITIVE")
pp_prov (WiredIn _) = ppPStr SLIT("/WIREDIN")
pp_prov _           = ppNil
\end{code}

%************************************************************************
%*									*
\subsection[ExportFlag-datatype]{The @ExportFlag@ datatype}
%*									*
%************************************************************************

The export flag @ExportAll@ means `export all there is', so there are
times when it is attached to a class or data type which has no
ops/constructors (if the class/type was imported abstractly).  In
fact, @ExportAll@ is attached to everything except to classes/types
which are being {\em exported} abstractly, regardless of how they were
imported.

\begin{code}
data ExportFlag
  = ExportAll		-- export with all constructors/methods
  | ExportAbs		-- export abstractly (tycons/classes only)
  | NotExported

exportFlagOn NotExported = False
exportFlagOn _		 = True

-- Be very wary about using "isExported"; perhaps you
-- really mean "externallyVisibleId"?

isExported a = exportFlagOn (getExportFlag a)
\end{code}

%************************************************************************
%*									*
\subsection{Overloaded functions related to Names}
%*									*
%************************************************************************

\begin{code}
class NamedThing a where
    getName :: a -> Name
\end{code}

\begin{code}
origName	    :: NamedThing a => String -> a -> OrigName
moduleOf	    :: OrigName -> Module
nameOf		    :: OrigName -> FAST_STRING

getOccName	    :: NamedThing a => a -> RdrName
getLocalName	    :: NamedThing a => a -> FAST_STRING
getExportFlag	    :: NamedThing a => a -> ExportFlag
getSrcLoc	    :: NamedThing a => a -> SrcLoc
getImpLocs	    :: NamedThing a => a -> [SrcLoc]
isLocallyDefined    :: NamedThing a => a -> Bool

origName str n	    = nameOrigName str (getName n)

moduleOf (OrigName m n) = m
nameOf   (OrigName m n) = n

getLocalName n
  = case (getName n) of
      Local  _ n _ _	         -> n
      Global _ m (Left  n) _ _ _ -> n
      Global _ m (Right n) _ _ _ -> let str = _CONCAT_ (glue n) in
				    -- pprTrace "getLocalName:" (ppPStr str) $
				    str

getOccName	    = nameOccName  	   . getName
getExportFlag	    = nameExportFlag	   . getName
getSrcLoc	    = nameSrcLoc	   . getName
getImpLocs	    = nameImpLocs	   . getName
isLocallyDefined    = isLocallyDefinedName . getName
\end{code}

\begin{code}
{-# SPECIALIZE getLocalName
	:: Name     -> FAST_STRING
	 , OrigName -> FAST_STRING
	 , RdrName  -> FAST_STRING
	 , RnName   -> FAST_STRING
  #-}
{-# SPECIALIZE isLocallyDefined
	:: Name	    -> Bool
	 , RnName   -> Bool
  #-}
{-# SPECIALIZE origName
	:: String -> Name     -> OrigName
	 , String -> RdrName  -> OrigName
	 , String -> RnName   -> OrigName
  #-}
\end{code}

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.  Normally applied as in e.g. @isCon
(getLocalName foo)@.

\begin{code}
isLexCon, isLexVar, isLexId, isLexSym, isLexConId, isLexConSym,
 isLexVarId, isLexVarSym, isLexSpecialSym :: FAST_STRING -> Bool

isLexCon cs = isLexConId  cs || isLexConSym cs
isLexVar cs = isLexVarId  cs || isLexVarSym cs

isLexId  cs = isLexConId  cs || isLexVarId  cs
isLexSym cs = isLexConSym cs || isLexVarSym cs

-------------

isLexConId cs
  | _NULL_ cs	= False
  | otherwise	= isUpper c || isUpperISO c
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
--	       || c  == '('	-- (), (,), (,,), ...
	       || cs == SLIT("->")
--	       || cs == SLIT("[]")
  where
    c = _HEAD_ cs

isLexVarSym cs
  | _NULL_ cs = False
  | otherwise = isSymbolASCII c
	     || isSymbolISO c
--	     || c  == '('	-- (), (,), (,,), ...
--	     || cs == SLIT("[]")
  where
    c = _HEAD_ cs

isLexSpecialSym cs
  | _NULL_ cs = False
  | otherwise = c  == '('	-- (), (,), (,,), ...
	     || cs == SLIT("[]")
  where
    c = _HEAD_ cs

-------------
isSymbolASCII c = c `elem` "!#$%&*+./<=>?@\\^|~-"
isSymbolISO   c = ord c `elem` (0xd7 : 0xf7 : [0xa1 .. 0xbf])
isUpperISO    c = 0xc0 <= oc && oc <= 0xde && oc /= 0xd7 where oc = ord c
isLowerISO    c = 0xdf <= oc && oc <= 0xff && oc /= 0xf7 where oc = ord c
\end{code}

And one ``higher-level'' interface to those:

\begin{code}
isSymLexeme :: NamedThing a => a -> Bool

isSymLexeme v
  = let str = getLocalName v in isLexSym str

-- print `vars`, (op) correctly
pprSym, pprNonSym :: (NamedThing name, Outputable name) => PprStyle -> name -> Pretty

pprSym sty var
  = let
	str = getLocalName var
    in
    if isLexSym str && not (isLexSpecialSym str)
    then ppr sty var
    else ppBesides [ppChar '`', ppr sty var, ppChar '`']

pprNonSym sty var
  = if isSymLexeme var
    then ppParens (ppr sty var)
    else ppr sty var
\end{code}
