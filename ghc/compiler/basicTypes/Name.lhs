%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
#include "HsVersions.h"

module Name (
	Module(..),

	RdrName(..),
	isUnqual,
	isQual,
	isRdrLexCon,
	appendRdr,
	showRdr,
	cmpRdr,

	Name,
	Provenance,
	mkLocalName, isLocalName, 
	mkTopLevName, mkImportedName,
	mkImplicitName,	isImplicitName,
	mkBuiltinName, mkCompoundName,

	mkFunTyConName, mkTupleDataConName, mkTupleTyConName,
	mkTupNameStr,

	NamedThing(..), -- class
	ExportFlag(..),
	isExported{-overloaded-}, exportFlagOn{-not-},

	nameUnique,
	nameOccName,
	nameOrigName,
	nameExportFlag,
	nameSrcLoc,
	nameImpLocs,
	nameImportFlag,
	isLocallyDefinedName,
	isPreludeDefinedName,

	origName, moduleOf, nameOf, moduleNamePair,
	getOccName, getExportFlag,
	getSrcLoc, getImpLocs,
	isLocallyDefined, isPreludeDefined,
	getLocalName, ltLexical,

	isSymLexeme, pprSym, pprNonSym,
	isLexCon, isLexVar, isLexId, isLexSym, isLexSpecialSym,
	isLexConId, isLexConSym, isLexVarId, isLexVarSym
    ) where

import Ubiq

import CStrings		( identToC, cSEP )
import Outputable	( Outputable(..) )
import PprStyle		( PprStyle(..), codeStyle )
import PrelMods		( pRELUDE, pRELUDE_BUILTIN, fromPrelude )
import Pretty
import SrcLoc		( mkBuiltinSrcLoc, mkUnknownSrcLoc )
import Unique		( funTyConKey, mkTupleDataConUnique, mkTupleTyConUnique,
			  pprUnique, Unique
			)
import Util		( thenCmp, _CMP_STRING_, nOfThem, panic, assertPanic )
\end{code}

%************************************************************************
%*									*
\subsection[RdrName]{The @RdrName@ datatype; names read from files}
%*									*
%************************************************************************

\begin{code}
type Module = FAST_STRING

data RdrName
  = Unqual FAST_STRING
  | Qual   Module FAST_STRING

isUnqual (Unqual _) = True
isUnqual (Qual _ _) = False

isQual (Unqual _) = False
isQual (Qual _ _) = True

isRdrLexCon (Unqual n) = isLexCon n
isRdrLexCon (Qual m n) = isLexCon n

appendRdr (Unqual n) str = Unqual (n _APPEND_ str)
appendRdr (Qual m n) str = ASSERT(not (fromPrelude m))
			   Qual m (n _APPEND_ str)

cmpRdr (Unqual n1)  (Unqual n2)  = _CMP_STRING_ n1 n2
cmpRdr (Unqual n1)  (Qual m2 n2) = LT_
cmpRdr (Qual m1 n1) (Unqual n2)  = GT_
cmpRdr (Qual m1 n1) (Qual m2 n2) = thenCmp (_CMP_STRING_ m1 m2) (_CMP_STRING_ n1 n2) 

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
    getName rdr_name
      = Global u rdr_name prov ex [rdr_name]
      where
	u    = panic "NamedThing.RdrName:Unique"
	prov = panic "NamedThing.RdrName:Provenance"
	ex   = panic "NamedThing.RdrName:ExportFlag"

instance Outputable RdrName where
    ppr sty (Unqual n) = pp_name sty n
    ppr sty (Qual m n) = ppBeside (pp_mod sty m) (pp_name sty n)

pp_mod PprForC             m = ppBesides [identToC m, ppPStr cSEP]
pp_mod (PprForAsm False _) m = ppBesides [identToC m, ppPStr cSEP]
pp_mod (PprForAsm True  _) m = ppBesides [ppPStr cSEP, identToC m, ppPStr cSEP]
pp_mod _                   m = ppBesides [ppPStr m, ppChar '.']

pp_name sty n | codeStyle sty = identToC n
              | otherwise     = ppPStr n	      

showRdr sty rdr = ppShow 100 (ppr sty rdr)
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
             SrcLoc

  | Global   Unique
             RdrName      -- original name; Unqual => prelude
             Provenance   -- where it came from
             ExportFlag   -- is it exported?
             [RdrName]    -- ordered occurrence names (usually just one);
			  -- first may be *un*qual.

data Provenance
  = LocalDef SrcLoc       -- locally defined; give its source location

  | Imported ExportFlag	  -- how it was imported
	     SrcLoc       -- *original* source location
             [SrcLoc]     -- any import source location(s)

  | Implicit
  | Builtin
\end{code}

\begin{code}
mkLocalName = Local

mkTopLevName   u orig locn exp occs = Global u orig (LocalDef locn) exp occs
mkImportedName u orig imp locn imp_locs exp occs = Global u orig (Imported imp locn imp_locs) exp occs

mkImplicitName :: Unique -> RdrName -> Name
mkImplicitName u o = Global u o Implicit NotExported []

mkBuiltinName :: Unique -> Module -> FAST_STRING -> Name
mkBuiltinName u m n = Global u (Unqual n) Builtin NotExported []

mkCompoundName :: Unique -> [FAST_STRING] -> Name
mkCompoundName u ns
  = Global u (Unqual{-???-} (_CONCAT_ (dotify ns))) Builtin{--} NotExported []
  where
    dotify []  = []
    dotify [n] = [n]
    dotify (n:ns) = n : (map (_CONS_ '.') ns)

mkFunTyConName
  = mkBuiltinName funTyConKey		       pRELUDE_BUILTIN SLIT("->")
mkTupleDataConName arity
  = mkBuiltinName (mkTupleDataConUnique arity) pRELUDE_BUILTIN (mkTupNameStr arity)
mkTupleTyConName   arity
  = mkBuiltinName (mkTupleTyConUnique   arity) pRELUDE_BUILTIN (mkTupNameStr arity)

mkTupNameStr 0 = SLIT("()")
mkTupNameStr 1 = panic "Name.mkTupNameStr: 1 ???"
mkTupNameStr 2 = SLIT("(,)")   -- not strictly necessary
mkTupNameStr 3 = SLIT("(,,)")  -- ditto
mkTupNameStr 4 = SLIT("(,,,)") -- ditto
mkTupNameStr n
  = _PK_ ("(" ++ nOfThem (n-1) ',' ++ ")")

	-- ToDo: what about module ???
	-- ToDo: exported when compiling builtin ???

isLocalName (Local _ _ _) = True
isLocalName _ 		= False

isImplicitName (Global _ _ Implicit _ _) = True
isImplicitName _ 		         = False

isBuiltinName  (Global _ _ Builtin  _ _) = True
isBuiltinName  _ 		         = False
\end{code}



%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = c n1 n2
  where
    c (Local    u1 _ _)	    (Local    u2 _ _)     = cmp u1 u2
    c (Global   u1 _ _ _ _) (Global   u2 _ _ _ _) = cmp u1 u2

    c other_1 other_2		-- the tags *must* be different
      = let tag1 = tag_Name n1
	    tag2 = tag_Name n2
	in
	if tag1 _LT_ tag2 then LT_ else GT_

    tag_Name (Local    _ _ _)	  = (ILIT(1) :: FAST_INT)
    tag_Name (Global   _ _ _ _ _) = ILIT(2)
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
nameUnique (Local    u _ _)     = u
nameUnique (Global   u _ _ _ _) = u

nameOrigName (Local    _ n _)	     = Unqual n
nameOrigName (Global   _ orig _ _ _) = orig

nameModuleNamePair (Local    _ n _) = (panic "nameModuleNamePair", n)
nameModuleNamePair (Global   _ (Unqual n) _ _ _) = (pRELUDE, n)
nameModuleNamePair (Global   _ (Qual m n) _ _ _) = (m, n)

nameOccName (Local    _ n _)	       = Unqual n
nameOccName (Global   _ orig _ _ []  ) = orig
nameOccName (Global   _ orig _ _ occs) = head occs

nameExportFlag (Local    _ _ _)	      = NotExported
nameExportFlag (Global   _ _ _ exp _) = exp

nameSrcLoc (Local  _ _ loc)	               = loc
nameSrcLoc (Global _ _ (LocalDef loc)     _ _) = loc
nameSrcLoc (Global _ _ (Imported _ loc _) _ _) = loc
nameSrcLoc (Global _ _ Implicit           _ _) = mkUnknownSrcLoc
nameSrcLoc (Global _ _ Builtin            _ _) = mkBuiltinSrcLoc
  
nameImpLocs (Global _ _ (Imported _ _ locs) _ _) = locs
nameImpLocs _ 					 = []

nameImportFlag (Local _ _ _)                       = NotExported
nameImportFlag (Global _ _ (LocalDef _)       _ _) = ExportAll
nameImportFlag (Global _ _ (Imported exp _ _) _ _) = exp
nameImportFlag (Global _ _ Implicit           _ _) = ExportAll
nameImportFlag (Global _ _ Builtin            _ _) = ExportAll

isLocallyDefinedName (Local  _ _ _)	               = True
isLocallyDefinedName (Global _ _ (LocalDef _)     _ _) = True
isLocallyDefinedName (Global _ _ (Imported _ _ _) _ _) = False
isLocallyDefinedName (Global _ _ Implicit         _ _) = False
isLocallyDefinedName (Global _ _ Builtin          _ _) = False

isPreludeDefinedName (Local    _ n _)        = False
isPreludeDefinedName (Global   _ orig _ _ _) = isUnqual orig
\end{code}

\begin{code}
instance Outputable Name where
#ifdef DEBUG
    ppr PprDebug (Local    u n _)     = pp_debug u (ppPStr n)
    ppr PprDebug (Global   u o _ _ _) = pp_debug u (ppr PprDebug o)
#endif
    ppr sty        (Local    u n _)             = pp_name sty n
    ppr PprForUser (Global   u o _ _ []  )      = ppr PprForUser o
    ppr PprForUser (Global   u o _ _ occs)      = ppr PprForUser (head occs)
    ppr PprShowAll (Global   u o prov exp occs) = pp_all o prov exp occs
    ppr sty        (Global   u o _ _ _)         = ppr sty o

pp_debug uniq thing
  = ppBesides [thing, ppStr "{-", pprUnique uniq, ppStr "-}" ]

pp_all orig prov exp occs
  = ppBesides [ppr PprShowAll orig, ppr PprShowAll occs, pp_prov prov, pp_exp exp]

pp_exp NotExported = ppNil
pp_exp ExportAll   = ppPStr SLIT("/EXP(..)")
pp_exp ExportAbs   = ppPStr SLIT("/EXP")

pp_prov Implicit = ppPStr SLIT("/IMPLICIT")
pp_prov Builtin  = ppPStr SLIT("/BUILTIN")
pp_prov _        = ppNil
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
origName	    :: NamedThing a => a -> RdrName
moduleOf	    :: RdrName -> Module
nameOf		    :: RdrName -> FAST_STRING
moduleNamePair	    :: NamedThing a => a -> (Module, FAST_STRING)

getOccName	    :: NamedThing a => a -> RdrName
getLocalName	    :: NamedThing a => a -> FAST_STRING
getExportFlag	    :: NamedThing a => a -> ExportFlag
getSrcLoc	    :: NamedThing a => a -> SrcLoc
getImpLocs	    :: NamedThing a => a -> [SrcLoc]
isLocallyDefined    :: NamedThing a => a -> Bool
isPreludeDefined    :: NamedThing a => a -> Bool

-- ToDo: specialise for RdrNames?
origName	    = nameOrigName 	   . getName
moduleNamePair	    = nameModuleNamePair   . getName

moduleOf (Unqual n) = pRELUDE
moduleOf (Qual m n) = m

nameOf (Unqual n)   = n
nameOf (Qual m n)   = n

getLocalName	    = nameOf . origName

getOccName	    = nameOccName  	   . getName
getExportFlag	    = nameExportFlag	   . getName
getSrcLoc	    = nameSrcLoc	   . getName
getImpLocs	    = nameImpLocs	   . getName
isLocallyDefined    = isLocallyDefinedName . getName
isPreludeDefined    = isPreludeDefinedName . getName
\end{code}

@ltLexical@ is used for sorting things into lexicographical order, so
as to canonicalize interfaces.  [Regular @(<)@ should be used for fast
comparison.]

\begin{code}
a `ltLexical` b = origName a < origName b
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
  | c == '_'	= isLexConId (_TAIL_ cs)	-- allow for leading _'s
  | otherwise	= isUpper c || isUpperISO c
  where					
    c = _HEAD_ cs

isLexVarId cs
  | _NULL_ cs	 = False
  | c == '_'	 = isLexVarId (_TAIL_ cs)	-- allow for leading _'s
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
  = let str = nameOf (origName v) in isLexSym str

-- print `vars`, (op) correctly
pprSym, pprNonSym :: (NamedThing name, Outputable name) => PprStyle -> name -> Pretty

pprSym sty var
  = let
	str = nameOf (origName var)
    in
    if isLexSym str && not (isLexSpecialSym str)
    then ppr sty var
    else ppBesides [ppChar '`', ppr sty var, ppChar '`']

pprNonSym sty var
  = if isSymLexeme var
    then ppParens (ppr sty var)
    else ppr sty var
\end{code}
