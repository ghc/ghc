%
% (c) The GRASP Project, Glasgow University, 1992-1995
%
\section[Outputable]{Classes for pretty-printing}

Defines classes for pretty-printing and forcing, both forms of
``output.''

\begin{code}
#include "HsVersions.h"

module Outputable (
	-- NAMED-THING-ERY
	NamedThing(..),		-- class
	ExportFlag(..),
	isExported, getLocalName, ltLexical,

	-- PRINTERY AND FORCERY
	Outputable(..), 	-- class
	PprStyle(..),		-- style-ry (re-exported)

	interppSP, interpp'SP,
--UNUSED: ifPprForUser,
	ifnotPprForUser,
	ifPprDebug, --UNUSED: ifnotPprDebug,
	ifPprShowAll, ifnotPprShowAll,
	ifPprInterface, --UNUSED: ifnotPprInterface,
--UNUSED: ifPprForC, ifnotPprForC,
--UNUSED: ifPprUnfolding, ifnotPprUnfolding,

	isOpLexeme, pprOp, pprNonOp,
	isConop, isAconop, isAvarid, isAvarop, --UNUSED: isAconid,

	-- and to make the interface self-sufficient...
	Pretty(..), GlobalSwitch,
	PrettyRep, UniType, Unique, SrcLoc
    ) where

import AbsUniType	( UniType,
			  TyCon, Class, TyVar, TyVarTemplate -- for SPECIALIZing
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyVar)
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			)
import Id		( Id ) -- for specialising
import NameTypes	-- for specialising
import ProtoName	-- for specialising
import Pretty
import SrcLoc		( SrcLoc )
import Unique		( Unique )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[NamedThing-class]{The @NamedThing@ class}
%*									*
%************************************************************************

\begin{code}
class NamedThing a where
    getExportFlag 	:: a -> ExportFlag
    isLocallyDefined	:: a -> Bool
    getOrigName		:: a -> (FAST_STRING{-module-}, FAST_STRING{-name therein-})
    getOccurrenceName	:: a -> FAST_STRING
    getInformingModules	:: a -> [FAST_STRING]
    getSrcLoc		:: a -> SrcLoc
    getTheUnique	:: a -> Unique
    hasType		:: a -> Bool
    getType		:: a -> UniType
    fromPreludeCore	:: a -> Bool
    -- see also friendly functions that follow...
\end{code}

\begin{description}
\item[@getExportFlag@:]
Obvious.

\item[@getOrigName@:]
Obvious.

\item[@isLocallyDefined@:]
Whether the thing is defined in this module or not.

\item[@getOccurrenceName@:]
Gets the name by which a thing is known in this module (e.g., if
renamed, or whatever)...

\item[@getInformingModules@:]
Gets the name of the modules that told me about this @NamedThing@.

\item[@getSrcLoc@:]
Obvious.

\item[@hasType@ and @getType@:]
In pretty-printing @AbsSyntax@, we need to query if a datatype has
types attached yet or not.  We use @hasType@ to see if there are types
available; and @getType@ if we want to grab one...  (Ugly but effective)

\item[@fromPreludeCore@:]
Tests a quite-delicate property: it is \tr{True} iff the entity is
actually defined in \tr{PreludeCore} (or \tr{PreludeBuiltin}), or if
it is re-exported by \tr{PreludeCore}.  See the @FullName@ type in
module \tr{NameTypes}.

NB: Some of the types in, e.g., \tr{PreludeGlaST} {\em fail} this test.
This is a bummer for types that are wired into the compiler.
\end{description}

Some functions to go with:
\begin{code}
isExported a
  = case (getExportFlag a) of
      NotExported -> False
      _		  -> True

getLocalName :: (NamedThing a) => a -> FAST_STRING

getLocalName = snd . getOrigName

#ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE isExported :: Class -> Bool #-}
{-# SPECIALIZE isExported :: Id -> Bool #-}
{-# SPECIALIZE isExported :: TyCon -> Bool #-}
{-# SPECIALIZE getLocalName :: ShortName -> FAST_STRING #-}
#endif
\end{code}

@ltLexical@ is used for sorting things into lexicographical order, so
as to canonicalize interfaces.  [Regular @(<)@ should be used for fast
comparison.]

\begin{code}
a `ltLexical` b
  = BIND isLocallyDefined a	_TO_ a_local ->
    BIND isLocallyDefined b	_TO_ b_local ->
    BIND getOrigName a		_TO_ (a_mod, a_name) ->
    BIND getOrigName b		_TO_ (b_mod, b_name) ->
    if a_local || b_local then
       a_name < b_name	-- can't compare module names
    else
       case _CMP_STRING_ a_mod b_mod of
	 LT_  -> True
	 EQ_  -> a_name < b_name
	 GT__ -> False
    BEND BEND BEND BEND

#ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE ltLexical :: Class -> Class -> Bool #-}
{-# SPECIALIZE ltLexical :: Id -> Id -> Bool #-}
{-# SPECIALIZE ltLexical :: TyCon -> TyCon -> Bool #-}
#endif
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
  | ExportAbs		-- export abstractly
  | NotExported
\end{code}

%************************************************************************
%*									*
\subsection[Outputable-class]{The @Outputable@ class}
%*									*
%************************************************************************

\begin{code}
class Outputable a where
	ppr :: PprStyle -> a -> Pretty
\end{code}

\begin{code}
-- the ppSep in the ppInterleave puts in the spaces
-- Death to ppSep! (WDP 94/11)

interppSP  :: Outputable a => PprStyle -> [a] -> Pretty
interppSP  sty xs = ppIntersperse ppSP (map (ppr sty) xs)

interpp'SP :: Outputable a => PprStyle -> [a] -> Pretty
interpp'SP sty xs
  = ppInterleave sep (map (ppr sty) xs)
  where
    sep = ppBeside ppComma ppSP

#ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE interppSP :: PprStyle -> [Id] -> Pretty #-}
{-# SPECIALIZE interppSP :: PprStyle -> [TyVar] -> Pretty #-}

{-# SPECIALIZE interpp'SP :: PprStyle -> [(Id, Id)] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [Id] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [ProtoName] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [TyVarTemplate] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [TyVar] -> Pretty #-}
{-# SPECIALIZE interpp'SP :: PprStyle -> [UniType] -> Pretty #-}
#endif
\end{code}

\begin{code}
--UNUSED: ifPprForUser	sty p = case sty of PprForUser	 -> p ; _ -> ppNil
ifPprDebug	sty p = case sty of PprDebug	 -> p ; _ -> ppNil
ifPprShowAll	sty p = case sty of PprShowAll	 -> p ; _ -> ppNil
ifPprInterface  sty p = case sty of PprInterface _ -> p ; _ -> ppNil
--UNUSED: ifPprForC   	sty p = case sty of PprForC      _ -> p ; _ -> ppNil
--UNUSED: ifPprUnfolding  sty p = case sty of PprUnfolding _ -> p ; _ -> ppNil

ifnotPprForUser	  sty p = case sty of PprForUser    -> ppNil ; _ -> p
--UNUSED: ifnotPprDebug	  sty p = case sty of PprDebug	    -> ppNil ; _ -> p
ifnotPprShowAll	  sty p = case sty of PprShowAll    -> ppNil ; _ -> p
--UNUSED: ifnotPprInterface sty p = case sty of PprInterface _ -> ppNil; _ -> p
--UNUSED: ifnotPprForC   	  sty p = case sty of PprForC      _ -> ppNil; _ -> p
--UNUSED: ifnotPprUnfolding sty p = case sty of PprUnfolding _ -> ppNil; _ -> p
\end{code}

These functions test strings to see if they fit the lexical categories
defined in the Haskell report.  Normally applied as in, e.g.,
@isConop (getOccurrenceName foo)@... [just for pretty-printing]

\begin{code}
isConop, isAconop, isAvarid, isAvarop :: FAST_STRING -> Bool

isConop cs
  | _NULL_ cs	= False
  | c == '_'	= isConop (_TAIL_ cs)	-- allow for leading _'s
  | otherwise	= isUpper c || c == ':'
  where
    c = _HEAD_ cs

{- UNUSED:
isAconid []       = False
isAconid ('_':cs) = isAconid cs
isAconid (c:cs)   = isUpper c
-}

isAconop cs
  | _NULL_ cs	= False
  | otherwise	= c == ':'
  where
    c = _HEAD_ cs

isAvarid cs
  | _NULL_ cs	= False
  | c == '_'	= isAvarid (_TAIL_ cs)	-- allow for leading _'s
  | otherwise	= isLower c
  where
    c = _HEAD_ cs

isAvarop cs
  | _NULL_ cs	= False
  | isLower c	= False -- shortcut
  | isUpper c	= False -- ditto
  | otherwise	= c `elem` "!#$%&*+./<=>?@\\^|~-" -- symbol or minus
  where
    c = _HEAD_ cs
\end{code}

And one ``higher-level'' interface to those:

\begin{code}
isOpLexeme :: NamedThing a => a -> Bool

isOpLexeme v
  = let str = getOccurrenceName v in isAvarop str || isAconop str

-- print `vars`, (op) correctly
pprOp, pprNonOp :: (NamedThing name, Outputable name) => PprStyle -> name -> Pretty

pprOp sty var
  = if isOpLexeme var
    then ppr sty var
    else ppBesides [ppChar '`', ppr sty var, ppChar '`']

pprNonOp sty var
  = if isOpLexeme var
    then ppBesides [ppLparen, ppr sty var, ppRparen]
    else ppr sty var

#ifdef USE_ATTACK_PRAGMAS
{-# SPECIALIZE isOpLexeme :: Id -> Bool #-}
{-# SPECIALIZE pprNonOp :: PprStyle -> Id -> Pretty #-}
{-# SPECIALIZE pprNonOp :: PprStyle -> TyCon -> Pretty #-}
{-# SPECIALIZE pprOp :: PprStyle -> Id -> Pretty #-}
#endif
\end{code}

\begin{code}
instance Outputable Bool where
    ppr sty True = ppPStr SLIT("True")
    ppr sty False = ppPStr SLIT("False")

instance (Outputable a) => Outputable [a] where
    ppr sty xs =
      ppBesides [ ppLbrack, ppInterleave ppComma (map (ppr sty) xs), ppRbrack ]

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr sty (x,y) =
      ppHang (ppBesides [ppLparen, ppr sty x, ppComma]) 4 (ppBeside (ppr sty y) ppRparen)

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr sty (x,y,z) =
      ppSep [ ppBesides [ppLparen, ppr sty x, ppComma],
	      ppBeside (ppr sty y) ppComma,
	      ppBeside (ppr sty z) ppRparen ]
\end{code}
