%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
#include "HsVersions.h"

module Name (
	-- things for the Name NON-abstract type
	Name(..),

	isTyConName, isClassName, isClassOpName,
	getTagFromClassOpName, isUnboundName,
	invisibleName,
	eqName, cmpName,

	-- to make the interface self-sufficient
	Id, FullName, ShortName, TyCon, Unique
#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
    ) where

import AbsUniType	( cmpTyCon, TyCon, Class, ClassOp, Arity(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Id		( cmpId, Id )
import NameTypes	-- all of them
import Outputable
import Pretty
import SrcLoc		( mkBuiltinSrcLoc, mkUnknownSrcLoc )
import Unique		( eqUnique, cmpUnique, pprUnique, Unique )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Name-datatype]{The @Name@ datatype}
%*									*
%************************************************************************

\begin{code}
data Name
  = Short	    Unique	-- Local ids and type variables
		    ShortName

	-- Nano-prelude things; truly wired in.
	-- Includes all type constructors and their associated data constructors
  | WiredInTyCon    TyCon
  | WiredInVal	    Id

	-- Prelude things not actually wired into the compiler, but important
	-- enough to get their own special lookup key (a magic Unique).
  | PreludeVal	    Unique{-IdKey-}    FullName
  | PreludeTyCon    Unique{-TyConKey-} FullName Arity Bool -- as for OtherTyCon
  | PreludeClass    Unique{-ClassKey-} FullName

  | OtherTyCon	    Unique	-- TyCons other than Prelude ones; need to
		    FullName	-- separate these because we want to pin on
		    Arity	-- their arity.
		    Bool	-- True <=> `data', False <=> `type'
		    [Name]	-- List of user-visible data constructors;
				-- NB: for `data' types only.
				-- Used in checking import/export lists.

  | OtherClass	    Unique
		    FullName
		    [Name]	-- List of class methods; used for checking
				-- import/export lists.

  | OtherTopId	    Unique	-- Top level id
		    FullName

  | ClassOpName	    Unique
		    Name	-- Name associated w/ the defined class
				-- (can get unique and export info, etc., from this)
		    FAST_STRING	-- The class operation
		    Int		-- Unique tag within the class

	-- Miscellaneous
  | Unbound	    FAST_STRING	-- Placeholder for a name which isn't in scope
				-- Used only so that the renamer can carry on after
				-- finding an unbound identifier.
				-- The string is grabbed from the unbound name, for
				-- debugging information only.
\end{code}

These @is..@ functions are used in the renamer to check that (eg) a tycon
is seen in a context which demands one.

\begin{code}
isTyConName, isClassName, isUnboundName :: Name -> Bool

isTyConName (WiredInTyCon _)	   = True
isTyConName (PreludeTyCon _ _ _ _) = True
isTyConName (OtherTyCon _ _ _ _ _) = True
isTyConName other		   = False

isClassName (PreludeClass _ _) = True
isClassName (OtherClass _ _ _) = True
isClassName other	       = False

isUnboundName (Unbound _) = True
isUnboundName other	  = False
\end{code}

@isClassOpName@ is a little cleverer: it checks to see whether the
class op comes from the correct class.

\begin{code}
isClassOpName :: Name	-- The name of the class expected for this op
	      -> Name	-- The name of the thing which should be a class op
	      -> Bool

isClassOpName (PreludeClass key1 _)  (ClassOpName _ (PreludeClass key2 _) _ _)
  = key1 == key2
isClassOpName (OtherClass uniq1 _ _) (ClassOpName _ (OtherClass uniq2 _ _) _ _)
  = eqUnique uniq1 uniq2
isClassOpName other_class other_op = False
\end{code}

A Name is ``invisible'' if the user has no business seeing it; e.g., a
data-constructor for an abstract data type (but whose constructors are
known because of a pragma).
\begin{code}
invisibleName :: Name -> Bool

invisibleName (PreludeVal _ n)	     = invisibleFullName n
invisibleName (PreludeTyCon _ n _ _) = invisibleFullName n
invisibleName (PreludeClass _ n)     = invisibleFullName n
invisibleName (OtherTyCon _ n _ _ _) = invisibleFullName n
invisibleName (OtherClass _ n _)     = invisibleFullName n
invisibleName (OtherTopId _ n)	     = invisibleFullName n
invisibleName _			     = False
\end{code}

\begin{code}
getTagFromClassOpName :: Name -> Int

getTagFromClassOpName (ClassOpName _ _ _ tag)  = tag
\end{code}


%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = cmp n1 n2
  where
    cmp (Short u1 _)		(Short u2 _)		= cmpUnique u1 u2
  				
    cmp (WiredInTyCon tc1)	(WiredInTyCon tc2)	= cmpTyCon tc1 tc2
    cmp (WiredInVal   id1)	(WiredInVal   id2)	= cmpId	   id1 id2
  				
    cmp (PreludeVal   k1 _)	(PreludeVal   k2 _)	= cmpUnique k1 k2
    cmp (PreludeTyCon k1 _ _ _) (PreludeTyCon k2 _ _ _) = cmpUnique k1 k2
    cmp (PreludeClass k1 _)	(PreludeClass k2 _)	= cmpUnique k1 k2

    cmp (OtherTyCon u1 _ _ _ _)	(OtherTyCon u2 _ _ _ _) = cmpUnique u1 u2
    cmp (OtherClass u1 _ _)	(OtherClass u2 _ _)	= cmpUnique u1 u2
    cmp (OtherTopId u1 _)	(OtherTopId u2 _)	= cmpUnique u1 u2
  				
    cmp (ClassOpName u1 _ _ _)	(ClassOpName u2 _ _ _)	= cmpUnique u1 u2
#if 0  				
    -- panic won't unify w/ CMP_TAG (Int#)
    cmp (Unbound a)		(Unbound b)		= panic "Eq.Name.Unbound"
#endif

    cmp other_1 other_2		-- the tags *must* be different
      = let tag1 = tag_Name n1
	    tag2 = tag_Name n2
	in
	if tag1 _LT_ tag2 then LT_ else GT_

    tag_Name (Short _ _)		= (ILIT(1) :: FAST_INT)
    tag_Name (WiredInTyCon _)		= ILIT(2)
    tag_Name (WiredInVal _)		= ILIT(3)
    tag_Name (PreludeVal _ _)		= ILIT(4)
    tag_Name (PreludeTyCon _ _ _ _)	= ILIT(5)
    tag_Name (PreludeClass _ _)		= ILIT(6)
    tag_Name (OtherTyCon _ _ _ _ _)	= ILIT(7)
    tag_Name (OtherClass _ _ _)		= ILIT(8)
    tag_Name (OtherTopId _ _)		= ILIT(9)
    tag_Name (ClassOpName _ _ _ _)	= ILIT(10)
    tag_Name (Unbound _)		= ILIT(11)
\end{code}

\begin{code}
eqName a b = case cmpName a b of { EQ_ -> True;  _   -> False }
gtName a b = case cmpName a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }

instance Eq Name where
    a == b = case cmpName a b of { EQ_ -> True;  _ -> False }
    a /= b = case cmpName a b of { EQ_ -> False; _ -> True }

instance Ord Name where
    a <= b = case cmpName a b of { LT_ -> True;	 EQ_ -> True;  GT__ -> False }
    a <	 b = case cmpName a b of { LT_ -> True;	 EQ_ -> False; GT__ -> False }
    a >= b = case cmpName a b of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case cmpName a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpName a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}

\begin{code}
instance NamedThing Name where
    getExportFlag (Short _ _)		= NotExported
    getExportFlag (WiredInTyCon _)	= NotExported -- compiler always know about these
    getExportFlag (WiredInVal _)	= NotExported
    getExportFlag (ClassOpName _ c _ _) = getExportFlag c
    getExportFlag other			= getExportFlag (get_nm "getExportFlag" other)

    isLocallyDefined (Short _ _)	   = True
    isLocallyDefined (WiredInTyCon _)	   = False
    isLocallyDefined (WiredInVal _)	   = False
    isLocallyDefined (ClassOpName _ c _ _) = isLocallyDefined c
    isLocallyDefined other		   = isLocallyDefined (get_nm "isLocallyDefined" other)

    getOrigName (Short _ sn)		= getOrigName sn
    getOrigName (WiredInTyCon tc)	= getOrigName tc
    getOrigName (WiredInVal id)		= getOrigName id
    getOrigName (ClassOpName _ c op _)	= (fst (getOrigName c), op)
    getOrigName other			= getOrigName (get_nm "getOrigName" other)

    getOccurrenceName (Short _ sn)	   = getOccurrenceName sn
    getOccurrenceName (WiredInTyCon tc)    = getOccurrenceName tc
    getOccurrenceName (WiredInVal id)	   = getOccurrenceName id
    getOccurrenceName (ClassOpName _ _ op _) = op
    getOccurrenceName (Unbound s)	   =  s _APPEND_ SLIT("<unbound>")
    getOccurrenceName other		   = getOccurrenceName (get_nm "getOccurrenceName" other)

    getInformingModules thing = panic "getInformingModule:Name"

    getSrcLoc (Short _ sn)	   = getSrcLoc sn
    getSrcLoc (WiredInTyCon tc)    = mkBuiltinSrcLoc
    getSrcLoc (WiredInVal id)	   = mkBuiltinSrcLoc
    getSrcLoc (ClassOpName _ c _ _)  = getSrcLoc c
    getSrcLoc (Unbound _)	   = mkUnknownSrcLoc
    getSrcLoc other		   = getSrcLoc (get_nm "getSrcLoc" other)

    getTheUnique (Short uniq _)		= uniq
    getTheUnique (OtherTopId uniq _)	= uniq
    getTheUnique other
      = pprPanic "NamedThing.Name.getTheUnique: not a Short or OtherTopId:" (ppr PprShowAll other)

    fromPreludeCore (WiredInTyCon _)	   = True
    fromPreludeCore (WiredInVal _)	   = True
    fromPreludeCore (PreludeVal	  _ n)	   = fromPreludeCore n
    fromPreludeCore (PreludeTyCon _ n _ _) = fromPreludeCore n
    fromPreludeCore (PreludeClass _ n)     = fromPreludeCore n
    fromPreludeCore (ClassOpName _ c _ _)  = fromPreludeCore c
    fromPreludeCore other		   = False

    hasType n			= False
    getType n			= panic "NamedThing.Name.getType"
\end{code}

A useful utility; most emphatically not for export!:
\begin{code}
get_nm :: String -> Name -> FullName

get_nm msg (PreludeVal _ n)	  = n
get_nm msg (PreludeTyCon _ n _ _) = n
get_nm msg (OtherTyCon _ n _ _ _) = n
get_nm msg (PreludeClass _ n)	  = n
get_nm msg (OtherClass _ n _)	  = n
get_nm msg (OtherTopId _ n)	  = n
#ifdef DEBUG
get_nm msg other = pprPanic ("get_nm:"++msg) (ppr PprShowAll other)
-- If match failure, probably on a ClassOpName or Unbound :-(
#endif
\end{code}

\begin{code}
instance Outputable Name where
#ifdef DEBUG
    ppr PprDebug (Short u s)	    = pp_debug u s
    ppr PprDebug (PreludeVal u i)   = pp_debug u i
    ppr PprDebug (PreludeTyCon u t _ _) = pp_debug u t
    ppr PprDebug (PreludeClass u c) = pp_debug u c

    ppr PprDebug (OtherTyCon u n _ _ _) = pp_debug u n
    ppr PprDebug (OtherClass u n _)     = pp_debug u n
    ppr PprDebug (OtherTopId u n)       = pp_debug u n
#endif
    ppr sty (Short u s)		= ppr sty s

    ppr sty (WiredInTyCon tc)	   = ppr sty tc
    ppr sty (WiredInVal   id)	   = ppr sty id
    ppr sty (PreludeVal	  _ i)	   = ppr sty i
    ppr sty (PreludeTyCon _ t _ _) = ppr sty t
    ppr sty (PreludeClass _ c)     = ppr sty c

    ppr sty (OtherTyCon u n a b c) = ppr sty n
    ppr sty (OtherClass u n c)	   = ppr sty n
    ppr sty (OtherTopId u n)	   = ppr sty n

    ppr sty (ClassOpName u c s i)
      = let
	    ps = ppPStr s
	in
	case sty of
	  PprForUser     -> ps
	  PprInterface _ -> ps
	  PprDebug	 -> ps
	  other	         -> ppBesides [ps, ppChar '{',
				       ppSep [pprUnique u,
					      ppStr "op", ppInt i,
					      ppStr "cls", ppr sty c],
				       ppChar '}']

    ppr sty (Unbound s) = ppStr ("*UNBOUND*"++ _UNPK_ s)

pp_debug uniq thing
  = ppBesides [ppr PprDebug thing, ppStr "{-", pprUnique uniq, ppStr "-}" ]
\end{code}
