%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[Name]{@Name@: to transmit name info from renamer to typechecker}

\begin{code}
#include "HsVersions.h"

module Name (
	-- things for the Name NON-abstract type
	Name(..),

	isTyConName, isClassName, isClassOpName,
	isUnboundName, invisibleName,

	getTagFromClassOpName, getSynNameArity,

	getNameShortName, getNameFullName

    ) where

import Ubiq{-uitous-}

import NameLoop		-- break Name/Id loop, Name/PprType/Id loop

import NameTypes
import Outputable	( ExportFlag(..) )
import Pretty
import PprStyle		( PprStyle(..) )
import SrcLoc		( mkBuiltinSrcLoc, mkUnknownSrcLoc )
import TyCon		( TyCon, getSynTyConArity )
import TyVar		( GenTyVar )
import Unique		( pprUnique, Unique )
import Util		( panic, panic#, pprPanic )
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

  | TyConName	    Unique	-- TyCons other than Prelude ones; need to
		    FullName	-- separate these because we want to pin on
		    Arity	-- their arity.
		    Bool        -- False <=> `type',
				-- True <=> `data' or `newtype'
		    [Name]	-- List of user-visible data constructors;
				-- NB: for `data' types only.
				-- Used in checking import/export lists.

  | ClassName	    Unique
		    FullName
		    [Name]	-- List of class methods; used for checking
				-- import/export lists.

  | ValName	    Unique	-- Top level id
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

isTyConName (TyConName _ _ _ _ _) = True
isTyConName (WiredInTyCon _)	  = True
isTyConName other		  = False

isClassName (ClassName _ _ _) = True
isClassName other	      = False

isUnboundName (Unbound _) = True
isUnboundName other	  = False
\end{code}

@isClassOpName@ is a little cleverer: it checks to see whether the
class op comes from the correct class.

\begin{code}
isClassOpName :: Name	-- The name of the class expected for this op
	      -> Name	-- The name of the thing which should be a class op
	      -> Bool

isClassOpName (ClassName uniq1 _ _) (ClassOpName _ (ClassName uniq2 _ _) _ _)
  = uniq1 == uniq2
isClassOpName other_class other_op = False
\end{code}

A Name is ``invisible'' if the user has no business seeing it; e.g., a
data-constructor for an abstract data type (but whose constructors are
known because of a pragma).
\begin{code}
invisibleName :: Name -> Bool

invisibleName (TyConName _ n _ _ _) = invisibleFullName n
invisibleName (ClassName _ n _)     = invisibleFullName n
invisibleName (ValName   _ n)	    = invisibleFullName n
invisibleName _			    = False
\end{code}

\begin{code}
getTagFromClassOpName :: Name -> Int
getTagFromClassOpName (ClassOpName _ _ _ tag)  = tag

getSynNameArity :: Name -> Maybe Arity
getSynNameArity (TyConName _ _ arity False{-syn-} _) = Just arity
getSynNameArity (WiredInTyCon tycon)	             = getSynTyConArity tycon
getSynNameArity other_name			     = Nothing

getNameShortName :: Name -> ShortName
getNameShortName (Short _ sn) = sn

getNameFullName :: Name -> FullName
getNameFullName n = get_nm "getNameFullName" n
\end{code}


%************************************************************************
%*									*
\subsection[Name-instances]{Instance declarations}
%*									*
%************************************************************************

\begin{code}
cmpName n1 n2 = c n1 n2
  where
    c (Short u1 _)	     (Short u2 _)		= cmp u1 u2
			      
    c (WiredInTyCon tc1)     (WiredInTyCon tc2)		= cmp tc1 tc2
    c (WiredInVal   id1)     (WiredInVal   id2)		= cmp id1 id2
			      
    c (TyConName u1 _ _ _ _) (TyConName u2 _ _ _ _) 	= cmp u1 u2
    c (ClassName u1 _ _)     (ClassName u2 _ _)		= cmp u1 u2
    c (ValName   u1 _)	     (ValName   u2 _)		= cmp u1 u2
			      
    c (ClassOpName u1 _ _ _) (ClassOpName u2 _ _ _)	= cmp u1 u2
    c (Unbound a)	     (Unbound b)		= panic# "Eq.Name.Unbound"

    c other_1 other_2		-- the tags *must* be different
      = let tag1 = tag_Name n1
	    tag2 = tag_Name n2
	in
	if tag1 _LT_ tag2 then LT_ else GT_

    tag_Name (Short _ _)		= (ILIT(1) :: FAST_INT)
    tag_Name (WiredInTyCon _)		= ILIT(2)
    tag_Name (WiredInVal _)		= ILIT(3)
    tag_Name (TyConName _ _ _ _ _)	= ILIT(7)
    tag_Name (ClassName _ _ _)		= ILIT(8)
    tag_Name (ValName _ _)		= ILIT(9)
    tag_Name (ClassOpName _ _ _ _)	= ILIT(10)
    tag_Name (Unbound _)		= ILIT(11)
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

    getItsUnique (Short		u _)	   = u
    getItsUnique (WiredInTyCon	t)	   = getItsUnique t
    getItsUnique (WiredInVal	i)	   = getItsUnique i
    getItsUnique (TyConName 	u _ _ _ _) = u
    getItsUnique (ClassName 	u _ _)	   = u
    getItsUnique (ValName 	u _)	   = u
    getItsUnique (ClassOpName 	u _ _ _)   = u

    fromPreludeCore (WiredInTyCon _)	   = True
    fromPreludeCore (WiredInVal _)	   = True
    fromPreludeCore (ClassOpName _ c _ _)  = fromPreludeCore c
    fromPreludeCore other		   = False
\end{code}

A useful utility; most emphatically not for export! (but see
@getNameFullName@...):
\begin{code}
get_nm :: String -> Name -> FullName

get_nm msg (TyConName _ n _ _ _) = n
get_nm msg (ClassName _ n _)	 = n
get_nm msg (ValName   _ n)	 = n
#ifdef DEBUG
get_nm msg other = pprPanic ("get_nm:"++msg) (ppr PprShowAll other)
-- If match failure, probably on a ClassOpName or Unbound :-(
#endif
\end{code}

\begin{code}
instance Outputable Name where
#ifdef DEBUG
    ppr PprDebug (Short u s)	    = pp_debug u s

    ppr PprDebug (TyConName u n _ _ _) = pp_debug u n
    ppr PprDebug (ClassName u n _)     = pp_debug u n
    ppr PprDebug (ValName u n)         = pp_debug u n
#endif
    ppr sty (Short u s)		  = ppr sty s

    ppr sty (WiredInTyCon tc)	  = ppr sty tc
    ppr sty (WiredInVal   id)	  = ppr sty id

    ppr sty (TyConName u n a b c) = ppr sty n
    ppr sty (ClassName u n c)	  = ppr sty n
    ppr sty (ValName   u n)	  = ppr sty n

    ppr sty (ClassOpName u c s i)
      = let
	    ps = ppPStr s
	in
	case sty of
	  PprForUser   -> ps
	  PprInterface -> ps
	  PprDebug     -> ps
	  other	       -> ppBesides [ps, ppChar '{',
				       ppSep [pprUnique u,
					      ppStr "op", ppInt i,
					      ppStr "cls", ppr sty c],
				       ppChar '}']

    ppr sty (Unbound s) = ppStr ("*UNBOUND*"++ _UNPK_ s)

pp_debug uniq thing
  = ppBesides [ppr PprDebug thing, ppStr "{-", pprUnique uniq, ppStr "-}" ]
\end{code}
