%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TyCon]{Type constructors}

\begin{code}
#include "HsVersions.h"

module TyCon (
	TyCon(..),	-- not abstract; usually grabbed via AbsUniType
	Arity(..),	-- synonym for Int
	mkSynonymTyCon, mkDataTyCon, mkTupleTyCon,
	mkPrimTyCon, mkSpecTyCon,
#ifdef DPH
	mkProcessorTyCon,
	mkPodizedPodTyCon,
	isProcessorTyCon,
	isPodizedPodTyCon,
	getPodizedPodDimension,
#endif {- Data Parallel Haskell -}

	isSynTyCon, isVisibleSynTyCon, isDataTyCon,
	isPrimTyCon, isBoxedTyCon,
	maybeCharLikeTyCon, maybeIntLikeTyCon,
	maybeFloatLikeTyCon, maybeDoubleLikeTyCon,
	isEnumerationTyCon, --UNUSED: isEnumerationTyConMostly,
	isTupleTyCon,
	isLocalSpecTyCon, isLocalGenTyCon, isBigTupleTyCon,
	maybeSingleConstructorTyCon,
	derivedFor, --UNUSED: preludeClassDerivedFor,
	cmpTyCon, eqTyCon,

	getTyConArity, getTyConDataCons,
	getTyConTyVarTemplates,
	getTyConKind,
	getTyConDerivings,
	getTyConFamilySize,

	-- to make the interface self-sufficient...
	Class, Id, FullName, PrimKind, TyVarTemplate, UniType,
	Unique, Maybe, DataCon(..)
    ) where

IMPORT_Trace		-- ToDo: rm (debugging)

import AbsPrel		( charPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, pRELUDE_BUILTIN
			)

import Class		( getClassKey, Class
			  IF_ATTACK_PRAGMAS(COMMA cmpClass)
			)
import Id		-- DPH wants to export various things as well
import IdInfo
import Maybes		( Maybe(..) )
import NameTypes	-- various types to do with names
import Outputable	-- class for printing, forcing
import Pretty		-- pretty-printing utilities
import PrimKind		( PrimKind(..) )
import SrcLoc
import TyVar		( TyVarTemplate, alphaTyVars )
import Unique		( cmpUnique, Unique )
import UniTyFuns	( getTauType, getUniDataTyCon, pprTyCon,
		          cmpUniTypeMaybeList, specMaybeTysSuffix
			  IF_ATTACK_PRAGMAS(COMMA pprUniType COMMA splitType)
			)
import UniType		( UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Util
\end{code}

%************************************************************************
%*									*
\subsection[TyCon-basics]{@TyCon@ type and basic operations}
%*									*
%************************************************************************

\begin{code}
data TyCon
  = SynonymTyCon Unique{-TyConKey-} -- for fast comparison
		 FullName
		 Arity
		 [TyVarTemplate]-- Argument type variables
		 UniType	-- Right-hand side, mentioning these type vars
				-- Acts as a template for the expansion when
				-- the tycon is applied to some types.
		 Bool		-- True <=> expansion is visible to user;
				-- i.e., *not* abstract

  | DataTyCon	Unique{-TyConKey-}
		FullName
		Arity
		[TyVarTemplate]	-- see note below
		[Id]		-- its data constructors
		[Class]		-- classes which have derived instances
		Bool		-- True <=> data constructors are visible
				-- to user; i.e., *not* abstract

  | TupleTyCon	Arity		-- just a special case of DataTyCon

  | PrimTyCon			-- Primitive types; cannot be defined in Haskell
				-- Always unboxed; hence never represented by a closure
				-- Often represented by a bit-pattern for the thing
				-- itself (eg Int#), but sometimes by a pointer to
				-- a heap-allocated object (eg ArrInt#).
				-- The primitive types Arr# and StablePtr# have
				-- parameters (hence arity /= 0); but the rest don't.
		Unique{-TyConKey-}
		FullName
		Arity		-- Arity is *usually* 0.
		([PrimKind] -> PrimKind)
				-- Only arrays use the list in a non-trivial way.
				-- Length of that list must == arity.

				-- Used only for naming purposes in CLabels
  | SpecTyCon   TyCon		-- original data (or tuple) tycon
		[Maybe UniType] -- specialising types

#ifdef DPH
  | ProcessorTyCon Arity	-- special cased in same way as tuples

  | PodizedPodTyCon Int		-- podized dimension
		    TyCon	-- Thing the pod contains
#endif

type Arity = Int
\end{code}

{\em Note about the the @[TyVarTemplates]@ in @DataTyCon@ (and
@SynonymTyCon@, too?  ToDo):} they should be the type variables which
appeared in the original @data@ declaration.  They are there {\em for
documentation purposes only}.  In particular, when printing out
interface files, we want to use the same type-variable names as
appeared in the @data@ declaration for that type constructor.
However, they have no semantic significance.

We could also ensure that the data constructors in the @[Id]@ had the
{\em same} type vars in their @[TyVarTemplate]@ lists, so that we
don't have to do a translation on printout.
{\em End of note.}

Constructor functions, and simple access functions:
\begin{code}
mkSynonymTyCon	= SynonymTyCon
mkDataTyCon	= DataTyCon
mkTupleTyCon	= TupleTyCon
mkPrimTyCon	= PrimTyCon
mkSpecTyCon	= SpecTyCon

#ifdef DPH
mkProcessorTyCon= ProcessorTyCon
mkPodizedPodTyCon = PodizedPodTyCon
#endif {- Data Parallell Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[TyCon-extractors]{Extractors for @TyCon@}
%*									*
%************************************************************************

\begin{code}
getTyConArity (PrimTyCon    _ _ a _)	    = a
getTyConArity (SynonymTyCon _ _ a _ _ _)    = a
getTyConArity (DataTyCon    _ _ a _ _ _ _)  = a
getTyConArity (SpecTyCon    tc tys)	    = getTyConArity tc - length tys
getTyConArity (TupleTyCon   a)		    = a
#ifdef DPH
getTyConArity (ProcessorTyCon a)	    = a
getTyConArity (PodizedPodTyCon _ _)	    = panic "getTyConArity: pod"
#endif {- Data Parallel Haskell -}

getTyConKind (PrimTyCon _ _ _ kind_fn) kinds = kind_fn kinds
#ifdef DPH
getTyConKind (PodizedPodTyCon _ tc) kinds    = getTyConKind tc kinds
#endif {- Data Parallel Haskell -}
getTyConKind other kinds		     = PtrKind -- the "default"

getTyConDerivings (DataTyCon _ _ _ _ _ derivings _) = derivings
getTyConDerivings (SpecTyCon tc tys)	            = panic "getTyConDerivings:SpecTyCon"
#ifdef DPH
getTyConDerivings (PodizedPodTyCon _ _) = panic "getTyConDerivings:pod"
#endif {- Data Parallel Haskell -}
getTyConDerivings other				  = []
    -- NB: we do *not* report the PreludeCore types "derivings"...

getTyConDataCons (DataTyCon  _ _ _ _ data_cons _ _) = data_cons
getTyConDataCons (SpecTyCon  tc tys)	            = panic "getTyConDataCons:SpecTyCon"
getTyConDataCons (TupleTyCon a)			    = [mkTupleCon a]
#ifdef DPH
getTyConDataCons (ProcessorTyCon a)		= [mkProcessorCon a]
getTyConDataCons (PodizedPodTyCon _ _)		= panic "getTyConDataCons: pod"
#endif {- Data Parallel Haskell -}
getTyConDataCons other_tycon			= []
\end{code}
For the use of @getTyConDataCons@ in @MkUnfoldings@, the behaviour
above is right: return @[]@ if not an algebraic data type.  I am not
certain if that's right for all uses (perhaps should @panic@?) [WDP]

The following function returns (free) type-variables associated with a
given @TyCon@. As the information about these variables is distributed
over the @TyCon@'s constructors we take them from the type of any
of the constructors assuming that the variables in the remaining
type constructors are the same (responsible for keeping this assumption
valid is the typechecker).  ToDo: rm this old comment?
\begin{code}
getTyConTyVarTemplates (SynonymTyCon _ _ _ tvs _ _)   = tvs
getTyConTyVarTemplates (DataTyCon    _ _ _ tvs _ _ _) = tvs
getTyConTyVarTemplates (SpecTyCon    tc tys)	      = panic "getTyConTyVarTemplates:SpecTyCon"
getTyConTyVarTemplates (TupleTyCon a)		      = take a alphaTyVars
getTyConTyVarTemplates (PrimTyCon _ _ _ _)	      = [] -- ToDo: ???
#ifdef DPH
getTyConTyVarTemplates (ProcessorTyCon a)	    = take a alphaTyVars
getTyConTyVarTemplates (PodizedPodTyCon _ _)	    = panic "getTyConTyVarTem"
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
getTyConFamilySize :: TyCon -> Maybe Int -- return Nothing if we don't know

getTyConFamilySize (TupleTyCon	 _)	        = Just 1
getTyConFamilySize (SpecTyCon tc tys)	        = getTyConFamilySize tc
getTyConFamilySize (DataTyCon  _ _ _ _ dcs _ _)
  = let
	no_data_cons = length dcs
    in
    if no_data_cons == 0 then Nothing else Just no_data_cons

#ifdef DEBUG
  -- ToDo: if 0 then the answer is really "I don't know"; what then?
getTyConFamilySize tc@(PrimTyCon _ _ _ _)
  = pprPanic "getTyConFamilySize:prim:" (ppr PprDebug tc)
getTyConFamilySize (SynonymTyCon _ _ _ _ expand _)
  = pprTrace "getTyConFamilySize:Syn:" (ppr PprDebug expand) (
    let
	(tycon,_,data_cons) = getUniDataTyCon (getTauType expand)
	no_data_cons = length data_cons
    in
    if no_data_cons == 0 then Nothing else Just no_data_cons
    )
#endif
#ifdef DPH
getTyConFamilySize (ProcessorTyCon   _)	  = Just 1
getTyConFamilySize (PodizedPodTyCon _ _)  = panic "getTyConFamilySize: Pod"
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[TyCon-predicates]{Predicates on @TyCon@s}
%*									*
%************************************************************************

\begin{code}
-- True <=> Algebraic data type
isDataTyCon (DataTyCon _ _ _ _ _ _ _) = True
isDataTyCon (SpecTyCon tc tys)        = isDataTyCon tc
isDataTyCon (TupleTyCon _)	      = True
#ifdef DPH
isDataTyCon (ProcessorTyCon _)	      = True
isDataTyCon (PodizedPodTyCon _ tc)    = isDataTyCon tc
#endif {- Data Parallel Haskell -}
isDataTyCon other		      = False

-- True <=> Synonym
isSynTyCon (SynonymTyCon _ _ _ _ _ _) = True
isSynTyCon (SpecTyCon tc tys)         = panic "isSynTyCon: SpecTyCon"
#ifdef DPH
isSynTyCon (PodizedPodTyCon _ _)      = panic "isSynTyCon: Pod"
#endif {- Data Parallel Haskell -}
isSynTyCon other		      = False

isVisibleSynTyCon (SynonymTyCon _ _ _ _ _ visible) = visible
isVisibleSynTyCon other_tycon			   = panic "isVisibleSynTyCon"

isPrimTyCon (PrimTyCon _ _ _ _)	= True
isPrimTyCon (SpecTyCon tc tys)  = isPrimTyCon tc
#ifdef DPH
isPrimTyCon (PodizedPodTyCon _ tc) = isPrimTyCon tc
#endif {- Data Parallel Haskell -}
isPrimTyCon other		= False

-- At present there are no unboxed non-primitive types, so isBoxedTyCon is
-- just the negation of isPrimTyCon.
isBoxedTyCon (PrimTyCon _ _ _ _) = False
isBoxedTyCon (SpecTyCon tc tys)  = isBoxedTyCon tc
#ifdef DPH
isBoxedTyCon (PodizedPodTyCon _ tc) = isBoxedTyCon tc
#endif {- Data Parallel Haskell -}
isBoxedTyCon other		= True

\end{code}

The @maybeCharLikeTyCon@ predicate tests for a tycon with no type
variables, and one constructor which has one argument of type
@CharPrim@.  Similarly @maybeIntLikeTyCon@, etc.

ToDo:SpecTyCon Do we want to CharLike etc for SpecTyCons ???

\begin{code}
maybeCharLikeTyCon (DataTyCon _ _ _ [] [con] [] _) = maybe_foo_like con charPrimTy
#ifdef DPH
maybeCharLikeTyCon (PodizedPodTyCon _ _) = panic "maybeCharLikeTyCon: Pod"
#endif {- Data Parallel Haskell -}
maybeCharLikeTyCon other = Nothing

maybeIntLikeTyCon (DataTyCon _ _ _ [] [con] [] _) = maybe_foo_like con intPrimTy
#ifdef DPH
maybeIntLikeTyCon (PodizedPodTyCon _ _) = panic "maybeIntLikeTyCon: Pod"
#endif {- Data Parallel Haskell -}
maybeIntLikeTyCon other = Nothing

maybeFloatLikeTyCon (DataTyCon _ _ _ [] [con] [] _) = maybe_foo_like con floatPrimTy
#ifdef DPH
maybeFloatLikeTyCon (PodizedPodTyCon _ _) = panic "maybeFloatLikeTyCon: Pod"
#endif {- Data Parallel Haskell -}
maybeFloatLikeTyCon other = Nothing

maybeDoubleLikeTyCon (DataTyCon _ _ _ [] [con] [] _) = maybe_foo_like con doublePrimTy
#ifdef DPH
maybeDoubleLikeTyCon (PodizedPodTyCon _ _) = panic "maybeDoubleLikeTyCon: Pod"
#endif {- Data Parallel Haskell -}
maybeDoubleLikeTyCon other = Nothing

maybe_foo_like con prim_type_to_match
  = case (getDataConSig con) of
      ([], [], [should_be_prim], _)
	| should_be_prim == prim_type_to_match	-> Just con
      other					-> Nothing

#ifdef DPH
isProcessorTyCon :: TyCon -> Bool
isProcessorTyCon (ProcessorTyCon _)	= True
isProcessorTyCon other			= False

isPodizedPodTyCon :: TyCon -> Bool
isPodizedPodTyCon (PodizedPodTyCon _ _) = True
isPodizedPodTyCon other			= False

getPodizedPodDimension::TyCon -> Int
getPodizedPodDimension (PodizedPodTyCon d _) = d
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
isEnumerationTyCon :: TyCon -> Bool

isEnumerationTyCon (TupleTyCon arity)
  = arity == 0
isEnumerationTyCon (DataTyCon _ _ _ _ data_cons _ _)
  = not (null data_cons) && all is_nullary data_cons
  where
    is_nullary con = case (getDataConSig con) of { (_,_, arg_tys, _) ->
		     null arg_tys }
#ifdef DEBUG
-- isEnumerationTyCon (SpecTyCon tc tys) -- ToDo:SpecTyCon
isEnumerationTyCon other = pprPanic "isEnumerationTyCon: " (ppr PprShowAll other)
#endif

-- this one is more of a *heuristic*
{- UNUSED:
isEnumerationTyConMostly :: TyCon -> Bool

isEnumerationTyConMostly (TupleTyCon arity) = arity == 0

isEnumerationTyConMostly tycon@(DataTyCon _ _ _ _ data_cons _ _)
  =  isEnumerationTyCon tycon
  || four_or_more data_cons 0
  where
    four_or_more :: [Id] -> Int -> Bool

    four_or_more []	acc = if acc >= 4 then True else False
    four_or_more (c:cs) acc
      = case (getDataConSig c) of { (_,_, arg_tys, _) ->
	four_or_more cs (if (null arg_tys) then acc+1 else acc)
	}
-- isEnumerationTyConMostly (SpecTyCon tc tys) -- ToDo:SpecTyCon
-}


maybeSingleConstructorTyCon :: TyCon -> Maybe Id
maybeSingleConstructorTyCon (TupleTyCon arity)	         = Just (mkTupleCon arity)
maybeSingleConstructorTyCon (DataTyCon _ _ _ _ [c] _ _)  = Just c
maybeSingleConstructorTyCon (DataTyCon _ _ _ _ _   _ _)  = Nothing
maybeSingleConstructorTyCon (PrimTyCon _ _ _ _)	         = Nothing
maybeSingleConstructorTyCon tycon@(SpecTyCon tc tys)     = pprPanic "maybeSingleConstructorTyCon:SpecTyCon:" (ppr PprDebug tycon)
							   -- requires DataCons of TyCon
\end{code}

@derivedFor@ reports if we have an {\em obviously}-derived instance
for the given class/tycon.  Of course, you might be deriving something
because it a superclass of some other obviously-derived class---this
function doesn't deal with that.

ToDo:SpecTyCon Do we want derivedFor etc for SpecTyCons ???

\begin{code}
derivedFor		:: Class    -> TyCon -> Bool

clas `derivedFor` (DataTyCon _ _ _ _ _ derivs _) = clas `is_elem` derivs
clas `derivedFor` something_weird		 = False

x `is_elem` y = isIn "X_derivedFor" x y

{- UNUSED:
preludeClassDerivedFor	:: Unique{-ClassKey-} -> TyCon -> Bool

preludeClassDerivedFor key (DataTyCon _ _ _ _ _ derivs _)
  = key `is_elem` (map getClassKey derivs)
preludeClassDerivedFor key something_weird = False
-}
\end{code}

\begin{code}
isTupleTyCon (TupleTyCon arity)	= arity >= 2    -- treat "0-tuple" specially
isTupleTyCon (SpecTyCon tc tys) = isTupleTyCon tc
isTupleTyCon other		= False
\end{code}

@isLocalSpecTyCon@ determines if a tycon has specialisations created
locally: locally defined tycons and any tycons from the prelude.
But *not* if we're compiling the prelude itself...

@isLocalGenTyCon@ determines if constructor code for a tycon is
generated locally: locally defined tycons and big tuple tycons.

\begin{code}
isLocalSpecTyCon :: Bool -> TyCon -> Bool
isLocalGenTyCon  :: TyCon -> Bool

isLocalSpecTyCon compiling_prelude tc
  = isLocallyDefined tc -- || (fromPreludeCore tc && not compiling_prelude)
			-- Not for now ... need to be local
			-- This will cause problem with splitting

isLocalGenTyCon tc
  = isLocallyDefined tc -- || isBigTupleTyCon tc
			-- Not for now ... need to be local
			-- This will cause problem with splitting

isBigTupleTyCon (TupleTyCon arity) = arity > 32
					-- Tuple0 to Tuple32 declared in prelude
					-- HEY!  Nice magic constant! WDP 95/06
isBigTupleTyCon (SpecTyCon tc _)   = isBigTupleTyCon tc
isBigTupleTyCon _		   = False
\end{code}

%************************************************************************
%*									*
\subsection[TyCon-instances]{Instance declarations for @TyCon@}
%*									*
%************************************************************************

@TyCon@s are compared by comparing their @Unique@s.

The strictness analyser needs @Ord@. It is a lexicographic order with
the property @(a<=b) || (b<=a)@.

\begin{code}
cmpTyCon (SynonymTyCon k1 _ _ _ _ _) (SynonymTyCon k2 _ _ _ _ _)= cmpUnique k1 k2
cmpTyCon (DataTyCon  k1 _ _ _ _ _ _) (DataTyCon	k2 _ _ _ _ _ _) = cmpUnique k1 k2
cmpTyCon (TupleTyCon   a1)	     (TupleTyCon	a2)	= cmp_i a1 a2
cmpTyCon (PrimTyCon k1 _ _ _)	     (PrimTyCon	k2 _ _ _)       = cmpUnique k1 k2
cmpTyCon (SpecTyCon tc1 mtys1)	     (SpecTyCon tc2 mtys2)
  = case cmpTyCon tc1 tc2 of { EQ_ -> cmpUniTypeMaybeList mtys1 mtys2; other -> other }
#ifdef DPH
cmpTyCon (ProcessorTyCon  a1)	     (ProcessorTyCon  a2)   	= cmp_i a1 a2
cmpTyCon (PodizedPodTyCon d1 tc1)    (PodizedPodTyCon d2 tc2)
  = case cmp_i d1 d2 of { EQ_ -> cmpTyCon tc1 tc2; other -> other }
#endif {- Data Parallel Haskell -}

    -- now we *know* the tags are different, so...
cmpTyCon other_1			  other_2
  = let
	tag1 = tag_TyCon other_1
	tag2 = tag_TyCon other_2
    in
    if tag1 _LT_ tag2 then LT_ else GT_
  where
    tag_TyCon (SynonymTyCon _ _ _ _ _ _)  = (ILIT(1) :: FAST_INT)
    tag_TyCon (DataTyCon    _ _ _ _ _ _ _)= ILIT(2)
    tag_TyCon (TupleTyCon   _)            = ILIT(3)
    tag_TyCon (PrimTyCon    _ _ _ _)      = ILIT(4)
    tag_TyCon (SpecTyCon    _ _) 	  = ILIT(5)
#ifdef DPH
    tag_TyCon (ProcessorTyCon   _)        = ILIT(6)
    tag_TyCon (PodizedPodTyCon _ _)       = ILIT(7)
#endif {- Data Parallel Haskell -}

cmp_i :: Int -> Int -> TAG_
cmp_i a1 a2
  = if a1 == a2 then EQ_ else if a1 < a2 then LT_ else GT_
\end{code}

\begin{code}
eqTyCon :: TyCon -> TyCon -> Bool

eqTyCon a b = case cmpTyCon a b of { EQ_ -> True;  _   -> False }

instance Eq TyCon where
    a == b = case cmpTyCon a b of { EQ_ -> True;   _ -> False }
    a /= b = case cmpTyCon a b of { EQ_ -> False;  _ -> True  }

instance Ord TyCon where
    a <= b = case cmpTyCon a b of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <	 b = case cmpTyCon a b of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case cmpTyCon a b of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case cmpTyCon a b of { LT_ -> False; EQ_ -> False; GT__ -> True  }
#ifdef __GLASGOW_HASKELL__
    _tagCmp a b = case cmpTyCon a b of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }
#endif
\end{code}

\begin{code}
instance NamedThing TyCon where
    getExportFlag	(TupleTyCon _)  = NotExported
#ifdef DPH
    getExportFlag    (ProcessorTyCon _)     = NotExported
    getExportFlag    (PodizedPodTyCon _ tc) = getExportFlag tc
#endif {- Data Parallel Haskell -}
    getExportFlag	other           = getExportFlag (get_name other)

    isLocallyDefined	(TupleTyCon _)  = False
#ifdef DPH
    isLocallyDefined (ProcessorTyCon _)     = False
    isLocallyDefined (PodizedPodTyCon _ tc) = isLocallyDefined tc
#endif {- Data Parallel Haskell -}
    isLocallyDefined	other           = isLocallyDefined (get_name other)

    getOrigName (TupleTyCon a)		= (pRELUDE_BUILTIN, _PK_ ("Tuple" ++ (show a)))
    getOrigName (SpecTyCon tc tys)	= let (m,n) = getOrigName tc in
					  (m, n _APPEND_ specMaybeTysSuffix tys)
#ifdef DPH
    getOrigName	    (ProcessorTyCon a)     = ("PreludeBuiltin", "Processor" ++ (show a))
    getOrigName     (PodizedPodTyCon d tc) = let (m,n) = getOrigName tc	in
					     (m,n++"Pod"++show d)
#endif {- Data Parallel Haskell -}
    getOrigName		other           = getOrigName (get_name other)

    getOccurrenceName	(TupleTyCon a)	     = _PK_ ("Tuple" ++ (show a))
    getOccurrenceName (SpecTyCon tc tys)     = getOccurrenceName tc _APPEND_ specMaybeTysSuffix tys
#ifdef DPH
    getOccurrenceName (ProcessorTyCon a)     = "Processor" ++ (show a)
    getOccurrenceName (PodizedPodTyCon d tc) = getOccurrenceName tc ++
					       "Pod" ++ show d
#endif {- Data Parallel Haskell -}
    getOccurrenceName	other           = getOccurrenceName (get_name other)

    getInformingModules	(TupleTyCon a)  = panic "getInformingModule:TupleTyCon"
#ifdef DPH
    getInformingModules (ProcessorTyCon a)     = "Processor" ++ (show a)
    getInformingModules (PodizedPodTyCon d tc) = getInformingModule tc ++
					       "Pod" ++ show d
#endif {- Data Parallel Haskell -}
    getInformingModules	other           = getInformingModules (get_name other)

    getSrcLoc		(TupleTyCon _)  = mkBuiltinSrcLoc
#ifdef DPH
    getSrcLoc	    (ProcessorTyCon _)     = mkBuiltinSrcLoc
    getSrcLoc	    (PodizedPodTyCon _ tc) = getSrcLoc tc
#endif {- Data Parallel Haskell -}
    getSrcLoc		other           = getSrcLoc (get_name other)

    getTheUnique	other           = panic "NamedThing.TyCon.getTheUnique"

    fromPreludeCore	(TupleTyCon a)  = True
#ifdef DPH
    fromPreludeCore (ProcessorTyCon a)     = True
    fromPreludeCore (PodizedPodTyCon _ tc) = fromPreludeCore tc
#endif {- Data Parallel Haskell -}
    fromPreludeCore	other           = fromPreludeCore (get_name other)

    hasType				= panic "NamedThing.TyCon.hasType"
    getType				= panic "NamedThing.TyCon.getType"
\end{code}

Emphatically un-exported:
\begin{code}
get_name (SynonymTyCon _ n _ _ _ _)	= n
get_name (DataTyCon    _ n _ _ _ _ _)	= n
get_name (PrimTyCon    _ n _ _)		= n
get_name (SpecTyCon    tc _)		= get_name tc
\end{code}

And the usual output stuff:
\begin{code}
instance Outputable TyCon where
    ppr sty tycon = pprTyCon sty tycon [{-No Specialisations-}]
\end{code}
