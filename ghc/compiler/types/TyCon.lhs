
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TyCon]{The @TyCon@ datatype}

\begin{code}
module TyCon(
	TyCon,

	Arity, NewOrData(..),

	isFunTyCon, isPrimTyCon, isBoxedTyCon, isProductTyCon,
	isAlgTyCon, isDataTyCon, isSynTyCon, isNewTyCon, 
	isEnumerationTyCon, isTupleTyCon, 

	mkDataTyCon,
	mkFunTyCon,
	mkPrimTyCon,
	mkSpecTyCon,
	mkTupleTyCon,

	mkSynTyCon,

	tyConKind,
	tyConUnique,
	tyConTyVars,
	tyConDataCons,
	tyConFamilySize,
	tyConDerivings,
	tyConTheta,
	tyConPrimRep,
	tyConArity,
	tyConClass_maybe,
	getSynTyConDefn,

        maybeTyConSingleCon
) where

#include "HsVersions.h"

import {-# SOURCE #-} Type  ( Type )
import {-# SOURCE #-} Class ( Class )
import {-# SOURCE #-} Id    ( Id, isNullaryDataCon, idType )
import {-# SOURCE #-} TysWiredIn ( tupleCon )


import BasicTypes	( Arity, NewOrData(..), RecFlag(..) )
import TyVar		( GenTyVar, alphaTyVars, alphaTyVar, betaTyVar, TyVar )
import Kind		( Kind, mkBoxedTypeKind, mkTypeKind, mkUnboxedTypeKind,
			  mkArrowKind, resultKind, argKind
			)
import Maybes
import Name		( Name, nameUnique, mkWiredInTyConName, NamedThing(getName) )
import Unique		( Unique, funTyConKey, Uniquable(..) )
import PrimRep		( PrimRep(..), isFollowableRep )
import PrelMods		( gHC__, pREL_TUP, pREL_BASE )
import Lex		( mkTupNameStr )
import SrcLoc		( SrcLoc, mkBuiltinSrcLoc )
import Util		( nOfThem, isIn )
import Outputable
\end{code}

\begin{code}
data TyCon
  = FunTyCon		-- Kind = Type -> Type -> Type

  | DataTyCon	Unique
		Name
		Kind
		[TyVar]
		[(Class,[Type])]	-- Its context
		[Id{-DataCon-}]	-- Its data constructors, with fully polymorphic types
				-- 	This list can be empty, when we import a data type abstractly,
				-- 	either (a) the interface is hand-written and doesn't give
				--		   the constructors, or
				--	       (b) in a quest for fast compilation we don't import 
				--		   the constructors
		[Class]		-- Classes which have derived instances
		(Maybe Class)	-- Nothing for ordinary types; Just c for the type constructor
				-- for dictionaries of class c.
		NewOrData
		RecFlag		-- Tells whether the data type is part of 
				-- a mutually-recursive group or not

  | TupleTyCon	Unique		-- cached
		Name		-- again, we could do without this, but
				-- it makes life somewhat easier
		Arity	-- just a special case of DataTyCon
			-- Kind = BoxedTypeKind
			--      -> ... (n times) ...
			--	-> BoxedTypeKind
			--      -> BoxedTypeKind

  | PrimTyCon		-- Primitive types; cannot be defined in Haskell
	Unique		-- Always unpointed; hence never represented by a closure
	Name		-- Often represented by a bit-pattern for the thing
	Kind		-- itself (eg Int#), but sometimes by a pointer to
	Arity		-- the thing.
	PrimRep

  | SpecTyCon		-- A specialised TyCon; eg (Arr# Int#), or (List Int#)
	TyCon
	[Maybe Type]	-- Specialising types

	-- 	OLD STUFF ABOUT Array types.  Use SpecTyCon instead
	-- ([PrimRep] -> PrimRep) -- a heap-allocated object (eg ArrInt#).
	-- The primitive types Arr# and StablePtr# have
	-- parameters (hence arity /= 0); but the rest don't.
	-- Only arrays use the list in a non-trivial way.
	-- Length of that list must == arity.

  | SynTyCon
	Unique
	Name
	Kind
	Arity
	[TyVar]		-- Argument type variables
	Type		-- Right-hand side, mentioning these type vars.
			-- Acts as a template for the expansion when
			-- the tycon is applied to some types.
\end{code}

\begin{code}
mkFunTyCon     = FunTyCon
mkFunTyConName = mkWiredInTyConName funTyConKey gHC__ SLIT("->") FunTyCon

mkSpecTyCon  = SpecTyCon
mkTupleTyCon = TupleTyCon

mkDataTyCon name = DataTyCon (nameUnique name) name

mkPrimTyCon name arity rep 
  = PrimTyCon (nameUnique name) name (mk_kind arity) arity rep
  where
    mk_kind 0 | isFollowableRep rep = mkBoxedTypeKind	-- Represented by a GC-ish ptr
	      | otherwise	    = mkUnboxedTypeKind	-- Represented by a non-ptr
    mk_kind n = mkTypeKind `mkArrowKind` mk_kind (n-1)

mkSynTyCon  name = SynTyCon  (nameUnique name) name

isFunTyCon FunTyCon = True
isFunTyCon _ = False

isPrimTyCon (PrimTyCon _ _ _ _ _) = True
isPrimTyCon _ = False

-- At present there are no unboxed non-primitive types, so
-- isBoxedTyCon is just the negation of isPrimTyCon.
isBoxedTyCon = not . isPrimTyCon

-- isAlgTyCon returns True for both @data@ and @newtype@
isAlgTyCon (DataTyCon _ _ _ _ _ _ _ _ _ _) = True
isAlgTyCon (TupleTyCon _ _ _)	           = True
isAlgTyCon other 		           = False

-- isDataTyCon returns False for @newtype@.
isDataTyCon (DataTyCon _ _ _ _ _ _ _ _ DataType _) = True
isDataTyCon (TupleTyCon _ _ _) 		           = True
isDataTyCon other 			           = False

isNewTyCon (DataTyCon _ _ _ _ _ _ _ _ NewType _) = True 
isNewTyCon other			         = False

-- A "product" tycon is non-recursive and has one constructor,
-- whether DataType or NewType
isProductTyCon (TupleTyCon _ _ _)			    = True
isProductTyCon (DataTyCon _ _ _ _ _ [c] _ _ _ NonRecursive) = True
isProductTyCon other					    = False

isSynTyCon (SynTyCon _ _ _ _ _ _) = True
isSynTyCon _			  = False

isEnumerationTyCon (TupleTyCon _ _ arity)
  = arity == 0
isEnumerationTyCon (DataTyCon _ _ _ _ _ data_cons _ _ DataType _)
  = not (null data_cons) && all isNullaryDataCon data_cons
isEnumerationTyCon other = False

isTupleTyCon (TupleTyCon _ _ arity) = arity >= 2    -- treat "0-tuple" specially
isTupleTyCon (SpecTyCon tc tys)     = isTupleTyCon tc
isTupleTyCon other		    = False
\end{code}

\begin{code}
-- Special cases to avoid reconstructing lots of kinds
kind1 = mkBoxedTypeKind `mkArrowKind` mkBoxedTypeKind
kind2 = mkBoxedTypeKind `mkArrowKind` kind1

tyConKind :: TyCon -> Kind
tyConKind FunTyCon 			     = kind2
tyConKind (DataTyCon _ _ kind _ _ _ _ _ _ _) = kind
tyConKind (PrimTyCon _ _ kind _ _)	     = kind
tyConKind (SynTyCon _ _ k _ _ _)	     = k

tyConKind (TupleTyCon _ _ n)
  = mkArrow n
   where
    mkArrow 0 = mkBoxedTypeKind
    mkArrow 1 = kind1
    mkArrow 2 = kind2
    mkArrow n = mkBoxedTypeKind `mkArrowKind` mkArrow (n-1)

tyConKind (SpecTyCon tc tys)
  = spec (tyConKind tc) tys
   where
    spec kind []	      = kind
    spec kind (Just _  : tys) = spec (resultKind kind) tys
    spec kind (Nothing : tys) =
      argKind kind `mkArrowKind` spec (resultKind kind) tys
\end{code}

\begin{code}
tyConUnique :: TyCon -> Unique
tyConUnique FunTyCon			       = funTyConKey
tyConUnique (DataTyCon uniq _ _ _ _ _ _ _ _ _) = uniq
tyConUnique (TupleTyCon uniq _ _)	       = uniq
tyConUnique (PrimTyCon uniq _ _ _ _) 	       = uniq
tyConUnique (SynTyCon uniq _ _ _ _ _)          = uniq
tyConUnique (SpecTyCon _ _ )		       = panic "tyConUnique:SpecTyCon"

tyConArity :: TyCon -> Arity 
tyConArity FunTyCon			        = 2
tyConArity (DataTyCon _ _ _ tyvars _ _ _ _ _ _) = length tyvars
tyConArity (TupleTyCon _ _ arity)	      	= arity
tyConArity (PrimTyCon _ _ _ arity _)	      	= arity 
tyConArity (SynTyCon _ _ _ arity _ _)	      	= arity
tyConArity (SpecTyCon _ _ )		      	= panic "tyConArity:SpecTyCon"
\end{code}

\begin{code}
tyConTyVars :: TyCon -> [TyVar]
tyConTyVars FunTyCon			      = [alphaTyVar,betaTyVar]
tyConTyVars (DataTyCon _ _ _ tvs _ _ _ _ _ _) = tvs
tyConTyVars (TupleTyCon _ _ arity)	      = take arity alphaTyVars
tyConTyVars (SynTyCon _ _ _ _ tvs _)          = tvs
#ifdef DEBUG
tyConTyVars (PrimTyCon _ _ _ _ _)     	  = panic "tyConTyVars:PrimTyCon"
tyConTyVars (SpecTyCon _ _ ) 	     	  = panic "tyConTyVars:SpecTyCon"
#endif
\end{code}

\begin{code}
tyConDataCons :: TyCon -> [Id]
tyConFamilySize  :: TyCon -> Int

tyConDataCons (DataTyCon _ _ _ _ _ data_cons _ _ _ _) = data_cons
tyConDataCons (TupleTyCon _ _ a)		      = [tupleCon a]
tyConDataCons other				      = []
	-- You may think this last equation should fail,
	-- but it's quite convenient to return no constructors for
	-- a synonym; see for example the call in TcTyClsDecls.

tyConFamilySize (DataTyCon _ _ _ _ _ data_cons _ _ _ _) = length data_cons
tyConFamilySize (TupleTyCon _ _ _)		        = 1
#ifdef DEBUG
--tyConFamilySize other = pprPanic "tyConFamilySize:" (pprTyCon other)
#endif

tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon _ __  _ rep) = rep
tyConPrimRep _			     = PtrRep
\end{code}

\begin{code}
tyConDerivings :: TyCon -> [Class]
tyConDerivings (DataTyCon _ _ _ _ _ _ derivs _ _ _) = derivs
tyConDerivings other				    = []
\end{code}

\begin{code}
tyConTheta :: TyCon -> [(Class, [Type])]
tyConTheta (DataTyCon _ _ _ _ theta _ _ _ _ _) = theta
tyConTheta (TupleTyCon _ _ _)		       = []
-- should ask about anything else
\end{code}

\begin{code}
getSynTyConDefn :: TyCon -> ([TyVar], Type)
getSynTyConDefn (SynTyCon _ _ _ _ tyvars ty) = (tyvars,ty)
\end{code}

\begin{code}
maybeTyConSingleCon :: TyCon -> Maybe Id

maybeTyConSingleCon (TupleTyCon _ _ arity)            = Just (tupleCon arity)
maybeTyConSingleCon (DataTyCon _ _ _ _ _ [c] _ _ _ _) = Just c
maybeTyConSingleCon (DataTyCon _ _ _ _ _ _   _ _ _ _) = Nothing
maybeTyConSingleCon (PrimTyCon _ _ _ _ _)             = Nothing
maybeTyConSingleCon (SpecTyCon tc tys)                = panic "maybeTyConSingleCon:SpecTyCon"
						  -- requires DataCons of TyCon
\end{code}

\begin{code}
tyConClass_maybe :: TyCon -> Maybe Class
tyConClass_maybe (DataTyCon _ _ _ _ _ _ _ maybe_cls _ _) = maybe_cls
tyConClass_maybe other_tycon			         = Nothing
\end{code}

@derivedFor@ reports if we have an {\em obviously}-derived instance
for the given class/tycon.  Of course, you might be deriving something
because it a superclass of some other obviously-derived class --- this
function doesn't deal with that.

ToDo: what about derivings for specialised tycons !!!

%************************************************************************
%*									*
\subsection[TyCon-instances]{Instance declarations for @TyCon@}
%*									*
%************************************************************************

@TyCon@s are compared by comparing their @Unique@s.

The strictness analyser needs @Ord@. It is a lexicographic order with
the property @(a<=b) || (b<=a)@.

\begin{code}
instance Eq TyCon where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord TyCon where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = uniqueOf a `compare` uniqueOf b

instance Uniquable TyCon where
    uniqueOf tc = tyConUnique tc
\end{code}

\begin{code}
instance NamedThing TyCon where
    getName (DataTyCon _ n _ _ _ _ _ _ _ _) = n
    getName (PrimTyCon _ n _ _ _)	    = n
    getName (SpecTyCon tc _)		    = getName tc
    getName (SynTyCon _ n _ _ _ _)	    = n
    getName FunTyCon			    = mkFunTyConName
    getName (TupleTyCon _ n _)		    = n

{- LATER:
    getName (SpecTyCon tc tys) = let (OrigName m n) = origName "????" tc in
       			     (m, n _APPEND_ specMaybeTysSuffix tys)
    getName	other_tc           = moduleNamePair (expectJust "tycon1" (getName other_tc))
    getName other			     = Nothing
-}
\end{code}
