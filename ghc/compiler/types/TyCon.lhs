%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TyCon]{The @TyCon@ datatype}

\begin{code}
#include "HsVersions.h"

module TyCon(
	TyCon(..), 	-- NB: some pals need to see representation

	SYN_IE(Arity), NewOrData(..),

	isFunTyCon, isPrimTyCon, isBoxedTyCon,
	isDataTyCon, isSynTyCon, isNewTyCon, maybeNewTyCon,

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
	synTyConArity,
	getSynTyConDefn,

        maybeTyConSingleCon,
	isEnumerationTyCon,
	derivedFor
) where

CHK_Ubiq()	-- debugging consistency check

IMPORT_DELOOPER(TyLoop)	( SYN_IE(Type), GenType,
			  SYN_IE(Class), GenClass,
			  SYN_IE(Id), GenId,
			  splitSigmaTy, splitFunTy,
			  mkTupleCon, isNullaryDataCon, idType
			  --LATER: specMaybeTysSuffix
			)

import TyVar		( GenTyVar, alphaTyVars, alphaTyVar, betaTyVar, SYN_IE(TyVar) )
import Usage		( GenUsage, SYN_IE(Usage) )
import Kind		( Kind, mkBoxedTypeKind, mkArrowKind, resultKind, argKind )

import Maybes
import Name		( Name, RdrName(..), appendRdr, nameUnique,
			  mkTupleTyConName, mkFunTyConName
			)
import Unique		( Unique, funTyConKey, mkTupleTyConUnique )
import Pretty		( SYN_IE(Pretty), PrettyRep )
import PrimRep		( PrimRep(..) )
import SrcLoc		( SrcLoc, mkBuiltinSrcLoc )
import Util		( nOfThem, isIn, Ord3(..), panic, panic#, assertPanic, pprPanic{-ToDo:rm-} )
import {-hide me-}
	PprType (pprTyCon)
import {-hide me-}
	PprStyle--ToDo:rm
\end{code}

\begin{code}
type Arity = Int

data TyCon
  = FunTyCon		-- Kind = Type -> Type -> Type

  | DataTyCon	Unique{-TyConKey-}
		Name
		Kind
		[TyVar]
		[(Class,Type)]	-- Its context
		[Id]		-- Its data constructors, with fully polymorphic types
		[Class]		-- Classes which have derived instances
		NewOrData

  | TupleTyCon	Unique		-- cached
		Name		-- again, we could do without this, but
				-- it makes life somewhat easier
		Arity	-- just a special case of DataTyCon
			-- Kind = BoxedTypeKind
			--      -> ... (n times) ...
			--	-> BoxedTypeKind
			--      -> BoxedTypeKind

  | PrimTyCon		-- Primitive types; cannot be defined in Haskell
	Unique		-- Always unboxed; hence never represented by a closure
	Name		-- Often represented by a bit-pattern for the thing
	Kind		-- itself (eg Int#), but sometimes by a pointer to
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

data NewOrData
  = NewType	    -- "newtype Blah ..."
  | DataType	    -- "data Blah ..."
\end{code}

\begin{code}
mkFunTyCon   = FunTyCon
mkSpecTyCon  = SpecTyCon

mkTupleTyCon arity
  = TupleTyCon u n arity 
  where
    n = mkTupleTyConName arity
    u = uniqueOf n

mkDataTyCon name = DataTyCon (nameUnique name) name
mkPrimTyCon name = PrimTyCon (nameUnique name) name
mkSynTyCon  name = SynTyCon  (nameUnique name) name

isFunTyCon FunTyCon = True
isFunTyCon _ = False

isPrimTyCon (PrimTyCon _ _ _ _) = True
isPrimTyCon _ = False

-- At present there are no unboxed non-primitive types, so
-- isBoxedTyCon is just the negation of isPrimTyCon.
isBoxedTyCon = not . isPrimTyCon

-- isDataTyCon returns False for @newtype@.
-- Not sure about this decision yet.
isDataTyCon (DataTyCon _ _ _ _ _ _ _ DataType) = True
isDataTyCon (TupleTyCon _ _ _)		       = True
isDataTyCon other 			       = False

maybeNewTyCon :: TyCon -> Maybe ([TyVar], Type) 	-- Returns representation type info
maybeNewTyCon (DataTyCon _ _ _ _ _ (con:null_cons) _ NewType) 
  = ASSERT( null null_cons && null null_tys)
    Just (tyvars, rep_ty)
  where
    (tyvars, theta, tau)      = splitSigmaTy (idType con)
    (rep_ty:null_tys, res_ty) = splitFunTy tau

maybeNewTyCon other = Nothing

isNewTyCon (DataTyCon _ _ _ _ _ _ _ NewType) = True 
isNewTyCon other			     = False

isSynTyCon (SynTyCon _ _ _ _ _ _) = True
isSynTyCon _			  = False
\end{code}

\begin{code}
-- Special cases to avoid reconstructing lots of kinds
kind1 = mkBoxedTypeKind `mkArrowKind` mkBoxedTypeKind
kind2 = mkBoxedTypeKind `mkArrowKind` kind1

tyConKind :: TyCon -> Kind
tyConKind FunTyCon 			 = kind2
tyConKind (DataTyCon _ _ kind _ _ _ _ _) = kind
tyConKind (PrimTyCon _ _ kind _)	 = kind
tyConKind (SynTyCon _ _ k _ _ _)	 = k

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
tyConUnique FunTyCon			   = funTyConKey
tyConUnique (DataTyCon uniq _ _ _ _ _ _ _) = uniq
tyConUnique (TupleTyCon uniq _ _)	   = uniq
tyConUnique (PrimTyCon uniq _ _ _) 	   = uniq
tyConUnique (SynTyCon uniq _ _ _ _ _)      = uniq
tyConUnique (SpecTyCon _ _ )		   = panic "tyConUnique:SpecTyCon"

synTyConArity :: TyCon -> Maybe Arity -- Nothing <=> not a syn tycon
synTyConArity (SynTyCon _ _ _ arity _ _) = Just arity
synTyConArity _				 = Nothing
\end{code}

\begin{code}
tyConTyVars :: TyCon -> [TyVar]
tyConTyVars FunTyCon			  = [alphaTyVar,betaTyVar]
tyConTyVars (DataTyCon _ _ _ tvs _ _ _ _) = tvs
tyConTyVars (TupleTyCon _ _ arity)	  = take arity alphaTyVars
tyConTyVars (SynTyCon _ _ _ _ tvs _)      = tvs
#ifdef DEBUG
tyConTyVars (PrimTyCon _ _ _ _)	     	  = panic "tyConTyVars:PrimTyCon"
tyConTyVars (SpecTyCon _ _ ) 	     	  = panic "tyConTyVars:SpecTyCon"
#endif
\end{code}

\begin{code}
tyConDataCons :: TyCon -> [Id]
tyConFamilySize  :: TyCon -> Int

tyConDataCons (DataTyCon _ _ _ _ _ data_cons _ _) = data_cons
tyConDataCons (TupleTyCon _ _ a)		  = [mkTupleCon a]
tyConDataCons other				  = []
	-- You may think this last equation should fail,
	-- but it's quite convenient to return no constructors for
	-- a synonym; see for example the call in TcTyClsDecls.

tyConFamilySize (DataTyCon _ _ _ _ _ data_cons _ _) = length data_cons
tyConFamilySize (TupleTyCon _ _ _)		    = 1
#ifdef DEBUG
tyConFamilySize other = pprPanic "tyConFamilySize:" (pprTyCon PprDebug other)
#endif

tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon _ _ _ rep) = rep
tyConPrimRep _			   = PtrRep
\end{code}

\begin{code}
tyConDerivings :: TyCon -> [Class]
tyConDerivings (DataTyCon _ _ _ _ _ _ derivs _) = derivs
tyConDerivings other				= []
\end{code}

\begin{code}
tyConTheta :: TyCon -> [(Class,Type)]
tyConTheta (DataTyCon _ _ _ _ theta _ _ _) = theta
tyConTheta (TupleTyCon _ _ _)		   = []
-- should ask about anything else
\end{code}

\begin{code}
getSynTyConDefn :: TyCon -> ([TyVar], Type)
getSynTyConDefn (SynTyCon _ _ _ _ tyvars ty) = (tyvars,ty)
\end{code}

\begin{code}
maybeTyConSingleCon :: TyCon -> Maybe Id

maybeTyConSingleCon (TupleTyCon _ _ arity)        = Just (mkTupleCon arity)
maybeTyConSingleCon (DataTyCon _ _ _ _ _ [c] _ _) = Just c
maybeTyConSingleCon (DataTyCon _ _ _ _ _ _   _ _) = Nothing
maybeTyConSingleCon (PrimTyCon _ _ _ _)	          = Nothing
maybeTyConSingleCon (SpecTyCon tc tys)            = panic "maybeTyConSingleCon:SpecTyCon"
						  -- requires DataCons of TyCon

isEnumerationTyCon (TupleTyCon _ _ arity)
  = arity == 0
isEnumerationTyCon (DataTyCon _ _ _ _ _ data_cons _ _)
  = not (null data_cons) && all isNullaryDataCon data_cons
\end{code}

@derivedFor@ reports if we have an {\em obviously}-derived instance
for the given class/tycon.  Of course, you might be deriving something
because it a superclass of some other obviously-derived class --- this
function doesn't deal with that.

ToDo: what about derivings for specialised tycons !!!

\begin{code}
derivedFor :: Class -> TyCon -> Bool
derivedFor clas (DataTyCon _ _ _ _ _ _ derivs _) = isIn "derivedFor" clas derivs
derivedFor clas something_weird		         = False
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
instance Ord3 TyCon where
  cmp tc1 tc2 = uniqueOf tc1 `cmp` uniqueOf tc2

instance Eq TyCon where
    a == b = case (a `cmp` b) of { EQ_ -> True;   _ -> False }
    a /= b = case (a `cmp` b) of { EQ_ -> False;  _ -> True  }

instance Ord TyCon where
    a <= b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> True;  GT__ -> False }
    a <	 b = case (a `cmp` b) of { LT_ -> True;  EQ_ -> False; GT__ -> False }
    a >= b = case (a `cmp` b) of { LT_ -> False; EQ_ -> True;  GT__ -> True  }
    a >	 b = case (a `cmp` b) of { LT_ -> False; EQ_ -> False; GT__ -> True  }
    _tagCmp a b = case (a `cmp` b) of { LT_ -> _LT; EQ_ -> _EQ; GT__ -> _GT }

instance Uniquable TyCon where
    uniqueOf (DataTyCon  u _ _ _ _ _ _ _) = u
    uniqueOf (TupleTyCon u _ _)		  = u
    uniqueOf (PrimTyCon  u _ _ _)	  = u
    uniqueOf (SynTyCon   u _ _ _ _ _)	  = u
    uniqueOf tc@(SpecTyCon _ _)		  = panic "uniqueOf:SpecTyCon"
    uniqueOf tc				  = uniqueOf (getName tc)
\end{code}

\begin{code}
instance NamedThing TyCon where
    getName (DataTyCon _ n _ _ _ _ _ _) = n
    getName (PrimTyCon _ n _ _)		= n
    getName (SpecTyCon tc _)		= getName tc
    getName (SynTyCon _ n _ _ _ _)	= n
    getName FunTyCon			= mkFunTyConName
    getName (TupleTyCon _ n _)		= n
    getName tc				= panic "TyCon.getName"

{- LATER:
    getName (SpecTyCon tc tys) = let (OrigName m n) = origName "????" tc in
       			     (m, n _APPEND_ specMaybeTysSuffix tys)
    getName	other_tc           = moduleNamePair (expectJust "tycon1" (getName other_tc))
    getName other			     = Nothing
-}
\end{code}
