%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TyCon]{The @TyCon@ datatype}

\begin{code}
#include "HsVersions.h"

module TyCon(
	TyCon(..), 	-- NB: some pals need to see representation

	Arity(..), NewOrData(..),

	isFunTyCon, isPrimTyCon, isBoxedTyCon,
	isDataTyCon, isSynTyCon,

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
	tyConArity, synTyConArity,
	getSynTyConDefn,

        maybeTyConSingleCon,
	isEnumerationTyCon,
	derivedFor
) where

CHK_Ubiq()	-- debugging consistency check

import TyLoop		( Type(..), GenType,
			  Class(..), GenClass,
			  Id(..), GenId,
			  mkTupleCon, dataConSig,
			  specMaybeTysSuffix
			)

import TyVar		( GenTyVar, alphaTyVars, alphaTyVar, betaTyVar )
import Usage		( GenUsage, Usage(..) )
import Kind		( Kind, mkBoxedTypeKind, mkArrowKind, resultKind, argKind )

import Maybes
import Name		( Name, RdrName(..), appendRdr, nameUnique,
			  mkTupleTyConName, mkFunTyConName
			)
import Unique		( Unique, funTyConKey, mkTupleTyConUnique )
import Pretty		( Pretty(..), PrettyRep )
import PprStyle		( PprStyle )
import SrcLoc		( SrcLoc, mkBuiltinSrcLoc )
import Util		( panic, panic#, nOfThem, isIn, Ord3(..) )
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

mkDataTyCon name
  = DataTyCon (nameUnique name) name
mkPrimTyCon name
  = PrimTyCon (nameUnique name) name
mkSynTyCon name
  = SynTyCon (nameUnique name) name

isFunTyCon FunTyCon = True
isFunTyCon _ = False

isPrimTyCon (PrimTyCon _ _ _) = True
isPrimTyCon _ = False

-- At present there are no unboxed non-primitive types, so
-- isBoxedTyCon is just the negation of isPrimTyCon.
isBoxedTyCon = not . isPrimTyCon

-- isDataTyCon returns False for @newtype@.
-- Not sure about this decision yet.
isDataTyCon (DataTyCon _ _ _ _ _ _ _ DataType) = True
isDataTyCon other 			       = False

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
tyConKind (PrimTyCon _ _ kind)		 = kind
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
tyConUnique (PrimTyCon uniq _ _) 	   = uniq
tyConUnique (SynTyCon uniq _ _ _ _ _)      = uniq
tyConUnique (SpecTyCon _ _ )		   = panic "tyConUnique:SpecTyCon"

tyConArity :: TyCon -> Arity
tyConArity FunTyCon			 = 2
tyConArity (DataTyCon _ _ _ tvs _ _ _ _) = length tvs
tyConArity (TupleTyCon _ _ arity)	 = arity
tyConArity (PrimTyCon _ _ _)		 = 0	-- ??
tyConArity (SpecTyCon _ _)		 = 0
tyConArity (SynTyCon _ _ _ arity _ _)    = arity

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
tyConTyVars (PrimTyCon _ _ _) 	     	  = panic "tyConTyVars:PrimTyCon"
tyConTyVars (SpecTyCon _ _ ) 	     	  = panic "tyConTyVars:SpecTyCon"
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
\end{code}

\begin{code}
tyConDerivings :: TyCon -> [Class]
tyConDerivings (DataTyCon _ _ _ _ _ _ derivs _) = derivs
tyConDerivings other				   = []
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
maybeTyConSingleCon (PrimTyCon _ _ _)	          = Nothing
maybeTyConSingleCon (SpecTyCon tc tys)            = panic "maybeTyConSingleCon:SpecTyCon"
						  -- requires DataCons of TyCon

isEnumerationTyCon (TupleTyCon _ _ arity)
  = arity == 0
isEnumerationTyCon (DataTyCon _ _ _ _ _ data_cons _ _)
  = not (null data_cons) && all is_nullary data_cons
  where
    is_nullary con = case (dataConSig con) of { (_,_, arg_tys, _) ->
		     null arg_tys }
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
  cmp FunTyCon		          FunTyCon		      = EQ_
  cmp (DataTyCon a _ _ _ _ _ _ _) (DataTyCon b _ _ _ _ _ _ _) = a `cmp` b
  cmp (SynTyCon a _ _ _ _ _)      (SynTyCon b _ _ _ _ _)      = a `cmp` b
  cmp (TupleTyCon _ _ a)          (TupleTyCon _ _ b)	      = a `cmp` b
  cmp (PrimTyCon a _ _)		  (PrimTyCon b _ _)	      = a `cmp` b
  cmp (SpecTyCon tc1 mtys1)	  (SpecTyCon tc2 mtys2)
    = panic# "cmp on SpecTyCons" -- case (tc1 `cmp` tc2) of { EQ_ -> mtys1 `cmp` mtys2; xxx -> xxx }

    -- now we *know* the tags are different, so...
  cmp other_1 other_2
    | tag1 _LT_ tag2 = LT_
    | otherwise      = GT_
    where
      tag1 = tag_TyCon other_1
      tag2 = tag_TyCon other_2

      tag_TyCon FunTyCon    		    = ILIT(1)
      tag_TyCon (DataTyCon _ _ _ _ _ _ _ _) = ILIT(2)
      tag_TyCon (TupleTyCon _ _ _)	    = ILIT(3)
      tag_TyCon (PrimTyCon  _ _ _)	    = ILIT(4)
      tag_TyCon (SpecTyCon  _ _) 	    = ILIT(5)
      tag_TyCon (SynTyCon _ _ _ _ _ _)	    = ILIT(6)

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
    uniqueOf (DataTyCon u _ _ _ _ _ _ _) = u
    uniqueOf (PrimTyCon u _ _)		 = u
    uniqueOf (SynTyCon  u _ _ _ _ _)	 = u
    uniqueOf tc@(SpecTyCon _ _)		 = panic "uniqueOf:SpecTyCon"
    uniqueOf tc				 = uniqueOf (getName tc)
\end{code}

\begin{code}
instance NamedThing TyCon where
    getName (DataTyCon _ n _ _ _ _ _ _) = n
    getName (PrimTyCon _ n _)		= n
    getName (SpecTyCon tc _)		= getName tc
    getName (SynTyCon _ n _ _ _ _)	= n
    getName FunTyCon			= mkFunTyConName
    getName (TupleTyCon _ n _)		= n
    getName tc				= panic "TyCon.getName"

{- LATER:
    getName (SpecTyCon tc tys) = let (m,n) = moduleNamePair tc in
       			     (m, n _APPEND_ specMaybeTysSuffix tys)
    getName	other_tc           = moduleNamePair (expectJust "tycon1" (getName other_tc))
    getName other			     = Nothing
-}
\end{code}
