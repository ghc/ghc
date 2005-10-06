%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TyCon]{The @TyCon@ datatype}

\begin{code}
module TyCon(
	TyCon, ArgVrcs, FieldLabel,

	PrimRep(..),
	tyConPrimRep,

	AlgTyConRhs(..), visibleDataCons,

	isFunTyCon, isUnLiftedTyCon, isProductTyCon, 
	isAlgTyCon, isDataTyCon, isSynTyCon, isNewTyCon, isPrimTyCon,
	isEnumerationTyCon, 
	isTupleTyCon, isUnboxedTupleTyCon, isBoxedTupleTyCon, tupleTyConBoxity,
	isRecursiveTyCon, newTyConRep, newTyConRhs, newTyConRhs_maybe, isHiBootTyCon,

	makeTyConAbstract, isAbstractTyCon,

	mkForeignTyCon, isForeignTyCon,

	mkAlgTyCon,
	mkClassTyCon,
	mkFunTyCon,
	mkPrimTyCon,
	mkLiftedPrimTyCon,
	mkTupleTyCon,
	mkSynTyCon,

	tyConName,
	tyConKind,
	tyConUnique,
	tyConTyVars,
	tyConArgVrcs,
	algTyConRhs, tyConDataCons, tyConDataCons_maybe, tyConFamilySize,
	tyConFields, tyConSelIds,
	tyConStupidTheta,
	tyConArity,
	isClassTyCon, tyConClass_maybe,
	getSynTyConDefn,
	tyConExtName,		-- External name for foreign types

        maybeTyConSingleCon,

	-- Generics
        tyConHasGenerics
) where

#include "HsVersions.h"

import {-# SOURCE #-} TypeRep ( Type, PredType )
 -- Should just be Type(Type), but this fails due to bug present up to
 -- and including 4.02 involving slurping of hi-boot files.  Bug is now fixed.

import {-# SOURCE #-} DataCon ( DataCon, isVanillaDataCon )


import Var   		( TyVar, Id )
import Class		( Class )
import Kind		( Kind )
import BasicTypes	( Arity, RecFlag(..), Boxity(..), isBoxed )
import Name		( Name, nameUnique, NamedThing(getName) )
import PrelNames	( Unique, Uniquable(..) )
import Maybes		( orElse )
import Util		( equalLength )
import Outputable
import FastString
\end{code}

%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************

\begin{code}
data TyCon
  = FunTyCon {
	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: Kind,
	tyConArity  :: Arity
    }


  | AlgTyCon {		-- Data type, and newtype decls.
			-- All lifted, all boxed
	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: Kind,
	tyConArity  :: Arity,
	
	tyConTyVars :: [TyVar],		-- Scopes over (a) the [PredType] in AlgTyConRhs.DataTyCon
					--	       (b) the cached types in AlgTyConRhs.NewTyCon
					--	       (c) the types in algTcFields
					-- But not over the data constructors
	argVrcs     :: ArgVrcs,

	algTcFields :: [(FieldLabel, Type, Id)],  
					-- Its fields (empty if none): 
					--  * field name
					--  * its type (scoped over by tyConTyVars)
					--  * record selector (name = field name)

	algTcStupidTheta :: [PredType],	-- The "stupid theta" for the data type
					-- (always empty for GADTs)

	algTcRhs :: AlgTyConRhs,	-- Data constructors in here

	algTcRec :: RecFlag,		-- Tells whether the data type is part of 
					-- a mutually-recursive group or not

	hasGenerics :: Bool,		-- True <=> generic to/from functions are available
					-- (in the exports of the data type's source module)

	algTcClass :: Maybe Class
		-- Just cl if this tycon came from a class declaration
    }

  | PrimTyCon {			-- Primitive types; cannot be defined in Haskell
				-- Now includes foreign-imported types
	tyConUnique   :: Unique,
	tyConName     :: Name,
	tyConKind     :: Kind,
	tyConArity    :: Arity,
	argVrcs       :: ArgVrcs,

	primTyConRep  :: PrimRep,
			-- Many primitive tycons are unboxed, but some are
			-- boxed (represented by pointers). The CgRep tells.

	isUnLifted   :: Bool,		-- Most primitive tycons are unlifted, 
					-- but foreign-imported ones may not be
	tyConExtName :: Maybe FastString	-- Just xx for foreign-imported types
    }

  | TupleTyCon {
	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: Kind,
	tyConArity  :: Arity,
	tyConBoxed  :: Boxity,
	tyConTyVars :: [TyVar],
	dataCon     :: DataCon,
	hasGenerics :: Bool
    }

  | SynTyCon {
	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: Kind,
	tyConArity  :: Arity,

	tyConTyVars     :: [TyVar],	-- Bound tyvars
	synTyConDefn    :: Type,	-- Right-hand side, mentioning these type vars.
					-- Acts as a template for the expansion when
					-- the tycon is applied to some types.
	argVrcs :: ArgVrcs
    }

type FieldLabel = Name

type ArgVrcs = [(Bool,Bool)]  -- Tyvar variance info: [(occPos,occNeg)]
	-- [] means "no information, assume the worst"

data AlgTyConRhs
  = AbstractTyCon	-- We know nothing about this data type, except 
			-- that it's represented by a pointer
			-- Used when we export a data type abstractly into
			-- an hi file

  | DataTyCon 
	[DataCon]	-- The constructors; can be empty if the user declares
			--   the type to have no constructors
			-- INVARIANT: Kept in order of increasing tag
			--	      (see the tag assignment in DataCon.mkDataCon)
	Bool		-- Cached: True <=> an enumeration type
			--	   Includes data types with no constructors.

  | NewTyCon 		-- Newtypes always have exactly one constructor
	DataCon		-- The unique constructor; it has no existentials
	Type		-- Cached: the argument type of the constructor
			--  = the representation type of the tycon

	Type		-- Cached: the *ultimate* representation type
			-- By 'ultimate' I mean that the rep type is not itself
			-- a newtype or type synonym.
			-- The rep type isn't entirely simple:
			--  for a recursive newtype we pick () as the rep type
			--	newtype T = MkT T
			--
			-- The rep type has free type variables the tyConTyVars
			-- Thus:
			-- 	newtype T a = MkT [(a,Int)]
			-- The rep type is [(a,Int)]
	-- NB: the rep type isn't necessarily the original RHS of the
	--     newtype decl, because the rep type looks through other
	--     newtypes.

visibleDataCons :: AlgTyConRhs -> [DataCon]
visibleDataCons AbstractTyCon    = []
visibleDataCons (DataTyCon cs _) = cs
visibleDataCons (NewTyCon c _ _) = [c]
\end{code}

%************************************************************************
%*									*
\subsection{PrimRep}
%*									*
%************************************************************************

A PrimRep is an abstraction of a type.  It contains information that
the code generator needs in order to pass arguments, return results,
and store values of this type.

A PrimRep is somewhat similar to a CgRep (see codeGen/SMRep) and a
MachRep (see cmm/MachOp), although each of these types has a distinct
and clearly defined purpose:

  - A PrimRep is a CgRep + information about signedness + information
    about primitive pointers (AddrRep).  Signedness and primitive
    pointers are required when passing a primitive type to a foreign
    function, but aren't needed for call/return conventions of Haskell
    functions.

  - A MachRep is a basic machine type (non-void, doesn't contain
    information on pointerhood or signedness, but contains some
    reps that don't have corresponding Haskell types).

\begin{code}
data PrimRep
  = VoidRep
  | PtrRep
  | IntRep		-- signed, word-sized
  | WordRep		-- unsinged, word-sized
  | Int64Rep		-- signed, 64 bit (32-bit words only)
  | Word64Rep		-- unsigned, 64 bit (32-bit words only)
  | AddrRep		-- a pointer, but not to a Haskell value
  | FloatRep
  | DoubleRep
\end{code}

%************************************************************************
%*									*
\subsection{TyCon Construction}
%*									*
%************************************************************************

Note: the TyCon constructors all take a Kind as one argument, even though
they could, in principle, work out their Kind from their other arguments.
But to do so they need functions from Types, and that makes a nasty
module mutual-recursion.  And they aren't called from many places.
So we compromise, and move their Kind calculation to the call site.

\begin{code}
mkFunTyCon :: Name -> Kind -> TyCon
mkFunTyCon name kind 
  = FunTyCon { 
	tyConUnique = nameUnique name,
	tyConName   = name,
	tyConKind   = kind,
	tyConArity  = 2
    }

-- This is the making of a TyCon. Just the same as the old mkAlgTyCon,
-- but now you also have to pass in the generic information about the type
-- constructor - you can get hold of it easily (see Generics module)
mkAlgTyCon name kind tyvars argvrcs stupid rhs flds is_rec gen_info
  = AlgTyCon {	
	tyConName 	 = name,
	tyConUnique	 = nameUnique name,
	tyConKind	 = kind,
	tyConArity	 = length tyvars,
	tyConTyVars	 = tyvars,
	argVrcs		 = argvrcs,
	algTcStupidTheta = stupid,
	algTcRhs         = rhs,
	algTcFields	 = flds,
	algTcClass	 = Nothing,
	algTcRec	 = is_rec,
	hasGenerics = gen_info
    }

mkClassTyCon name kind tyvars argvrcs rhs clas is_rec
  = AlgTyCon {	
	tyConName 	 = name,
	tyConUnique	 = nameUnique name,
	tyConKind	 = kind,
	tyConArity	 = length tyvars,
	tyConTyVars	 = tyvars,
	argVrcs		 = argvrcs,
	algTcStupidTheta = [],
	algTcRhs	 = rhs,
	algTcFields	 = [],
	algTcClass	 = Just clas,
	algTcRec	 = is_rec,
	hasGenerics = False
    }


mkTupleTyCon name kind arity tyvars con boxed gen_info
  = TupleTyCon {
	tyConUnique = nameUnique name,
	tyConName = name,
	tyConKind = kind,
	tyConArity = arity,
	tyConBoxed = boxed,
	tyConTyVars = tyvars,
	dataCon = con,
	hasGenerics = gen_info
    }

-- Foreign-imported (.NET) type constructors are represented
-- as primitive, but *lifted*, TyCons for now. They are lifted
-- because the Haskell type T representing the (foreign) .NET
-- type T is actually implemented (in ILX) as a thunk<T>
mkForeignTyCon name ext_name kind arity arg_vrcs
  = PrimTyCon {
	tyConName    = name,
	tyConUnique  = nameUnique name,
	tyConKind    = kind,
	tyConArity   = arity,
        argVrcs      = arg_vrcs,
	primTyConRep = PtrRep, -- they all do
	isUnLifted   = False,
	tyConExtName = ext_name
    }


-- most Prim tycons are lifted
mkPrimTyCon name kind arity arg_vrcs rep
  = mkPrimTyCon' name kind arity arg_vrcs rep True  

-- but RealWorld is lifted
mkLiftedPrimTyCon name kind arity arg_vrcs rep
  = mkPrimTyCon' name kind arity arg_vrcs rep False

mkPrimTyCon' name kind arity arg_vrcs rep is_unlifted
  = PrimTyCon {
	tyConName    = name,
	tyConUnique  = nameUnique name,
	tyConKind    = kind,
	tyConArity   = arity,
        argVrcs      = arg_vrcs,
	primTyConRep = rep,
	isUnLifted   = is_unlifted,
	tyConExtName = Nothing
    }

mkSynTyCon name kind tyvars rhs argvrcs
  = SynTyCon {	
	tyConName = name,
	tyConUnique = nameUnique name,
	tyConKind = kind,
	tyConArity = length tyvars,
	tyConTyVars = tyvars,
	synTyConDefn = rhs,
	argVrcs      = argvrcs
    }
\end{code}

\begin{code}
isFunTyCon :: TyCon -> Bool
isFunTyCon (FunTyCon {}) = True
isFunTyCon _             = False

isAbstractTyCon :: TyCon -> Bool
isAbstractTyCon (AlgTyCon { algTcRhs = AbstractTyCon }) = True
isAbstractTyCon _ = False

makeTyConAbstract :: TyCon -> TyCon
makeTyConAbstract tc@(AlgTyCon {}) = tc { algTcRhs = AbstractTyCon }
makeTyConAbstract tc = pprPanic "makeTyConAbstract" (ppr tc)

isPrimTyCon :: TyCon -> Bool
isPrimTyCon (PrimTyCon {}) = True
isPrimTyCon _              = False

isUnLiftedTyCon :: TyCon -> Bool
isUnLiftedTyCon (PrimTyCon  {isUnLifted = is_unlifted}) = is_unlifted
isUnLiftedTyCon (TupleTyCon {tyConBoxed = boxity})      = not (isBoxed boxity)
isUnLiftedTyCon _    				        = False

-- isAlgTyCon returns True for both @data@ and @newtype@
isAlgTyCon :: TyCon -> Bool
isAlgTyCon (AlgTyCon {})   = True
isAlgTyCon (TupleTyCon {}) = True
isAlgTyCon other 	   = False

isDataTyCon :: TyCon -> Bool
-- isDataTyCon returns True for data types that are represented by
-- heap-allocated constructors.
-- These are srcutinised by Core-level @case@ expressions, and they
-- get info tables allocated for them.
--	True for all @data@ types
--	False for newtypes
--		  unboxed tuples
isDataTyCon tc@(AlgTyCon {algTcRhs = rhs})  
  = case rhs of
	DataTyCon _ _  -> True
	NewTyCon _ _ _ -> False
	AbstractTyCon  -> pprPanic "isDataTyCon" (ppr tc)

isDataTyCon (TupleTyCon {tyConBoxed = boxity}) = isBoxed boxity
isDataTyCon other = False

isNewTyCon :: TyCon -> Bool
isNewTyCon (AlgTyCon {algTcRhs = NewTyCon _ _ _}) = True 
isNewTyCon other			          = False

isProductTyCon :: TyCon -> Bool
-- A "product" tycon
--	has *one* constructor, 
--	is *not* existential
-- but
--	may be  DataType or NewType, 
-- 	may be  unboxed or not, 
--	may be  recursive or not
isProductTyCon tc@(AlgTyCon {}) = case algTcRhs tc of
				    DataTyCon [data_con] _ -> isVanillaDataCon data_con
				    NewTyCon _ _ _         -> True
				    other		   -> False
isProductTyCon (TupleTyCon {})  = True   
isProductTyCon other		= False

isSynTyCon :: TyCon -> Bool
isSynTyCon (SynTyCon {}) = True
isSynTyCon _		 = False

isEnumerationTyCon :: TyCon -> Bool
isEnumerationTyCon (AlgTyCon {algTcRhs = DataTyCon _ is_enum}) = is_enum
isEnumerationTyCon other				       = False

isTupleTyCon :: TyCon -> Bool
-- The unit tycon didn't used to be classed as a tuple tycon
-- but I thought that was silly so I've undone it
-- If it can't be for some reason, it should be a AlgTyCon
--
-- NB: when compiling Data.Tuple, the tycons won't reply True to
-- isTupleTyCon, becuase they are built as AlgTyCons.  However they
-- get spat into the interface file as tuple tycons, so I don't think
-- it matters.
isTupleTyCon (TupleTyCon {}) = True
isTupleTyCon other 	     = False

isUnboxedTupleTyCon :: TyCon -> Bool
isUnboxedTupleTyCon (TupleTyCon {tyConBoxed = boxity}) = not (isBoxed boxity)
isUnboxedTupleTyCon other = False

isBoxedTupleTyCon :: TyCon -> Bool
isBoxedTupleTyCon (TupleTyCon {tyConBoxed = boxity}) = isBoxed boxity
isBoxedTupleTyCon other = False

tupleTyConBoxity tc = tyConBoxed tc

isRecursiveTyCon :: TyCon -> Bool
isRecursiveTyCon (AlgTyCon {algTcRec = Recursive}) = True
isRecursiveTyCon other				      = False

isHiBootTyCon :: TyCon -> Bool
-- Used for knot-tying in hi-boot files
isHiBootTyCon (AlgTyCon {algTcRhs = AbstractTyCon}) = True
isHiBootTyCon other			            = False

isForeignTyCon :: TyCon -> Bool
-- isForeignTyCon identifies foreign-imported type constructors
isForeignTyCon (PrimTyCon {tyConExtName = Just _}) = True
isForeignTyCon other				   = False
\end{code}

\begin{code}
tyConHasGenerics :: TyCon -> Bool
tyConHasGenerics (AlgTyCon {hasGenerics = hg})   = hg
tyConHasGenerics (TupleTyCon {hasGenerics = hg}) = hg
tyConHasGenerics other				 = False	-- Synonyms

tyConDataCons :: TyCon -> [DataCon]
-- It's convenient for tyConDataCons to return the
-- empty list for type synonyms etc
tyConDataCons tycon = tyConDataCons_maybe tycon `orElse` []

tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConDataCons_maybe (AlgTyCon {algTcRhs = DataTyCon cons _}) = Just cons
tyConDataCons_maybe (AlgTyCon {algTcRhs = NewTyCon con _ _}) = Just [con]
tyConDataCons_maybe (TupleTyCon {dataCon = con})	     = Just [con]
tyConDataCons_maybe other			             = Nothing

tyConFamilySize  :: TyCon -> Int
tyConFamilySize (AlgTyCon {algTcRhs = DataTyCon cons _}) = length cons
tyConFamilySize (AlgTyCon {algTcRhs = NewTyCon _ _ _})   = 1
tyConFamilySize (TupleTyCon {})	 			 = 1
#ifdef DEBUG
tyConFamilySize other = pprPanic "tyConFamilySize:" (ppr other)
#endif

tyConFields :: TyCon -> [(FieldLabel,Type,Id)]
tyConFields (AlgTyCon {algTcFields = fs}) = fs
tyConFields other_tycon		          = []

tyConSelIds :: TyCon -> [Id]
tyConSelIds tc = [id | (_,_,id) <- tyConFields tc]

algTyConRhs :: TyCon -> AlgTyConRhs
algTyConRhs (AlgTyCon {algTcRhs = rhs})  = rhs
algTyConRhs (TupleTyCon {dataCon = con}) = DataTyCon [con] False
algTyConRhs other = pprPanic "algTyConRhs" (ppr other)
\end{code}

\begin{code}
newTyConRhs :: TyCon -> ([TyVar], Type)
newTyConRhs (AlgTyCon {tyConTyVars = tvs, algTcRhs = NewTyCon _ rhs _}) = (tvs, rhs)
newTyConRhs tycon = pprPanic "newTyConRhs" (ppr tycon)

newTyConRhs_maybe :: TyCon 
		  -> [Type]			-- Args to tycon
		  -> Maybe ([(TyVar,Type)], 	-- Substitution
			    Type)		-- Body type (not yet substituted)
-- Non-recursive newtypes are transparent to Core; 
-- Given an application to some types, return Just (tenv, ty)
-- if it's a saturated, non-recursive newtype.
newTyConRhs_maybe (AlgTyCon {tyConTyVars = tvs, 
			     algTcRec = NonRecursive,	-- Not recursive
			     algTcRhs = NewTyCon _ rhs _}) tys
   | tvs `equalLength` tys 	-- Saturated
   = Just (tvs `zip` tys, rhs)
	
newTyConRhs_maybe other_tycon tys = Nothing


newTyConRep :: TyCon -> ([TyVar], Type)
newTyConRep (AlgTyCon {tyConTyVars = tvs, algTcRhs = NewTyCon _ _ rep}) = (tvs, rep)
newTyConRep tycon = pprPanic "newTyConRep" (ppr tycon)

tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon {primTyConRep = rep}) = rep
tyConPrimRep tc = ASSERT(not (isUnboxedTupleTyCon tc)) PtrRep
\end{code}

\begin{code}
tyConStupidTheta :: TyCon -> [PredType]
tyConStupidTheta (AlgTyCon {algTcStupidTheta = stupid}) = stupid
tyConStupidTheta (TupleTyCon {})			= []
tyConStupidTheta tycon = pprPanic "tyConStupidTheta" (ppr tycon)
\end{code}

@tyConArgVrcs_maybe@ gives a list of (occPos,occNeg) flags, one for
each tyvar, if available.  See @calcAlgTyConArgVrcs@ for how this is
actually computed (in another file).

\begin{code}
tyConArgVrcs :: TyCon -> ArgVrcs
tyConArgVrcs (FunTyCon   {})		       = [(False,True),(True,False)]
tyConArgVrcs (AlgTyCon   {argVrcs = oi})       = oi
tyConArgVrcs (PrimTyCon  {argVrcs = oi})       = oi
tyConArgVrcs (TupleTyCon {tyConArity = arity}) = (replicate arity (True,False))
tyConArgVrcs (SynTyCon   {argVrcs = oi})       = oi
\end{code}

\begin{code}
getSynTyConDefn :: TyCon -> ([TyVar], Type)
getSynTyConDefn (SynTyCon {tyConTyVars = tyvars, synTyConDefn = ty}) = (tyvars,ty)
getSynTyConDefn tycon = pprPanic "getSynTyConDefn" (ppr tycon)
\end{code}

\begin{code}
maybeTyConSingleCon :: TyCon -> Maybe DataCon
maybeTyConSingleCon (AlgTyCon {algTcRhs = DataTyCon [c] _}) = Just c
maybeTyConSingleCon (AlgTyCon {algTcRhs = NewTyCon c _ _})  = Just c
maybeTyConSingleCon (AlgTyCon {})	        	    = Nothing
maybeTyConSingleCon (TupleTyCon {dataCon = con}) 	    = Just con
maybeTyConSingleCon (PrimTyCon {})               	    = Nothing
maybeTyConSingleCon (FunTyCon {})                	    = Nothing  -- case at funty
maybeTyConSingleCon tc = pprPanic "maybeTyConSingleCon: unexpected tycon " $ ppr tc
\end{code}

\begin{code}
isClassTyCon :: TyCon -> Bool
isClassTyCon (AlgTyCon {algTcClass = Just _}) = True
isClassTyCon other_tycon			 = False

tyConClass_maybe :: TyCon -> Maybe Class
tyConClass_maybe (AlgTyCon {algTcClass = maybe_clas}) = maybe_clas
tyConClass_maybe ther_tycon				 = Nothing
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
instance Eq TyCon where
    a == b = case (a `compare` b) of { EQ -> True;   _ -> False }
    a /= b = case (a `compare` b) of { EQ -> False;  _ -> True  }

instance Ord TyCon where
    a <= b = case (a `compare` b) of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case (a `compare` b) of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case (a `compare` b) of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case (a `compare` b) of { LT -> False; EQ -> False; GT -> True  }
    compare a b = getUnique a `compare` getUnique b

instance Uniquable TyCon where
    getUnique tc = tyConUnique tc

instance Outputable TyCon where
    ppr tc  = ppr (getName tc) 

instance NamedThing TyCon where
    getName = tyConName
\end{code}
