%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TyCon]{The @TyCon@ datatype}

\begin{code}
module TyCon(
	TyCon, ArgVrcs, 

	AlgTyConFlavour(..), 
	DataConDetails(..), visibleDataCons,

	isFunTyCon, isUnLiftedTyCon, isProductTyCon,
	isAlgTyCon, isDataTyCon, isSynTyCon, isNewTyCon, isPrimTyCon,
	isEnumerationTyCon, 
	isTupleTyCon, isUnboxedTupleTyCon, isBoxedTupleTyCon, tupleTyConBoxity,
	isRecursiveTyCon, newTyConRep, isHiBootTyCon,

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
	tyConDataConDetails, tyConDataCons, tyConDataCons_maybe, tyConFamilySize,
	tyConSelIds,
	tyConTheta,
	tyConPrimRep,
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

import {-# SOURCE #-} DataCon ( DataCon, isExistentialDataCon )


import Var   		( TyVar, Id )
import Class		( Class )
import Kind		( Kind )
import BasicTypes	( Arity, RecFlag(..), Boxity(..), isBoxed )
import Name		( Name, nameUnique, NamedThing(getName) )
import PrelNames	( Unique, Uniquable(..) )
import PrimRep		( PrimRep(..) )
import Maybes		( orElse )
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


  | AlgTyCon {		-- Tuples, data type, and newtype decls.
			-- All lifted, all boxed
	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: Kind,
	tyConArity  :: Arity,
	
	tyConTyVars   :: [TyVar],
	argVrcs       :: ArgVrcs,
	algTyConTheta :: [PredType],

	dataCons :: DataConDetails DataCon,

	selIds :: [Id],	-- Its record selectors (if any)

	algTyConFlavour	:: AlgTyConFlavour,
	algTyConRec     :: RecFlag,	-- Tells whether the data type is part of 
					-- a mutually-recursive group or not

	hasGenerics :: Bool,	-- True <=> generic to/from functions are available
					--	    (in the exports of the data type's source module)

	algTyConClass :: Maybe Class
		-- Just cl if this tycon came from a class declaration
    }

  | PrimTyCon {			-- Primitive types; cannot be defined in Haskell
				-- Now includes foreign-imported types
	tyConUnique  :: Unique,
	tyConName    :: Name,
	tyConKind    :: Kind,
	tyConArity   :: Arity,
	argVrcs      :: ArgVrcs,
	primTyConRep :: PrimRep,	-- Many primitive tycons are unboxed, but some are
					-- boxed (represented by pointers). The PrimRep tells.

	isUnLifted   :: Bool,	-- Most primitive tycons are unlifted, 
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

type ArgVrcs = [(Bool,Bool)]  -- Tyvar variance info: [(occPos,occNeg)]
	-- [] means "no information, assume the worst"

data AlgTyConFlavour
  = DataTyCon Bool	-- Data type; True <=> an enumeration type

  | NewTyCon Type	-- Newtype, with its *ultimate* representation type
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
	--     newtypes.  If you want hte original RHS, look at the 
	--     argument type of the data constructor.

data DataConDetails datacon
  = DataCons [datacon]	-- Its data constructors, with fully polymorphic types
			-- A type can have zero constructors

  | Unknown		-- Used only when We're importing this data type from an 
			-- hi-boot file, so we don't know what its constructors are

visibleDataCons (DataCons cs) = cs
visibleDataCons other	      = []
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
mkAlgTyCon name kind tyvars theta argvrcs cons sels flavour is_rec gen_info
  = AlgTyCon {	
	tyConName 	 = name,
	tyConUnique	 = nameUnique name,
	tyConKind	 = kind,
	tyConArity	 = length tyvars,
	tyConTyVars	 = tyvars,
	argVrcs		 = argvrcs,
	algTyConTheta	 = theta,
	dataCons	 = cons, 
	selIds		 = sels,
	algTyConClass	 = Nothing,
	algTyConFlavour  = flavour,
	algTyConRec	 = is_rec,
	hasGenerics = gen_info
    }

mkClassTyCon name kind tyvars argvrcs con clas flavour is_rec
  = AlgTyCon {	
	tyConName 	 = name,
	tyConUnique	 = nameUnique name,
	tyConKind	 = kind,
	tyConArity	 = length tyvars,
	tyConTyVars	 = tyvars,
	argVrcs		 = argvrcs,
	algTyConTheta	 = [],
	dataCons	 = DataCons [con],
	selIds		 = [],
	algTyConClass	 = Just clas,
	algTyConFlavour	 = flavour,
	algTyConRec	 = is_rec,
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
-- They have PtrRep
mkForeignTyCon name ext_name kind arity arg_vrcs
  = PrimTyCon {
	tyConName    = name,
	tyConUnique  = nameUnique name,
	tyConKind    = kind,
	tyConArity   = arity,
        argVrcs      = arg_vrcs,
	primTyConRep = PtrRep,
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

isPrimTyCon :: TyCon -> Bool
isPrimTyCon (PrimTyCon {}) = True
isPrimTyCon _              = False

isUnLiftedTyCon :: TyCon -> Bool
isUnLiftedTyCon (PrimTyCon  {isUnLifted = is_unlifted}) = is_unlifted
isUnLiftedTyCon (TupleTyCon {tyConBoxed = boxity})      = not (isBoxed boxity)
isUnLiftedTyCon _    				        = False

#ifdef UNUSED
-- isBoxedTyCon should not be applied to SynTyCon, nor KindCon
isBoxedTyCon :: TyCon -> Bool
isBoxedTyCon (AlgTyCon {}) = True
isBoxedTyCon (FunTyCon {}) = True
isBoxedTyCon (TupleTyCon {tyConBoxed = boxity}) = isBoxed boxity
isBoxedTyCon (PrimTyCon {primTyConRep = rep}) = isFollowableRep rep
#endif

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
isDataTyCon (AlgTyCon {algTyConFlavour = new_or_data})  
  = case new_or_data of
	NewTyCon _ -> False
	other	   -> True

isDataTyCon (TupleTyCon {tyConBoxed = boxity}) = isBoxed boxity
isDataTyCon other = False

isNewTyCon :: TyCon -> Bool
isNewTyCon (AlgTyCon {algTyConFlavour = NewTyCon _}) = True 
isNewTyCon other			             = False

isProductTyCon :: TyCon -> Bool
-- A "product" tycon
--	has *one* constructor, 
--	is *not* existential
-- but
--	may be  DataType or NewType, 
-- 	may be  unboxed or not, 
--	may be  recursive or not
isProductTyCon (AlgTyCon {dataCons = DataCons [data_con]}) = not (isExistentialDataCon data_con)
isProductTyCon (TupleTyCon {}) 			  	   = True   
isProductTyCon other				  	   = False

isSynTyCon :: TyCon -> Bool
isSynTyCon (SynTyCon {}) = True
isSynTyCon _		 = False

isEnumerationTyCon :: TyCon -> Bool
isEnumerationTyCon (AlgTyCon {algTyConFlavour = DataTyCon is_enum}) = is_enum
isEnumerationTyCon other				  	    = False

isTupleTyCon :: TyCon -> Bool
-- The unit tycon didn't used to be classed as a tuple tycon
-- but I thought that was silly so I've undone it
-- If it can't be for some reason, it should be a AlgTyCon
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
isRecursiveTyCon (AlgTyCon {algTyConRec = Recursive}) = True
isRecursiveTyCon other				      = False

isHiBootTyCon :: TyCon -> Bool
-- Used for knot-tying in hi-boot files
isHiBootTyCon (AlgTyCon {dataCons = Unknown}) = True
isHiBootTyCon other			      = False

isForeignTyCon :: TyCon -> Bool
-- isForeignTyCon identifies foreign-imported type constructors
-- For the moment, they are primitive but lifted, but that may change
isForeignTyCon (PrimTyCon {isUnLifted = is_unlifted}) = not is_unlifted
isForeignTyCon other				      = False
\end{code}

\begin{code}
tyConHasGenerics :: TyCon -> Bool
tyConHasGenerics (AlgTyCon {hasGenerics = hg})   = hg
tyConHasGenerics (TupleTyCon {hasGenerics = hg}) = hg
tyConHasGenerics other				 = False	-- Synonyms

tyConDataConDetails :: TyCon -> DataConDetails DataCon
tyConDataConDetails (AlgTyCon {dataCons = cons}) = cons
tyConDataConDetails (TupleTyCon {dataCon = con}) = DataCons [con]
tyConDataConDetails other			 = pprPanic "tyConDataConDetails" (ppr other)

tyConDataCons :: TyCon -> [DataCon]
-- It's convenient for tyConDataCons to return the
-- empty list for type synonyms etc
tyConDataCons tycon = tyConDataCons_maybe tycon `orElse` []

tyConDataCons_maybe :: TyCon -> Maybe [DataCon]
tyConDataCons_maybe (AlgTyCon {dataCons = DataCons cons}) = Just cons
tyConDataCons_maybe (TupleTyCon {dataCon = con})	  = Just [con]
tyConDataCons_maybe other			          = Nothing

tyConFamilySize  :: TyCon -> Int
tyConFamilySize (AlgTyCon {dataCons = DataCons cs}) = length cs
tyConFamilySize (TupleTyCon {}) 	  	    = 1
#ifdef DEBUG
tyConFamilySize other = pprPanic "tyConFamilySize:" (ppr other)
#endif

tyConSelIds :: TyCon -> [Id]
tyConSelIds (AlgTyCon {selIds = sels}) = sels
tyConSelIds other_tycon		       = []
\end{code}

\begin{code}
newTyConRep :: TyCon -> ([TyVar], Type)
newTyConRep (AlgTyCon {tyConTyVars = tvs, algTyConFlavour = NewTyCon rep}) = (tvs, rep)

tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon {primTyConRep = rep}) = rep
tyConPrimRep tc			              = ASSERT( not (isUnboxedTupleTyCon tc) )
						PtrRep
	-- We should not be asking what the representation of an
	-- unboxed tuple is, because it isn't a first class value.
\end{code}

\begin{code}
tyConTheta :: TyCon -> [PredType]
tyConTheta (AlgTyCon {algTyConTheta = theta}) = theta
tyConTheta (TupleTyCon {}) = []
-- shouldn't ask about anything else
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
\end{code}

\begin{code}
maybeTyConSingleCon :: TyCon -> Maybe DataCon
maybeTyConSingleCon (AlgTyCon {dataCons = DataCons [c]})  = Just c
maybeTyConSingleCon (AlgTyCon {})	         	  = Nothing
maybeTyConSingleCon (TupleTyCon {dataCon = con}) 	  = Just con
maybeTyConSingleCon (PrimTyCon {})               	  = Nothing
maybeTyConSingleCon (FunTyCon {})                	  = Nothing  -- case at funty
maybeTyConSingleCon tc = pprPanic "maybeTyConSingleCon: unexpected tycon " $ ppr tc
\end{code}

\begin{code}
isClassTyCon :: TyCon -> Bool
isClassTyCon (AlgTyCon {algTyConClass = Just _}) = True
isClassTyCon other_tycon			 = False

tyConClass_maybe :: TyCon -> Maybe Class
tyConClass_maybe (AlgTyCon {algTyConClass = maybe_clas}) = maybe_clas
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
