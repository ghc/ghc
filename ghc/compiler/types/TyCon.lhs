%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TyCon]{The @TyCon@ datatype}

\begin{code}
module TyCon(
	TyCon, KindCon, SuperKindCon, ArgVrcs, AlgTyConFlavour(..),

	isFunTyCon, isUnLiftedTyCon, isBoxedTyCon, isProductTyCon,
	isAlgTyCon, isDataTyCon, isSynTyCon, isNewTyCon, isPrimTyCon,
	isEnumerationTyCon, isTupleTyCon, isUnboxedTupleTyCon,
	isRecursiveTyCon, newTyConRep,

	mkAlgTyCon,
	mkClassTyCon,
	mkFunTyCon,
	mkPrimTyCon,
	mkTupleTyCon,
	mkSynTyCon,
	mkKindCon,
	mkSuperKindCon,

	setTyConName,

	tyConKind,
	tyConUnique,
	tyConTyVars,
	tyConArgVrcs_maybe,
	tyConDataCons,
	tyConFamilySize,
	tyConDerivings,
	tyConTheta,
	tyConPrimRep,
	tyConArity,
	tyConClass_maybe,
	getSynTyConDefn,

        maybeTyConSingleCon,

	matchesTyCon
) where

#include "HsVersions.h"

import {-# SOURCE #-} TypeRep ( Type, Kind, SuperKind )
 -- Should just be Type(Type), but this fails due to bug present up to
 -- and including 4.02 involving slurping of hi-boot files.  Bug is now fixed.

import {-# SOURCE #-} DataCon ( DataCon, isExistentialDataCon )

import Class 		( Class )
import Var   		( TyVar )
import BasicTypes	( Arity, NewOrData(..), RecFlag(..) )
import Maybes
import Name		( Name, nameUnique, NamedThing(getName) )
import Unique		( Unique, Uniquable(..), anyBoxConKey )
import PrimRep		( PrimRep(..), isFollowableRep )
import Outputable
\end{code}

%************************************************************************
%*									*
\subsection{The data type}
%*									*
%************************************************************************

\begin{code}
type KindCon      = TyCon
type SuperKindCon = TyCon

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
	tyConArgVrcs  :: ArgVrcs,
	algTyConTheta :: [(Class,[Type])],

	dataCons :: [DataCon],
		-- Its data constructors, with fully polymorphic types
		-- 	This list can be empty, when we import a data type abstractly,
		-- 	either (a) the interface is hand-written and doesn't give
		--		   the constructors, or
		--	       (b) in a quest for fast compilation we don't import 
		--		   the constructors

	algTyConDerivings   :: [Class],	-- Classes which have derived instances

	algTyConFlavour	:: AlgTyConFlavour,
	algTyConRec     :: RecFlag,		-- Tells whether the data type is part of 
						-- a mutually-recursive group or not

	algTyConClass_maybe :: Maybe Class	-- Nothing for ordinary types; 
						-- Just c for the type constructor
						-- for dictionaries of class c.

    }

  | PrimTyCon {		-- Primitive types; cannot be defined in Haskell
			-- NB: All of these guys are *unlifted*, but not all are *unboxed*
	tyConUnique  :: Unique,
	tyConName    :: Name,
	tyConKind    :: Kind,
	tyConArity   :: Arity,
	tyConArgVrcs :: ArgVrcs,
	primTyConRep :: PrimRep
    }

  | TupleTyCon {

	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: Kind,
	tyConArity  :: Arity,
	tyConBoxed  :: Bool,		-- True for boxed; False for unboxed
	tyConTyVars :: [TyVar],
	dataCon     :: DataCon
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
	tyConArgVrcs :: ArgVrcs
    }

  | KindCon {		-- Type constructor at the kind level
	tyConUnique :: Unique,
	tyConName   :: Name,
	tyConKind   :: SuperKind,
	tyConArity  :: Arity
    }

  | SuperKindCon	{		-- The type of kind variables or boxity variables,
	tyConUnique :: Unique,
	tyConName   :: Name
    }

type ArgVrcs = [(Bool,Bool)]  -- Tyvar variance info: [(occPos,occNeg)]
                              -- *NB*: this is tyvar variance info, *not*
                              --       termvar usage info.

data AlgTyConFlavour
  = DataTyCon		-- Data type
  | EnumTyCon		-- Special sort of enumeration type
  | NewTyCon Type	-- Newtype, with its *ultimate* representation type
			-- By 'ultimate' I mean that the rep type is not itself
			-- a newtype or type synonym.

			-- The rep type has explicit for-alls for the tyvars of
			-- the TyCon.  Thus:
			-- 	newtype T a = MkT [(a,Int)]
			-- The rep type is forall a. [(a,Int)]
			--
			-- The rep type isn't entirely simple:
			--  for a recursive newtype we pick () as the rep type
			--	newtype T = MkT T
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
mkSuperKindCon :: Name -> SuperKindCon
mkSuperKindCon name = SuperKindCon {
			tyConUnique = nameUnique name,
			tyConName = name
		      }

mkKindCon :: Name -> SuperKind -> KindCon
mkKindCon name kind
  = KindCon { 
	tyConUnique = nameUnique name,
	tyConName = name,
	tyConArity = 0,
	tyConKind = kind
     }

mkFunTyCon :: Name -> Kind -> TyCon
mkFunTyCon name kind 
  = FunTyCon { 
	tyConUnique = nameUnique name,
	tyConName   = name,
	tyConKind   = kind,
	tyConArity  = 2
    }
			    
mkAlgTyCon name kind tyvars theta argvrcs cons derivs flavour rec
  = AlgTyCon {	
	tyConName 		= name,
	tyConUnique		= nameUnique name,
	tyConKind		= kind,
	tyConArity		= length tyvars,
	tyConTyVars		= tyvars,
	tyConArgVrcs		= argvrcs,
	algTyConTheta		= theta,
	dataCons		= cons, 
	algTyConDerivings	= derivs,
	algTyConClass_maybe	= Nothing,
	algTyConFlavour 	= flavour,
	algTyConRec		= rec
    }

mkClassTyCon name kind tyvars argvrcs con clas flavour
  = AlgTyCon {	
	tyConName 		= name,
	tyConUnique		= nameUnique name,
	tyConKind		= kind,
	tyConArity		= length tyvars,
	tyConTyVars		= tyvars,
	tyConArgVrcs		= argvrcs,
	algTyConTheta		= [],
	dataCons		= [con],
	algTyConDerivings	= [],
	algTyConClass_maybe	= Just clas,
	algTyConFlavour		= flavour,
	algTyConRec		= NonRecursive
    }


mkTupleTyCon name kind arity tyvars con boxed
  = TupleTyCon {
	tyConUnique = nameUnique name,
	tyConName = name,
	tyConKind = kind,
	tyConArity = arity,
	tyConBoxed = boxed,
	tyConTyVars = tyvars,
	dataCon = con
    }

mkPrimTyCon name kind arity arg_vrcs rep 
  = PrimTyCon {
	tyConName = name,
	tyConUnique = nameUnique name,
	tyConKind = kind,
	tyConArity = arity,
        tyConArgVrcs = arg_vrcs,
	primTyConRep = rep
    }

mkSynTyCon name kind arity tyvars rhs argvrcs
  = SynTyCon {	
	tyConName = name,
	tyConUnique = nameUnique name,
	tyConKind = kind,
	tyConArity = arity,
	tyConTyVars = tyvars,
	synTyConDefn = rhs,
	tyConArgVrcs = argvrcs
    }

setTyConName tc name = tc {tyConName = name, tyConUnique = nameUnique name}
\end{code}

\begin{code}
isFunTyCon (FunTyCon {}) = True
isFunTyCon _             = False

isPrimTyCon (PrimTyCon {}) = True
isPrimTyCon _              = False

isUnLiftedTyCon (PrimTyCon {}) = True
isUnLiftedTyCon (TupleTyCon { tyConBoxed = False }) = True
isUnLiftedTyCon _              = False

-- isBoxedTyCon should not be applied to SynTyCon, nor KindCon
isBoxedTyCon (AlgTyCon {}) = True
isBoxedTyCon (FunTyCon {}) = True
isBoxedTyCon (TupleTyCon {tyConBoxed = boxed}) = boxed
isBoxedTyCon (PrimTyCon {primTyConRep = rep}) = isFollowableRep rep

-- isAlgTyCon returns True for both @data@ and @newtype@
isAlgTyCon (AlgTyCon {})   = True
isAlgTyCon (TupleTyCon {}) = True
isAlgTyCon other 	   = False

-- isDataTyCon returns False for @newtype@ and for unboxed tuples
isDataTyCon (AlgTyCon {algTyConFlavour = new_or_data})  = case new_or_data of
								NewTyCon _ -> False
								other	-> True
isDataTyCon (TupleTyCon {tyConBoxed = True}) = True	
isDataTyCon other = False

isNewTyCon (AlgTyCon {algTyConFlavour = NewTyCon _}) = True 
isNewTyCon other			             = False

newTyConRep (AlgTyCon {algTyConFlavour = NewTyCon rep}) = Just rep
newTyConRep other					= Nothing

-- A "product" tycon
--	has *one* constructor, 
--	is *not* existential
-- but
--	may be  DataType or NewType, 
-- 	may be  unboxed or not, 
--	may be  recursive or not
isProductTyCon (AlgTyCon {dataCons = [data_con]}) = not (isExistentialDataCon data_con)
isProductTyCon (TupleTyCon {}) 			  = True
isProductTyCon other				  = False

isSynTyCon (SynTyCon {}) = True
isSynTyCon _		 = False

isEnumerationTyCon (AlgTyCon {algTyConFlavour = EnumTyCon}) = True
isEnumerationTyCon other				    = False

-- The unit tycon isn't classed as a tuple tycon
isTupleTyCon (TupleTyCon {tyConArity = arity, tyConBoxed = True}) = arity >= 2
isTupleTyCon other = False

isUnboxedTupleTyCon (TupleTyCon {tyConBoxed = False}) = True
isUnboxedTupleTyCon other = False

isRecursiveTyCon (AlgTyCon {algTyConRec = Recursive}) = True
isRecursiveTyCon other				      = False
\end{code}

\begin{code}
tyConDataCons :: TyCon -> [DataCon]
tyConDataCons (AlgTyCon {dataCons = cons}) = cons
tyConDataCons (TupleTyCon {dataCon = con}) = [con]
tyConDataCons other			   = []
	-- You may think this last equation should fail,
	-- but it's quite convenient to return no constructors for
	-- a synonym; see for example the call in TcTyClsDecls.

tyConFamilySize  :: TyCon -> Int
tyConFamilySize (AlgTyCon {dataCons = cons}) = length cons
tyConFamilySize (TupleTyCon {}) = 1
#ifdef DEBUG
tyConFamilySize other = pprPanic "tyConFamilySize:" (ppr other)
#endif

tyConPrimRep :: TyCon -> PrimRep
tyConPrimRep (PrimTyCon {primTyConRep = rep}) = rep
tyConPrimRep _			              = PtrRep
\end{code}

\begin{code}
tyConDerivings :: TyCon -> [Class]
tyConDerivings (AlgTyCon {algTyConDerivings = derivs}) = derivs
tyConDerivings other				       = []
\end{code}

\begin{code}
tyConTheta :: TyCon -> [(Class, [Type])]
tyConTheta (AlgTyCon {algTyConTheta = theta}) = theta
-- should ask about anything else
\end{code}

@tyConArgVrcs_maybe@ gives a list of (occPos,occNeg) flags, one for
each tyvar, if available.  See @calcAlgTyConArgVrcs@ for how this is
actually computed (in another file).

\begin{code}
tyConArgVrcs_maybe :: TyCon -> Maybe ArgVrcs

tyConArgVrcs_maybe (FunTyCon   {}                     ) = Just [(False,True),(True,False)]
tyConArgVrcs_maybe (AlgTyCon   {tyConArgVrcs = oi})     = Just oi
tyConArgVrcs_maybe (PrimTyCon  {tyConArgVrcs = oi})     = Just oi
tyConArgVrcs_maybe (TupleTyCon {tyConArity = arity   }) = Just (replicate arity (True,False))
tyConArgVrcs_maybe (SynTyCon   {tyConArgVrcs = oi })    = Just oi
tyConArgVrcs_maybe _                                    = Nothing
\end{code}

\begin{code}
getSynTyConDefn :: TyCon -> ([TyVar], Type)
getSynTyConDefn (SynTyCon {tyConTyVars = tyvars, synTyConDefn = ty}) = (tyvars,ty)
\end{code}

\begin{code}
maybeTyConSingleCon :: TyCon -> Maybe DataCon
maybeTyConSingleCon (AlgTyCon {dataCons = [c]})  = Just c
maybeTyConSingleCon (AlgTyCon {})	         = Nothing
maybeTyConSingleCon (TupleTyCon {dataCon = con}) = Just con
maybeTyConSingleCon (PrimTyCon {})               = Nothing
maybeTyConSingleCon (FunTyCon {})                = Nothing  -- case at funty
maybeTyConSingleCon tc = pprPanic "maybeTyConSingleCon: unexpected tycon " $
                         ppr tc
\end{code}

\begin{code}
tyConClass_maybe :: TyCon -> Maybe Class
tyConClass_maybe (AlgTyCon {algTyConClass_maybe = maybe_cls}) = maybe_cls
tyConClass_maybe other_tycon			              = Nothing
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


%************************************************************************
%*									*
\subsection{Kind constructors}
%*									*
%************************************************************************

@matchesTyCon tc1 tc2@ checks whether an appliation
(tc1 t1..tn) matches (tc2 t1..tn).  By "matches" we basically mean "equals",
except that at the kind level tc2 might have more boxity info than tc1.

\begin{code}
matchesTyCon :: TyCon	-- Expected (e.g. arg type of function)
	     -> TyCon	-- Inferred (e.g. type of actual arg to function)
	     -> Bool

matchesTyCon tc1 tc2 =  uniq1 == uniq2 || uniq1 == anyBoxConKey
		     where
			uniq1 = tyConUnique tc1
			uniq2 = tyConUnique tc2
\end{code}
