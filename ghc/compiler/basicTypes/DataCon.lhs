%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[Literal]{@Literal@: Machine literals (unboxed, of course)}

\begin{code}
module DataCon (
	DataCon,
	ConTag, fIRST_TAG,
	mkDataCon,
	dataConType, dataConSig, dataConName, dataConTag,
	dataConArgTys, dataConRawArgTys, dataConTyCon,
	dataConFieldLabels, dataConStrictMarks, dataConSourceArity,
	dataConNumFields, dataConNumInstArgs, dataConId,
	isNullaryDataCon, isTupleCon, isUnboxedTupleCon,
	isExistentialDataCon
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_DictsStrict )
import TysPrim
import Type		( Type, ThetaType, TauType,
			  mkSigmaTy, mkFunTys, mkTyConApp, 
			  mkTyVarTys, mkDictTy, substTy
			)
import TyCon		( TyCon, tyConDataCons, isDataTyCon,
			  isTupleTyCon, isUnboxedTupleTyCon )
import Class		( classTyCon )
import Name		( Name, NamedThing(..), nameUnique )
import Var		( TyVar, Id )
import VarEnv
import FieldLabel	( FieldLabel )
import BasicTypes	( StrictnessMark(..), Arity )
import Outputable
import Unique		( Unique, Uniquable(..) )
import Util		( assoc )
\end{code}


%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

\begin{code}
data DataCon
  = MkData {			-- Used for data constructors only;
				-- there *is* no constructor for a newtype
	dcName   :: Name,
	dcUnique :: Unique, 		-- Cached from Name
	dcTag    :: ConTag,

	-- Running example:
	--
	--	data Eq a => T a = forall b. Ord b => MkT a [b]

	dcType   :: Type,	-- Type of the constructor 
				-- 	forall ab . Ord b => a -> [b] -> MkT a
				-- (this is *not* of the constructor Id: 
				--  see notes after this data type declaration)

	-- The next six fields express the type of the constructor, in pieces
	-- e.g.
	--
	--	dcTyVars   = [a]
	-- 	dcTheta    = [Eq a]
	--	dcExTyVars = [b]
	--	dcExTheta  = [Ord b]
	--	dcArgTys   = [a,List b]
	--	dcTyCon    = T

	dcTyVars :: [TyVar], 		-- Type vars and context for the data type decl
	dcTheta  ::  ThetaType,

	dcExTyVars :: [TyVar], 		-- Ditto for the context of the constructor, 
	dcExTheta  :: ThetaType,	-- the existentially quantified stuff
					
	dcArgTys :: [Type],		-- Argument types
	dcTyCon  :: TyCon,		-- Result tycon 

	-- Now the strictness annotations and field labels of the constructor
	dcStricts :: [StrictnessMark],	-- Strict args, in the same order as the argument types;
					-- length = dataConNumFields dataCon

	dcFields  :: [FieldLabel],	-- Field labels for this constructor, in the
					-- same order as the argument types; 
					-- length = 0 (if not a record) or dataConSourceArity.

	-- Finally, the curried function that corresponds to the constructor
	-- 	mkT :: forall a b. (Eq a, Ord b) => a -> [b] -> T a
	--	mkT = /\ab. \deq dord p qs. Con MkT [a, b, dord, p, qs]
	-- This unfolding is built in MkId.mkDataConId

	dcId :: Id			-- The corresponding Id
  }

type ConTag = Int

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

The dcType field contains the type of the representation of a contructor
This may differ from the type of the contructor *Id* (built
by MkId.mkDataConId) for two reasons:
	a) the constructor Id may be overloaded, but the dictionary isn't stored
	   e.g.    data Eq a => T a = MkT a a

	b) the constructor may store an unboxed version of a strict field.

Here's an example illustrating both:
	data Ord a => T a = MkT Int! a
Here
	T :: Ord a => Int -> a -> T a
but the rep type is
	Trep :: Int# -> a -> T a
Actually, the unboxed part isn't implemented yet!


\begin{code}
instance Eq DataCon where
    a == b = getUnique a == getUnique b
    a /= b = getUnique a /= getUnique b

instance Ord DataCon where
    a <= b = getUnique a <= getUnique b
    a <	 b = getUnique a <  getUnique b
    a >= b = getUnique a >= getUnique b
    a >	 b = getUnique a > getUnique b
    compare a b = getUnique a `compare` getUnique b

instance Uniquable DataCon where
    getUnique = dcUnique

instance NamedThing DataCon where
    getName = dcName

instance Outputable DataCon where
    ppr con = ppr (dataConName con)

instance Show DataCon where
    showsPrec p con = showsPrecSDoc p (ppr con)
\end{code}

\begin{code}
mkDataCon :: Name
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ThetaType
	  -> [TyVar] -> ThetaType
	  -> [TauType] -> TyCon
	  -> Id
	  -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name arg_stricts fields tyvars theta ex_tyvars ex_theta arg_tys tycon id
  = ASSERT(length arg_stricts == length arg_tys)
	-- The 'stricts' passed to mkDataCon are simply those for the
	-- source-language arguments.  We add extra ones for the
	-- dictionary arguments right here.
    con
  where
    con = MkData {dcName = name, dcUnique = nameUnique name,
	  	  dcTyVars = tyvars, dcTheta = theta, dcArgTys = arg_tys,
	     	  dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		  dcStricts = all_stricts, dcFields = fields,
	     	  dcTag = tag, dcTyCon = tycon, dcType = ty,
		  dcId = id}

    all_stricts = (map mk_dict_strict_mark ex_theta) ++ arg_stricts
	-- Add a strictness flag for the existential dictionary arguments

    tag = assoc "mkDataCon" (tyConDataCons tycon `zip` [fIRST_TAG..]) con
    ty  = mkSigmaTy (tyvars ++ ex_tyvars) 
	            ex_theta
	            (mkFunTys arg_tys (mkTyConApp tycon (mkTyVarTys tyvars)))

mk_dict_strict_mark (clas,tys)
  | opt_DictsStrict &&
    isDataTyCon (classTyCon clas) = MarkedStrict	-- Don't mark newtype things as strict!
  | otherwise		          = NotMarkedStrict
\end{code}


\begin{code}
dataConName :: DataCon -> Name
dataConName = dcName

dataConTag :: DataCon -> ConTag
dataConTag  = dcTag

dataConTyCon :: DataCon -> TyCon
dataConTyCon = dcTyCon

dataConType :: DataCon -> Type
dataConType = dcType

dataConId :: DataCon -> Id
dataConId = dcId


dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels = dcFields

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks = dcStricts

dataConRawArgTys :: DataCon -> [TauType] -- a function of convenience
dataConRawArgTys = dcArgTys

dataConSourceArity :: DataCon -> Arity
	-- Source-level arity of the data constructor
dataConSourceArity dc = length (dcArgTys dc)

dataConSig :: DataCon -> ([TyVar], ThetaType, 
			  [TyVar], ThetaType, 
			  [TauType], TyCon)

dataConSig (MkData {dcTyVars = tyvars, dcTheta = theta,
		     dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		     dcArgTys = arg_tys, dcTyCon = tycon})
  = (tyvars, theta, ex_tyvars, ex_theta, arg_tys, tycon)

dataConArgTys :: DataCon 
	      -> [Type] 	-- Instantiated at these types
				-- NB: these INCLUDE the existentially quantified arg types
	      -> [Type]		-- Needs arguments of these types
				-- NB: these INCLUDE the existentially quantified dict args
				--     but EXCLUDE the data-decl context which is discarded

dataConArgTys (MkData {dcArgTys = arg_tys, dcTyVars = tyvars, 
		       dcExTyVars = ex_tyvars, dcExTheta = ex_theta}) inst_tys
 = map (substTy (zipVarEnv (tyvars ++ ex_tyvars) inst_tys)) 
       ([mkDictTy cls tys | (cls,tys) <- ex_theta] ++ arg_tys)
\end{code}

dataConNumFields gives the number of actual fields in the
{\em representation} of the data constructor.  This may be more than appear
in the source code; the extra ones are the existentially quantified
dictionaries

\begin{code}
-- Number of type-instantiation arguments
-- All the remaining arguments of the DataCon are (notionally)
-- stored in the DataCon, and are matched in a case expression
dataConNumInstArgs (MkData {dcTyVars = tyvars}) = length tyvars

dataConNumFields (MkData {dcExTheta = theta, dcArgTys = arg_tys})
  = length theta + length arg_tys

isNullaryDataCon con
  = dataConNumFields con == 0 -- function of convenience

isTupleCon :: DataCon -> Bool
isTupleCon (MkData {dcTyCon = tc}) = isTupleTyCon tc
	
isUnboxedTupleCon :: DataCon -> Bool
isUnboxedTupleCon (MkData {dcTyCon = tc}) = isUnboxedTupleTyCon tc

isExistentialDataCon :: DataCon -> Bool
isExistentialDataCon (MkData {dcExTyVars = tvs}) = not (null tvs)
\end{code}
