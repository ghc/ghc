%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[DataCon]{@DataCon@: Data Constructors}

\begin{code}
module DataCon (
	DataCon,
	ConTag, fIRST_TAG,
	mkDataCon,
	dataConRepType, dataConSig, dataConName, dataConTag, dataConTyCon,
	dataConArgTys, dataConOrigArgTys, dataConInstOrigArgTys,
	dataConRepArgTys, dataConTheta,
	dataConFieldLabels, dataConStrictMarks,
	dataConSourceArity, dataConRepArity,
	dataConNumInstArgs, dataConId, dataConWrapId, dataConRepStrictness,
	isNullaryDataCon, isTupleCon, isUnboxedTupleCon,
	isExistentialDataCon, classDataCon,

	splitProductType_maybe, splitProductType,
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Subst( substTyWith )

import CmdLineOpts	( opt_DictsStrict )
import Type		( Type, TauType, ThetaType, 
			  mkForAllTys, mkFunTys, mkTyConApp,
			  mkTyVarTys, splitTyConApp_maybe, repType
			)
import TcType		( isStrictPred, mkPredTys )
import TyCon		( TyCon, tyConDataCons, tyConDataConsIfAvailable, isProductTyCon,
			  isTupleTyCon, isUnboxedTupleTyCon, isRecursiveTyCon )
import Class		( Class, classTyCon )
import Name		( Name, NamedThing(..), nameUnique )
import Var		( TyVar, Id )
import FieldLabel	( FieldLabel )
import BasicTypes	( Arity, StrictnessMark(..) )
import NewDemand 	( Demand, lazyDmd, seqDmd )
import Outputable
import Unique		( Unique, Uniquable(..) )
import CmdLineOpts	( opt_UnboxStrictFields )
import PprType		()	-- Instances
import Maybe
import ListSetOps	( assoc )
import Util		( zipEqual, zipWithEqual )
\end{code}


Stuff about data constructors
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Every constructor, C, comes with a

  *wrapper*, called C, whose type is exactly what it looks like
	in the source program. It is an ordinary function,
	and it gets a top-level binding like any other function

  *worker*, called $wC, which is the actual data constructor.
	Its type may be different to C, because:
		- useless dict args are dropped
		- strict args may be flattened
	It does not have a binding.

  The worker is very like a primop, in that it has no binding,



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

	dcRepType   :: Type,	-- Type of the constructor
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
	--	dcOrigArgTys   = [a,List b]
	--	dcTyCon    = T

	dcTyVars :: [TyVar], 		-- Type vars and context for the data type decl
					-- These are ALWAYS THE SAME AS THE TYVARS
					-- FOR THE PARENT TyCon.  We occasionally rely on
					-- this just to avoid redundant instantiation
	dcTheta  ::  ThetaType,

	dcExTyVars :: [TyVar], 		-- Ditto for the context of the constructor,
	dcExTheta  :: ThetaType,	-- the existentially quantified stuff
					
	dcOrigArgTys :: [Type],		-- Original argument types
					-- (before unboxing and flattening of
					--  strict fields)

	dcRepArgTys :: [Type],		-- Final, representation argument types, after unboxing and flattening,
					-- and including existential dictionaries

	dcRepStrictness :: [Demand],	-- One for each representation argument	

	dcTyCon  :: TyCon,		-- Result tycon

	-- Now the strictness annotations and field labels of the constructor
	dcStrictMarks :: [StrictnessMark],
		-- Strictness annotations as deduced by the compiler.  
		-- Has no MarkedUserStrict; they have been changed to MarkedStrict
		-- or MarkedUnboxed by the compiler.
		-- *Includes the existential dictionaries*
		-- length = length dcExTheta + dataConSourceArity dataCon

	dcFields  :: [FieldLabel],
		-- Field labels for this constructor, in the
		-- same order as the argument types; 
		-- length = 0 (if not a record) or dataConSourceArity.

	-- Finally, the curried worker function that corresponds to the constructor
	-- It doesn't have an unfolding; the code generator saturates these Ids
	-- and allocates a real constructor when it finds one.
	--
	-- An entirely separate wrapper function is built in TcTyDecls

	dcId :: Id,		-- The corresponding worker Id
				-- Takes dcRepArgTys as its arguments

	dcWrapId :: Id		-- The wrapper Id
  }

type ConTag = Int

fIRST_TAG :: ConTag
fIRST_TAG =  1	-- Tags allocated from here for real constructors
\end{code}

The dcRepType field contains the type of the representation of a contructor
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


%************************************************************************
%*									*
\subsection{Instances}
%*									*
%************************************************************************

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


%************************************************************************
%*									*
\subsection{Consruction}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Name
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ThetaType
	  -> [TyVar] -> ThetaType
	  -> [TauType] -> TyCon
	  -> Id -> Id
	  -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name arg_stricts fields
	  tyvars theta ex_tyvars ex_theta orig_arg_tys tycon
	  work_id wrap_id
  = ASSERT(length arg_stricts == length orig_arg_tys)
	-- The 'stricts' passed to mkDataCon are simply those for the
	-- source-language arguments.  We add extra ones for the
	-- dictionary arguments right here.
    con
  where
    con = MkData {dcName = name, dcUnique = nameUnique name,
	  	  dcTyVars = tyvars, dcTheta = theta,
		  dcOrigArgTys = orig_arg_tys,
		  dcRepArgTys = rep_arg_tys,
	     	  dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		  dcStrictMarks = real_stricts, dcRepStrictness = rep_arg_demands,
		  dcFields = fields, dcTag = tag, dcTyCon = tycon, dcRepType = ty,
		  dcId = work_id, dcWrapId = wrap_id}

	-- Strictness marks for source-args
	--	*after unboxing choices*, 
	-- but  *including existential dictionaries*
    real_stricts = (map mk_dict_strict_mark ex_theta) ++
		   zipWithEqual "mkDataCon1" (chooseBoxingStrategy tycon) 
				orig_arg_tys arg_stricts 

	-- Representation arguments and demands
    (rep_arg_demands, rep_arg_tys) 
	= unzip $ concat $ 
	  zipWithEqual "mkDataCon2" unbox_strict_arg_ty 
		       real_stricts 
		       (mkPredTys ex_theta ++ orig_arg_tys)

    tag = assoc "mkDataCon" (tyConDataCons tycon `zip` [fIRST_TAG..]) con
    ty  = mkForAllTys (tyvars ++ ex_tyvars)
	              (mkFunTys rep_arg_tys result_ty)
		-- NB: the existential dict args are already in rep_arg_tys

    result_ty = mkTyConApp tycon (mkTyVarTys tyvars)

mk_dict_strict_mark pred | isStrictPred pred = MarkedStrict
			 | otherwise	     = NotMarkedStrict
\end{code}

\begin{code}
dataConName :: DataCon -> Name
dataConName = dcName

dataConTag :: DataCon -> ConTag
dataConTag  = dcTag

dataConTyCon :: DataCon -> TyCon
dataConTyCon = dcTyCon

dataConRepType :: DataCon -> Type
dataConRepType = dcRepType

dataConId :: DataCon -> Id
dataConId = dcId

dataConWrapId :: DataCon -> Id
dataConWrapId = dcWrapId

dataConFieldLabels :: DataCon -> [FieldLabel]
dataConFieldLabels = dcFields

dataConStrictMarks :: DataCon -> [StrictnessMark]
dataConStrictMarks = dcStrictMarks

-- Number of type-instantiation arguments
-- All the remaining arguments of the DataCon are (notionally)
-- stored in the DataCon, and are matched in a case expression
dataConNumInstArgs (MkData {dcTyVars = tyvars}) = length tyvars

dataConSourceArity :: DataCon -> Arity
	-- Source-level arity of the data constructor
dataConSourceArity dc = length (dcOrigArgTys dc)

-- dataConRepArity gives the number of actual fields in the
-- {\em representation} of the data constructor.  This may be more than appear
-- in the source code; the extra ones are the existentially quantified
-- dictionaries
dataConRepArity (MkData {dcRepArgTys = arg_tys}) = length arg_tys

isNullaryDataCon con  = dataConRepArity con == 0

dataConRepStrictness :: DataCon -> [Demand]
	-- Give the demands on the arguments of a
	-- Core constructor application (Con dc args)
dataConRepStrictness dc = dcRepStrictness dc

dataConSig :: DataCon -> ([TyVar], ThetaType,
			  [TyVar], ThetaType,
			  [TauType], TyCon)

dataConSig (MkData {dcTyVars = tyvars, dcTheta = theta,
		     dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		     dcOrigArgTys = arg_tys, dcTyCon = tycon})
  = (tyvars, theta, ex_tyvars, ex_theta, arg_tys, tycon)

dataConArgTys :: DataCon
	      -> [Type] 	-- Instantiated at these types
				-- NB: these INCLUDE the existentially quantified arg types
	      -> [Type]		-- Needs arguments of these types
				-- NB: these INCLUDE the existentially quantified dict args
				--     but EXCLUDE the data-decl context which is discarded
				-- It's all post-flattening etc; this is a representation type

dataConArgTys (MkData {dcRepArgTys = arg_tys, dcTyVars = tyvars,
		       dcExTyVars = ex_tyvars}) inst_tys
 = map (substTyWith (tyvars ++ ex_tyvars) inst_tys) arg_tys

dataConTheta :: DataCon -> ThetaType
dataConTheta dc = dcTheta dc

-- And the same deal for the original arg tys:

dataConInstOrigArgTys :: DataCon -> [Type] -> [Type]
dataConInstOrigArgTys (MkData {dcOrigArgTys = arg_tys, dcTyVars = tyvars,
		       dcExTyVars = ex_tyvars}) inst_tys
 = map (substTyWith (tyvars ++ ex_tyvars) inst_tys) arg_tys
\end{code}

These two functions get the real argument types of the constructor,
without substituting for any type variables.

dataConOrigArgTys returns the arg types of the wrapper, excluding all dictionary args.

dataConRepArgTys retuns the arg types of the worker, including all dictionaries, and
after any flattening has been done.

\begin{code}
dataConOrigArgTys :: DataCon -> [Type]
dataConOrigArgTys dc = dcOrigArgTys dc

dataConRepArgTys :: DataCon -> [TauType]
dataConRepArgTys dc = dcRepArgTys dc
\end{code}


\begin{code}
isTupleCon :: DataCon -> Bool
isTupleCon (MkData {dcTyCon = tc}) = isTupleTyCon tc
	
isUnboxedTupleCon :: DataCon -> Bool
isUnboxedTupleCon (MkData {dcTyCon = tc}) = isUnboxedTupleTyCon tc

isExistentialDataCon :: DataCon -> Bool
isExistentialDataCon (MkData {dcExTyVars = tvs}) = not (null tvs)
\end{code}


\begin{code}
classDataCon :: Class -> DataCon
classDataCon clas = case tyConDataCons (classTyCon clas) of
		      (dict_constr:no_more) -> ASSERT( null no_more ) dict_constr 
\end{code}

%************************************************************************
%*									*
\subsection{Splitting products}
%*									*
%************************************************************************

\begin{code}
splitProductType_maybe
	:: Type 			-- A product type, perhaps
	-> Maybe (TyCon, 		-- The type constructor
		  [Type],		-- Type args of the tycon
		  DataCon,		-- The data constructor
		  [Type])		-- Its *representation* arg types

	-- Returns (Just ...) for any
	--	concrete (i.e. constructors visible)
	--	single-constructor
	--	not existentially quantified
	-- type whether a data type or a new type
	--
	-- Rejecing existentials is conservative.  Maybe some things
	-- could be made to work with them, but I'm not going to sweat
	-- it through till someone finds it's important.

splitProductType_maybe ty
  = case splitTyConApp_maybe ty of
	Just (tycon,ty_args)
	   | isProductTyCon tycon  	-- Includes check for non-existential,
					-- and for constructors visible
	   -> Just (tycon, ty_args, data_con, dataConArgTys data_con ty_args)
	   where
	      data_con = head (tyConDataConsIfAvailable tycon)
	other -> Nothing

splitProductType str ty
  = case splitProductType_maybe ty of
	Just stuff -> stuff
	Nothing    -> pprPanic (str ++ ": not a product") (ppr ty)

-- We attempt to unbox/unpack a strict field when either:
--   (i)  The tycon is imported, and the field is marked '! !', or
--   (ii) The tycon is defined in this module, the field is marked '!',
--	  and the -funbox-strict-fields flag is on.
--
-- This ensures that if we compile some modules with -funbox-strict-fields and
-- some without, the compiler doesn't get confused about the constructor
-- representations.

chooseBoxingStrategy :: TyCon -> Type -> StrictnessMark -> StrictnessMark
	-- Transforms any MarkedUserStricts into MarkUnboxed or MarkedStrict
chooseBoxingStrategy tycon arg_ty strict
  = case strict of
	MarkedUserStrict
	  | opt_UnboxStrictFields
		&& unbox arg_ty -> MarkedUnboxed
	  | otherwise -> MarkedStrict
	other -> strict
  where
	-- beware: repType will go into a loop if we try this on a recursive
	-- type (for reasons unknown...), hence the check for recursion below.
    unbox ty =  
	case splitTyConApp_maybe ty of
		Nothing	-> False
		Just (arg_tycon, _)
		  | isRecursiveTyCon arg_tycon -> False
		  | otherwise ->
			  case splitTyConApp_maybe (repType ty) of
		     		Nothing -> False
		     		Just (arg_tycon, _) -> isProductTyCon arg_tycon

unbox_strict_arg_ty 
	:: StrictnessMark	-- After strategy choice; can't be MarkedUserStrict
	-> Type			-- Source argument type
	-> [(Demand,Type)]	-- Representation argument types and demamds

unbox_strict_arg_ty NotMarkedStrict ty = [(lazyDmd, ty)]
unbox_strict_arg_ty MarkedStrict    ty = [(seqDmd,  ty)]
unbox_strict_arg_ty MarkedUnboxed   ty 
  = zipEqual "unbox_strict_arg_ty" (dataConRepStrictness arg_data_con) arg_tys
  where
    (_, _, arg_data_con, arg_tys) = splitProductType "unbox_strict_arg_ty" (repType ty)
\end{code}
