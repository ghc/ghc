%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[DataCon]{@DataCon@: Data Constructors}

\begin{code}
module DataCon (
	DataCon,
	ConTag, fIRST_TAG,
	mkDataCon,
	dataConType, dataConSig, dataConName, dataConTag,
	dataConOrigArgTys, dataConArgTys, dataConRawArgTys, dataConTyCon,
	dataConFieldLabels, dataConStrictMarks, dataConSourceArity,
	dataConNumFields, dataConNumInstArgs, dataConId, dataConRepStrictness,
	isNullaryDataCon, isTupleCon, isUnboxedTupleCon,
	isExistentialDataCon,

	StrictnessMark(..), 	-- Representation visible to MkId only
	markedStrict, notMarkedStrict, markedUnboxed, maybeMarkedUnboxed
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Subst( substTy, mkTyVarSubst )

import CmdLineOpts	( opt_DictsStrict )
import TysPrim
import Type		( Type, ThetaType, TauType,
			  mkSigmaTy, mkFunTys, mkTyConApp, 
			  mkTyVarTys, mkDictTy,
			  splitAlgTyConApp_maybe
			)
import PprType
import TyCon		( TyCon, tyConDataCons, isDataTyCon,
			  isTupleTyCon, isUnboxedTupleTyCon )
import Class		( classTyCon )
import Name		( Name, NamedThing(..), nameUnique, isLocallyDefinedName )
import Var		( TyVar, Id )
import FieldLabel	( FieldLabel )
import BasicTypes	( Arity )
import Demand		( Demand, wwStrict, wwLazy )
import Outputable
import Unique		( Unique, Uniquable(..) )
import CmdLineOpts	( opt_UnboxStrictFields )
import UniqSet
import Maybe
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
	--	dcOrigArgTys   = [a,List b]
	--	dcTyCon    = T

	dcTyVars :: [TyVar], 		-- Type vars and context for the data type decl
	dcTheta  ::  ThetaType,

	dcExTyVars :: [TyVar], 		-- Ditto for the context of the constructor, 
	dcExTheta  :: ThetaType,	-- the existentially quantified stuff
					
	dcOrigArgTys :: [Type],		-- Original argument types
					-- (before unboxing and flattening of
					--  strict fields)
	dcRepArgTys :: [Type],		-- Constructor Argument types
	dcTyCon  :: TyCon,		-- Result tycon 

	-- Now the strictness annotations and field labels of the constructor
	dcUserStricts :: [StrictnessMark], 
		-- Strictness annotations, as placed on the data type defn,
		-- in the same order as the argument types;
		-- length = dataConNumFields dataCon

	dcRealStricts :: [StrictnessMark],
		-- Strictness annotations as deduced by the compiler.  May
		-- include some MarkedUnboxed fields that are MarkedStrict
		-- in dcUserStricts.
		-- length = dataConNumFields dataCon

	dcFields  :: [FieldLabel],
		-- Field labels for this constructor, in the
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


%************************************************************************
%*									*
\subsection{Strictness indication}
%*									*
%************************************************************************

\begin{code}
data StrictnessMark = MarkedStrict
		    | MarkedUnboxed DataCon [Type]
		    | NotMarkedStrict

markedStrict    = MarkedStrict
notMarkedStrict = NotMarkedStrict
markedUnboxed   = MarkedUnboxed (panic "markedUnboxed1") (panic "markedUnboxed2")

maybeMarkedUnboxed (MarkedUnboxed dc tys) = Just (dc,tys)
maybeMarkedUnboxed other		  = Nothing
\end{code}


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
	  -> Id
	  -> DataCon
  -- Can get the tag from the TyCon

mkDataCon name arg_stricts fields tyvars theta ex_tyvars ex_theta orig_arg_tys tycon id
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
		  dcRealStricts = all_stricts, dcUserStricts = user_stricts,
		  dcFields = fields, dcTag = tag, dcTyCon = tycon, dcType = ty,
		  dcId = id}

    (real_arg_stricts, strict_arg_tyss) 
	= unzip (zipWith (unbox_strict_arg_ty tycon) arg_stricts orig_arg_tys)
    rep_arg_tys = concat strict_arg_tyss
	
    ex_dict_stricts = map mk_dict_strict_mark ex_theta
	-- Add a strictness flag for the existential dictionary arguments
    all_stricts     = ex_dict_stricts ++ real_arg_stricts
    user_stricts    = ex_dict_stricts ++ arg_stricts

    tag = assoc "mkDataCon" (tyConDataCons tycon `zip` [fIRST_TAG..]) con
    ty  = mkSigmaTy (tyvars ++ ex_tyvars) 
	            ex_theta
	            (mkFunTys rep_arg_tys 
			(mkTyConApp tycon (mkTyVarTys tyvars)))

mk_dict_strict_mark (clas,tys)
  | opt_DictsStrict &&
	-- Don't mark newtype things as strict!
    isDataTyCon (classTyCon clas) = MarkedStrict
  | otherwise		          = NotMarkedStrict

-- We attempt to unbox/unpack a strict field when either:
--   (i)  The tycon is imported, and the field is marked '! !', or
--   (ii) The tycon is defined in this module, the field is marked '!', 
--	  and the -funbox-strict-fields flag is on.
--
-- This ensures that if we compile some modules with -funbox-strict-fields and
-- some without, the compiler doesn't get confused about the constructor
-- representations.

unbox_strict_arg_ty :: TyCon -> StrictnessMark -> Type -> (StrictnessMark, [Type])
unbox_strict_arg_ty tycon NotMarkedStrict ty 
  = (NotMarkedStrict, [ty])
unbox_strict_arg_ty tycon MarkedStrict ty 
  | not opt_UnboxStrictFields
  || not (isLocallyDefinedName (getName tycon)) = (MarkedStrict, [ty])
unbox_strict_arg_ty tycon marked_unboxed ty
  -- MarkedUnboxed || (MarkedStrict && opt_UnboxStrictFields && not imported)
  = case splitAlgTyConApp_maybe ty of
	Just (tycon,_,[])
	   -> panic (showSDoc (hcat [
			text "unbox_strict_arg_ty: constructors for ",
			ppr tycon,
			text " not available."
		     ]))
	Just (tycon,ty_args,[con]) 
	   -> case maybe_unpack_fields emptyUniqSet 
		     (zip (dataConOrigArgTys con ty_args) 
			  (dcUserStricts con))
	      of 
		 Nothing  -> (MarkedStrict, [ty])
	         Just tys -> (MarkedUnboxed con tys, tys)
	_ -> (MarkedStrict, [ty])

-- bail out if we encounter the same tycon twice.  This avoids problems like
--
--   data A = !B
--   data B = !A
--
-- where no useful unpacking can be done.

maybe_unpack_field :: UniqSet TyCon -> Type -> StrictnessMark -> Maybe [Type]
maybe_unpack_field set ty NotMarkedStrict
  = Just [ty]
maybe_unpack_field set ty MarkedStrict | not opt_UnboxStrictFields
  = Just [ty]
maybe_unpack_field set ty strict
  = case splitAlgTyConApp_maybe ty of
	Just (tycon,ty_args,[con])
		-- loop breaker
	   | tycon `elementOfUniqSet` set -> Nothing
		-- don't unpack constructors with existential tyvars
	   | not (null ex_tyvars) -> Nothing
		-- ok, let's do it
	   | otherwise ->
		let set' = addOneToUniqSet set tycon in
		maybe_unpack_fields set' 
		    (zip (dataConOrigArgTys con ty_args)
			 (dcUserStricts con))
	   where (_, _, ex_tyvars, _, _, _) = dataConSig con
	_ -> Just [ty]

maybe_unpack_fields :: UniqSet TyCon -> [(Type,StrictnessMark)] -> Maybe [Type]
maybe_unpack_fields set tys
  | all isJust unpacked_fields = Just (concat (catMaybes unpacked_fields))
  | otherwise = Nothing
  where unpacked_fields = map (\(ty,str) -> maybe_unpack_field set ty str) tys
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
dataConStrictMarks = dcRealStricts

dataConRawArgTys :: DataCon -> [TauType] -- a function of convenience
dataConRawArgTys = dcRepArgTys

dataConSourceArity :: DataCon -> Arity
	-- Source-level arity of the data constructor
dataConSourceArity dc = length (dcOrigArgTys dc)

dataConRepStrictness :: DataCon -> [Demand]
	-- Give the demands on the arguments of a 
	-- Core constructor application (Con dc args)
dataConRepStrictness dc
  = go (dcRealStricts dc) 
  where
    go []			  = []
    go (MarkedStrict        : ss) = wwStrict : go ss
    go (NotMarkedStrict     : ss) = wwLazy   : go ss
    go (MarkedUnboxed con _ : ss) = go (dcRealStricts con ++ ss)

dataConSig :: DataCon -> ([TyVar], ThetaType, 
			  [TyVar], ThetaType, 
			  [TauType], TyCon)

dataConSig (MkData {dcTyVars = tyvars, dcTheta = theta,
		     dcExTyVars = ex_tyvars, dcExTheta = ex_theta,
		     dcOrigArgTys = arg_tys, dcTyCon = tycon})
  = (tyvars, theta, ex_tyvars, ex_theta, arg_tys, tycon)

dataConArgTys, dataConOrigArgTys :: DataCon 
	      -> [Type] 	-- Instantiated at these types
				-- NB: these INCLUDE the existentially quantified arg types
	      -> [Type]		-- Needs arguments of these types
				-- NB: these INCLUDE the existentially quantified dict args
				--     but EXCLUDE the data-decl context which is discarded

dataConArgTys (MkData {dcRepArgTys = arg_tys, dcTyVars = tyvars, 
		       dcExTyVars = ex_tyvars, dcExTheta = ex_theta}) inst_tys
 = map (substTy (mkTyVarSubst (tyvars ++ ex_tyvars) inst_tys)) 
       ([mkDictTy cls tys | (cls,tys) <- ex_theta] ++ arg_tys)

dataConOrigArgTys (MkData {dcOrigArgTys = arg_tys, dcTyVars = tyvars, 
		       dcExTyVars = ex_tyvars, dcExTheta = ex_theta}) inst_tys
 = map (substTy (mkTyVarSubst (tyvars ++ ex_tyvars) inst_tys)) 
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

dataConNumFields (MkData {dcExTheta = theta, dcRepArgTys = arg_tys})
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
