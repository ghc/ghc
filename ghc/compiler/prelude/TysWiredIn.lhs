%
% (c) The GRASP Project, Glasgow University, 1994-1995
%
\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}

This module is about types that can be defined in Haskell, but which
must be wired into the compiler nonetheless.

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
module TysWiredIn (
	addrDataCon,
	addrTy,
	addrTyCon,
	boolTy,
	boolTyCon,
	charDataCon,
	charTy,
	charTyCon,
	consDataCon,
	doubleDataCon,
	doubleTy,
	isDoubleTy,
	doubleTyCon,
	falseDataCon,
	floatDataCon,
	floatTy,
	isFloatTy,
	floatTyCon,
	getStatePairingConInfo,

	intDataCon,
	intTy,
	intTyCon,
	isIntTy,
	inIntRange,

	int8TyCon,
	int16TyCon,
	int32TyCon,

	int64TyCon,
	int64DataCon,
--	int64Ty,

	integerTy,
	integerTyCon,
	integerDataCon,
	isIntegerTy,

	liftDataCon,
	liftTyCon,
	listTyCon,
	foreignObjTyCon,

	mkLiftTy,
	mkListTy,
	mkTupleTy,
	tupleTyCon, tupleCon, unitTyCon, unitDataCon, pairTyCon, pairDataCon,
	nilDataCon,
	realWorldStateTy,
	return2GMPsTyCon,
	returnIntAndGMPTyCon,

	-- ST and STret types
	mkStateTy,
	mkStateTransformerTy,
	mkSTretTy,
	stTyCon,
	stDataCon,
	stRetDataCon,
	stRetTyCon,

	-- CCall result types
	stateAndAddrPrimTyCon,
	stateAndArrayPrimTyCon,
	stateAndByteArrayPrimTyCon,
	stateAndCharPrimTyCon,
	stateAndDoublePrimTyCon,
	stateAndFloatPrimTyCon,
	stateAndIntPrimTyCon,
	stateAndInt64PrimTyCon,
	stateAndForeignObjPrimTyCon,
	stateAndMutableArrayPrimTyCon,
	stateAndMutableByteArrayPrimTyCon,
	stateAndPtrPrimTyCon,
	stateAndPtrPrimDataCon,
	stateAndStablePtrPrimTyCon,
	stateAndSynchVarPrimTyCon,
	stateAndWordPrimTyCon,
	stateAndWord64PrimTyCon,
	stateDataCon,
	stateTyCon,

	stablePtrTyCon,
	stringTy,
	trueDataCon,
	unitTy,
	wordDataCon,
	wordTy,
	wordTyCon,

	word8TyCon,
	word16TyCon,
	word32TyCon,

	word64DataCon,
--	word64Ty,
	word64TyCon,
	
	isFFIArgumentTy,  -- :: Type -> Bool
	isFFIResultTy,    -- :: Type -> Bool
	isFFIExternalTy,  -- :: Type -> Bool
	isAddrTy,	  -- :: Type -> Bool

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} MkId ( mkDataCon, mkTupleCon )
import {-# SOURCE #-} Id ( Id, StrictnessMark(..) )

-- friends:
import PrelMods
import TysPrim

-- others:
import Kind		( mkBoxedTypeKind, mkArrowKind )
import Name		( mkWiredInTyConName, mkWiredInIdName )
import TyCon		( mkDataTyCon, mkTupleTyCon, mkSynTyCon,
			  TyCon, Arity
			)
import BasicTypes	( Module, NewOrData(..), RecFlag(..) )
import Type		( Type, mkTyConTy, mkTyConApp, mkSigmaTy, mkTyVarTys, 
			  mkFunTy, mkFunTys, splitTyConApp_maybe, splitAlgTyConApp_maybe,
			  GenType(..), ThetaType, TauType, isUnpointedType )
import TyVar		( GenTyVar, TyVar, tyVarKind, alphaTyVars, alphaTyVar, betaTyVar )
import Lex		( mkTupNameStr )
import Unique
import CmdLineOpts      ( opt_GlasgowExts )
import Util		( assoc, panic )


alpha_tyvar	  = [alphaTyVar]
alpha_ty	  = [alphaTy]
alpha_beta_tyvars = [alphaTyVar, betaTyVar]

pcRecDataTyCon, pcNonRecDataTyCon, pcNonRecNewTyCon
	:: Unique{-TyConKey-} -> Module -> FAST_STRING
	-> [TyVar] -> [Id] -> TyCon

pcRecDataTyCon    = pc_tycon DataType Recursive
pcNonRecDataTyCon = pc_tycon DataType NonRecursive
pcNonRecNewTyCon  = pc_tycon NewType  NonRecursive

pc_tycon new_or_data is_rec key mod str tyvars cons
  = tycon
  where
    tycon = mkDataTyCon name tycon_kind 
		tyvars 
		[] 		-- No context
		cons
		[]		-- No derivings
		Nothing		-- Not a dictionary
		new_or_data
		is_rec

    name = mkWiredInTyConName key mod str tycon
    tycon_kind = foldr (mkArrowKind . tyVarKind) mkBoxedTypeKind tyvars

pcSynTyCon key mod str kind arity tyvars expansion
  = tycon
  where
    tycon = mkSynTyCon name kind arity tyvars expansion
    name  = mkWiredInTyConName key mod str tycon

pcDataCon :: Unique{-DataConKey-} -> Module -> FAST_STRING
	  -> [TyVar] -> ThetaType -> [TauType] -> TyCon -> Id
pcDataCon key mod str tyvars context arg_tys tycon
  = data_con
  where
    data_con = mkDataCon name 
		[ NotMarkedStrict | a <- arg_tys ]
		[ {- no labelled fields -} ]
		tyvars context [] [] arg_tys tycon
    name = mkWiredInIdName key mod str data_con
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-tuples]{The tuple types}
%*									*
%************************************************************************

\begin{code}
tupleTyCon :: Arity -> TyCon
tupleTyCon arity
  = tycon
  where
    tycon = mkTupleTyCon uniq name arity
    uniq  = mkTupleTyConUnique arity
    name  = mkWiredInTyConName uniq mod_name (mkTupNameStr arity) tycon
    mod_name | arity == 0 = pREL_BASE
	     | otherwise  = pREL_TUP 

tupleCon :: Arity -> Id
tupleCon arity
  = tuple_con
  where
    tuple_con = mkTupleCon arity name ty
    uniq      = mkTupleDataConUnique arity
    name      = mkWiredInIdName uniq mod_name (mkTupNameStr arity) tuple_con
    mod_name  | arity == 0 = pREL_BASE
	      | otherwise  = pREL_TUP
    ty 		= mkSigmaTy tyvars [] (mkFunTys tyvar_tys (mkTyConApp tycon tyvar_tys))
    tyvars	= take arity alphaTyVars
    tyvar_tys	= mkTyVarTys tyvars
    tycon	= tupleTyCon arity

unitTyCon = tupleTyCon 0
pairTyCon = tupleTyCon 2

unitDataCon = tupleCon 0
pairDataCon = tupleCon 2
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-boxed-prim]{The ``boxed primitive'' types (@Char@, @Int@, etc)}
%*									*
%************************************************************************

\begin{code}
charTy = mkTyConTy charTyCon

charTyCon = pcNonRecDataTyCon charTyConKey  pREL_BASE  SLIT("Char") [] [charDataCon]
charDataCon = pcDataCon charDataConKey pREL_BASE SLIT("C#") [] [] [charPrimTy] charTyCon

stringTy = mkListTy charTy -- convenience only
\end{code}

\begin{code}
intTy = mkTyConTy intTyCon 

intTyCon = pcNonRecDataTyCon intTyConKey pREL_BASE SLIT("Int") [] [intDataCon]
intDataCon = pcDataCon intDataConKey pREL_BASE SLIT("I#") [] [] [intPrimTy] intTyCon

isIntTy :: GenType flexi -> Bool
isIntTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> uniqueOf tycon == intTyConKey
	_		    -> False

inIntRange :: Integer -> Bool	-- Tells if an integer lies in the legal range of Ints
inIntRange i = (min_int <= i) && (i <= max_int)

max_int, min_int :: Integer
max_int = toInteger maxInt  
min_int = toInteger minInt

int8TyCon = pcNonRecDataTyCon int8TyConKey iNT SLIT("Int8") [] [int8DataCon]
  where
   int8DataCon = pcDataCon int8DataConKey iNT SLIT("I8#") [] [] [intPrimTy] int8TyCon

int16TyCon = pcNonRecDataTyCon int16TyConKey iNT SLIT("Int16") [] [int16DataCon]
  where
   int16DataCon = pcDataCon int16DataConKey iNT SLIT("I16#") [] [] [intPrimTy] int16TyCon

int32TyCon = pcNonRecDataTyCon int32TyConKey iNT SLIT("Int32") [] [int32DataCon]
  where
   int32DataCon = pcDataCon int32DataConKey iNT SLIT("I32#") [] [] [intPrimTy] int32TyCon

int64Ty = mkTyConTy int64TyCon 

int64TyCon = pcNonRecDataTyCon int64TyConKey pREL_CCALL SLIT("Int64") [] [int64DataCon]
int64DataCon = pcDataCon int64DataConKey pREL_CCALL SLIT("I64#") [] [] [int64PrimTy] int64TyCon
\end{code}

\begin{code}

wordTy = mkTyConTy wordTyCon

wordTyCon = pcNonRecDataTyCon wordTyConKey   pREL_FOREIGN SLIT("Word") [] [wordDataCon]
wordDataCon = pcDataCon wordDataConKey pREL_FOREIGN SLIT("W#") [] [] [wordPrimTy] wordTyCon

word8TyCon = pcNonRecDataTyCon word8TyConKey   wORD SLIT("Word8") [] [word8DataCon]
  where
   word8DataCon = pcDataCon word8DataConKey wORD SLIT("W8#") [] [] [wordPrimTy] word8TyCon

word16TyCon = pcNonRecDataTyCon word16TyConKey   wORD SLIT("Word16") [] [word16DataCon]
  where
   word16DataCon = pcDataCon word16DataConKey wORD SLIT("W16#") [] [] [wordPrimTy] word16TyCon

word32TyCon = pcNonRecDataTyCon word32TyConKey   wORD SLIT("Word32") [] [word32DataCon]
  where
   word32DataCon = pcDataCon word32DataConKey wORD SLIT("W32#") [] [] [wordPrimTy] word32TyCon

word64Ty = mkTyConTy word64TyCon

word64TyCon = pcNonRecDataTyCon word64TyConKey   pREL_CCALL SLIT("Word64") [] [word64DataCon]
word64DataCon = pcDataCon word64DataConKey pREL_CCALL SLIT("W64#") [] [] [word64PrimTy] word64TyCon
\end{code}

\begin{code}
addrTy = mkTyConTy addrTyCon

addrTyCon = pcNonRecDataTyCon addrTyConKey   pREL_ADDR SLIT("Addr") [] [addrDataCon]
addrDataCon = pcDataCon addrDataConKey pREL_ADDR SLIT("A#") [] [] [addrPrimTy] addrTyCon

isAddrTy :: GenType flexi -> Bool
isAddrTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> uniqueOf tycon == addrTyConKey
	_		    -> False

\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon = pcNonRecDataTyCon floatTyConKey pREL_BASE SLIT("Float") [] [floatDataCon]
floatDataCon = pcDataCon floatDataConKey pREL_BASE SLIT("F#") [] [] [floatPrimTy] floatTyCon

isFloatTy :: GenType flexi -> Bool
isFloatTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> uniqueOf tycon == floatTyConKey
	_		    -> False

\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

isDoubleTy :: GenType flexi -> Bool
isDoubleTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> uniqueOf tycon == doubleTyConKey
	_		    -> False

doubleTyCon = pcNonRecDataTyCon doubleTyConKey pREL_BASE SLIT("Double") [] [doubleDataCon]
doubleDataCon = pcDataCon doubleDataConKey pREL_BASE SLIT("D#") [] [] [doublePrimTy] doubleTyCon
\end{code}

\begin{code}
mkStateTy ty	 = mkTyConApp stateTyCon [ty]
realWorldStateTy = mkStateTy realWorldTy -- a common use

stateTyCon = pcNonRecDataTyCon stateTyConKey pREL_ST SLIT("State") alpha_tyvar [stateDataCon]
stateDataCon
  = pcDataCon stateDataConKey pREL_ST SLIT("S#")
	alpha_tyvar [] [mkStatePrimTy alphaTy] stateTyCon
\end{code}

\begin{code}
stablePtrTyCon
  = pcNonRecDataTyCon stablePtrTyConKey pREL_FOREIGN SLIT("StablePtr")
	alpha_tyvar [stablePtrDataCon]
  where
    stablePtrDataCon
      = pcDataCon stablePtrDataConKey pREL_FOREIGN SLIT("StablePtr")
	    alpha_tyvar [] [mkStablePtrPrimTy alphaTy] stablePtrTyCon
\end{code}

\begin{code}
foreignObjTyCon
  = pcNonRecDataTyCon foreignObjTyConKey pREL_IO_BASE SLIT("ForeignObj")
	[] [foreignObjDataCon]
  where
    foreignObjDataCon
      = pcDataCon foreignObjDataConKey pREL_IO_BASE SLIT("ForeignObj")
	    [] [] [foreignObjPrimTy] foreignObjTyCon
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Integer]{@Integer@ and its related ``pairing'' types}
%*									*
%************************************************************************

@Integer@ and its pals are not really primitive.  @Integer@ itself, first:
\begin{code}
integerTy :: GenType t
integerTy    = mkTyConTy integerTyCon

integerTyCon = pcNonRecDataTyCon integerTyConKey pREL_BASE SLIT("Integer") [] [integerDataCon]

integerDataCon = pcDataCon integerDataConKey pREL_BASE SLIT("J#")
		[] [] [intPrimTy, intPrimTy, byteArrayPrimTy] integerTyCon

isIntegerTy :: GenType flexi -> Bool
isIntegerTy ty
  = case (splitAlgTyConApp_maybe ty) of
	Just (tycon, [], _) -> uniqueOf tycon == integerTyConKey
	_		    -> False
\end{code}

And the other pairing types:
\begin{code}
return2GMPsTyCon = pcNonRecDataTyCon return2GMPsTyConKey
	pREL_NUM SLIT("Return2GMPs") [] [return2GMPsDataCon]

return2GMPsDataCon
  = pcDataCon return2GMPsDataConKey pREL_NUM SLIT("Return2GMPs") [] []
	[intPrimTy, intPrimTy, byteArrayPrimTy,
	 intPrimTy, intPrimTy, byteArrayPrimTy] return2GMPsTyCon

returnIntAndGMPTyCon = pcNonRecDataTyCon returnIntAndGMPTyConKey
	pREL_NUM SLIT("ReturnIntAndGMP") [] [returnIntAndGMPDataCon]

returnIntAndGMPDataCon
  = pcDataCon returnIntAndGMPDataConKey pREL_NUM SLIT("ReturnIntAndGMP") [] []
	[intPrimTy, intPrimTy, intPrimTy, byteArrayPrimTy] returnIntAndGMPTyCon
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-state-pairing]{``State-pairing'' types}
%*									*
%************************************************************************

These boring types pair a \tr{State#} with another primitive type.
They are not really primitive, so they are given here, not in
\tr{TysPrim.lhs}.

We fish one of these \tr{StateAnd<blah>#} things with
@getStatePairingConInfo@ (given a little way down).

\begin{code}
stateAndPtrPrimTyCon
  = pcNonRecDataTyCon stateAndPtrPrimTyConKey pREL_ST SLIT("StateAndPtr#")
		alpha_beta_tyvars [stateAndPtrPrimDataCon]
stateAndPtrPrimDataCon
  = pcDataCon stateAndPtrPrimDataConKey pREL_ST SLIT("StateAndPtr#")
		alpha_beta_tyvars [] [mkStatePrimTy alphaTy, betaTy]
		stateAndPtrPrimTyCon

stateAndCharPrimTyCon
  = pcNonRecDataTyCon stateAndCharPrimTyConKey pREL_ST SLIT("StateAndChar#")
		alpha_tyvar [stateAndCharPrimDataCon]
stateAndCharPrimDataCon
  = pcDataCon stateAndCharPrimDataConKey pREL_ST SLIT("StateAndChar#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, charPrimTy]
		stateAndCharPrimTyCon

stateAndIntPrimTyCon
  = pcNonRecDataTyCon stateAndIntPrimTyConKey pREL_ST SLIT("StateAndInt#")
		alpha_tyvar [stateAndIntPrimDataCon]
stateAndIntPrimDataCon
  = pcDataCon stateAndIntPrimDataConKey pREL_ST SLIT("StateAndInt#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, intPrimTy]
		stateAndIntPrimTyCon

stateAndInt64PrimTyCon
  = pcNonRecDataTyCon stateAndInt64PrimTyConKey pREL_ST SLIT("StateAndInt64#")
		alpha_tyvar [stateAndInt64PrimDataCon]
stateAndInt64PrimDataCon
  = pcDataCon stateAndInt64PrimDataConKey pREL_ST SLIT("StateAndInt64#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, int64PrimTy]
		stateAndInt64PrimTyCon

stateAndWordPrimTyCon
  = pcNonRecDataTyCon stateAndWordPrimTyConKey pREL_ST SLIT("StateAndWord#")
		alpha_tyvar [stateAndWordPrimDataCon]
stateAndWordPrimDataCon
  = pcDataCon stateAndWordPrimDataConKey pREL_ST SLIT("StateAndWord#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, wordPrimTy]
		stateAndWordPrimTyCon

stateAndWord64PrimTyCon
  = pcNonRecDataTyCon stateAndWord64PrimTyConKey pREL_ST SLIT("StateAndWord64#")
		alpha_tyvar [stateAndWord64PrimDataCon]
stateAndWord64PrimDataCon
  = pcDataCon stateAndWord64PrimDataConKey pREL_ST SLIT("StateAndWord64#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, word64PrimTy]
		stateAndWord64PrimTyCon

stateAndAddrPrimTyCon
  = pcNonRecDataTyCon stateAndAddrPrimTyConKey pREL_ST SLIT("StateAndAddr#")
		alpha_tyvar [stateAndAddrPrimDataCon]
stateAndAddrPrimDataCon
  = pcDataCon stateAndAddrPrimDataConKey pREL_ST SLIT("StateAndAddr#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, addrPrimTy]
		stateAndAddrPrimTyCon

stateAndStablePtrPrimTyCon
  = pcNonRecDataTyCon stateAndStablePtrPrimTyConKey pREL_FOREIGN SLIT("StateAndStablePtr#")
		alpha_beta_tyvars [stateAndStablePtrPrimDataCon]
stateAndStablePtrPrimDataCon
  = pcDataCon stateAndStablePtrPrimDataConKey pREL_FOREIGN SLIT("StateAndStablePtr#")
		alpha_beta_tyvars []
		[mkStatePrimTy alphaTy, mkTyConApp stablePtrPrimTyCon [betaTy]]
		stateAndStablePtrPrimTyCon

stateAndForeignObjPrimTyCon
  = pcNonRecDataTyCon stateAndForeignObjPrimTyConKey pREL_IO_BASE SLIT("StateAndForeignObj#")
		alpha_tyvar [stateAndForeignObjPrimDataCon]
stateAndForeignObjPrimDataCon
  = pcDataCon stateAndForeignObjPrimDataConKey pREL_IO_BASE SLIT("StateAndForeignObj#")
		alpha_tyvar []
		[mkStatePrimTy alphaTy, mkTyConTy foreignObjPrimTyCon]
		stateAndForeignObjPrimTyCon

stateAndFloatPrimTyCon
  = pcNonRecDataTyCon stateAndFloatPrimTyConKey pREL_ST SLIT("StateAndFloat#")
		alpha_tyvar [stateAndFloatPrimDataCon]
stateAndFloatPrimDataCon
  = pcDataCon stateAndFloatPrimDataConKey pREL_ST SLIT("StateAndFloat#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, floatPrimTy]
		stateAndFloatPrimTyCon

stateAndDoublePrimTyCon
  = pcNonRecDataTyCon stateAndDoublePrimTyConKey pREL_ST SLIT("StateAndDouble#")
		alpha_tyvar [stateAndDoublePrimDataCon]
stateAndDoublePrimDataCon
  = pcDataCon stateAndDoublePrimDataConKey pREL_ST SLIT("StateAndDouble#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, doublePrimTy]
		stateAndDoublePrimTyCon
\end{code}

\begin{code}
stateAndArrayPrimTyCon
  = pcNonRecDataTyCon stateAndArrayPrimTyConKey pREL_ARR SLIT("StateAndArray#")
		alpha_beta_tyvars [stateAndArrayPrimDataCon]
stateAndArrayPrimDataCon
  = pcDataCon stateAndArrayPrimDataConKey pREL_ARR SLIT("StateAndArray#")
		alpha_beta_tyvars [] [mkStatePrimTy alphaTy, mkArrayPrimTy betaTy]
		stateAndArrayPrimTyCon

stateAndMutableArrayPrimTyCon
  = pcNonRecDataTyCon stateAndMutableArrayPrimTyConKey pREL_ARR SLIT("StateAndMutableArray#")
		alpha_beta_tyvars [stateAndMutableArrayPrimDataCon]
stateAndMutableArrayPrimDataCon
  = pcDataCon stateAndMutableArrayPrimDataConKey pREL_ARR SLIT("StateAndMutableArray#")
		alpha_beta_tyvars [] [mkStatePrimTy alphaTy, mkMutableArrayPrimTy alphaTy betaTy]
		stateAndMutableArrayPrimTyCon

stateAndByteArrayPrimTyCon
  = pcNonRecDataTyCon stateAndByteArrayPrimTyConKey pREL_ARR SLIT("StateAndByteArray#")
		alpha_tyvar [stateAndByteArrayPrimDataCon]
stateAndByteArrayPrimDataCon
  = pcDataCon stateAndByteArrayPrimDataConKey pREL_ARR SLIT("StateAndByteArray#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, byteArrayPrimTy]
		stateAndByteArrayPrimTyCon

stateAndMutableByteArrayPrimTyCon
  = pcNonRecDataTyCon stateAndMutableByteArrayPrimTyConKey pREL_ARR SLIT("StateAndMutableByteArray#")
		alpha_tyvar [stateAndMutableByteArrayPrimDataCon]
stateAndMutableByteArrayPrimDataCon
  = pcDataCon stateAndMutableByteArrayPrimDataConKey pREL_ARR SLIT("StateAndMutableByteArray#")
		alpha_tyvar [] [mkStatePrimTy alphaTy, mkTyConApp mutableByteArrayPrimTyCon alpha_ty]
		stateAndMutableByteArrayPrimTyCon

stateAndSynchVarPrimTyCon
  = pcNonRecDataTyCon stateAndSynchVarPrimTyConKey pREL_CONC SLIT("StateAndSynchVar#")
		alpha_beta_tyvars [stateAndSynchVarPrimDataCon]
stateAndSynchVarPrimDataCon
  = pcDataCon stateAndSynchVarPrimDataConKey pREL_CONC SLIT("StateAndSynchVar#")
		alpha_beta_tyvars [] [mkStatePrimTy alphaTy, mkSynchVarPrimTy alphaTy betaTy]
		stateAndSynchVarPrimTyCon
\end{code}

The ccall-desugaring mechanism uses this function to figure out how to
rebox the result.  It's really a HACK, especially the part about
how many types to drop from \tr{tys_applied}.

\begin{code}
getStatePairingConInfo
	:: Type	-- primitive type
	-> (Id,		-- state pair constructor for prim type
	    Type)	-- type of state pair

getStatePairingConInfo prim_ty
  = case (splitTyConApp_maybe prim_ty) of
      Nothing -> panic "getStatePairingConInfo:1"
      Just (prim_tycon, tys_applied) ->
	let
	    (pair_con, pair_tycon, num_tys) = assoc "getStatePairingConInfo" tbl prim_tycon
	    pair_ty = mkTyConApp pair_tycon (realWorldTy : drop num_tys tys_applied)
	in
	(pair_con, pair_ty)
  where
    tbl = [
	(charPrimTyCon, (stateAndCharPrimDataCon, stateAndCharPrimTyCon, 0)),
	(intPrimTyCon, (stateAndIntPrimDataCon, stateAndIntPrimTyCon, 0)),
	(wordPrimTyCon, (stateAndWordPrimDataCon, stateAndWordPrimTyCon, 0)),
	(int64PrimTyCon, (stateAndInt64PrimDataCon, stateAndInt64PrimTyCon, 0)),
	(word64PrimTyCon, (stateAndWord64PrimDataCon, stateAndWord64PrimTyCon, 0)),
	(addrPrimTyCon, (stateAndAddrPrimDataCon, stateAndAddrPrimTyCon, 0)),
	(stablePtrPrimTyCon, (stateAndStablePtrPrimDataCon, stateAndStablePtrPrimTyCon, 0)),
	(foreignObjPrimTyCon, (stateAndForeignObjPrimDataCon, stateAndForeignObjPrimTyCon, 0)),
	(floatPrimTyCon, (stateAndFloatPrimDataCon, stateAndFloatPrimTyCon, 0)),
	(doublePrimTyCon, (stateAndDoublePrimDataCon, stateAndDoublePrimTyCon, 0)),
	(arrayPrimTyCon, (stateAndArrayPrimDataCon, stateAndArrayPrimTyCon, 0)),
	(mutableArrayPrimTyCon, (stateAndMutableArrayPrimDataCon, stateAndMutableArrayPrimTyCon, 1)),
	(byteArrayPrimTyCon, (stateAndByteArrayPrimDataCon, stateAndByteArrayPrimTyCon, 0)),
	(mutableByteArrayPrimTyCon, (stateAndMutableByteArrayPrimDataCon, stateAndMutableByteArrayPrimTyCon, 1)),
	(synchVarPrimTyCon, (stateAndSynchVarPrimDataCon, stateAndSynchVarPrimTyCon, 1))
	-- (PtrPrimTyCon, (stateAndPtrPrimDataCon, stateAndPtrPrimTyCon, 0)),
	]
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-ext-type]{External types}
%*									*
%************************************************************************

The compiler's foreign function interface supports the passing of a
restricted set of types as arguments and results (the restricting factor
being the )

\begin{code}
isFFIArgumentTy :: Type -> Bool
isFFIArgumentTy ty =
  (opt_GlasgowExts && isUnpointedType ty) || --leave out for now: maybeToBool (maybeBoxedPrimType ty))) ||
  case (splitAlgTyConApp_maybe ty) of
    Just (tycon, _, _) -> (uniqueOf tycon) `elem` primArgTyConKeys
    _		       -> False

-- types that can be passed as arguments to "foreign" functions
primArgTyConKeys 
  = [ intTyConKey, int8TyConKey, int16TyConKey, int32TyConKey, int64TyConKey
    , wordTyConKey, word8TyConKey, word16TyConKey, word32TyConKey, word64TyConKey
    , floatTyConKey, doubleTyConKey
    , addrTyConKey, charTyConKey, foreignObjTyConKey
    , stablePtrTyConKey, byteArrayTyConKey, mutableByteArrayTyConKey
    ]

-- types that can be passed from the outside world into Haskell.
-- excludes (mutable) byteArrays.
isFFIExternalTy :: Type -> Bool
isFFIExternalTy ty = 
  (opt_GlasgowExts && isUnpointedType ty) || --leave out for now: maybeToBool (maybeBoxedPrimType ty))) ||
  case (splitAlgTyConApp_maybe ty) of
    Just (tycon, _, _) -> 
       let 
        u_tycon = uniqueOf tycon
       in  
       (u_tycon `elem` primArgTyConKeys) &&
       not (u_tycon `elem` notLegalExternalTyCons)
    _		       -> False


isFFIResultTy :: Type -> Bool
isFFIResultTy ty =
   not (isUnpointedType ty) &&
   case (splitAlgTyConApp_maybe ty) of
    Just (tycon, _, _) -> 
	let
	 u_tycon = uniqueOf tycon
	in
	(u_tycon == uniqueOf unitTyCon) ||
        ((u_tycon `elem` primArgTyConKeys) && 
	 not (u_tycon `elem` notLegalExternalTyCons))
    _		       -> False

-- it's illegal to return foreign objects and (mutable)
-- bytearrays from a _ccall_ / foreign declaration
-- (or be passed them as arguments in foreign exported functions).
notLegalExternalTyCons =
  [ foreignObjTyConKey, byteArrayTyConKey, mutableByteArrayTyConKey ]
    
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-ST]{The basic @_ST@ state-transformer type}
%*									*
%************************************************************************

The only reason this is wired in is because we have to represent the
type of runST.

\begin{code}
mkStateTransformerTy s a = mkTyConApp stTyCon [s, a]

stTyCon = pcNonRecNewTyCon stTyConKey pREL_ST SLIT("ST") alpha_beta_tyvars [stDataCon]

stDataCon = pcDataCon stDataConKey pREL_ST SLIT("ST")
			alpha_beta_tyvars [] [ty] stTyCon
  where
    ty = mkFunTy (mkStatePrimTy alphaTy) (mkSTretTy alphaTy betaTy)

mkSTretTy alpha beta = mkTyConApp stRetTyCon [alpha,beta]

stRetTyCon
  = pcNonRecDataTyCon stRetTyConKey pREL_ST SLIT("STret") 
	alpha_beta_tyvars [stRetDataCon]
stRetDataCon
  = pcDataCon stRetDataConKey pREL_ST SLIT("STret")
	alpha_beta_tyvars [] [mkStatePrimTy alphaTy, betaTy] 
		stRetTyCon
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Bool]{The @Bool@ type}
%*									*
%************************************************************************

An ordinary enumeration type, but deeply wired in.  There are no
magical operations on @Bool@ (just the regular Prelude code).

{\em BEGIN IDLE SPECULATION BY SIMON}

This is not the only way to encode @Bool@.  A more obvious coding makes
@Bool@ just a boxed up version of @Bool#@, like this:
\begin{verbatim}
type Bool# = Int#
data Bool = MkBool Bool#
\end{verbatim}

Unfortunately, this doesn't correspond to what the Report says @Bool@
looks like!  Furthermore, we get slightly less efficient code (I
think) with this coding. @gtInt@ would look like this:

\begin{verbatim}
gtInt :: Int -> Int -> Bool
gtInt x y = case x of I# x# ->
	    case y of I# y# ->
	    case (gtIntPrim x# y#) of
		b# -> MkBool b#
\end{verbatim}

Notice that the result of the @gtIntPrim@ comparison has to be turned
into an integer (here called @b#@), and returned in a @MkBool@ box.

The @if@ expression would compile to this:
\begin{verbatim}
case (gtInt x y) of
  MkBool b# -> case b# of { 1# -> e1; 0# -> e2 }
\end{verbatim}

I think this code is a little less efficient than the previous code,
but I'm not certain.  At all events, corresponding with the Report is
important.  The interesting thing is that the language is expressive
enough to describe more than one alternative; and that a type doesn't
necessarily need to be a straightforwardly boxed version of its
primitive counterpart.

{\em END IDLE SPECULATION BY SIMON}

\begin{code}
boolTy = mkTyConTy boolTyCon

boolTyCon = pcNonRecDataTyCon boolTyConKey pREL_BASE SLIT("Bool") [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConKey pREL_BASE SLIT("False") [] [] [] boolTyCon
trueDataCon  = pcDataCon trueDataConKey	 pREL_BASE SLIT("True")  [] [] [] boolTyCon
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-List]{The @List@ type (incl ``build'' magic)}
%*									*
%************************************************************************

Special syntax, deeply wired in, but otherwise an ordinary algebraic
data types:
\begin{verbatim}
data [] a = [] | a : (List a)
data () = ()
data (,) a b = (,,) a b
...
\end{verbatim}

\begin{code}
mkListTy :: GenType t -> GenType t
mkListTy ty = mkTyConApp listTyCon [ty]

alphaListTy = mkSigmaTy alpha_tyvar [] (mkTyConApp listTyCon alpha_ty)

listTyCon = pcRecDataTyCon listTyConKey pREL_BASE SLIT("[]") 
			alpha_tyvar [nilDataCon, consDataCon]

nilDataCon  = pcDataCon nilDataConKey  pREL_BASE SLIT("[]") alpha_tyvar [] [] listTyCon
consDataCon = pcDataCon consDataConKey pREL_BASE SLIT(":")
		alpha_tyvar [] [alphaTy, mkTyConApp listTyCon alpha_ty] listTyCon
-- Interesting: polymorphic recursion would help here.
-- We can't use (mkListTy alphaTy) in the defn of consDataCon, else mkListTy
-- gets the over-specific type (Type -> Type)
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Tuples]{The @Tuple@ types}
%*									*
%************************************************************************

The tuple types are definitely magic, because they form an infinite
family.

\begin{itemize}
\item
They have a special family of type constructors, of type @TyCon@
These contain the tycon arity, but don't require a Unique.

\item
They have a special family of constructors, of type
@Id@. Again these contain their arity but don't need a Unique.

\item
There should be a magic way of generating the info tables and
entry code for all tuples.

But at the moment we just compile a Haskell source
file\srcloc{lib/prelude/...} containing declarations like:
\begin{verbatim}
data Tuple0		= Tup0
data Tuple2  a b	= Tup2	a b
data Tuple3  a b c	= Tup3	a b c
data Tuple4  a b c d	= Tup4	a b c d
...
\end{verbatim}
The print-names associated with the magic @Id@s for tuple constructors
``just happen'' to be the same as those generated by these
declarations.

\item
The instance environment should have a magic way to know
that each tuple type is an instances of classes @Eq@, @Ix@, @Ord@ and
so on. \ToDo{Not implemented yet.}

\item
There should also be a way to generate the appropriate code for each
of these instances, but (like the info tables and entry code) it is
done by enumeration\srcloc{lib/prelude/InTup?.hs}.
\end{itemize}

\begin{code}
mkTupleTy :: Int -> [GenType t] -> GenType t

mkTupleTy arity tys = mkTyConApp (tupleTyCon arity) tys

unitTy    = mkTupleTy 0 []
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-_Lift]{@_Lift@ type: to support array indexing}
%*									*
%************************************************************************

Again, deeply turgid: \tr{data _Lift a = _Lift a}.

\begin{code}
mkLiftTy ty = mkTyConApp liftTyCon [ty]

{-
mkLiftTy ty
  = mkSigmaTy tvs theta (mkTyConApp liftTyCon [tau])
  where
    (tvs, theta, tau) = splitSigmaTy ty

isLiftTy ty
  = case (splitAlgTyConApp_maybeExpandingDicts tau) of
      Just (tycon, tys, _) -> tycon == liftTyCon
      Nothing -> False
  where
    (tvs, theta, tau) = splitSigmaTy ty
-}


alphaLiftTy = mkSigmaTy alpha_tyvar [] (mkTyConApp liftTyCon alpha_ty)

liftTyCon
  = pcNonRecDataTyCon liftTyConKey pREL_BASE SLIT("Lift") alpha_tyvar [liftDataCon]

liftDataCon
  = pcDataCon liftDataConKey pREL_BASE SLIT("Lift")
		alpha_tyvar [] alpha_ty liftTyCon
  where
    bottom = panic "liftDataCon:State# _RealWorld"
\end{code}
