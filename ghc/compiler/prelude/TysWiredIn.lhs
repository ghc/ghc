%
% (c) The GRASP Project, Glasgow University, 1994-1995
%
\section[TysWiredIn]{Wired-in knowledge about {\em non-primitive} types}

This module is about types that can be defined in Haskell, but which
must be wired into the compiler nonetheless.

This module tracks the ``state interface'' document, ``GHC prelude:
types and operations.''

\begin{code}
#include "HsVersions.h"

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
	doubleTyCon,
	eqDataCon,
	falseDataCon,
	floatDataCon,
	floatTy,
	floatTyCon,
	getStatePairingConInfo,
	gtDataCon,
	intDataCon,
	intTy,
	intTyCon,
	integerTy,
	integerTyCon,
	integerDataCon,
	liftDataCon,
	liftTyCon,
	listTyCon,
	ltDataCon,
	mallocPtrTyCon,
	mkLiftTy,
	mkListTy,
	mkPrimIoTy,
	mkStateTransformerTy,
	mkTupleTy,
	nilDataCon,
	orderingTy,
	orderingTyCon,
	primIoTyCon,
	ratioDataCon,
	ratioTyCon,
	rationalTy,
	rationalTyCon,
	realWorldStateTy,
	return2GMPsTyCon,
	returnIntAndGMPTyCon,
	stTyCon,
	stablePtrTyCon,
	stateAndAddrPrimTyCon,
	stateAndArrayPrimTyCon,
	stateAndByteArrayPrimTyCon,
	stateAndCharPrimTyCon,
	stateAndDoublePrimTyCon,
	stateAndFloatPrimTyCon,
	stateAndIntPrimTyCon,
	stateAndMallocPtrPrimTyCon,
	stateAndMutableArrayPrimTyCon,
	stateAndMutableByteArrayPrimTyCon,
	stateAndPtrPrimTyCon,
	stateAndStablePtrPrimTyCon,
	stateAndSynchVarPrimTyCon,
	stateAndWordPrimTyCon,
	stateDataCon,
	stateTyCon,
	stringTy,
	stringTyCon,
	trueDataCon,
	unitTy,
	voidTy, voidTyCon,
	wordDataCon,
	wordTy,
	wordTyCon

    ) where

--ToDo:rm
--import Pretty
--import Util
--import PprType
--import PprStyle
--import Kind

import Ubiq
import TyLoop		( mkDataCon, StrictnessMark(..) )

-- friends:
import PrelMods
import TysPrim

-- others:
import SpecEnv		( SpecEnv(..) )
import Kind		( mkBoxedTypeKind, mkArrowKind )
import Name		( mkBuiltinName )
import SrcLoc		( mkBuiltinSrcLoc )
import TyCon		( mkDataTyCon, mkTupleTyCon, mkSynTyCon,
			  NewOrData(..), TyCon
			)
import Type		( mkTyConTy, applyTyCon, mkSynTy, mkSigmaTy,
			  mkFunTys, maybeAppDataTyConExpandingDicts,
			  GenType(..), ThetaType(..), TauType(..) )
import TyVar		( tyVarKind, alphaTyVar, betaTyVar )
import Unique
import Util		( assoc, panic )

nullSpecEnv =  error "TysWiredIn:nullSpecEnv =  "
addOneToSpecEnv =  error "TysWiredIn:addOneToSpecEnv =  "
pc_gen_specs = error "TysWiredIn:pc_gen_specs  "
mkSpecInfo = error "TysWiredIn:SpecInfo"

pcDataTyCon :: Unique{-TyConKey-} -> Module -> FAST_STRING
            -> [TyVar] -> [Id] -> TyCon
pcDataTyCon key mod str tyvars cons
  = mkDataTyCon (mkBuiltinName key mod str) tycon_kind 
		tyvars [{-no context-}] cons [{-no derivings-}]
		DataType
  where
    tycon_kind = foldr (mkArrowKind . tyVarKind) mkBoxedTypeKind tyvars

pcDataCon :: Unique{-DataConKey-} -> Module -> FAST_STRING
	  -> [TyVar] -> ThetaType -> [TauType] -> TyCon -> SpecEnv -> Id
pcDataCon key mod str tyvars context arg_tys tycon specenv
  = mkDataCon (mkBuiltinName key mod str)
	[ NotMarkedStrict | a <- arg_tys ]
	[ {- no labelled fields -} ]
	tyvars context arg_tys tycon
	-- specenv

pcGenerateDataSpecs :: Type -> SpecEnv
pcGenerateDataSpecs ty
  = pc_gen_specs False err err err ty
  where
    err = panic "PrelUtils:GenerateDataSpecs"
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-boxed-prim]{The ``boxed primitive'' types (@Char@, @Int@, etc)}
%*									*
%************************************************************************

\begin{code}
-- The Void type is represented as a data type with no constructors
voidTy = mkTyConTy voidTyCon

voidTyCon = pcDataTyCon voidTyConKey pRELUDE_BUILTIN SLIT("Void") [] []
\end{code}

\begin{code}
charTy = mkTyConTy charTyCon

charTyCon = pcDataTyCon charTyConKey pRELUDE_BUILTIN SLIT("Char") [] [charDataCon]
charDataCon = pcDataCon charDataConKey pRELUDE_BUILTIN SLIT("C#") [] [] [charPrimTy] charTyCon nullSpecEnv
\end{code}

\begin{code}
intTy = mkTyConTy intTyCon 

intTyCon = pcDataTyCon intTyConKey pRELUDE_BUILTIN SLIT("Int") [] [intDataCon]
intDataCon = pcDataCon intDataConKey pRELUDE_BUILTIN SLIT("I#") [] [] [intPrimTy] intTyCon nullSpecEnv
\end{code}

\begin{code}
wordTy = mkTyConTy wordTyCon

wordTyCon = pcDataTyCon wordTyConKey pRELUDE_BUILTIN SLIT("_Word") [] [wordDataCon]
wordDataCon = pcDataCon wordDataConKey pRELUDE_BUILTIN SLIT("W#") [] [] [wordPrimTy] wordTyCon nullSpecEnv
\end{code}

\begin{code}
addrTy = mkTyConTy addrTyCon

addrTyCon = pcDataTyCon addrTyConKey pRELUDE_BUILTIN SLIT("_Addr") [] [addrDataCon]
addrDataCon = pcDataCon addrDataConKey pRELUDE_BUILTIN SLIT("A#") [] [] [addrPrimTy] addrTyCon nullSpecEnv
\end{code}

\begin{code}
floatTy	= mkTyConTy floatTyCon

floatTyCon = pcDataTyCon floatTyConKey pRELUDE_BUILTIN SLIT("Float") [] [floatDataCon]
floatDataCon = pcDataCon floatDataConKey pRELUDE_BUILTIN SLIT("F#") [] [] [floatPrimTy] floatTyCon nullSpecEnv
\end{code}

\begin{code}
doubleTy = mkTyConTy doubleTyCon

doubleTyCon = pcDataTyCon doubleTyConKey pRELUDE_BUILTIN SLIT("Double") [] [doubleDataCon]
doubleDataCon = pcDataCon doubleDataConKey pRELUDE_BUILTIN SLIT("D#") [] [] [doublePrimTy] doubleTyCon nullSpecEnv
\end{code}

\begin{code}
mkStateTy ty	 = applyTyCon stateTyCon [ty]
realWorldStateTy = mkStateTy realWorldTy -- a common use

stateTyCon = pcDataTyCon stateTyConKey pRELUDE_BUILTIN SLIT("_State") [alphaTyVar] [stateDataCon]
stateDataCon
  = pcDataCon stateDataConKey pRELUDE_BUILTIN SLIT("S#")
	[alphaTyVar] [] [mkStatePrimTy alphaTy] stateTyCon nullSpecEnv
\end{code}

\begin{code}
stablePtrTyCon
  = pcDataTyCon stablePtrTyConKey gLASGOW_MISC SLIT("_StablePtr")
	[alphaTyVar] [stablePtrDataCon]
  where
    stablePtrDataCon
      = pcDataCon stablePtrDataConKey gLASGOW_MISC SLIT("_StablePtr")
	    [alphaTyVar] [] [applyTyCon stablePtrPrimTyCon [alphaTy]] stablePtrTyCon nullSpecEnv
\end{code}

\begin{code}
mallocPtrTyCon
  = pcDataTyCon mallocPtrTyConKey gLASGOW_MISC SLIT("_MallocPtr")
	[] [mallocPtrDataCon]
  where
    mallocPtrDataCon
      = pcDataCon mallocPtrDataConKey gLASGOW_MISC SLIT("_MallocPtr")
	    [] [] [applyTyCon mallocPtrPrimTyCon []] mallocPtrTyCon nullSpecEnv
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Integer]{@Integer@ and its related ``pairing'' types}
%*									*
%************************************************************************

@Integer@ and its pals are not really primitive.  @Integer@ itself, first:
\begin{code}
integerTy :: GenType t u
integerTy    = mkTyConTy integerTyCon

integerTyCon = pcDataTyCon integerTyConKey pRELUDE_BUILTIN SLIT("Integer") [] [integerDataCon]

integerDataCon = pcDataCon integerDataConKey pRELUDE_BUILTIN SLIT("J#")
		[] [] [intPrimTy, intPrimTy, byteArrayPrimTy] integerTyCon nullSpecEnv
\end{code}

And the other pairing types:
\begin{code}
return2GMPsTyCon = pcDataTyCon return2GMPsTyConKey
	pRELUDE_BUILTIN SLIT("_Return2GMPs") [] [return2GMPsDataCon]

return2GMPsDataCon
  = pcDataCon return2GMPsDataConKey pRELUDE_BUILTIN SLIT("_Return2GMPs") [] []
	[intPrimTy, intPrimTy, byteArrayPrimTy,
	 intPrimTy, intPrimTy, byteArrayPrimTy] return2GMPsTyCon nullSpecEnv

returnIntAndGMPTyCon = pcDataTyCon returnIntAndGMPTyConKey
	pRELUDE_BUILTIN SLIT("_ReturnIntAndGMP") [] [returnIntAndGMPDataCon]

returnIntAndGMPDataCon
  = pcDataCon returnIntAndGMPDataConKey pRELUDE_BUILTIN SLIT("_ReturnIntAndGMP") [] []
	[intPrimTy, intPrimTy, intPrimTy, byteArrayPrimTy] returnIntAndGMPTyCon nullSpecEnv
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
  = pcDataTyCon stateAndPtrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndPtr#")
		[alphaTyVar, betaTyVar] [stateAndPtrPrimDataCon]
stateAndPtrPrimDataCon
  = pcDataCon stateAndPtrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndPtr#")
		[alphaTyVar, betaTyVar] [] [mkStatePrimTy alphaTy, betaTy]
		stateAndPtrPrimTyCon nullSpecEnv

stateAndCharPrimTyCon
  = pcDataTyCon stateAndCharPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndChar#")
		[alphaTyVar] [stateAndCharPrimDataCon]
stateAndCharPrimDataCon
  = pcDataCon stateAndCharPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndChar#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, charPrimTy]
		stateAndCharPrimTyCon nullSpecEnv

stateAndIntPrimTyCon
  = pcDataTyCon stateAndIntPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndInt#")
		[alphaTyVar] [stateAndIntPrimDataCon]
stateAndIntPrimDataCon
  = pcDataCon stateAndIntPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndInt#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, intPrimTy]
		stateAndIntPrimTyCon nullSpecEnv

stateAndWordPrimTyCon
  = pcDataTyCon stateAndWordPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndWord#")
		[alphaTyVar] [stateAndWordPrimDataCon]
stateAndWordPrimDataCon
  = pcDataCon stateAndWordPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndWord#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, wordPrimTy]
		stateAndWordPrimTyCon nullSpecEnv

stateAndAddrPrimTyCon
  = pcDataTyCon stateAndAddrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndAddr#")
		[alphaTyVar] [stateAndAddrPrimDataCon]
stateAndAddrPrimDataCon
  = pcDataCon stateAndAddrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndAddr#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, addrPrimTy]
		stateAndAddrPrimTyCon nullSpecEnv

stateAndStablePtrPrimTyCon
  = pcDataTyCon stateAndStablePtrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndStablePtr#")
		[alphaTyVar, betaTyVar] [stateAndStablePtrPrimDataCon]
stateAndStablePtrPrimDataCon
  = pcDataCon stateAndStablePtrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndStablePtr#")
		[alphaTyVar, betaTyVar] []
		[mkStatePrimTy alphaTy, applyTyCon stablePtrPrimTyCon [betaTy]]
		stateAndStablePtrPrimTyCon nullSpecEnv

stateAndMallocPtrPrimTyCon
  = pcDataTyCon stateAndMallocPtrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndMallocPtr#")
		[alphaTyVar] [stateAndMallocPtrPrimDataCon]
stateAndMallocPtrPrimDataCon
  = pcDataCon stateAndMallocPtrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndMallocPtr#")
		[alphaTyVar] []
		[mkStatePrimTy alphaTy, applyTyCon mallocPtrPrimTyCon []]
		stateAndMallocPtrPrimTyCon nullSpecEnv

stateAndFloatPrimTyCon
  = pcDataTyCon stateAndFloatPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndFloat#")
		[alphaTyVar] [stateAndFloatPrimDataCon]
stateAndFloatPrimDataCon
  = pcDataCon stateAndFloatPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndFloat#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, floatPrimTy]
		stateAndFloatPrimTyCon nullSpecEnv

stateAndDoublePrimTyCon
  = pcDataTyCon stateAndDoublePrimTyConKey pRELUDE_BUILTIN SLIT("StateAndDouble#")
		[alphaTyVar] [stateAndDoublePrimDataCon]
stateAndDoublePrimDataCon
  = pcDataCon stateAndDoublePrimDataConKey pRELUDE_BUILTIN SLIT("StateAndDouble#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, doublePrimTy]
		stateAndDoublePrimTyCon nullSpecEnv
\end{code}

\begin{code}
stateAndArrayPrimTyCon
  = pcDataTyCon stateAndArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndArray#")
		[alphaTyVar, betaTyVar] [stateAndArrayPrimDataCon]
stateAndArrayPrimDataCon
  = pcDataCon stateAndArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndArray#")
		[alphaTyVar, betaTyVar] [] [mkStatePrimTy alphaTy, mkArrayPrimTy betaTy]
		stateAndArrayPrimTyCon nullSpecEnv

stateAndMutableArrayPrimTyCon
  = pcDataTyCon stateAndMutableArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndMutableArray#")
		[alphaTyVar, betaTyVar] [stateAndMutableArrayPrimDataCon]
stateAndMutableArrayPrimDataCon
  = pcDataCon stateAndMutableArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndMutableArray#")
		[alphaTyVar, betaTyVar] [] [mkStatePrimTy alphaTy, mkMutableArrayPrimTy alphaTy betaTy]
		stateAndMutableArrayPrimTyCon nullSpecEnv

stateAndByteArrayPrimTyCon
  = pcDataTyCon stateAndByteArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndByteArray#")
		[alphaTyVar] [stateAndByteArrayPrimDataCon]
stateAndByteArrayPrimDataCon
  = pcDataCon stateAndByteArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndByteArray#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, byteArrayPrimTy]
		stateAndByteArrayPrimTyCon nullSpecEnv

stateAndMutableByteArrayPrimTyCon
  = pcDataTyCon stateAndMutableByteArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndMutableByteArray#")
		[alphaTyVar] [stateAndMutableByteArrayPrimDataCon]
stateAndMutableByteArrayPrimDataCon
  = pcDataCon stateAndMutableByteArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndMutableByteArray#")
		[alphaTyVar] [] [mkStatePrimTy alphaTy, applyTyCon mutableByteArrayPrimTyCon [alphaTy]]
		stateAndMutableByteArrayPrimTyCon nullSpecEnv

stateAndSynchVarPrimTyCon
  = pcDataTyCon stateAndSynchVarPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndSynchVar#")
		[alphaTyVar, betaTyVar] [stateAndSynchVarPrimDataCon]
stateAndSynchVarPrimDataCon
  = pcDataCon stateAndSynchVarPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndSynchVar#")
		[alphaTyVar, betaTyVar] [] [mkStatePrimTy alphaTy, mkSynchVarPrimTy alphaTy betaTy]
		stateAndSynchVarPrimTyCon nullSpecEnv
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
  = case (maybeAppDataTyConExpandingDicts prim_ty) of
      Nothing -> panic "getStatePairingConInfo:1"
      Just (prim_tycon, tys_applied, _) ->
	let
	    (pair_con, pair_tycon, num_tys) = assoc "getStatePairingConInfo" tbl prim_tycon
	    pair_ty = applyTyCon pair_tycon (realWorldTy : drop num_tys tys_applied)
	in
	(pair_con, pair_ty)
  where
    tbl = [
	(charPrimTyCon, (stateAndCharPrimDataCon, stateAndCharPrimTyCon, 0)),
	(intPrimTyCon, (stateAndIntPrimDataCon, stateAndIntPrimTyCon, 0)),
	(wordPrimTyCon, (stateAndWordPrimDataCon, stateAndWordPrimTyCon, 0)),
	(addrPrimTyCon, (stateAndAddrPrimDataCon, stateAndAddrPrimTyCon, 0)),
	(stablePtrPrimTyCon, (stateAndStablePtrPrimDataCon, stateAndStablePtrPrimTyCon, 0)),
	(mallocPtrPrimTyCon, (stateAndMallocPtrPrimDataCon, stateAndMallocPtrPrimTyCon, 0)),
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
\subsection[TysWiredIn-ST]{The basic @_ST@ state-transformer type}
%*									*
%************************************************************************

This is really just an ordinary synonym, except it is ABSTRACT.

\begin{code}
mkStateTransformerTy s a = mkSynTy stTyCon [s, a]

stTyCon
  = let
	ty = mkFunTys [mkStateTy alphaTy] (mkTupleTy 2 [betaTy, mkStateTy alphaTy])
    in
    mkSynTyCon
     (mkBuiltinName stTyConKey gLASGOW_ST SLIT("_ST"))
     (mkBoxedTypeKind `mkArrowKind` (mkBoxedTypeKind `mkArrowKind` mkBoxedTypeKind))
     2 [alphaTyVar, betaTyVar]
     ty
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-IO]{The @PrimIO@ and @IO@ monadic-I/O types}
%*									*
%************************************************************************

@PrimIO@ and @IO@ really are just plain synonyms.

\begin{code}
mkPrimIoTy a = mkSynTy primIoTyCon [a]

primIoTyCon
  = let
	ty = mkStateTransformerTy realWorldTy alphaTy
    in
--  pprTrace "primIOTyCon:" (ppCat [pprType PprDebug ty, ppr PprDebug (typeKind ty)]) $
    mkSynTyCon
     (mkBuiltinName primIoTyConKey pRELUDE_PRIMIO SLIT("PrimIO"))
     (mkBoxedTypeKind `mkArrowKind` mkBoxedTypeKind)
     1 [alphaTyVar] ty
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

boolTyCon = pcDataTyCon boolTyConKey pRELUDE_CORE SLIT("Bool") [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConKey pRELUDE_CORE SLIT("False") [] [] [] boolTyCon nullSpecEnv
trueDataCon  = pcDataCon trueDataConKey	 pRELUDE_CORE SLIT("True")  [] [] [] boolTyCon nullSpecEnv
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Ordering]{The @Ordering@ type}
%*									*
%************************************************************************

\begin{code}
---------------------------------------------
-- data Ordering = LT | EQ | GT deriving ()
---------------------------------------------

orderingTy = mkTyConTy orderingTyCon

orderingTyCon = pcDataTyCon orderingTyConKey pRELUDE_BUILTIN SLIT("Ordering") []
		            [ltDataCon, eqDataCon, gtDataCon]

ltDataCon  = pcDataCon ltDataConKey pRELUDE_BUILTIN SLIT("LT") [] [] [] orderingTyCon nullSpecEnv
eqDataCon  = pcDataCon eqDataConKey pRELUDE_BUILTIN SLIT("EQ") [] [] [] orderingTyCon nullSpecEnv
gtDataCon  = pcDataCon gtDataConKey pRELUDE_BUILTIN SLIT("GT") [] [] [] orderingTyCon nullSpecEnv
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-List]{The @List@ type (incl ``build'' magic)}
%*									*
%************************************************************************

Special syntax, deeply wired in, but otherwise an ordinary algebraic
data type:
\begin{verbatim}
data List a = Nil | a : (List a)
ToDo: data [] a = [] | a : (List a)
ToDo: data () = ()
      data (,,) a b c = (,,) a b c
\end{verbatim}

\begin{code}
mkListTy :: GenType t u -> GenType t u
mkListTy ty = applyTyCon listTyCon [ty]

alphaListTy = mkSigmaTy [alphaTyVar] [] (applyTyCon listTyCon [alphaTy])

listTyCon = pcDataTyCon listTyConKey pRELUDE_BUILTIN SLIT("[]") 
			[alphaTyVar] [nilDataCon, consDataCon]

nilDataCon  = pcDataCon nilDataConKey  pRELUDE_BUILTIN SLIT("[]") [alphaTyVar] [] [] listTyCon
		(pcGenerateDataSpecs alphaListTy)
consDataCon = pcDataCon consDataConKey pRELUDE_BUILTIN SLIT(":")
		[alphaTyVar] [] [alphaTy, applyTyCon listTyCon [alphaTy]] listTyCon
		(pcGenerateDataSpecs alphaListTy)
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
mkTupleTy :: Int -> [GenType t u] -> GenType t u

mkTupleTy arity tys = applyTyCon (mkTupleTyCon arity) tys

unitTy    = mkTupleTy 0 []
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Ratios]{@Ratio@ and @Rational@}
%*									*
%************************************************************************

ToDo: make this (mostly) go away.

\begin{code}
rationalTy :: GenType t u

mkRatioTy ty = applyTyCon ratioTyCon [ty]
rationalTy   = mkRatioTy integerTy

ratioTyCon = pcDataTyCon ratioTyConKey pRELUDE_RATIO SLIT("Ratio") [alphaTyVar] [ratioDataCon]

ratioDataCon = pcDataCon ratioDataConKey pRELUDE_RATIO SLIT(":%")
		[alphaTyVar] [{-(integralClass,alphaTy)-}] [alphaTy, alphaTy] ratioTyCon nullSpecEnv
	-- context omitted to match lib/prelude/ defn of "data Ratio ..."

rationalTyCon
  = mkSynTyCon
      (mkBuiltinName rationalTyConKey pRELUDE_RATIO SLIT("Rational"))
      mkBoxedTypeKind
      0	[] rationalTy -- == mkRatioTy integerTy
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-_Lift]{@_Lift@ type: to support array indexing}
%*									*
%************************************************************************

Again, deeply turgid: \tr{data _Lift a = _Lift a}.

\begin{code}
mkLiftTy ty = applyTyCon liftTyCon [ty]

{-
mkLiftTy ty
  = mkSigmaTy tvs theta (applyTyCon liftTyCon [tau])
  where
    (tvs, theta, tau) = splitSigmaTy ty

isLiftTy ty
  = case (maybeAppDataTyConExpandingDicts tau) of
      Just (tycon, tys, _) -> tycon == liftTyCon
      Nothing -> False
  where
    (tvs, theta, tau) = splitSigmaTy ty
-}


alphaLiftTy = mkSigmaTy [alphaTyVar] [] (applyTyCon liftTyCon [alphaTy])

liftTyCon
  = pcDataTyCon liftTyConKey pRELUDE_BUILTIN SLIT("_Lift") [alphaTyVar] [liftDataCon]

liftDataCon
  = pcDataCon liftDataConKey pRELUDE_BUILTIN SLIT("_Lift")
		[alphaTyVar] [] [alphaTy] liftTyCon
		((pcGenerateDataSpecs alphaLiftTy) `addOneToSpecEnv`
		 (mkSpecInfo [Just realWorldStatePrimTy] 0 bottom))
  where
    bottom = panic "liftDataCon:State# _RealWorld"
\end{code}


%************************************************************************
%*									*
\subsection[TysWiredIn-for-convenience]{Types wired in for convenience (e.g., @String@)}
%*									*
%************************************************************************

\begin{code}
stringTy = mkListTy charTy

stringTyCon
 = mkSynTyCon
     (mkBuiltinName stringTyConKey pRELUDE_CORE SLIT("String"))
     mkBoxedTypeKind
     0 [] stringTy
\end{code}
