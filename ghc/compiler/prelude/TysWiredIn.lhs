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
	cmpTagTy,
	cmpTagTyCon,
	consDataCon,
	doubleDataCon,
	doubleTy,
	doubleTyCon,
	eqPrimDataCon,
	falseDataCon,
	floatDataCon,
	floatTy,
	floatTyCon,
	getStatePairingConInfo,
	gtPrimDataCon,
	intDataCon,
	intTy,
	intTyCon,
	integerTy,
	integerTyCon,
	integerDataCon,
	liftDataCon,
	liftTyCon,
	listTyCon,
	ltPrimDataCon,
	mallocPtrTyCon,
	mkLiftTy,
	mkListTy,
	mkPrimIoTy,
	mkStateTransformerTy,
	mkTupleTy,
	nilDataCon,
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
	wordDataCon,
	wordTy,
	wordTyCon
    ) where

import Pretty		--ToDo:rm debugging only

import PrelFuns		-- help functions, types and things
import TysPrim

import AbsUniType	( applyTyCon, mkTupleTyCon, mkSynonymTyCon,
			  getUniDataTyCon_maybe, mkSigmaTy, TyCon
			  , pprUniType --ToDo: rm debugging only
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			)
import IdInfo
import Maybes		( Maybe(..) )
import Unique
import Util
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-boxed-prim]{The ``boxed primitive'' types (@Char@, @Int@, etc)}
%*									*
%************************************************************************

\begin{code}
charTy = UniData charTyCon []

charTyCon = pcDataTyCon charTyConKey pRELUDE_BUILTIN SLIT("Char") [] [charDataCon]
charDataCon = pcDataCon charDataConKey pRELUDE_BUILTIN SLIT("C#") [] [] [charPrimTy] charTyCon nullSpecEnv
\end{code}

\begin{code}
intTy = UniData intTyCon []

intTyCon = pcDataTyCon intTyConKey pRELUDE_BUILTIN SLIT("Int") [] [intDataCon]
intDataCon = pcDataCon intDataConKey pRELUDE_BUILTIN SLIT("I#") [] [] [intPrimTy] intTyCon nullSpecEnv 
\end{code}

\begin{code}
wordTy = UniData wordTyCon []

wordTyCon = pcDataTyCon wordTyConKey pRELUDE_BUILTIN SLIT("_Word") [] [wordDataCon]
wordDataCon = pcDataCon wordDataConKey pRELUDE_BUILTIN SLIT("W#") [] [] [wordPrimTy] wordTyCon nullSpecEnv
\end{code}

\begin{code}
addrTy = UniData addrTyCon []

addrTyCon = pcDataTyCon addrTyConKey pRELUDE_BUILTIN SLIT("_Addr") [] [addrDataCon]
addrDataCon = pcDataCon addrDataConKey pRELUDE_BUILTIN SLIT("A#") [] [] [addrPrimTy] addrTyCon nullSpecEnv
\end{code}

\begin{code}
floatTy	= UniData floatTyCon []

floatTyCon = pcDataTyCon floatTyConKey pRELUDE_BUILTIN SLIT("Float") [] [floatDataCon]
floatDataCon = pcDataCon floatDataConKey pRELUDE_BUILTIN SLIT("F#") [] [] [floatPrimTy] floatTyCon nullSpecEnv
\end{code}

\begin{code}
doubleTy = UniData doubleTyCon []

doubleTyCon = pcDataTyCon doubleTyConKey pRELUDE_BUILTIN SLIT("Double") [] [doubleDataCon]
doubleDataCon = pcDataCon doubleDataConKey pRELUDE_BUILTIN SLIT("D#") [] [] [doublePrimTy] doubleTyCon nullSpecEnv
\end{code}

\begin{code}
mkStateTy ty	 = applyTyCon stateTyCon [ty]
realWorldStateTy = mkStateTy realWorldTy -- a common use

stateTyCon = pcDataTyCon stateTyConKey pRELUDE_BUILTIN SLIT("_State") [alpha_tv] [stateDataCon]
stateDataCon
  = pcDataCon stateDataConKey pRELUDE_BUILTIN SLIT("S#")
	[alpha_tv] [] [mkStatePrimTy alpha] stateTyCon nullSpecEnv
\end{code}

\begin{code}
{- OLD:
byteArrayTyCon
  = pcDataTyCon byteArrayTyConKey pRELUDE_ARRAY SLIT("_ByteArray")
	[alpha_tv] [byteArrayDataCon]

byteArrayDataCon
  = pcDataCon byteArrayDataConKey pRELUDE_ARRAY SLIT("_ByteArray")
	[alpha_tv] []
	[mkTupleTy 2 [alpha, alpha], byteArrayPrimTy]
	byteArrayTyCon nullSpecEnv
-}
\end{code}

\begin{code}
{- OLD:
mutableArrayTyCon
  = pcDataTyCon mutableArrayTyConKey gLASGOW_ST SLIT("_MutableArray")
	[alpha_tv, beta_tv, gamma_tv] [mutableArrayDataCon]
  where
    mutableArrayDataCon
      = pcDataCon mutableArrayDataConKey gLASGOW_ST SLIT("_MutableArray")
	    [alpha_tv, beta_tv, gamma_tv] []
	    [mkTupleTy 2 [beta, beta], applyTyCon mutableArrayPrimTyCon [alpha, gamma]]
	    mutableArrayTyCon nullSpecEnv
-}
\end{code}

\begin{code}
{-
mutableByteArrayTyCon
  = pcDataTyCon mutableByteArrayTyConKey gLASGOW_ST SLIT("_MutableByteArray")
	[alpha_tv, beta_tv] [mutableByteArrayDataCon]

mutableByteArrayDataCon
  = pcDataCon mutableByteArrayDataConKey gLASGOW_ST SLIT("_MutableByteArray")
	[alpha_tv, beta_tv] []
	[mkTupleTy 2 [beta, beta], mkMutableByteArrayPrimTy alpha]
	mutableByteArrayTyCon nullSpecEnv
-}
\end{code}

\begin{code}
stablePtrTyCon
  = pcDataTyCon stablePtrTyConKey gLASGOW_MISC SLIT("_StablePtr")
	[alpha_tv] [stablePtrDataCon]
  where
    stablePtrDataCon
      = pcDataCon stablePtrDataConKey gLASGOW_MISC SLIT("_StablePtr")
	    [alpha_tv] [] [applyTyCon stablePtrPrimTyCon [alpha]] stablePtrTyCon nullSpecEnv
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
integerTy :: UniType
integerTy    = UniData integerTyCon []

integerTyCon = pcDataTyCon integerTyConKey pRELUDE_BUILTIN SLIT("Integer") [] [integerDataCon]

#ifndef DPH
integerDataCon = pcDataCon integerDataConKey pRELUDE_BUILTIN SLIT("J#")
		[] [] [intPrimTy, intPrimTy, byteArrayPrimTy] integerTyCon nullSpecEnv
#else
-- DPH: For the time being we implement Integers in the same way as Ints.
integerDataCon = pcDataCon integerDataConKey pRELUDE_BUILTIN SLIT("J#")
		[] [] [intPrimTy] integerTyCon nullSpecEnv
#endif {- Data Parallel Haskell -}
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
		[alpha_tv, beta_tv] [stateAndPtrPrimDataCon]
stateAndPtrPrimDataCon
  = pcDataCon stateAndPtrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndPtr#")
		[alpha_tv, beta_tv] [] [mkStatePrimTy alpha, beta]
		stateAndPtrPrimTyCon nullSpecEnv

stateAndCharPrimTyCon
  = pcDataTyCon stateAndCharPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndChar#")
		[alpha_tv] [stateAndCharPrimDataCon]
stateAndCharPrimDataCon
  = pcDataCon stateAndCharPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndChar#")
		[alpha_tv] [] [mkStatePrimTy alpha, charPrimTy]
		stateAndCharPrimTyCon nullSpecEnv

stateAndIntPrimTyCon
  = pcDataTyCon stateAndIntPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndInt#")
		[alpha_tv] [stateAndIntPrimDataCon]
stateAndIntPrimDataCon
  = pcDataCon stateAndIntPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndInt#")
		[alpha_tv] [] [mkStatePrimTy alpha, intPrimTy]
		stateAndIntPrimTyCon nullSpecEnv

stateAndWordPrimTyCon
  = pcDataTyCon stateAndWordPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndWord#")
		[alpha_tv] [stateAndWordPrimDataCon]
stateAndWordPrimDataCon
  = pcDataCon stateAndWordPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndWord#")
		[alpha_tv] [] [mkStatePrimTy alpha, wordPrimTy]
		stateAndWordPrimTyCon nullSpecEnv

stateAndAddrPrimTyCon
  = pcDataTyCon stateAndAddrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndAddr#")
		[alpha_tv] [stateAndAddrPrimDataCon]
stateAndAddrPrimDataCon
  = pcDataCon stateAndAddrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndAddr#")
		[alpha_tv] [] [mkStatePrimTy alpha, addrPrimTy]
		stateAndAddrPrimTyCon nullSpecEnv

stateAndStablePtrPrimTyCon
  = pcDataTyCon stateAndStablePtrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndStablePtr#")
		[alpha_tv, beta_tv] [stateAndStablePtrPrimDataCon]
stateAndStablePtrPrimDataCon
  = pcDataCon stateAndStablePtrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndStablePtr#")
		[alpha_tv, beta_tv] []
		[mkStatePrimTy alpha, applyTyCon stablePtrPrimTyCon [beta]]
		stateAndStablePtrPrimTyCon nullSpecEnv

stateAndMallocPtrPrimTyCon
  = pcDataTyCon stateAndMallocPtrPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndMallocPtr#")
		[alpha_tv] [stateAndMallocPtrPrimDataCon]
stateAndMallocPtrPrimDataCon
  = pcDataCon stateAndMallocPtrPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndMallocPtr#")
		[alpha_tv] []
		[mkStatePrimTy alpha, applyTyCon mallocPtrPrimTyCon []]
		stateAndMallocPtrPrimTyCon nullSpecEnv

stateAndFloatPrimTyCon
  = pcDataTyCon stateAndFloatPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndFloat#")
		[alpha_tv] [stateAndFloatPrimDataCon]
stateAndFloatPrimDataCon
  = pcDataCon stateAndFloatPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndFloat#")
		[alpha_tv] [] [mkStatePrimTy alpha, floatPrimTy]
		stateAndFloatPrimTyCon nullSpecEnv

stateAndDoublePrimTyCon
  = pcDataTyCon stateAndDoublePrimTyConKey pRELUDE_BUILTIN SLIT("StateAndDouble#")
		[alpha_tv] [stateAndDoublePrimDataCon]
stateAndDoublePrimDataCon
  = pcDataCon stateAndDoublePrimDataConKey pRELUDE_BUILTIN SLIT("StateAndDouble#")
		[alpha_tv] [] [mkStatePrimTy alpha, doublePrimTy]
		stateAndDoublePrimTyCon nullSpecEnv
\end{code}

\begin{code}
stateAndArrayPrimTyCon
  = pcDataTyCon stateAndArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndArray#")
		[alpha_tv, beta_tv] [stateAndArrayPrimDataCon]
stateAndArrayPrimDataCon
  = pcDataCon stateAndArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndArray#")
		[alpha_tv, beta_tv] [] [mkStatePrimTy alpha, mkArrayPrimTy beta]
		stateAndArrayPrimTyCon nullSpecEnv

stateAndMutableArrayPrimTyCon
  = pcDataTyCon stateAndMutableArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndMutableArray#")
		[alpha_tv, beta_tv] [stateAndMutableArrayPrimDataCon]
stateAndMutableArrayPrimDataCon
  = pcDataCon stateAndMutableArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndMutableArray#")
		[alpha_tv, beta_tv] [] [mkStatePrimTy alpha, mkMutableArrayPrimTy alpha beta]
		stateAndMutableArrayPrimTyCon nullSpecEnv

stateAndByteArrayPrimTyCon
  = pcDataTyCon stateAndByteArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndByteArray#")
		[alpha_tv] [stateAndByteArrayPrimDataCon]
stateAndByteArrayPrimDataCon
  = pcDataCon stateAndByteArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndByteArray#")
		[alpha_tv] [] [mkStatePrimTy alpha, byteArrayPrimTy]
		stateAndByteArrayPrimTyCon nullSpecEnv

stateAndMutableByteArrayPrimTyCon
  = pcDataTyCon stateAndMutableByteArrayPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndMutableByteArray#")
		[alpha_tv] [stateAndMutableByteArrayPrimDataCon]
stateAndMutableByteArrayPrimDataCon
  = pcDataCon stateAndMutableByteArrayPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndMutableByteArray#")
		[alpha_tv] [] [mkStatePrimTy alpha, applyTyCon mutableByteArrayPrimTyCon [alpha]]
		stateAndMutableByteArrayPrimTyCon nullSpecEnv

stateAndSynchVarPrimTyCon
  = pcDataTyCon stateAndSynchVarPrimTyConKey pRELUDE_BUILTIN SLIT("StateAndSynchVar#")
		[alpha_tv, beta_tv] [stateAndSynchVarPrimDataCon]
stateAndSynchVarPrimDataCon
  = pcDataCon stateAndSynchVarPrimDataConKey pRELUDE_BUILTIN SLIT("StateAndSynchVar#")
		[alpha_tv, beta_tv] [] [mkStatePrimTy alpha, mkSynchVarPrimTy alpha beta]
		stateAndSynchVarPrimTyCon nullSpecEnv
\end{code}

The ccall-desugaring mechanism uses this function to figure out how to
rebox the result.  It's really a HACK, especially the part about
how many types to drop from \tr{tys_applied}.

\begin{code}
getStatePairingConInfo
	:: UniType	-- primitive type
	-> (Id,		-- state pair constructor for prim type
	    UniType)	-- type of state pair

getStatePairingConInfo prim_ty
  = case (getUniDataTyCon_maybe prim_ty) of
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
mkStateTransformerTy s a = applyTyCon stTyCon [s, a]

stTyCon
  = mkSynonymTyCon
     stTyConKey
     (mkPreludeCoreName gLASGOW_ST SLIT("_ST"))
     2
     [alpha_tv, beta_tv]
     (mkStateTy alpha `UniFun` mkTupleTy 2 [beta, mkStateTy alpha])
     True -- ToDo: make... *** ABSTRACT ***
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-IO]{The @PrimIO@ and @IO@ monadic-I/O types}
%*									*
%************************************************************************

@PrimIO@ and @IO@ really are just a plain synonyms.

\begin{code}
mkPrimIoTy a = applyTyCon primIoTyCon [a]

primIoTyCon
  = mkSynonymTyCon
     primIoTyConKey
     (mkPreludeCoreName pRELUDE_PRIMIO SLIT("PrimIO"))
     1
     [alpha_tv]
     (mkStateTransformerTy realWorldTy alpha)
     True -- need not be abstract
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
boolTy = UniData boolTyCon []

boolTyCon = pcDataTyCon boolTyConKey pRELUDE_CORE SLIT("Bool") [] [falseDataCon, trueDataCon]

falseDataCon = pcDataCon falseDataConKey pRELUDE_CORE SLIT("False") [] [] [] boolTyCon nullSpecEnv
trueDataCon  = pcDataCon trueDataConKey	 pRELUDE_CORE SLIT("True")  [] [] [] boolTyCon nullSpecEnv
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-CMP-TAG]{The @CMP_TAG#@ type (for fast `derived' comparisons)}
%*									*
%************************************************************************

\begin{code}
---------------------------------------------
-- data _CMP_TAG = _LT | _EQ | _GT deriving ()
---------------------------------------------

cmpTagTy = UniData cmpTagTyCon []

cmpTagTyCon = pcDataTyCon cmpTagTyConKey pRELUDE_BUILTIN SLIT("_CMP_TAG") []
		[ltPrimDataCon, eqPrimDataCon, gtPrimDataCon]

ltPrimDataCon  = pcDataCon ltTagDataConKey pRELUDE_BUILTIN SLIT("_LT") [] [] [] cmpTagTyCon nullSpecEnv
eqPrimDataCon  = pcDataCon eqTagDataConKey pRELUDE_BUILTIN SLIT("_EQ") [] [] [] cmpTagTyCon nullSpecEnv
gtPrimDataCon  = pcDataCon gtTagDataConKey pRELUDE_BUILTIN SLIT("_GT") [] [] [] cmpTagTyCon nullSpecEnv
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
\end{verbatim}

\begin{code}
mkListTy :: UniType -> UniType
mkListTy ty = UniData listTyCon [ty]

alphaListTy = mkSigmaTy [alpha_tv] [] (mkListTy alpha)

listTyCon = pcDataTyCon listTyConKey pRELUDE_BUILTIN SLIT("List") [alpha_tv] [nilDataCon, consDataCon]

nilDataCon  = pcDataCon nilDataConKey  pRELUDE_BUILTIN SLIT("Nil") [alpha_tv] [] [] listTyCon
		(pcGenerateDataSpecs alphaListTy)
consDataCon = pcDataCon consDataConKey pRELUDE_BUILTIN SLIT(":")
		[alpha_tv] [] [alpha, mkListTy alpha] listTyCon
		(pcGenerateDataSpecs alphaListTy)
\end{code}

This is the @_Build@ data constructor, it does {\em not} appear inside
listTyCon.  It has this type: \tr{((a -> b -> b) -> b -> b) -> [a]}.
\begin{code}
{- NOT USED:
buildDataCon
  = pcDataCon buildDataConKey  pRELUDE_BUILTIN "Build"
	[alpha_tv] [] [
		mkSigmaTy [beta_tv] []
			((alpha `UniFun` (beta `UniFun` beta))
			`UniFun` (beta
			`UniFun` beta))] listTyCon nullSpecEnv
-}
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
They have a special family of type constructors, of type
@TyCon@\srcloc{uniType/TyCon.lhs}.
These contain the tycon arity, but don't require a Unique.

\item
They have a special family of constructors, of type
@Id@\srcloc{basicTypes/Id.lhs}.	 Again these contain their arity but
don't need a Unique.

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
mkTupleTy :: Int -> [UniType] -> UniType

mkTupleTy arity tys = applyTyCon (mkTupleTyCon arity) tys

unitTy = mkTupleTy 0 []
\end{code}

%************************************************************************
%*									*
\subsection[TysWiredIn-Ratios]{@Ratio@ and @Rational@}
%*									*
%************************************************************************

ToDo: make this (mostly) go away.

\begin{code}
rationalTy :: UniType

mkRatioTy ty = UniData ratioTyCon [ty]
rationalTy   = mkRatioTy integerTy

ratioTyCon = pcDataTyCon ratioTyConKey pRELUDE_RATIO SLIT("Ratio") [alpha_tv] [ratioDataCon]

ratioDataCon = pcDataCon ratioDataConKey pRELUDE_RATIO SLIT(":%")
		[alpha_tv] [{-(integralClass,alpha)-}] [alpha, alpha] ratioTyCon nullSpecEnv
	-- context omitted to match lib/prelude/ defn of "data Ratio ..."

rationalTyCon
  = mkSynonymTyCon
      rationalTyConKey
      (mkPreludeCoreName pRELUDE_RATIO SLIT("Rational"))
      0	 -- arity
      [] -- tyvars
      rationalTy -- == mkRatioTy integerTy
      True -- unabstract
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
  = mkSigmaTy tvs theta (UniData liftTyCon [tau])
  where
    (tvs, theta, tau) = splitType ty

isLiftTy ty
  = case getUniDataTyCon_maybe tau of
      Just (tycon, tys, _) -> tycon == liftTyCon
      Nothing -> False
  where
    (tvs, theta, tau) = splitType ty
-}


alphaLiftTy = mkSigmaTy [alpha_tv] [] (UniData liftTyCon [alpha])

liftTyCon
  = pcDataTyCon liftTyConKey pRELUDE_BUILTIN SLIT("_Lift") [alpha_tv] [liftDataCon]

liftDataCon
  = pcDataCon liftDataConKey pRELUDE_BUILTIN SLIT("_Lift")
		[alpha_tv] [] [alpha] liftTyCon 
		((pcGenerateDataSpecs alphaLiftTy) `addOneToSpecEnv`
		 (SpecInfo [Just realWorldStatePrimTy] 0 bottom))
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
 = mkSynonymTyCon
     stringTyConKey
     (mkPreludeCoreName pRELUDE_CORE SLIT("String"))
     0
     []   -- type variables
     stringTy
     True -- unabstract
\end{code}

\begin{code}
{- UNUSED:
packedStringTy = applyTyCon packedStringTyCon []

packedStringTyCon
  = pcDataTyCon packedStringTyConKey pRELUDE_PS SLIT("_PackedString") []
	[psDataCon, cpsDataCon]

psDataCon
  = pcDataCon psDataConKey pRELUDE_PS SLIT("_PS")
		[] [] [intPrimTy, byteArrayPrimTy] packedStringTyCon

cpsDataCon
  = pcDataCon cpsDataConKey pRELUDE_PS SLIT("_CPS")
		[] [] [addrPrimTy] packedStringTyCon
-}
\end{code}
