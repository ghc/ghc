%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[AbsPrel]{The @AbsPrel@ interface to the compiler's prelude knowledge}

\begin{code}
#include "HsVersions.h"

module AbsPrel (

-- unlike most export lists, this one is actually interesting :-)

	-- re-export some PrimOp stuff:
	PrimOp(..), typeOfPrimOp, primOpNameInfo,
	HeapRequirement(..), primOpHeapReq, primOpCanTriggerGC, 
	primOpNeedsWrapper, primOpOkForSpeculation, primOpIsCheap,
	fragilePrimOp,
	PrimOpResultInfo(..), getPrimOpResultInfo,
	pprPrimOp, showPrimOp, isCompareOp,
	readUnfoldingPrimOp,  -- actually, defined herein

	pRELUDE, pRELUDE_BUILTIN, pRELUDE_CORE, pRELUDE_RATIO,
	pRELUDE_LIST, pRELUDE_TEXT, --OLD: pRELUDE_ARRAY, pRELUDE_COMPLEX,
	pRELUDE_PRIMIO, pRELUDE_IO, pRELUDE_PS,
	gLASGOW_ST, {-gLASGOW_IO,-} gLASGOW_MISC,

	-- lookup functions for built-in names, for the renamer:
	builtinNameInfo,

	-- *odd* values that need to be reached out and grabbed:
	eRROR_ID, pAT_ERROR_ID, aBSENT_ERROR_ID,
	packStringForCId,
	unpackCStringId, unpackCString2Id,
	unpackCStringAppendId, unpackCStringFoldrId,
	integerZeroId, integerPlusOneId,
	integerPlusTwoId, integerMinusOneId,

#ifdef DPH
	-- ProcessorClass
	toPodId,

	-- Pid Class
	fromDomainId, toDomainId,
#endif {- Data Parallel Haskell -}

	-----------------------------------------------------
	-- the rest of the export list is organised by *type*
	-----------------------------------------------------

	-- "type": functions ("arrow" type constructor)
	mkFunTy,

	-- type: Bool
	boolTyCon, boolTy, falseDataCon, trueDataCon,

	-- types: Char#, Char, String (= [Char])
	charPrimTy, charTy, stringTy,
	charPrimTyCon, charTyCon, charDataCon,

	-- type: CMP_TAG (used in deriving)
	cmpTagTy, ltPrimDataCon, eqPrimDataCon, gtPrimDataCon,

	-- types: Double#, Double
	doublePrimTy, doubleTy,
	doublePrimTyCon, doubleTyCon, doubleDataCon,

	-- types: Float#, Float
	floatPrimTy, floatTy,
	floatPrimTyCon, floatTyCon, floatDataCon,

	-- types: Glasgow *primitive* arrays, sequencing and I/O
	mkPrimIoTy, -- to typecheck "mainIO", "mainPrimIO" & for _ccall_s
	realWorldStatePrimTy, realWorldStateTy{-boxed-},
	realWorldTy, realWorldTyCon, realWorldPrimId,
	stateDataCon, getStatePairingConInfo,

	-- types: Void# (only used within the compiler)
	voidPrimTy, voidPrimId,

	-- types: Addr#, Int#, Word#, Int
	intPrimTy, intTy, intPrimTyCon, intTyCon, intDataCon,
	wordPrimTyCon, wordPrimTy, wordTy, wordTyCon, wordDataCon,
	addrPrimTyCon, addrPrimTy, addrTy, addrTyCon, addrDataCon,

	-- types: Integer, Rational (= Ratio Integer)
	integerTy, rationalTy,
	integerTyCon, integerDataCon,
	rationalTyCon, ratioDataCon,

	-- type: Lift
	liftTyCon, liftDataCon, mkLiftTy,

	-- type: List
	listTyCon, mkListTy, nilDataCon, consDataCon,
	-- NOT USED: buildDataCon,

	-- type: tuples
	mkTupleTy, unitTy,

	-- packed Strings
--	packedStringTyCon, packedStringTy, psDataCon, cpsDataCon,

	-- for compilation of List Comprehensions and foldr
	foldlId, foldrId, mkFoldl, mkFoldr,
	mkBuild, buildId, augmentId, appendId,

#ifdef DPH
	mkProcessorTy,
        mkPodTy, mkPodNTy, podTyCon,			     -- user model
	mkPodizedPodNTy, 		     		     -- podized model
	mkInterfacePodNTy, interfacePodTyCon, mKINTERPOD_ID, -- interface model

        -- Misc used during podization
        primIfromPodNSelectorId,
#endif {- Data Parallel Haskell -}

	-- and, finally, we must put in some (abstract) data types,
	-- to make the interface self-sufficient
	GlobalSwitch, Id, Maybe, Name, PprStyle, PrimKind, HeapOffset,
	TyCon, UniType, TauType(..), Unique, CoreExpr, PlainCoreExpr(..)

	IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
	IF_ATTACK_PRAGMAS(COMMA mkStatePrimTy)

#ifndef __GLASGOW_HASKELL__
	,TAG_
#endif
    ) where

#ifdef DPH
import TyPod
import TyProcs
#endif {- Data Parallel Haskell -}

import PrelFuns		-- help functions, types and things
import PrimKind

import TysPrim		-- TYPES
import TysWiredIn
import PrelVals		-- VALUES
import PrimOps		-- PRIMITIVE OPS

import AbsUniType	( getTyConDataCons, TyCon
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon)
			)
import CmdLineOpts	( GlobalSwitch(..) )
import FiniteMap
import Id		( Id )
--OLD:import NameEnv
import Maybes
import Unique		-- *Key stuff
import Util
\end{code}

This little devil is too small to merit its own ``TyFun'' module:

\begin{code}
mkFunTy = UniFun
\end{code}

%************************************************************************
%*									*
\subsection[builtinNameInfo]{Lookup built-in names}
%*									*
%************************************************************************

We have two ``builtin name funs,'' one to look up @TyCons@ and
@Classes@, the other to look up values.

\begin{code}
builtinNameInfo :: (GlobalSwitch -> Bool)	-- access to global cmd-line flags
		-> (FAST_STRING -> Maybe Name,	-- name lookup fn for values
		    FAST_STRING -> Maybe Name)	-- name lookup fn for tycons/classes

builtinNameInfo switch_is_on
  = (init_val_lookup_fn, init_tc_lookup_fn)
  where
    --
    -- values (including data constructors)
    --
    init_val_lookup_fn
      =	if	switch_is_on HideBuiltinNames then
		(\ x -> Nothing)
	else if switch_is_on HideMostBuiltinNames then
		lookupFM (listToFM min_val_assoc_list)
		-- OLD: mkStringLookupFn min_val_assoc_list False{-not pre-sorted-}
	else
		lookupFM (listToFM (concat list_of_val_assoc_lists))
		-- mkStringLookupFn (concat list_of_val_assoc_lists) False{-not pre-sorted-}

    min_val_assoc_list		-- this is an ad-hoc list; what "happens"
	=  totally_wired_in_Ids	-- to be needed (when compiling bits of
	++ unboxed_ops		-- Prelude).
	++ (concat (map pcDataConNameInfo min_nonprim_tycon_list))

    -- We let a lot of "non-standard" values be visible, so that we
    -- can make sense of them in interface pragmas.  It's cool, though
    -- -- they all have "non-standard" names, so they won't get past
    -- the parser in user code.
    list_of_val_assoc_lists
    	= [ -- each list is empty or all there

	    totally_wired_in_Ids,

	    concat (map pcDataConNameInfo data_tycons),

	    unboxed_ops,

	    if switch_is_on ForConcurrent then parallel_vals else []
	  ]

    --
    -- type constructors and classes
    --
    init_tc_lookup_fn
      =	if	switch_is_on HideBuiltinNames then
		(\ x -> Nothing)
	else if switch_is_on HideMostBuiltinNames then
		lookupFM (listToFM min_tc_assoc_list)
		--OLD: mkStringLookupFn min_tc_assoc_list False{-not pre-sorted-}
	else
		lookupFM (listToFM (
		-- OLD: mkStringLookupFn
		    map pcTyConNameInfo (data_tycons ++ synonym_tycons)
		    ++ std_tycon_list -- TyCons not quite so wired in
		    ++ std_class_list
		    ++ prim_tys))
		    -- The prim_tys,etc., are OK, because they all
		    -- have "non-standard" names (and we really
		    -- want them for interface pragmas).
		  --OLD: False{-not pre-sorted-}

    min_tc_assoc_list	-- again, pretty ad-hoc
	= prim_tys ++ (map pcTyConNameInfo min_nonprim_tycon_list)
--HA!	  ++ std_class_list -- no harm in this

min_nonprim_tycon_list -- used w/ HideMostBuiltinNames
  = [ boolTyCon,
      cmpTagTyCon,
      charTyCon,
      intTyCon,
      floatTyCon,
      doubleTyCon,
      integerTyCon,
      ratioTyCon,
      liftTyCon,
      return2GMPsTyCon,	-- ADR asked for these last two (WDP 94/11)
      returnIntAndGMPTyCon ]

-- sigh: I (WDP) think these should be local defns
-- but you cannot imagine how bad it is for speed (w/ GHC)
prim_tys    = map pcTyConNameInfo prim_tycons

-- values

totally_wired_in_Ids
  = [(SLIT(":"),		WiredInVal consDataCon),
     (SLIT("error"),		WiredInVal eRROR_ID),
     (SLIT("patError#"),	WiredInVal pAT_ERROR_ID), -- occurs in i/faces
     (SLIT("parError#"),	WiredInVal pAR_ERROR_ID), -- ditto
     (SLIT("_trace"),		WiredInVal tRACE_ID),

     -- now the foldr/build Ids, which need to be built in
     -- because they have magic unfoldings
     (SLIT("_build"),		WiredInVal buildId),
     (SLIT("_augment"),		WiredInVal augmentId),
     (SLIT("foldl"),		WiredInVal foldlId),
     (SLIT("foldr"),		WiredInVal foldrId),
     (SLIT("unpackAppendPS#"),	WiredInVal unpackCStringAppendId),
     (SLIT("unpackFoldrPS#"),	WiredInVal unpackCStringFoldrId),

     (SLIT("_runST"),		WiredInVal runSTId),
     (SLIT("_seq_"),		WiredInVal seqId),  -- yes, used in sequential-land, too
						    -- WDP 95/11
    (SLIT("realWorld#"),	WiredInVal realWorldPrimId)
    ]

parallel_vals
  =[(SLIT("_par_"),		WiredInVal parId),
    (SLIT("_fork_"),		WiredInVal forkId)
#ifdef GRAN
    ,
    (SLIT("_parLocal_"),	WiredInVal parLocalId),
    (SLIT("_parGlobal_"),	WiredInVal parGlobalId)
    -- Add later:
    -- (SLIT("_parAt_"),	WiredInVal parAtId)
    -- (SLIT("_parAtForNow_"),	WiredInVal parAtForNowId)
    -- (SLIT("_copyable_"),	WiredInVal copyableId)
    -- (SLIT("_noFollow_"),	WiredInVal noFollowId)
#endif {-GRAN-}
   ]

unboxed_ops
  = (map primOpNameInfo lots_of_primops)
   ++
    -- plus some of the same ones but w/ different names
   [case (primOpNameInfo IntAddOp) 	of (_,n) -> (SLIT("+#"),   n),
    case (primOpNameInfo IntSubOp) 	of (_,n) -> (SLIT("-#"),   n),
    case (primOpNameInfo IntMulOp) 	of (_,n) -> (SLIT("*#"),   n),
    case (primOpNameInfo IntGtOp)  	of (_,n) -> (SLIT(">#"),   n),
    case (primOpNameInfo IntGeOp)  	of (_,n) -> (SLIT(">=#"),  n),
    case (primOpNameInfo IntEqOp)  	of (_,n) -> (SLIT("==#"),  n),
    case (primOpNameInfo IntNeOp)  	of (_,n) -> (SLIT("/=#"),  n),
    case (primOpNameInfo IntLtOp)  	of (_,n) -> (SLIT("<#"),   n),
    case (primOpNameInfo IntLeOp)  	of (_,n) -> (SLIT("<=#"),  n),
    case (primOpNameInfo DoubleAddOp)   of (_,n) -> (SLIT("+##"),  n),
    case (primOpNameInfo DoubleSubOp)   of (_,n) -> (SLIT("-##"),  n),
    case (primOpNameInfo DoubleMulOp)   of (_,n) -> (SLIT("*##"),  n),
    case (primOpNameInfo DoubleDivOp)   of (_,n) -> (SLIT("/##"),  n),
    case (primOpNameInfo DoublePowerOp) of (_,n) -> (SLIT("**##"), n),
    case (primOpNameInfo DoubleGtOp)    of (_,n) -> (SLIT(">##"),  n),
    case (primOpNameInfo DoubleGeOp)    of (_,n) -> (SLIT(">=##"), n),
    case (primOpNameInfo DoubleEqOp)    of (_,n) -> (SLIT("==##"), n),
    case (primOpNameInfo DoubleNeOp)    of (_,n) -> (SLIT("/=##"), n),
    case (primOpNameInfo DoubleLtOp)    of (_,n) -> (SLIT("<##"),  n),
    case (primOpNameInfo DoubleLeOp)    of (_,n) -> (SLIT("<=##"), n)]

prim_tycons
  = [addrPrimTyCon,
     arrayPrimTyCon,
     byteArrayPrimTyCon,
     charPrimTyCon,
     doublePrimTyCon,
     floatPrimTyCon,
     intPrimTyCon,
     mallocPtrPrimTyCon,
     mutableArrayPrimTyCon,
     mutableByteArrayPrimTyCon,
     synchVarPrimTyCon,
     realWorldTyCon,
     stablePtrPrimTyCon,
     statePrimTyCon,
     wordPrimTyCon
    ]

std_tycon_list
  = let
	swizzle_over (mod, nm, key, arity, is_data)
	  = let
		fname = mkPreludeCoreName mod nm
	    in
	    (nm, PreludeTyCon key fname arity is_data)
    in
    map swizzle_over
	[--(pRELUDE_IO,	   SLIT("Request"),  requestTyConKey,  0, True),
--OLD:	 (pRELUDE_IO,	   SLIT("Response"), responseTyConKey, 0, True),
	 (pRELUDE_IO,	   SLIT("Dialogue"), dialogueTyConKey, 0, False),
	 (SLIT("PreludeMonadicIO"), SLIT("IO"), iOTyConKey,    1, False)
	]

-- Several of these are non-std, but they have non-std
-- names, so they won't get past the parser in user code
-- (but will be visible for interface-pragma purposes).

data_tycons
  = [addrTyCon,
     boolTyCon,
--   byteArrayTyCon,
     charTyCon,
     cmpTagTyCon,
     doubleTyCon,
     floatTyCon,
     intTyCon,
     integerTyCon,
     liftTyCon,
     mallocPtrTyCon,
--   mutableArrayTyCon,
--   mutableByteArrayTyCon,
     ratioTyCon,
     return2GMPsTyCon,
     returnIntAndGMPTyCon,
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
     stateAndSynchVarPrimTyCon,
     stateAndPtrPrimTyCon,
     stateAndStablePtrPrimTyCon,
     stateAndWordPrimTyCon,
     stateTyCon,
     wordTyCon
#ifdef DPH
     ,podTyCon
#endif {- Data Parallel Haskell -}
    ]

synonym_tycons
  = [primIoTyCon,
     rationalTyCon,
     stTyCon,
     stringTyCon]

std_class_list
  = let
	swizzle_over (str, key)
	  = (str, PreludeClass key (mkPreludeCoreName pRELUDE_CORE str))
    in
    map swizzle_over
	[(SLIT("Eq"),		eqClassKey),
	 (SLIT("Ord"),		ordClassKey),
	 (SLIT("Num"),		numClassKey),
	 (SLIT("Real"),		realClassKey),
	 (SLIT("Integral"),	integralClassKey),
	 (SLIT("Fractional"),	fractionalClassKey),
	 (SLIT("Floating"),	floatingClassKey),
	 (SLIT("RealFrac"),	realFracClassKey),
	 (SLIT("RealFloat"),	realFloatClassKey),
	 (SLIT("Ix"),		ixClassKey),
	 (SLIT("Enum"),		enumClassKey),
	 (SLIT("Text"),		textClassKey),
	 (SLIT("_CCallable"),	cCallableClassKey),
	 (SLIT("_CReturnable"), cReturnableClassKey),
	 (SLIT("Binary"),	binaryClassKey)
#ifdef DPH
	 , (SLIT("Pid"),	pidClassKey)
	 , (SLIT("Processor"),processorClassKey)
#endif {- Data Parallel Haskell -}
	]

lots_of_primops
  = [	CharGtOp,
	CharGeOp,
	CharEqOp,
	CharNeOp,
	CharLtOp,
	CharLeOp,
	IntGtOp,
	IntGeOp,
	IntEqOp,
	IntNeOp,
	IntLtOp,
	IntLeOp,
	WordGtOp,
	WordGeOp,
	WordEqOp,
	WordNeOp,
	WordLtOp,
	WordLeOp,
	AddrGtOp,
	AddrGeOp,
	AddrEqOp,
	AddrNeOp,
	AddrLtOp,
	AddrLeOp,
	FloatGtOp,
	FloatGeOp,
	FloatEqOp,
	FloatNeOp,
	FloatLtOp,
	FloatLeOp,
	DoubleGtOp,
	DoubleGeOp,
	DoubleEqOp,
	DoubleNeOp,
	DoubleLtOp,
	DoubleLeOp,
	OrdOp,
	ChrOp,
	IntAddOp,
	IntSubOp,
	IntMulOp,
	IntQuotOp,
	IntRemOp,
	IntNegOp,
	AndOp,
	OrOp,
	NotOp,
    	SllOp,
    	SraOp,
    	SrlOp,
    	ISllOp,
    	ISraOp,
    	ISrlOp,
	Int2WordOp,
	Word2IntOp,
	Int2AddrOp,
	Addr2IntOp,
	FloatAddOp,
	FloatSubOp,
	FloatMulOp,
	FloatDivOp,
	FloatNegOp,
	Float2IntOp,
	Int2FloatOp,
	FloatExpOp,
	FloatLogOp,
	FloatSqrtOp,
	FloatSinOp,
	FloatCosOp,
	FloatTanOp,
	FloatAsinOp,
	FloatAcosOp,
	FloatAtanOp,
	FloatSinhOp,
	FloatCoshOp,
	FloatTanhOp,
	FloatPowerOp,
	DoubleAddOp,
	DoubleSubOp,
	DoubleMulOp,
	DoubleDivOp,
	DoubleNegOp,
	Double2IntOp,
	Int2DoubleOp,
	Double2FloatOp,
	Float2DoubleOp,
	DoubleExpOp,
	DoubleLogOp,
	DoubleSqrtOp,
	DoubleSinOp,
	DoubleCosOp,
	DoubleTanOp,
	DoubleAsinOp,
	DoubleAcosOp,
	DoubleAtanOp,
	DoubleSinhOp,
	DoubleCoshOp,
	DoubleTanhOp,
	DoublePowerOp,
	IntegerAddOp,
	IntegerSubOp,
	IntegerMulOp,
	IntegerQuotRemOp,
	IntegerDivModOp,
	IntegerNegOp,
	IntegerCmpOp,
	Integer2IntOp,
	Int2IntegerOp,
	Word2IntegerOp,
	Addr2IntegerOp,
	FloatEncodeOp,
	FloatDecodeOp,
	DoubleEncodeOp,
	DoubleDecodeOp,
	NewArrayOp,
	NewByteArrayOp CharKind,
	NewByteArrayOp IntKind,
	NewByteArrayOp AddrKind,
	NewByteArrayOp FloatKind,
	NewByteArrayOp DoubleKind,
	SameMutableArrayOp,
	SameMutableByteArrayOp,
	ReadArrayOp,
	WriteArrayOp,
	IndexArrayOp,
	ReadByteArrayOp CharKind,
	ReadByteArrayOp IntKind,
	ReadByteArrayOp AddrKind,
	ReadByteArrayOp FloatKind,
	ReadByteArrayOp DoubleKind,
	WriteByteArrayOp CharKind,
	WriteByteArrayOp IntKind,
	WriteByteArrayOp AddrKind,
	WriteByteArrayOp FloatKind,
	WriteByteArrayOp DoubleKind,
	IndexByteArrayOp CharKind,
	IndexByteArrayOp IntKind,
	IndexByteArrayOp AddrKind,
	IndexByteArrayOp FloatKind,
	IndexByteArrayOp DoubleKind,
	IndexOffAddrOp CharKind,
	IndexOffAddrOp IntKind,
	IndexOffAddrOp AddrKind,
	IndexOffAddrOp FloatKind,
	IndexOffAddrOp DoubleKind,
	UnsafeFreezeArrayOp,
	UnsafeFreezeByteArrayOp,
    	NewSynchVarOp,
	ReadArrayOp,
	TakeMVarOp,
	PutMVarOp,
	ReadIVarOp,
	WriteIVarOp,
	MakeStablePtrOp,
	DeRefStablePtrOp,
	ReallyUnsafePtrEqualityOp,
	ErrorIOPrimOp,
#ifdef GRAN
	ParGlobalOp,
	ParLocalOp,
#endif {-GRAN-}
	SeqOp,
    	ParOp,
    	ForkOp,
	DelayOp,
	WaitOp
    ]
\end{code}

\begin{code}
readUnfoldingPrimOp :: FAST_STRING -> PrimOp

readUnfoldingPrimOp
  = let
	-- "reverse" lookup table
	tbl = map (\ o -> let { (str,_) = primOpNameInfo o } in (str, o)) lots_of_primops
    in
    \ str -> case [ op | (s, op) <- tbl, s == str ] of
	       (op:_) -> op
#ifdef DEBUG
	       [] -> panic "readUnfoldingPrimOp" -- ++ _UNPK_ str ++"::"++show (map fst tbl))
#endif
\end{code}

Make table entries for various things:
\begin{code}
pcTyConNameInfo :: TyCon -> (FAST_STRING, Name)
pcTyConNameInfo tycon
  = (getOccurrenceName tycon, WiredInTyCon tycon)

pcDataConNameInfo :: TyCon -> [(FAST_STRING, Name)]
pcDataConNameInfo tycon
  = -- slurp out its data constructors...
    [(getOccurrenceName con, WiredInVal con) | con <- getTyConDataCons tycon]
\end{code}
