%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
#include "HsVersions.h"

module PrelInfo (

	pRELUDE, pRELUDE_BUILTIN, pRELUDE_CORE, pRELUDE_RATIO,
	pRELUDE_LIST, pRELUDE_TEXT,
	pRELUDE_PRIMIO, pRELUDE_IO, pRELUDE_PS,
	gLASGOW_ST, gLASGOW_MISC,

	-- lookup functions for built-in names, for the renamer:
	builtinNameInfo,

	-- *odd* values that need to be reached out and grabbed:
	eRROR_ID, pAT_ERROR_ID, aBSENT_ERROR_ID,
	packStringForCId,
	unpackCStringId, unpackCString2Id,
	unpackCStringAppendId, unpackCStringFoldrId,
	integerZeroId, integerPlusOneId,
	integerPlusTwoId, integerMinusOneId,

	-----------------------------------------------------
	-- the rest of the export list is organised by *type*
	-----------------------------------------------------

	-- type: Bool
	boolTyCon, boolTy, falseDataCon, trueDataCon,

	-- types: Char#, Char, String (= [Char])
	charPrimTy, charTy, stringTy,
	charPrimTyCon, charTyCon, charDataCon,

	-- type: Ordering (used in deriving)
	orderingTy, ltDataCon, eqDataCon, gtDataCon,

	-- types: Double#, Double
	doublePrimTy, doubleTy,
	doublePrimTyCon, doubleTyCon, doubleDataCon,

	-- types: Float#, Float
	floatPrimTy, floatTy,
	floatPrimTyCon, floatTyCon, floatDataCon,

	-- types: Glasgow *primitive* arrays, sequencing and I/O
	mkPrimIoTy, -- to typecheck "mainPrimIO" & for _ccall_s
	realWorldStatePrimTy, realWorldStateTy{-boxed-},
	realWorldTy, realWorldTyCon, realWorldPrimId,
	statePrimTyCon, stateDataCon, getStatePairingConInfo,

	byteArrayPrimTy,

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

	-- type: tuples
	mkTupleTy, unitTy,

	-- for compilation of List Comprehensions and foldr
	foldlId, foldrId,
	mkBuild, buildId, augmentId, appendId

	-- and, finally, we must put in some (abstract) data types,
	-- to make the interface self-sufficient
    ) where

import Ubiq
import PrelLoop		( primOpNameInfo )

-- friends:
import PrelMods		-- Prelude module names
import PrelVals		-- VALUES
import PrimOp		( PrimOp(..), allThePrimOps )
import PrimRep		( PrimRep(..) )
import TysPrim		-- TYPES
import TysWiredIn

-- others:
import CmdLineOpts
import FiniteMap
import Id		( mkTupleCon, GenId{-instances-} )
import Name		( Name(..) )
import NameTypes	( mkPreludeCoreName, FullName, ShortName )
import TyCon		( tyConDataCons, mkFunTyCon, mkTupleTyCon, TyCon{-instances-} )
import Type
import Unique		-- *Key stuff
import Util		( nOfThem, panic )
\end{code}

%************************************************************************
%*									*
\subsection[builtinNameInfo]{Lookup built-in names}
%*									*
%************************************************************************

We have two ``builtin name funs,'' one to look up @TyCons@ and
@Classes@, the other to look up values.

\begin{code}
builtinNameInfo :: (FAST_STRING -> Maybe Name,	-- name lookup fn for values
		    FAST_STRING -> Maybe Name)	-- name lookup fn for tycons/classes

builtinNameInfo
  = (init_val_lookup_fn, init_tc_lookup_fn)
  where
    --
    -- values (including data constructors)
    --
    init_val_lookup_fn
      =	if	opt_HideBuiltinNames then
		(\ x -> Nothing)
	else if opt_HideMostBuiltinNames then
		lookupFM (listToFM (concat min_val_assoc_lists))
	else
		lookupFM (listToFM (concat val_assoc_lists))

    min_val_assoc_lists		-- min needed when compiling bits of Prelude
	= [
	    concat (map pcDataConNameInfo g_con_tycons),
	    concat (map pcDataConNameInfo min_nonprim_tycon_list),
	    totally_wired_in_Ids,
	    unboxed_ops
	  ]

    val_assoc_lists
    	= [
	    concat (map pcDataConNameInfo g_con_tycons),
	    concat (map pcDataConNameInfo data_tycons),
	    totally_wired_in_Ids,
	    unboxed_ops,
	    special_class_ops,
	    if opt_ForConcurrent then parallel_vals else []
	  ]

    --
    -- type constructors and classes
    --
    init_tc_lookup_fn
      =	if	opt_HideBuiltinNames then
		(\ x -> Nothing)
	else if opt_HideMostBuiltinNames then
		lookupFM (listToFM (concat min_tc_assoc_lists))
	else
		lookupFM (listToFM (concat tc_assoc_lists))

    min_tc_assoc_lists	-- again, pretty ad-hoc
	= [
	    map pcTyConNameInfo prim_tycons,
	    map pcTyConNameInfo g_tycons,
	    map pcTyConNameInfo min_nonprim_tycon_list
	  ]

    tc_assoc_lists
	= [
	    map pcTyConNameInfo prim_tycons,
	    map pcTyConNameInfo g_tycons,
	    map pcTyConNameInfo data_tycons,
	    map pcTyConNameInfo synonym_tycons,
	    std_tycon_list,
	    std_class_list
	  ]

    -- We let a lot of "non-standard" values be visible, so that we
    -- can make sense of them in interface pragmas. It's cool, though
    -- they all have "non-standard" names, so they won't get past
    -- the parser in user code.


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

g_tycons
  = mkFunTyCon : g_con_tycons

g_con_tycons
  = listTyCon : mkTupleTyCon 0 : [mkTupleTyCon i | i <- [2..32] ]

min_nonprim_tycon_list 	-- used w/ HideMostBuiltinNames
  = [ boolTyCon,
      orderingTyCon,
      charTyCon,
      intTyCon,
      floatTyCon,
      doubleTyCon,
      integerTyCon,
      ratioTyCon,
      liftTyCon,
      return2GMPsTyCon,	-- ADR asked for these last two (WDP 94/11)
      returnIntAndGMPTyCon ]

data_tycons
  = [addrTyCon,
     boolTyCon,
--   byteArrayTyCon,
     charTyCon,
     orderingTyCon,
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
    ]

synonym_tycons
  = [primIoTyCon,
     rationalTyCon,
     stTyCon,
     stringTyCon]


totally_wired_in_Ids
  = [(SLIT("error"),		WiredInVal eRROR_ID),
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

special_class_ops
  = let
	swizzle_over (str, key)
	  = (str, ClassOpName key bottom1 str bottom2)

	bottom1 = panic "PrelInfo.special_class_ops:class"
	bottom2 = panic "PrelInfo.special_class_ops:tag"
    in
     map swizzle_over
      [	(SLIT("fromInt"),	fromIntClassOpKey),
	(SLIT("fromInteger"),	fromIntegerClassOpKey),
	(SLIT("fromRational"),	fromRationalClassOpKey),
	(SLIT("enumFrom"),	enumFromClassOpKey),
	(SLIT("enumFromThen"),	enumFromThenClassOpKey),
	(SLIT("enumFromTo"),	enumFromToClassOpKey),
	(SLIT("enumFromThenTo"),enumFromThenToClassOpKey),
	(SLIT("=="),		eqClassOpKey),
	(SLIT(">="),		geClassOpKey),
	(SLIT("-"),		negateClassOpKey)
      ]

unboxed_ops
  =  map primOpNameInfo allThePrimOps
     -- plus some of the same ones but w/ different names ...
  ++ map fn funny_name_primops
  where
    fn (op,s) = case (primOpNameInfo op) of (_,n) -> (s,n)

funny_name_primops
  = [(IntAddOp,	     SLIT("+#")),
     (IntSubOp,      SLIT("-#")),
     (IntMulOp,      SLIT("*#")),
     (IntGtOp,       SLIT(">#")),
     (IntGeOp,       SLIT(">=#")),
     (IntEqOp,       SLIT("==#")),
     (IntNeOp,       SLIT("/=#")),
     (IntLtOp,       SLIT("<#")),
     (IntLeOp,       SLIT("<=#")),
     (DoubleAddOp,   SLIT("+##")),
     (DoubleSubOp,   SLIT("-##")),
     (DoubleMulOp,   SLIT("*##")),
     (DoubleDivOp,   SLIT("/##")),
     (DoublePowerOp, SLIT("**##")),
     (DoubleGtOp,    SLIT(">##")),
     (DoubleGeOp,    SLIT(">=##")),
     (DoubleEqOp,    SLIT("==##")),
     (DoubleNeOp,    SLIT("/=##")),
     (DoubleLtOp,    SLIT("<##")),
     (DoubleLeOp,    SLIT("<=##"))]


std_tycon_list
  = let
	swizzle_over (mod, nm, key, arity, is_data)
	  = let
		fname = mkPreludeCoreName mod nm
	    in
	    (nm, TyConName key fname arity is_data (panic "std_tycon_list:data_cons"))
    in
    map swizzle_over
	[(SLIT("PreludeMonadicIO"), SLIT("IO"), iOTyConKey,    1, False)
	]

std_class_list
  = let
	swizzle_over (str, key)
	  = (str, ClassName key (mkPreludeCoreName pRELUDE_CORE str) (panic "std_class_list:ops"))
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
	 (SLIT("Show"),		showClassKey),
	 (SLIT("Read"),		readClassKey),
	 (SLIT("Monad"),	monadClassKey),
	 (SLIT("MonadZero"),	monadZeroClassKey),
	 (SLIT("Binary"),	binaryClassKey),
	 (SLIT("_CCallable"),	cCallableClassKey),
	 (SLIT("_CReturnable"), cReturnableClassKey)
	]

\end{code}

Make table entries for various things:
\begin{code}
pcTyConNameInfo :: TyCon -> (FAST_STRING, Name)
pcTyConNameInfo tc = (getOccurrenceName tc, WiredInTyCon tc)

pcDataConNameInfo :: TyCon -> [(FAST_STRING, Name)]
pcDataConNameInfo tycon
  = -- slurp out its data constructors...
    [ (getOccurrenceName con, WiredInVal con) | con <- tyConDataCons tycon ]
\end{code}
