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

	-- finite maps for built-in things (for the renamer and typechecker):
	builtinNameInfo, BuiltinNames(..),
	BuiltinKeys(..), BuiltinIdInfos(..),

	-- *odd* values that need to be reached out and grabbed:
	eRROR_ID,
	pAT_ERROR_ID,
	rEC_CON_ERROR_ID,
	rEC_UPD_ERROR_ID,
	iRREFUT_PAT_ERROR_ID,
	nON_EXHAUSTIVE_GUARDS_ERROR_ID,
	aBSENT_ERROR_ID,
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
import CmdLineOpts	( opt_HideBuiltinNames,
			  opt_HideMostBuiltinNames,
			  opt_ForConcurrent
			)
import FiniteMap	( FiniteMap, emptyFM, listToFM )
import Id		( mkTupleCon, GenId, Id(..) )
import Maybes		( catMaybes )
import Name		( origName, nameOf )
import RnHsSyn		( RnName(..) )
import TyCon		( tyConDataCons, mkFunTyCon, mkTupleTyCon, TyCon )
import Type
import UniqFM		( UniqFM, emptyUFM, listToUFM )
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
builtinNameInfo :: ( BuiltinNames, BuiltinKeys, BuiltinIdInfos )

type BuiltinNames   = (FiniteMap FAST_STRING RnName, -- WiredIn Ids
		       FiniteMap FAST_STRING RnName) -- WiredIn TyCons
			-- Two maps because "[]" is in both...
type BuiltinKeys    = FiniteMap FAST_STRING (Unique, Name -> RnName)
						    -- Names with known uniques
type BuiltinIdInfos = UniqFM IdInfo		     -- Info for known unique Ids

builtinNameInfo
  = if opt_HideBuiltinNames then
	(
	 (emptyFM, emptyFM),
	 emptyFM,
	 emptyUFM
	)
    else if opt_HideMostBuiltinNames then
	(
	 (listToFM min_assoc_val_wired, listToFM min_assoc_tc_wired),
	 emptyFM,
	 emptyUFM
	)
    else
	(
	 (listToFM assoc_val_wired, listToFM assoc_tc_wired),
	 listToFM assoc_keys,
	 listToUFM assoc_id_infos
	)

  where
    min_assoc_val_wired	-- min needed when compiling bits of Prelude
      = concat [
	    -- data constrs
	    concat (map pcDataConWiredInInfo g_con_tycons),
	    concat (map pcDataConWiredInInfo min_nonprim_tycon_list),

	    -- values
	    map pcIdWiredInInfo wired_in_ids,
	    primop_ids
	 ]
    min_assoc_tc_wired
      = concat [
	    -- tycons
	    map pcTyConWiredInInfo prim_tycons,
	    map pcTyConWiredInInfo g_tycons,
	    map pcTyConWiredInInfo min_nonprim_tycon_list
	 ]

    assoc_val_wired
    	= concat [
	    -- data constrs
	    concat (map pcDataConWiredInInfo g_con_tycons),
	    concat (map pcDataConWiredInInfo data_tycons),

	    -- values
	    map pcIdWiredInInfo wired_in_ids,
	    map pcIdWiredInInfo parallel_ids,
	    primop_ids
	  ]
    assoc_tc_wired
    	= concat [
	    -- tycons
	    map pcTyConWiredInInfo prim_tycons,
	    map pcTyConWiredInInfo g_tycons,
	    map pcTyConWiredInInfo data_tycons,
	    map pcTyConWiredInInfo synonym_tycons
	  ]

    assoc_keys
	= concat
	  [
	    id_keys,
	    tysyn_keys,
	    class_keys,
	    class_op_keys
	  ]

    id_keys = map id_key id_keys_infos
    id_key (str, uniq, info) = (str, (uniq, RnImplicit))

    assoc_id_infos = catMaybes (map assoc_info id_keys_infos)
    assoc_info (str, uniq, Just info) = Just (uniq, info)
    assoc_info (str, uniq, Nothing)   = Nothing
\end{code}


We let a lot of "non-standard" values be visible, so that we can make
sense of them in interface pragmas. It's cool, though they all have
"non-standard" names, so they won't get past the parser in user code.

The WiredIn TyCons and DataCons ...
\begin{code}

prim_tycons
  = [ addrPrimTyCon
    , arrayPrimTyCon
    , byteArrayPrimTyCon
    , charPrimTyCon
    , doublePrimTyCon
    , floatPrimTyCon
    , intPrimTyCon
    , mallocPtrPrimTyCon
    , mutableArrayPrimTyCon
    , mutableByteArrayPrimTyCon
    , synchVarPrimTyCon
    , realWorldTyCon
    , stablePtrPrimTyCon
    , statePrimTyCon
    , wordPrimTyCon
    ]

g_tycons
  = mkFunTyCon : g_con_tycons

g_con_tycons
  = listTyCon : mkTupleTyCon 0 : [mkTupleTyCon i | i <- [2..32] ]

min_nonprim_tycon_list 	-- used w/ HideMostBuiltinNames
  = [ boolTyCon
    , orderingTyCon
    , charTyCon
    , intTyCon
    , floatTyCon
    , doubleTyCon
    , integerTyCon
    , ratioTyCon
    , liftTyCon
    , return2GMPsTyCon	-- ADR asked for these last two (WDP 94/11)
    , returnIntAndGMPTyCon
    ]


data_tycons
  = [ addrTyCon
    , boolTyCon
    , charTyCon
    , orderingTyCon
    , doubleTyCon
    , floatTyCon
    , intTyCon
    , integerTyCon
    , liftTyCon
    , mallocPtrTyCon
    , ratioTyCon
    , return2GMPsTyCon
    , returnIntAndGMPTyCon
    , stablePtrTyCon
    , stateAndAddrPrimTyCon
    , stateAndArrayPrimTyCon
    , stateAndByteArrayPrimTyCon
    , stateAndCharPrimTyCon
    , stateAndDoublePrimTyCon
    , stateAndFloatPrimTyCon
    , stateAndIntPrimTyCon
    , stateAndMallocPtrPrimTyCon
    , stateAndMutableArrayPrimTyCon
    , stateAndMutableByteArrayPrimTyCon
    , stateAndSynchVarPrimTyCon
    , stateAndPtrPrimTyCon
    , stateAndStablePtrPrimTyCon
    , stateAndWordPrimTyCon
    , stateTyCon
    , wordTyCon
    ]

synonym_tycons
  = [ primIoTyCon
    , rationalTyCon
    , stTyCon
    , stringTyCon
    ]

pcTyConWiredInInfo :: TyCon -> (FAST_STRING, RnName)
pcTyConWiredInInfo tc = (nameOf (origName tc), WiredInTyCon tc)

pcDataConWiredInInfo :: TyCon -> [(FAST_STRING, RnName)]
pcDataConWiredInInfo tycon
  = [ (nameOf (origName con), WiredInId con) | con <- tyConDataCons tycon ]
\end{code}

The WiredIn Ids ...
ToDo: Some of these should be moved to id_keys_infos!
\begin{code}
wired_in_ids
  = [ eRROR_ID
    , pAT_ERROR_ID	-- occurs in i/faces
    , pAR_ERROR_ID	-- ditto
    , tRACE_ID
 
    , runSTId
    , seqId
    , realWorldPrimId

      -- foldr/build Ids have magic unfoldings
    , buildId
    , augmentId
    , foldlId
    , foldrId
    , unpackCStringAppendId
    , unpackCStringFoldrId
    ]

parallel_ids
  = if not opt_ForConcurrent then
	[]
    else
        [ parId
        , forkId
#ifdef GRAN
    	, parLocalId
	, parGlobalId
	    -- Add later:
	    -- ,parAtId
	    -- ,parAtForNowId
	    -- ,copyableId
	    -- ,noFollowId
#endif {-GRAN-}
	]

pcIdWiredInInfo :: Id -> (FAST_STRING, RnName)
pcIdWiredInInfo id = (nameOf (origName id), WiredInId id)
\end{code}

WiredIn primitive numeric operations ...
\begin{code}
primop_ids
  =  map primOpNameInfo allThePrimOps ++ map fn funny_name_primops
  where
    fn (op,s) = case (primOpNameInfo op) of (_,n) -> (s,n)

funny_name_primops
  = [ (IntAddOp,      SLIT("+#"))
    , (IntSubOp,      SLIT("-#"))
    , (IntMulOp,      SLIT("*#"))
    , (IntGtOp,       SLIT(">#"))
    , (IntGeOp,       SLIT(">=#"))
    , (IntEqOp,       SLIT("==#"))
    , (IntNeOp,       SLIT("/=#"))
    , (IntLtOp,       SLIT("<#"))
    , (IntLeOp,       SLIT("<=#"))
    , (DoubleAddOp,   SLIT("+##"))
    , (DoubleSubOp,   SLIT("-##"))
    , (DoubleMulOp,   SLIT("*##"))
    , (DoubleDivOp,   SLIT("/##"))
    , (DoublePowerOp, SLIT("**##"))
    , (DoubleGtOp,    SLIT(">##"))
    , (DoubleGeOp,    SLIT(">=##"))
    , (DoubleEqOp,    SLIT("==##"))
    , (DoubleNeOp,    SLIT("/=##"))
    , (DoubleLtOp,    SLIT("<##"))
    , (DoubleLeOp,    SLIT("<=##"))
    ]
\end{code}


Ids, Synonyms, Classes and ClassOps with builtin keys.
For the Ids we may also have some builtin IdInfo.
\begin{code}
id_keys_infos :: [(FAST_STRING, Unique, Maybe IdInfo)]
id_keys_infos
  = [ (SLIT("main"),	    mainIdKey,		Nothing)
    , (SLIT("mainPrimIO"),  mainPrimIOIdKey,    Nothing)
    ]

tysyn_keys
  = [ (SLIT("IO"), (iOTyConKey, RnImplicitTyCon))
    ]

-- this "class_keys" list *must* include:
--  classes that are grabbed by key (e.g., eqClassKey)
--  classes in "Class.standardClassKeys" (quite a few)

class_keys
  = [ (s, (k, RnImplicitClass)) | (s,k) <-
    [ (SLIT("Eq"),		eqClassKey)		-- mentioned, derivable
    , (SLIT("Ord"),		ordClassKey)		-- derivable
    , (SLIT("Num"),		numClassKey)		-- mentioned, numeric
    , (SLIT("Real"),		realClassKey)		-- numeric
    , (SLIT("Integral"),	integralClassKey)	-- numeric
    , (SLIT("Fractional"),	fractionalClassKey)	-- numeric
    , (SLIT("Floating"),	floatingClassKey)	-- numeric
    , (SLIT("RealFrac"),	realFracClassKey)	-- numeric
    , (SLIT("RealFloat"),	realFloatClassKey)	-- numeric
--  , (SLIT("Ix"),		ixClassKey)
    , (SLIT("Bounded"),		boundedClassKey)	-- derivable
    , (SLIT("Enum"),		enumClassKey)		-- derivable
    , (SLIT("Show"),		showClassKey)		-- derivable
    , (SLIT("Read"),		readClassKey)		-- derivable
    , (SLIT("Monad"),		monadClassKey)
    , (SLIT("MonadZero"),	monadZeroClassKey)
    , (SLIT("CCallable"),	cCallableClassKey)	-- mentioned, ccallish
    , (SLIT("CReturnable"), 	cReturnableClassKey)	-- mentioned, ccallish
    ]]

class_op_keys
  = [ (s, (k, RnImplicit)) | (s,k) <-
    [ (SLIT("fromInt"),		fromIntClassOpKey)
    , (SLIT("fromInteger"),	fromIntegerClassOpKey)
    , (SLIT("fromRational"),	fromRationalClassOpKey)
    , (SLIT("enumFrom"),	enumFromClassOpKey)
    , (SLIT("enumFromThen"),	enumFromThenClassOpKey)
    , (SLIT("enumFromTo"),	enumFromToClassOpKey)
    , (SLIT("enumFromThenTo"),	enumFromThenToClassOpKey)
    , (SLIT("=="),		eqClassOpKey)
--  , (SLIT(">="),		geClassOpKey)
    ]]
\end{code}
