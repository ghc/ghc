%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
#include "HsVersions.h"

module PrelInfo (

	-- finite maps for built-in things (for the renamer and typechecker):
	builtinNameInfo, builtinNameMaps,
	builtinValNamesMap, builtinTcNamesMap,
	builtinKeysMap,
	SYN_IE(BuiltinNames),
	SYN_IE(BuiltinKeys), SYN_IE(BuiltinIdInfos),

	maybeCharLikeTyCon, maybeIntLikeTyCon
    ) where

IMP_Ubiq()
IMPORT_DELOOPER(PrelLoop) ( primOpNameInfo )

-- friends:
import PrelMods		-- Prelude module names
import PrelVals		-- VALUES
import PrimOp		( PrimOp(..), allThePrimOps )
import PrimRep		( PrimRep(..) )
import TysPrim		-- TYPES
import TysWiredIn

-- others:
import FiniteMap	( FiniteMap, emptyFM, listToFM )
import Id		( mkTupleCon, GenId, SYN_IE(Id) )
import Maybes		( catMaybes )
import Name		( origName, OrigName(..), Name )
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

type BuiltinNames   = (FiniteMap OrigName RnName, -- WiredIn Ids
		       FiniteMap OrigName RnName) -- WiredIn TyCons
			-- Two maps because "[]" is in both...

type BuiltinKeys    = FiniteMap OrigName (Unique, Name -> RnName)
						     -- Names with known uniques

type BuiltinIdInfos = UniqFM IdInfo		     -- Info for known unique Ids

builtinNameMaps    = case builtinNameInfo of { (x,_,_) -> x }
builtinKeysMap	   = case builtinNameInfo of { (_,x,_) -> x }
builtinValNamesMap = fst builtinNameMaps
builtinTcNamesMap  = snd builtinNameMaps

builtinNameInfo
  = ( (listToFM assoc_val_wired, listToFM assoc_tc_wired)
    , listToFM assoc_keys
    , listToUFM assoc_id_infos
    )
  where
    assoc_val_wired
    	= concat [
	    -- data constrs
	    concat (map pcDataConWiredInInfo g_con_tycons),
	    concat (map pcDataConWiredInInfo data_tycons),

	    -- values
	    map pcIdWiredInInfo wired_in_ids,
	    primop_ids
	  ]
    assoc_tc_wired
    	= concat [
	    -- tycons
	    map pcTyConWiredInInfo prim_tycons,
	    map pcTyConWiredInInfo g_tycons,
	    map pcTyConWiredInInfo data_tycons
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
    id_key (str_mod, uniq, info) = (str_mod, (uniq, RnImplicit))

    assoc_id_infos = catMaybes (map assoc_info id_keys_infos)
    assoc_info (str_mod, uniq, Just info) = Just (uniq, info)
    assoc_info (str_mod, uniq, Nothing)   = Nothing
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
    , foreignObjPrimTyCon
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
  = listTyCon : mkTupleTyCon 0 : [mkTupleTyCon i | i <- [2..37] ]

data_tycons
  = [ addrTyCon
    , boolTyCon
    , charTyCon
    , doubleTyCon
    , floatTyCon
    , foreignObjTyCon
    , intTyCon
    , integerTyCon
    , liftTyCon
    , primIoTyCon
    , return2GMPsTyCon
    , returnIntAndGMPTyCon
    , stTyCon
    , stablePtrTyCon
    , stateAndAddrPrimTyCon
    , stateAndArrayPrimTyCon
    , stateAndByteArrayPrimTyCon
    , stateAndCharPrimTyCon
    , stateAndDoublePrimTyCon
    , stateAndFloatPrimTyCon
    , stateAndForeignObjPrimTyCon
    , stateAndIntPrimTyCon
    , stateAndMutableArrayPrimTyCon
    , stateAndMutableByteArrayPrimTyCon
    , stateAndPtrPrimTyCon
    , stateAndStablePtrPrimTyCon
    , stateAndSynchVarPrimTyCon
    , stateAndWordPrimTyCon
    , stateTyCon
    , voidTyCon
    , wordTyCon
    ]
\end{code}

The WiredIn Ids ...
ToDo: Some of these should be moved to id_keys_infos!
\begin{code}
wired_in_ids
  = [ aBSENT_ERROR_ID
    , augmentId
    , buildId
--  , copyableId
    , eRROR_ID
    , foldlId
    , foldrId
--  , forkId
    , iRREFUT_PAT_ERROR_ID
    , integerMinusOneId
    , integerPlusOneId
    , integerPlusTwoId
    , integerZeroId
    , nON_EXHAUSTIVE_GUARDS_ERROR_ID
    , nO_DEFAULT_METHOD_ERROR_ID
    , nO_EXPLICIT_METHOD_ERROR_ID
--  , noFollowId
    , pAR_ERROR_ID
    , pAT_ERROR_ID
    , packStringForCId
--    , parAtAbsId
--    , parAtForNowId
--    , parAtId
--    , parAtRelId
--    , parGlobalId
--    , parId
--    , parLocalId
    , rEC_CON_ERROR_ID
    , rEC_UPD_ERROR_ID
    , realWorldPrimId
    , runSTId
--    , seqId
    , tRACE_ID
    , unpackCString2Id
    , unpackCStringAppendId
    , unpackCStringFoldrId
    , unpackCStringId
    , voidId
    ]

pcTyConWiredInInfo :: TyCon -> (OrigName, RnName)
pcTyConWiredInInfo tc = (origName "pcTyConWiredInInfo" tc, WiredInTyCon tc)

pcDataConWiredInInfo :: TyCon -> [(OrigName, RnName)]
pcDataConWiredInInfo tycon
  = [ (origName "pcDataConWiredInInfo" con, WiredInId con) | con <- tyConDataCons tycon ]

pcIdWiredInInfo :: Id -> (OrigName, RnName)
pcIdWiredInInfo id = (origName "pcIdWiredInInfo" id, WiredInId id)
\end{code}

WiredIn primitive numeric operations ...
\begin{code}
primop_ids
  = map prim_fn allThePrimOps ++ map funny_fn funny_name_primops
  where
    prim_fn  op     = case (primOpNameInfo op) of (s,n) -> ((OrigName gHC_BUILTINS s),n)
    funny_fn (op,s) = case (primOpNameInfo op) of (_,n) -> ((OrigName gHC_BUILTINS s),n)

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
id_keys_infos :: [(OrigName, Unique, Maybe IdInfo)]
id_keys_infos
  = [ -- here because we use them in derived instances
      (OrigName pRELUDE SLIT("&&"),		andandIdKey,	Nothing)
    , (OrigName pRELUDE SLIT("."),		composeIdKey,	Nothing)
    , (OrigName gHC__   SLIT("lex"),		lexIdKey,	Nothing)
    , (OrigName pRELUDE SLIT("not"),		notIdKey,	Nothing)
    , (OrigName pRELUDE SLIT("readParen"),	readParenIdKey,	Nothing)
    , (OrigName pRELUDE SLIT("showParen"),	showParenIdKey,	Nothing)
    , (OrigName pRELUDE SLIT("showString"),	showStringIdKey,Nothing)
    , (OrigName gHC__   SLIT("readList__"),	ureadListIdKey,	Nothing)
    , (OrigName gHC__   SLIT("showList__"),	ushowListIdKey,	Nothing)
    , (OrigName gHC__   SLIT("showSpace"),	showSpaceIdKey,	Nothing)
    ]

tysyn_keys
  = [ (OrigName gHC__   SLIT("IO"),       (iOTyConKey, RnImplicitTyCon))
    , (OrigName pRELUDE SLIT("Ordering"), (orderingTyConKey, RnImplicitTyCon))
    , (OrigName rATIO   SLIT("Rational"), (rationalTyConKey, RnImplicitTyCon))
    , (OrigName rATIO   SLIT("Ratio"),    (ratioTyConKey, RnImplicitTyCon))
    ]

-- this "class_keys" list *must* include:
--  classes that are grabbed by key (e.g., eqClassKey)
--  classes in "Class.standardClassKeys" (quite a few)

class_keys
  = [ (str_mod, (k, RnImplicitClass)) | (str_mod,k) <-
    [ (OrigName pRELUDE SLIT("Eq"),		eqClassKey)		-- mentioned, derivable
    , (OrigName pRELUDE SLIT("Eval"),		evalClassKey)		-- mentioned
    , (OrigName pRELUDE SLIT("Ord"),		ordClassKey)		-- derivable
    , (OrigName pRELUDE SLIT("Num"),		numClassKey)		-- mentioned, numeric
    , (OrigName pRELUDE SLIT("Real"),		realClassKey)		-- numeric
    , (OrigName pRELUDE SLIT("Integral"),	integralClassKey)	-- numeric
    , (OrigName pRELUDE SLIT("Fractional"),	fractionalClassKey)	-- numeric
    , (OrigName pRELUDE SLIT("Floating"),	floatingClassKey)	-- numeric
    , (OrigName pRELUDE SLIT("RealFrac"),	realFracClassKey)	-- numeric
    , (OrigName pRELUDE SLIT("RealFloat"),	realFloatClassKey)	-- numeric
    , (OrigName iX	SLIT("Ix"),		ixClassKey)		-- derivable (but it isn't Prelude.Ix; hmmm)
    , (OrigName pRELUDE SLIT("Bounded"),	boundedClassKey)	-- derivable
    , (OrigName pRELUDE SLIT("Enum"),		enumClassKey)		-- derivable
    , (OrigName pRELUDE SLIT("Show"),		showClassKey)		-- derivable
    , (OrigName pRELUDE SLIT("Read"),		readClassKey)		-- derivable
    , (OrigName pRELUDE SLIT("Monad"),		monadClassKey)
    , (OrigName pRELUDE SLIT("MonadZero"),	monadZeroClassKey)
    , (OrigName pRELUDE SLIT("MonadPlus"),	monadPlusClassKey)
    , (OrigName pRELUDE SLIT("Functor"),	functorClassKey)
    , (OrigName gHC__	SLIT("CCallable"),	cCallableClassKey)	-- mentioned, ccallish
    , (OrigName gHC__   SLIT("CReturnable"), 	cReturnableClassKey)	-- mentioned, ccallish
    ]]

class_op_keys
  = [ (str_mod, (k, RnImplicit)) | (str_mod,k) <-
    [ (OrigName pRELUDE SLIT("fromInt"),	fromIntClassOpKey)
    , (OrigName pRELUDE SLIT("fromInteger"),	fromIntegerClassOpKey)
    , (OrigName pRELUDE SLIT("fromRational"),	fromRationalClassOpKey)
    , (OrigName pRELUDE SLIT("enumFrom"),	enumFromClassOpKey)
    , (OrigName pRELUDE SLIT("enumFromThen"),	enumFromThenClassOpKey)
    , (OrigName pRELUDE SLIT("enumFromTo"),	enumFromToClassOpKey)
    , (OrigName pRELUDE SLIT("enumFromThenTo"),enumFromThenToClassOpKey)
    , (OrigName pRELUDE SLIT("=="),		eqClassOpKey)
    , (OrigName pRELUDE SLIT(">>="),		thenMClassOpKey)
    , (OrigName pRELUDE SLIT("zero"),		zeroClassOpKey)
    ]]
\end{code}

ToDo: make it do the ``like'' part properly (as in 0.26 and before).
\begin{code}
maybeCharLikeTyCon tc = if (uniqueOf tc == charDataConKey) then Just charDataCon else Nothing
maybeIntLikeTyCon  tc = if (uniqueOf tc == intDataConKey)  then Just intDataCon  else Nothing
\end{code}
