%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[PrelInfo]{The @PrelInfo@ interface to the compiler's prelude knowledge}

\begin{code}
#include "HsVersions.h"

module PrelInfo (

	-- finite maps for built-in things (for the renamer and typechecker):
	builtinNameInfo, BuiltinNames(..),
	BuiltinKeys(..), BuiltinIdInfos(..),

	maybeCharLikeTyCon, maybeIntLikeTyCon
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
import Name		( moduleNamePair )
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

type BuiltinNames   = (FiniteMap (FAST_STRING,Module) RnName, -- WiredIn Ids
		       FiniteMap (FAST_STRING,Module) RnName) -- WiredIn TyCons
			-- Two maps because "[]" is in both...

type BuiltinKeys    = FiniteMap (FAST_STRING,Module) (Unique, Name -> RnName)
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
    , foreignObjTyCon
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
    , stateAndForeignObjPrimTyCon
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
	, copyableId
	, noFollowId
	, parAtAbsId
	, parAtForNowId
	, parAtId
	, parAtRelId
	, parGlobalId
    	, parLocalId
	]


pcTyConWiredInInfo :: TyCon -> ((FAST_STRING,Module), RnName)
pcTyConWiredInInfo tc = (swap (moduleNamePair tc), WiredInTyCon tc)

pcDataConWiredInInfo :: TyCon -> [((FAST_STRING,Module), RnName)]
pcDataConWiredInInfo tycon
  = [ (swap (moduleNamePair con), WiredInId con) | con <- tyConDataCons tycon ]

pcIdWiredInInfo :: Id -> ((FAST_STRING,Module), RnName)
pcIdWiredInInfo id = (swap (moduleNamePair id), WiredInId id)

swap (x,y) = (y,x)
\end{code}

WiredIn primitive numeric operations ...
\begin{code}
primop_ids
  = map prim_fn allThePrimOps ++ map funny_fn funny_name_primops
  where
    prim_fn  op     = case (primOpNameInfo op) of (s,n) -> ((s,pRELUDE),n)
    funny_fn (op,s) = case (primOpNameInfo op) of (_,n) -> ((s,pRELUDE),n)

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
id_keys_infos :: [((FAST_STRING,Module), Unique, Maybe IdInfo)]
id_keys_infos
  = [ ((SLIT("main"),SLIT("Main")),	  mainIdKey,	   Nothing)
    , ((SLIT("mainPrimIO"),SLIT("Main")), mainPrimIOIdKey, Nothing)
    ]

tysyn_keys
  = [ ((SLIT("IO"),pRELUDE), (iOTyConKey, RnImplicitTyCon))
    ]

-- this "class_keys" list *must* include:
--  classes that are grabbed by key (e.g., eqClassKey)
--  classes in "Class.standardClassKeys" (quite a few)

class_keys
  = [ (str_mod, (k, RnImplicitClass)) | (str_mod,k) <-
    [ ((SLIT("Eq"),pRELUDE),		eqClassKey)		-- mentioned, derivable
    , ((SLIT("Eval"),pRELUDE),		evalClassKey)		-- mentioned
    , ((SLIT("Ord"),pRELUDE),		ordClassKey)		-- derivable
    , ((SLIT("Num"),pRELUDE),		numClassKey)		-- mentioned, numeric
    , ((SLIT("Real"),pRELUDE),		realClassKey)		-- numeric
    , ((SLIT("Integral"),pRELUDE),	integralClassKey)	-- numeric
    , ((SLIT("Fractional"),pRELUDE),	fractionalClassKey)	-- numeric
    , ((SLIT("Floating"),pRELUDE),	floatingClassKey)	-- numeric
    , ((SLIT("RealFrac"),pRELUDE),	realFracClassKey)	-- numeric
    , ((SLIT("RealFloat"),pRELUDE),	realFloatClassKey)	-- numeric
    , ((SLIT("Ix"),iX),			ixClassKey)		-- derivable (but it isn't Prelude.Ix; hmmm)
    , ((SLIT("Bounded"),pRELUDE),	boundedClassKey)	-- derivable
    , ((SLIT("Enum"),pRELUDE),		enumClassKey)		-- derivable
    , ((SLIT("Show"),pRELUDE),		showClassKey)		-- derivable
    , ((SLIT("Read"),pRELUDE),		readClassKey)		-- derivable
    , ((SLIT("Monad"),pRELUDE),		monadClassKey)
    , ((SLIT("MonadZero"),pRELUDE),	monadZeroClassKey)
    , ((SLIT("MonadPlus"),pRELUDE),	monadPlusClassKey)
    , ((SLIT("Functor"),pRELUDE),	functorClassKey)
    , ((SLIT("CCallable"),pRELUDE),	cCallableClassKey)	-- mentioned, ccallish
    , ((SLIT("CReturnable"),pRELUDE), 	cReturnableClassKey)	-- mentioned, ccallish
    ]]

class_op_keys
  = [ (str_mod, (k, RnImplicit)) | (str_mod,k) <-
    [ ((SLIT("fromInt"),pRELUDE),	fromIntClassOpKey)
    , ((SLIT("fromInteger"),pRELUDE),	fromIntegerClassOpKey)
    , ((SLIT("fromRational"),pRELUDE),	fromRationalClassOpKey)
    , ((SLIT("enumFrom"),pRELUDE),	enumFromClassOpKey)
    , ((SLIT("enumFromThen"),pRELUDE),	enumFromThenClassOpKey)
    , ((SLIT("enumFromTo"),pRELUDE),	enumFromToClassOpKey)
    , ((SLIT("enumFromThenTo"),pRELUDE),enumFromThenToClassOpKey)
    , ((SLIT("=="),pRELUDE),		eqClassOpKey)
    ]]
\end{code}

ToDo: make it do the ``like'' part properly (as in 0.26 and before).
\begin{code}
maybeCharLikeTyCon tc = if (uniqueOf tc == charDataConKey) then Just charDataCon else Nothing
maybeIntLikeTyCon  tc = if (uniqueOf tc == intDataConKey)  then Just intDataCon  else Nothing
\end{code}
