%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section{Thin air Ids}

\begin{code}
module ThinAir (
	thinAirIdNames,	-- Names of non-wired-in Ids that may be used out of
	setThinAirIds,	-- thin air in any compilation. If they are not wired in
			-- we must be sure to import them from some Prelude 
			-- interface file even if they are not overtly 
			-- mentioned.  Subset of builtinNames.
	-- Here are the thin-air Ids themselves
	addr2IntegerId,
	packStringForCId, unpackCStringId, unpackCString2Id,
	unpackCStringAppendId, unpackCStringFoldrId,
	foldrId, buildId,

	noRepIntegerIds,
	noRepStrIds

	) where

#include "HsVersions.h"

import Var	( Id, varUnique )
import Name	( mkKnownKeyGlobal, varName )
import RdrName	( mkPreludeQual )
import PrelMods
import UniqFM	( UniqFM, listToUFM, lookupWithDefaultUFM ) 
import Unique
import Outputable
import IOExts
\end{code}


%************************************************************************
%*									*
\subsection{Thin air entities}
%*									*
%************************************************************************

These are Ids that we need to reference in various parts of the
system, and we'd like to pull them out of thin air rather than pass
them around.  We'd also like to have all the IdInfo available for each
one: i.e. everything that gets pulled out of the interface file.

The solution is to generate this map of global Ids after the
typechecker, and assign it to a global variable.  Any subsequent
pass may refer to the map to pull Ids out.  Any invalid
(i.e. pre-typechecker) access to the map will result in a panic.

\begin{code}
thinAirIdNames 
  = map mkKnownKeyGlobal
    [
	-- Needed for converting literals to Integers (used in tidyCoreExpr)
      (varQual pREL_NUM_Name SLIT("addr2Integer"), addr2IntegerIdKey)

	-- String literals
    , (varQual pREL_PACK_Name SLIT("packCString#"),   packCStringIdKey)
    , (varQual pREL_PACK_Name SLIT("unpackCString#"), unpackCStringIdKey)
    , (varQual pREL_PACK_Name SLIT("unpackNBytes#"),  unpackCString2IdKey)
    , (varQual pREL_PACK_Name SLIT("unpackAppendCString#"), unpackCStringAppendIdKey)
    , (varQual pREL_PACK_Name SLIT("unpackFoldrCString#"),  unpackCStringFoldrIdKey)

	-- Folds and builds; introduced by desugaring list comprehensions
    , (varQual pREL_BASE_Name SLIT("foldr"), foldrIdKey)
    , (varQual pREL_BASE_Name SLIT("build"), buildIdKey)
    ]

varQual = mkPreludeQual varName
\end{code}


\begin{code}
noRepIntegerIds = [addr2IntegerId]

noRepStrIds = [unpackCString2Id, unpackCStringId]

addr2IntegerId = lookupThinAirId addr2IntegerIdKey

packStringForCId = lookupThinAirId packCStringIdKey
unpackCStringId  = lookupThinAirId unpackCStringIdKey
unpackCString2Id = lookupThinAirId unpackCString2IdKey 
unpackCStringAppendId = lookupThinAirId unpackCStringAppendIdKey 
unpackCStringFoldrId  = lookupThinAirId unpackCStringFoldrIdKey 

foldrId = lookupThinAirId foldrIdKey
buildId = lookupThinAirId buildIdKey
\end{code}

\begin{code}
thinAirIdMapRef :: IORef (UniqFM Id)
thinAirIdMapRef = unsafePerformIO (newIORef (panic "thinAirIdMap: still empty"))

setThinAirIds :: [Id] -> IO ()
setThinAirIds thin_air_ids
  = writeIORef thinAirIdMapRef the_map
  where
    the_map = listToUFM [(varUnique id, id) | id <- thin_air_ids]

thinAirIdMap :: UniqFM Id
thinAirIdMap = unsafePerformIO (readIORef thinAirIdMapRef)
  -- Read it just once, the first time someone tugs on thinAirIdMap

lookupThinAirId :: Unique -> Id
lookupThinAirId uniq = lookupWithDefaultUFM thinAirIdMap
			(panic "lookupThinAirId: no mapping") uniq 
\end{code}

