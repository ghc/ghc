%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixInfo ( genCodeInfoTable, genBitmapInfoTable ) where

#include "HsVersions.h"
#include "../includes/config.h"

import AbsCSyn		( AbstractC(..), Liveness(..) )
import CLabel		( CLabel )
import StgSyn		( SRT(..) )
import ClosureInfo	( closurePtrsSize,
			  closureNonHdrSize, closureSMRep,
			  infoTableLabelFromCI,
			  infoTblNeedsSRT, getSRTInfo
			)
import PrimRep		( PrimRep(..) )
import SMRep		( SMRep(..), getSMRepClosureTypeInt )
import Stix		-- all of it
import UniqSupply	( returnUs, UniqSM )
import Outputable	( int )
import BitSet		( intBS )

import Bits
import Word

#if __GLASGOW_HASKELL__ >= 404
import GlaExts		( fromInt )
#endif
\end{code}

Generating code for info tables (arrays of data).

\begin{code}
genCodeInfoTable
    :: AbstractC
    -> UniqSM StixTreeList

genCodeInfoTable (CClosureInfoAndCode cl_info _ _ cl_descr)
  = returnUs (\xs -> StData PtrRep table : StLabel info_lbl : xs)

    where
	info_lbl = infoTableLabelFromCI cl_info

	table | infoTblNeedsSRT cl_info	= srt_label : rest_of_table
	      | otherwise               = rest_of_table

	rest_of_table = 
		[
		{- par, prof, debug -} 
		  StInt (toInteger layout_info)
		, StInt (toInteger type_info)
		]

	-- sigh: building up the info table is endian-dependent.
	-- ToDo: do this using .byte and .word directives.
	type_info :: Word32
#ifdef WORDS_BIGENDIAN
        type_info = (fromInt flags `shiftL` 24) .|.
		    (fromInt closure_type `shiftL` 16) .|.
		    (fromInt srt_len)
#else 
        type_info = (fromInt flags) .|.
		    (fromInt closure_type `shiftL` 8) .|.
		    (fromInt srt_len `shiftL` 16)
#endif	     
	srt = getSRTInfo cl_info	     

	(srt_label,srt_len) = 
	     case srt of
		(lbl, NoSRT) -> (StInt 0, 0)
		(lbl, SRT off len) -> 
			(StIndex DataPtrRep (StCLbl lbl) 
				(StInt (toInteger off)), len)

	layout_info :: Word32
#ifdef WORDS_BIGENDIAN
	layout_info = (fromInt ptrs `shiftL` 16) .|. fromInt nptrs
#else 
	layout_info = (fromInt ptrs) .|. (fromInt nptrs `shiftL` 16)
#endif	     

    	ptrs    = closurePtrsSize cl_info
	nptrs	= size - ptrs

        size = closureNonHdrSize cl_info

	closure_type = getSMRepClosureTypeInt (closureSMRep cl_info)

	flags = 0 -- for now


genBitmapInfoTable
	:: Liveness
	-> (CLabel, SRT)
	-> Int
	-> Bool			-- must include SRT field (i.e. it's a vector)
	-> UniqSM StixTreeList

genBitmapInfoTable liveness srt closure_type include_srt
  = returnUs (\xs -> StData PtrRep table : xs)

  where
	table = if srt_len == 0 && not include_srt then
		   rest_of_table
		else
		   srt_label : rest_of_table

	rest_of_table = 
		[
		{- par, prof, debug -} 
		  layout_info
		, StInt (toInteger type_info)
		]

	layout_info = case liveness of
			LvSmall mask -> StInt (toInteger (intBS mask))
			LvLarge lbl  -> StCLbl lbl

	type_info :: Word32
#ifdef WORDS_BIGENDIAN
        type_info = (fromInt flags `shiftL` 24) .|.
		    (fromInt closure_type `shiftL` 16) .|.
		    (fromInt srt_len)
#else 
        type_info = (fromInt flags) .|.
		    (fromInt closure_type `shiftL` 8) .|.
		    (fromInt srt_len `shiftL` 16)
#endif	     

	(srt_label,srt_len) = 
	     case srt of
		(lbl, NoSRT) -> (StInt 0, 0)
		(lbl, SRT off len) -> 
			(StIndex DataPtrRep (StCLbl lbl) 
				(StInt (toInteger off)), len)

	flags = 0 -- for now
\end{code}
