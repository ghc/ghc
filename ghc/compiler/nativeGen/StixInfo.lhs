%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixInfo (

	genCodeInfoTable, genBitmapInfoTable,

	bitmapToIntegers, bitmapIsSmall, livenessIsSmall

    ) where

#include "HsVersions.h"
#include "../includes/config.h"
#include "NCG.h"

import AbsCSyn		( AbstractC(..), Liveness(..), C_SRT(..), needsSRT )
import StgSyn		( SRT(..) )
import ClosureInfo	( closurePtrsSize,
			  closureNonHdrSize, closureSMRep,
			  infoTableLabelFromCI,
			  closureSRT, closureSemiTag
			)
import PrimRep		( PrimRep(..) )
import SMRep		( getSMRepClosureTypeInt )
import Stix		-- all of it
import UniqSupply	( returnUs, UniqSM )
import BitSet		( BitSet, intBS )
import Maybes		( maybeToBool )

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
    -> UniqSM StixStmtList

genCodeInfoTable (CClosureInfoAndCode cl_info _ _ cl_descr)
  = returnUs (\xs -> StData PtrRep table : StLabel info_lbl : xs)

    where
	info_lbl  = infoTableLabelFromCI cl_info

	table | needs_srt = srt_label : rest_of_table
	      | otherwise = rest_of_table

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
        type_info = (fromInt closure_type `shiftL` 16) .|.
		    (fromInt srt_len)
#else 
        type_info = (fromInt closure_type) .|.
		    (fromInt srt_len `shiftL` 16)
#endif	     
	srt	  = closureSRT cl_info	     
        needs_srt = needsSRT srt

	(srt_label,srt_len)
           | is_constr
           = (StInt 0, tag)
           | otherwise
	   = case srt of
		NoC_SRT 	  -> (StInt 0, 0)
		C_SRT lbl off len -> (StIndex DataPtrRep (StCLbl lbl) (StInt (toInteger off)), len)

        maybe_tag = closureSemiTag cl_info
        is_constr = maybeToBool maybe_tag
        (Just tag) = maybe_tag

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



genBitmapInfoTable
	:: Liveness
	-> C_SRT
	-> Int
	-> Bool			-- must include SRT field (i.e. it's a vector)
	-> UniqSM StixStmtList

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
		      Liveness lbl mask ->
			case bitmapToIntegers mask of
			[ ] -> StInt 0
			[i] -> StInt i
			_   -> StCLbl lbl

	type_info :: Word32
#ifdef WORDS_BIGENDIAN
        type_info = (fromInt closure_type `shiftL` 16) .|.
		    (fromInt srt_len)
#else 
        type_info = (fromInt closure_type) .|.
		    (fromInt srt_len `shiftL` 16)
#endif	     

	(srt_label,srt_len) = 
	     case srt of
		NoC_SRT -> (StInt 0, 0)
		C_SRT lbl off len -> 
			(StIndex DataPtrRep (StCLbl lbl) 
				(StInt (toInteger off)), len)

bitmapToIntegers :: [BitSet] -> [Integer]
bitmapToIntegers = bundle . map (toInteger . intBS)
  where
#if BYTES_PER_WORD == 4
    bundle = id
#else
    bundle [] = []
    bundle is = case splitAt (BYTES_PER_WORD/4) is of
                (these, those) ->
		    ( foldr1 (\x y -> x + 4294967296 * y)
			     [x `mod` 4294967296 | x <- these]
		    : bundle those
		    )
#endif

bitmapIsSmall :: [BitSet] -> Bool
bitmapIsSmall bitmap
  = case bitmapToIntegers bitmap of
    _:_:_ -> False
    _     -> True

livenessIsSmall :: Liveness -> Bool
livenessIsSmall (Liveness _ mask) = bitmapIsSmall mask
\end{code}
