%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"

module StixInfo (
	genCodeInfoTable
    ) where

import AbsCSyn
import ClosureInfo
import MachDesc
import Maybes		( maybeToBool, Maybe(..) )
import SMRep		( SMRep(..), SMSpecRepKind(..), SMUpdateKind(..) )
import Stix
import SplitUniq
import Unique
import Unpretty
import Util

\end{code}

Generating code for info tables (arrays of data).

\begin{code}
static___rtbl	= sStLitLbl SLIT("Static___rtbl") -- out here to avoid CAF (sigh)
const___rtbl	= sStLitLbl SLIT("Const___rtbl")
charlike___rtbl	= sStLitLbl SLIT("CharLike___rtbl")
intlike___rtbl	= sStLitLbl SLIT("IntLike___rtbl")
gen_N___rtbl	= sStLitLbl SLIT("Gen_N___rtbl")
gen_S___rtbl	= sStLitLbl SLIT("Gen_S___rtbl")
gen_U___rtbl	= sStLitLbl SLIT("Gen_U___rtbl")
tuple___rtbl	= sStLitLbl SLIT("Tuple___rtbl")
data___rtbl	= sStLitLbl SLIT("Data___rtbl")
dyn___rtbl	= sStLitLbl SLIT("Dyn___rtbl")

genCodeInfoTable
    :: {-Target-}
       (HeapOffset -> Int)	-- needed bit of Target
    -> (CAddrMode -> StixTree)	-- ditto
    -> AbstractC
    -> SUniqSM StixTreeList

genCodeInfoTable hp_rel amode2stix (CClosureInfoAndCode cl_info _ _ upd cl_descr _) =
    returnSUs (\xs -> info : lbl : xs)

    where
	info = StData PtrKind table
	lbl = StLabel info_lbl

	table = case sm_rep of
	    StaticRep _ _ -> [
		StInt (toInteger ptrs),
                StInt (toInteger size),
                upd_code,
		static___rtbl,
                tag]

	    SpecialisedRep ConstantRep _ _ _ -> [
		StCLbl closure_lbl,
                upd_code,
                const___rtbl,
                tag]

	    SpecialisedRep CharLikeRep _ _ _ -> [
		upd_code,
		charlike___rtbl,
                tag]

	    SpecialisedRep IntLikeRep _ _ _ -> [
		upd_code,
                intlike___rtbl,
		tag]

	    SpecialisedRep _ _ _ updatable ->
		let rtbl = uppBesides (
		       if is_selector then
			  [uppPStr SLIT("Select__"),
			   uppInt select_word,
			   uppPStr SLIT("_rtbl")]
		       else
			  [uppPStr (case updatable of
				    SMNormalForm -> SLIT("Spec_N_")
				    SMSingleEntry -> SLIT("Spec_S_")
				    SMUpdatable -> SLIT("Spec_U_")
			           ),
			   uppInt size,
			   uppChar '_',
			   uppInt ptrs,
			   uppPStr SLIT("_rtbl")])
                in
		    case updatable of
			SMNormalForm -> [upd_code, StLitLbl rtbl, tag]
		        _            -> [StLitLbl rtbl, tag]

	    GenericRep _ _ updatable ->
                let rtbl = case updatable of
                            SMNormalForm  -> gen_N___rtbl
                            SMSingleEntry -> gen_S___rtbl
			    SMUpdatable   -> gen_U___rtbl
                in [
		    StInt (toInteger ptrs),
                    StInt (toInteger size),
		    upd_code,
                    rtbl,
                    tag]

	    BigTupleRep _ -> [
		tuple___rtbl,
		tag]
	    DataRep _     -> [
		data___rtbl,
		tag]
	    DynamicRep    -> [
		dyn___rtbl,
		tag]

	    PhantomRep -> [
		upd_code,
		info_unused,	-- no rep table
		tag]

	info_lbl	= infoTableLabelFromCI cl_info
	closure_lbl	= closureLabelFromCI   cl_info

	sm_rep	= closureSMRep cl_info
        maybe_selector = maybeSelectorInfo cl_info
        is_selector = maybeToBool maybe_selector
        (Just (_, select_word)) = maybe_selector

	tag = StInt (toInteger (closureSemiTag cl_info))

	size	= if isSpecRep sm_rep
		  then closureNonHdrSize cl_info
		  else hp_rel (closureSizeWithoutFixedHdr cl_info)
	ptrs	= closurePtrsSize cl_info

	upd_code = amode2stix upd

	info_unused = StInt (-1)

\end{code}
