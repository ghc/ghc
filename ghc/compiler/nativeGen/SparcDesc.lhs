%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[SparcDesc]{The Sparc Machine Description}

\begin{code}
#include "HsVersions.h"

module SparcDesc (
    	mkSparc,

    	-- and assorted nonsense referenced by the class methods

        PprStyle, SMRep, MagicId, RegLoc, StixTree, PrimKind, SwitchResult

    ) where

import AbsCSyn
import AbsPrel	    ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import AsmRegAlloc  ( Reg, MachineCode(..), MachineRegisters(..),
		      RegLiveness(..), RegUsage(..), FutureLive(..)
		    )
import CLabelInfo   ( CLabel )
import CmdLineOpts  ( GlobalSwitch(..), stringSwitchSet, switchIsOn, SwitchResult(..) )
import HeapOffs	    ( hpRelToInt )
import MachDesc
import Maybes	    ( Maybe(..) )
import OrdList
import Outputable
import PrimKind	    ( PrimKind(..) )
import SMRep	    ( SMRep(..), SMSpecRepKind(..), SMUpdateKind(..) )
import SparcCode
import SparcGen	    ( sparcCodeGen )
import Stix
import StixMacro
import StixPrim
import SplitUniq
import Unique
import Util

\end{code}

Header sizes depend only on command-line options, not on the target
architecture.  (I think.)

\begin{code}

fhs :: (GlobalSwitch -> SwitchResult) -> Int

fhs switches = 1 + profFHS + ageFHS
  where
    profFHS = if switchIsOn switches SccProfilingOn then 1 else 0
    ageFHS  = if switchIsOn switches SccProfilingOn then 1 else 0

vhs :: (GlobalSwitch -> SwitchResult) -> SMRep -> Int

vhs switches sm = case sm of
    StaticRep _ _	   -> 0
    SpecialisedRep _ _ _ _ -> 0
    GenericRep _ _ _	   -> 0
    BigTupleRep _	   -> 1
    MuTupleRep _	   -> 2 {- (1 + GC_MUT_RESERVED_WORDS) -}
    DataRep _		   -> 1
    DynamicRep		   -> 2
    BlackHoleRep	   -> 0
    PhantomRep		   -> panic "vhs:phantom"

\end{code}

Here we map STG registers onto appropriate Stix Trees.  First, we
handle the two constants, @STK_STUB_closure@ and @vtbl_StdUpdFrame@.
The rest are either in real machine registers or stored as offsets
from BaseReg.

\begin{code}

sparcReg :: (GlobalSwitch -> SwitchResult) -> MagicId -> RegLoc

sparcReg switches x =
    case stgRegMap x of
	Just reg -> Save nonReg
	Nothing -> Always nonReg
    where nonReg = case x of
    	    StkStubReg -> sStLitLbl SLIT("STK_STUB_closure")
    	    StdUpdRetVecReg -> sStLitLbl SLIT("vtbl_StdUpdFrame")
    	    BaseReg -> sStLitLbl SLIT("MainRegTable")
    	    Hp -> StInd PtrKind (sStLitLbl SLIT("StorageMgrInfo"))
    	    HpLim -> StInd PtrKind (sStLitLbl SLIT("StorageMgrInfo+4"))
    	    TagReg -> StInd IntKind (StPrim IntSubOp [infoptr, StInt (1*4)])
    	    	      where 
    	    	    	  r2 = VanillaReg PtrKind ILIT(2)
    	    	    	  infoptr = case sparcReg switches r2 of
    	    	    	    	    	Always tree -> tree
    	    	    	    	    	Save _ -> StReg (StixMagicId r2)
    	    _ -> StInd (kindFromMagicId x)
	    	       (StPrim IntAddOp [baseLoc, StInt (toInteger (offset*4))])
    	  baseLoc = case stgRegMap BaseReg of
    	    Just _ -> StReg (StixMagicId BaseReg)
    	    Nothing -> sStLitLbl SLIT("MainRegTable")
          offset = baseRegOffset x
		    
\end{code}

Sizes in bytes.

\begin{code}

size pk = case kindToSize pk of
    {SB -> 1; UB -> 1; HW -> 2; UHW -> 2; W -> 4; D -> 8; F -> 4; DF -> 8}

\end{code}

Now the volatile saves and restores.  We add the basic guys to the list of ``user''
registers provided.  Note that there are more basic registers on the restore list,
because some are reloaded from constants.

\begin{code}

vsaves switches vols = 
    map save ((filter callerSaves) ([BaseReg,SpA,SuA,SpB,SuB,Hp,HpLim,RetReg{-,ActivityReg-}] ++ vols))
    where
        save x = StAssign (kindFromMagicId x) loc reg
    	    	    where reg = StReg (StixMagicId x)
    	    	    	  loc = case sparcReg switches x of
    	    	    	    	    Save loc -> loc
    	    	    	    	    Always loc -> panic "vsaves"

vrests switches vols = 
    map restore ((filter callerSaves) 
    	([BaseReg,SpA,SuA,SpB,SuB,Hp,HpLim,RetReg{-,ActivityReg-},StkStubReg,StdUpdRetVecReg] ++ vols))
    where
        restore x = StAssign (kindFromMagicId x) reg loc
    	    	    where reg = StReg (StixMagicId x)
    	    	    	  loc = case sparcReg switches x of
    	    	    	    	    Save loc -> loc
    	    	    	    	    Always loc -> panic "vrests"

\end{code}

Static closure sizes.

\begin{code}

charLikeSize, intLikeSize :: Target -> Int

charLikeSize target = 
    size PtrKind * (fixedHeaderSize target + varHeaderSize target charLikeRep + 1)
    where charLikeRep = SpecialisedRep CharLikeRep 0 1 SMNormalForm

intLikeSize target = 
    size PtrKind * (fixedHeaderSize target + varHeaderSize target intLikeRep + 1)
    where intLikeRep = SpecialisedRep IntLikeRep 0 1 SMNormalForm

mhs, dhs :: (GlobalSwitch -> SwitchResult) -> StixTree

mhs switches = StInt (toInteger words)
  where 
    words = fhs switches + vhs switches (MuTupleRep 0)

dhs switches = StInt (toInteger words)
  where 
    words = fhs switches + vhs switches (DataRep 0)

\end{code}

Setting up a sparc target.

\begin{code}

mkSparc :: Bool
	-> (GlobalSwitch -> SwitchResult)
	-> (Target,
	    (PprStyle -> [[StixTree]] -> SUniqSM Unpretty), -- codeGen
	    Bool,					    -- underscore
	    (String -> String))				    -- fmtAsmLbl

mkSparc decentOS switches = 
    let
	fhs' = fhs switches
    	vhs' = vhs switches
    	sparcReg' = sparcReg switches
    	vsaves' = vsaves switches
    	vrests' = vrests switches
    	hprel = hpRelToInt target 
        as = amodeCode target
        as' = amodeCode' target
    	csz = charLikeSize target
    	isz = intLikeSize target
    	mhs' = mhs switches
    	dhs' = dhs switches
    	ps = genPrimCode target
    	mc = genMacroCode target
    	hc = doHeapCheck --UNUSED NOW: target
    	target = mkTarget {-switches-} fhs' vhs' sparcReg' {-id-} size
    	    	    	  hprel as as'
			  (vsaves', vrests', csz, isz, mhs', dhs', ps, mc, hc)
    	    	    	  {-sparcCodeGen decentOS id-}
    in
    (target, sparcCodeGen, decentOS, id)
\end{code}
