%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

Machine- and flag- specific bits that the abstract code generator has to know about.

No doubt there will be more...

\begin{code}
#include "HsVersions.h"

module MachDesc (
	Target, mkTarget, RegLoc(..), 

    	saveLoc,

	targetSwitches, fixedHeaderSize, varHeaderSize, stgReg,
	nativeOpt, sizeof, volatileSaves, volatileRestores, hpRel,
	amodeToStix, amodeToStix', charLikeClosureSize,
	intLikeClosureSize, mutHS, dataHS, primToStix, macroCode,
	heapCheck, codeGen, underscore, fmtAsmLbl,

	-- and, for self-sufficiency...
	AbstractC, CAddrMode, CExprMacro, CStmtMacro, MagicId,
	RegRelative, CSeq, BasicLit, CLabel, GlobalSwitch,
	SwitchResult, HeapOffset, PrimOp, PprStyle,
	PrimKind, SMRep, StixTree, Unique, SplitUniqSupply,
	StixTreeList(..), SUniqSM(..), Unpretty(..)
    ) where

import AbsCSyn
import CmdLineOpts  ( GlobalSwitch(..), stringSwitchSet, switchIsOn, SwitchResult(..) )
import Outputable
import OrdList	    ( OrdList )
import PrimKind	    ( PrimKind )
import SMRep	    ( SMRep )
import Stix
import SplitUniq
import Unique
import Unpretty	    ( PprStyle, CSeq )
import Util

data RegLoc = Save (StixTree) | Always (StixTree)

\end{code}

Think of this as a big runtime class dictionary

\begin{code}

data Target = Target
    (GlobalSwitch -> SwitchResult)  	-- switches
    Int     	    	    	    	-- fixedHeaderSize
    (SMRep -> Int)     	    	    	-- varHeaderSize
    (MagicId -> RegLoc)     	    	-- stgReg
    (StixTree -> StixTree)         	-- nativeOpt
    (PrimKind -> Int)     	    	-- sizeof
    ([MagicId] -> [StixTree])     	-- volatileSaves
    ([MagicId] -> [StixTree])     	-- volatileRestores
    (HeapOffset -> Int)	    	    	-- hpRel
    (CAddrMode -> StixTree)     	-- amodeToStix
    (CAddrMode -> StixTree)     	-- amodeToStix'
    Int 	    	    	    	-- charLikeClosureSize
    Int 	    	    	    	-- intLikeClosureSize
    StixTree    	    	    	-- mutHS
    StixTree    	    	    	-- dataHS
    ([CAddrMode] -> PrimOp -> [CAddrMode] -> SUniqSM StixTreeList)
    	    	    	    	    	-- primToStix
    (CStmtMacro -> [CAddrMode] -> SUniqSM StixTreeList)
    	    	    	    	    	-- macroCode
    (StixTree -> StixTree -> StixTree -> SUniqSM StixTreeList)
    	    	    	    	    	-- heapCheck

    (PprStyle -> [[StixTree]] -> SUniqSM Unpretty)
    	    	    	    	    	-- codeGen

    Bool    	    	   	    	-- underscore
    (String -> String)    	    	-- fmtAsmLbl

mkTarget = Target

targetSwitches (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = sw
fixedHeaderSize (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = fhs
varHeaderSize (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = vhs
stgReg (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = reg
nativeOpt (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = opt
sizeof (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = size
volatileSaves (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = vsave
volatileRestores (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = vrest
hpRel (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = hprel
amodeToStix (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = am
amodeToStix' (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = am'
charLikeClosureSize (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = csz
intLikeClosureSize (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = isz
mutHS (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = mhs
dataHS (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = dhs
primToStix (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = ps
macroCode (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = mc
heapCheck (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = hc
codeGen (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = cg
underscore (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = us
fmtAsmLbl (Target sw fhs vhs reg opt size vsave vrest hprel am am' csz isz mhs dhs ps mc hc cg us fmt) = fmt
\end{code}

Trees for register save locations

\begin{code}

saveLoc :: Target -> MagicId -> StixTree
saveLoc target reg = case stgReg target reg of {Always loc -> loc; Save loc -> loc}

\end{code}

