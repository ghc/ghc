%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

Machine- and flag- specific bits that the abstract code generator has to know about.

No doubt there will be more...

\begin{code}
#include "HsVersions.h"

module MachDesc (
	Target(..){-(..) for target_STRICT only-}, mkTarget, RegLoc(..), 

    	saveLoc,

--	targetSwitches, UNUSED FOR NOW
	fixedHeaderSize, varHeaderSize, stgReg,
--	nativeOpt, UNUSED FOR NOW
	sizeof, volatileSaves, volatileRestores, hpRel,
	amodeToStix, amodeToStix', charLikeClosureSize,
	intLikeClosureSize, mutHS, dataHS, primToStix, macroCode,
	heapCheck,
--	codeGen, underscore, fmtAsmLbl, UNUSED FOR NOW (done a diff way)

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
--  (GlobalSwitch -> SwitchResult)  	-- switches
    Int     	    	    	    	-- fixedHeaderSize
    (SMRep -> Int)     	    	    	-- varHeaderSize
    (MagicId -> RegLoc)     	    	-- stgReg
--  (StixTree -> StixTree)         	-- nativeOpt
    (PrimKind -> Int)     	    	-- sizeof
    (HeapOffset -> Int)	    	    	-- hpRel
    (CAddrMode -> StixTree)     	-- amodeToStix
    (CAddrMode -> StixTree)     	-- amodeToStix'
    (
    ([MagicId] -> [StixTree]),     	-- volatileSaves
    ([MagicId] -> [StixTree]),     	-- volatileRestores
    Int, 	    	    	    	-- charLikeClosureSize
    Int, 	    	    	    	-- intLikeClosureSize
    StixTree,    	    	    	-- mutHS
    StixTree,    	    	    	-- dataHS
    ([CAddrMode] -> PrimOp -> [CAddrMode] -> SUniqSM StixTreeList),
    	    	    	    	    	-- primToStix
    (CStmtMacro -> [CAddrMode] -> SUniqSM StixTreeList),
    	    	    	    	    	-- macroCode
    (StixTree -> StixTree -> StixTree -> SUniqSM StixTreeList)
    	    	    	    	    	-- heapCheck
    )
{- UNUSED: done a diff way:
    (PprStyle -> [[StixTree]] -> SUniqSM Unpretty)
    	    	    	    	    	-- codeGen

    Bool    	    	   	    	-- underscore
    (String -> String)    	    	-- fmtAsmLbl
-}

mkTarget = Target

{- UNUSED FOR NOW:
targetSwitches (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = {-sw-} x
-}
fixedHeaderSize (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) = fhs
varHeaderSize (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = vhs x
stgReg (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = reg x
{- UNUSED FOR NOW:
nativeOpt (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = {-opt-} x
-}
sizeof (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = size x
-- used only for wrapper-hungry PrimOps:
hpRel (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = hprel x
amodeToStix (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = am x
amodeToStix' (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' ~(vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = am' x

volatileSaves (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = vsave x
-- used only for wrapper-hungry PrimOps:
volatileRestores (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = vrest x
charLikeClosureSize (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) = csz
intLikeClosureSize (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) = isz
mutHS (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) = mhs
dataHS (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) = dhs
primToStix (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x y z = ps x y z
macroCode (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x y = mc x y
heapCheck (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x y z = hc x y z
{- UNUSED: done a diff way:
codeGen (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x y = cg x y
underscore (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) = us
fmtAsmLbl (Target {-sw-} fhs vhs reg {-opt-} size hprel am am' (vsave, vrest, csz, isz, mhs, dhs, ps, mc, hc) {-cg us fmt-}) x = fmt x
-}
\end{code}

Trees for register save locations

\begin{code}

saveLoc :: Target -> MagicId -> StixTree
saveLoc target reg = case stgReg target reg of {Always loc -> loc; Save loc -> loc}

\end{code}

