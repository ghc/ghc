%
% (c) The AQUA Project, Glasgow University, 1993-1996
%

\begin{code}
#include "HsVersions.h"

module Stix (
	CodeSegment(..), StixReg(..), StixTree(..), StixTreeList(..),
	sStLitLbl,

	stgBaseReg, stgStkOReg, stgNode, stgTagReg, stgRetReg,
	stgSpA, stgSuA, stgSpB, stgSuB, stgHp, stgHpLim, stgLivenessReg,
	stgStdUpdRetVecReg, stgStkStubReg,
	getUniqLabelNCG
    ) where

import Ubiq{-uitous-}

import AbsCSyn		( node, infoptr, MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import CLabel		( mkAsmTempLabel )
import UniqSupply	( returnUs, thenUs, getUnique, UniqSM(..) )
import Unpretty		( uppPStr, Unpretty(..) )
\end{code}

Here is the tag at the nodes of our @StixTree@.	 Notice its
relationship with @PrimOp@ in prelude/PrimOp.

\begin{code}
data StixTree
  = -- Segment (text or data)

    StSegment CodeSegment

    -- We can tag the leaves with constants/immediates.

  | StInt	Integer	    -- ** add Kind at some point
  | StDouble	Rational
  | StString	FAST_STRING
  | StLitLbl	Unpretty    -- literal labels
			    -- (will be _-prefixed on some machines)
  | StLitLit	FAST_STRING -- innards from CLitLit
  | StCLbl	CLabel	    -- labels that we might index into

    -- Abstract registers of various kinds

  | StReg StixReg

    -- A typed offset from a base location

  | StIndex PrimRep StixTree StixTree -- kind, base, offset

    -- An indirection from an address to its contents.

  | StInd PrimRep StixTree

    -- Assignment is typed to determine size and register placement

  | StAssign PrimRep StixTree StixTree -- dst, src

    -- A simple assembly label that we might jump to.

  | StLabel CLabel

    -- A function header and footer

  | StFunBegin CLabel
  | StFunEnd CLabel

    -- An unconditional jump. This instruction is terminal.
    -- Dynamic targets are allowed

  | StJump StixTree

    -- A fall-through, from slow to fast

  | StFallThrough CLabel

    -- A conditional jump. This instruction can be non-terminal :-)
    -- Only static, local, forward labels are allowed

  | StCondJump CLabel StixTree

    -- Raw data (as in an info table).

  | StData PrimRep [StixTree]

    -- Primitive Operations

  | StPrim PrimOp [StixTree]

    -- Calls to C functions

  | StCall FAST_STRING PrimRep [StixTree]

    -- Assembly-language comments

  | StComment FAST_STRING

sStLitLbl :: FAST_STRING -> StixTree
sStLitLbl s = StLitLbl (uppPStr s)
\end{code}

Stix registers can have two forms.  They {\em may} or {\em may not}
map to real, machine-level registers.

\begin{code}
data StixReg
  = StixMagicId MagicId	-- Regs which are part of the abstract machine model

  | StixTemp Unique PrimRep -- "Regs" which model local variables (CTemps) in
					-- the abstract C.
\end{code}

We hope that every machine supports the idea of data segment and text
segment (or that it has no segments at all, and we can lump these
together).

\begin{code}
data CodeSegment = DataSegment | TextSegment deriving Eq

type StixTreeList = [StixTree] -> [StixTree]
\end{code}

Stix Trees for STG registers:
\begin{code}
stgBaseReg, stgStkOReg, stgNode, stgTagReg, stgRetReg, stgSpA,
    stgSuA, stgSpB, stgSuB, stgHp, stgHpLim, stgLivenessReg,
    stgStdUpdRetVecReg, stgStkStubReg :: StixTree

stgBaseReg 	    = StReg (StixMagicId BaseReg)
stgStkOReg 	    = StReg (StixMagicId StkOReg)
stgNode    	    = StReg (StixMagicId node)
stgInfoPtr 	    = StReg (StixMagicId infoptr)
stgTagReg  	    = StReg (StixMagicId TagReg)
stgRetReg  	    = StReg (StixMagicId RetReg)
stgSpA 		    = StReg (StixMagicId SpA)
stgSuA 		    = StReg (StixMagicId SuA)
stgSpB 		    = StReg (StixMagicId SpB)
stgSuB 		    = StReg (StixMagicId SuB)
stgHp		    = StReg (StixMagicId Hp)
stgHpLim	    = StReg (StixMagicId HpLim)
stgLivenessReg	    = StReg (StixMagicId LivenessReg)
stgStdUpdRetVecReg  = StReg (StixMagicId StdUpdRetVecReg)
stgStkStubReg	    = StReg (StixMagicId StkStubReg)

getUniqLabelNCG :: UniqSM CLabel
getUniqLabelNCG
  = getUnique	      `thenUs` \ u ->
    returnUs (mkAsmTempLabel u)
\end{code}
