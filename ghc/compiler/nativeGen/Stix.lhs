%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module Stix (
	CodeSegment(..), StixReg(..), StixTree(..), StixTreeList,
	sStLitLbl, pprStixTrees,

	stgBaseReg, stgNode, stgSp, stgSu, stgSpLim, stgHp, stgHpLim, stgTagReg,
	getUniqLabelNCG,

	fixedHS, arrHS
    ) where

#include "HsVersions.h"

import Ratio		( Rational )

import AbsCSyn		( node, tagreg, MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import CallConv		( CallConv, pprCallConv )
import CLabel		( mkAsmTempLabel, CLabel, pprCLabel )
import PrimRep          ( PrimRep, showPrimRep )
import PrimOp           ( PrimOp, pprPrimOp )
import Unique           ( Unique )
import SMRep		( fixedHdrSize, arrHdrSize )
import UniqSupply	( returnUs, thenUs, getUniqueUs, UniqSM )
import Outputable
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
  | StLitLbl	SDoc    -- literal labels
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

  | StCall FAST_STRING CallConv PrimRep [StixTree]

    -- Assembly-language comments

  | StComment FAST_STRING

sStLitLbl :: FAST_STRING -> StixTree
sStLitLbl s = StLitLbl (ptext s)


pprStixTrees :: [StixTree] -> SDoc
pprStixTrees ts 
  = vcat [
       vcat (map ppStixTree ts),
       char ' ',
       char ' '
    ]

paren t = char '(' <> t <> char ')'

ppStixTree :: StixTree -> SDoc
ppStixTree t 
   = case t of
       StSegment cseg -> paren (ppCodeSegment cseg)
       StInt i        -> paren (integer i)
       StDouble	rat   -> paren (text "Double" <+> rational rat)
       StString str   -> paren (text "Str" <+> ptext str)
       StComment str  -> paren (text "Comment" <+> ptext str)
       StLitLbl sd    -> sd
       StLitLit ll    -> paren (text "LitLit" <+> ptext ll)
       StCLbl lbl     -> pprCLabel lbl
       StReg reg      -> ppStixReg reg
       StIndex k b o  -> paren (ppStixTree b <+> char '+' <> 
                                pprPrimRep k <+> ppStixTree o)
       StInd k t      -> pprPrimRep k <> char '[' <> ppStixTree t <> char ']'
       StAssign k d s -> ppStixTree d <> text "  :=" <> pprPrimRep k 
                                          <> text "  " <> ppStixTree s
       StLabel ll     -> pprCLabel ll <+> char ':'
       StFunBegin ll  -> char ' ' $$ paren (text "FunBegin" <+> pprCLabel ll)
       StFunEnd ll    -> paren (text "FunEnd" <+> pprCLabel ll)
       StJump t       -> paren (text "Jump" <+> ppStixTree t)
       StFallThrough ll -> paren (text "FallThru" <+> pprCLabel ll)
       StCondJump l t -> paren (text "JumpC" <+> pprCLabel l <+> ppStixTree t)
       StData k ds -> paren (text "Data" <+> pprPrimRep k <+>
                      hsep (map ppStixTree ds))
       StPrim op ts -> paren (text "Prim" <+> pprPrimOp op <+> hsep (map ppStixTree ts))
       StCall nm cc k args
          -> paren (text "Call" <+> ptext nm <+>
               pprCallConv cc <+> pprPrimRep k <+> hsep (map ppStixTree args))
     where 
        pprPrimRep = text . showPrimRep
\end{code}

Stix registers can have two forms.  They {\em may} or {\em may not}
map to real, machine-level registers.

\begin{code}
data StixReg
  = StixMagicId MagicId	-- Regs which are part of the abstract machine model

  | StixTemp Unique PrimRep -- "Regs" which model local variables (CTemps) in
					-- the abstract C.

ppStixReg (StixMagicId mid)
   = ppMId mid
ppStixReg (StixTemp u pr)
   = hcat [text "Temp(", ppr u, ppr pr, char ')']


ppMId BaseReg              = text "BaseReg"
ppMId (VanillaReg kind n)  = hcat [text "IntReg(", int (I# n), char ')']
ppMId (FloatReg n)         = hcat [text "FltReg(", int (I# n), char ')']
ppMId (DoubleReg n)        = hcat [text "DblReg(", int (I# n), char ')']
ppMId (LongReg kind n)     = hcat [text "LongReg(", int (I# n), char ')']
ppMId Sp                   = text "Sp"
ppMId Su                   = text "Su"
ppMId SpLim                = text "SpLim"
ppMId Hp                   = text "Hp"
ppMId HpLim                = text "HpLim"
ppMId CurCostCentre        = text "CCC"
ppMId VoidReg              = text "VoidReg"
\end{code}

We hope that every machine supports the idea of data segment and text
segment (or that it has no segments at all, and we can lump these
together).

\begin{code}
data CodeSegment = DataSegment | TextSegment deriving (Eq, Show)
ppCodeSegment = text . show

type StixTreeList = [StixTree] -> [StixTree]
\end{code}

Stix Trees for STG registers:
\begin{code}
stgBaseReg, stgNode, stgSp, stgSu, stgSpLim, stgHp, stgHpLim 
	:: StixTree

stgBaseReg 	    = StReg (StixMagicId BaseReg)
stgNode    	    = StReg (StixMagicId node)
stgTagReg	    = StReg (StixMagicId tagreg)
stgSp 		    = StReg (StixMagicId Sp)
stgSu 		    = StReg (StixMagicId Su)
stgSpLim	    = StReg (StixMagicId SpLim)
stgHp		    = StReg (StixMagicId Hp)
stgHpLim	    = StReg (StixMagicId HpLim)

getUniqLabelNCG :: UniqSM CLabel
getUniqLabelNCG
  = getUniqueUs	      `thenUs` \ u ->
    returnUs (mkAsmTempLabel u)

fixedHS = StInt (toInteger fixedHdrSize)
arrHS   = StInt (toInteger arrHdrSize)
\end{code}
