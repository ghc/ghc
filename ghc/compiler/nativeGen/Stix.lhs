%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module Stix (
	CodeSegment(..), StixReg(..), StixTree(..), StixTreeList,
	sStLitLbl, pprStixTrees, ppStixTree, ppStixReg,
        stixCountTempUses, stixSubst,

	stgBaseReg, stgNode, stgSp, stgSu, stgSpLim, 
        stgHp, stgHpLim, stgTagReg, stgR9, stgR10,

	fixedHS, arrWordsHS, arrPtrsHS,

        NatM, initNat, thenNat, returnNat, 
        mapNat, mapAndUnzipNat,
        getUniqueNat, getDeltaNat, setDeltaNat,
        NatM_State, mkNatM_State,
        uniqOfNatM_State, deltaOfNatM_State,

	getUniqLabelNCG, getNatLabelNCG,
    ) where

#include "HsVersions.h"

import Ratio		( Rational )

import AbsCSyn		( node, tagreg, MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import CallConv		( CallConv, pprCallConv )
import CLabel		( mkAsmTempLabel, CLabel, pprCLabel, pprCLabel_asm )
import PrimRep          ( PrimRep(..), showPrimRep )
import PrimOp           ( PrimOp, pprPrimOp )
import Unique           ( Unique )
import SMRep		( fixedHdrSize, arrWordsHdrSize, arrPtrsHdrSize )
import UniqSupply	( UniqSupply, splitUniqSupply, uniqFromSupply,
                          UniqSM, thenUs, returnUs, getUniqueUs )
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

    -- A volatile memory scratch array, which is allocated
    -- relative to the stack pointer.  It is an array of
    -- ptr/word/int sized things.  Do not expect to be preserved
    -- beyond basic blocks or over a ccall.  Current max size
    -- is 6, used in StixInteger.

  | StScratchWord Int

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
       StSegment cseg   -> paren (ppCodeSegment cseg)
       StInt i          -> paren (integer i)
       StDouble	rat     -> paren (text "Double" <+> rational rat)
       StString str     -> paren (text "Str" <+> ptext str)
       StComment str    -> paren (text "Comment" <+> ptext str)
       StLitLbl sd      -> sd
       StCLbl lbl       -> pprCLabel lbl
       StReg reg        -> ppStixReg reg
       StIndex k b o    -> paren (ppStixTree b <+> char '+' <> 
                                  pprPrimRep k <+> ppStixTree o)
       StInd k t        -> pprPrimRep k <> char '[' <> ppStixTree t <> char ']'
       StAssign k d s   -> ppStixTree d <> text "  :=" <> pprPrimRep k 
                                          <> text "  " <> ppStixTree s
       StLabel ll       -> pprCLabel ll <+> char ':'
       StFunBegin ll    -> char ' ' $$ paren (text "FunBegin" <+> pprCLabel ll)
       StFunEnd ll      -> paren (text "FunEnd" <+> pprCLabel ll)
       StJump t         -> paren (text "Jump" <+> ppStixTree t)
       StFallThrough ll -> paren (text "FallThru" <+> pprCLabel ll)
       StCondJump l t   -> paren (text "JumpC" <+> pprCLabel l 
                                               <+> ppStixTree t)
       StData k ds      -> paren (text "Data" <+> pprPrimRep k <+>
                                  hsep (map ppStixTree ds))
       StPrim op ts     -> paren (text "Prim" <+> pprPrimOp op <+> 
                                  hsep (map ppStixTree ts))
       StCall nm cc k args
                        -> paren (text "Call" <+> ptext nm <+>
                                  pprCallConv cc <+> pprPrimRep k <+> 
                                  hsep (map ppStixTree args))
       StScratchWord i  -> text "ScratchWord" <> paren (int i)

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
ppMId (VanillaReg kind n)  = hcat [pprPrimRep kind, text "IntReg(", 
                                   int (I# n), char ')']
ppMId (FloatReg n)         = hcat [text "FltReg(", int (I# n), char ')']
ppMId (DoubleReg n)        = hcat [text "DblReg(", int (I# n), char ')']
ppMId (LongReg kind n)     = hcat [pprPrimRep kind, text "LongReg(", 
                                   int (I# n), char ')']
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
stgR9               = StReg (StixMagicId (VanillaReg WordRep ILIT(9)))
stgR10              = StReg (StixMagicId (VanillaReg WordRep ILIT(10)))

getNatLabelNCG :: NatM CLabel
getNatLabelNCG
  = getUniqueNat `thenNat` \ u ->
    returnNat (mkAsmTempLabel u)

getUniqLabelNCG :: UniqSM CLabel
getUniqLabelNCG
  = getUniqueUs `thenUs` \ u ->
    returnUs (mkAsmTempLabel u)

fixedHS     = StInt (toInteger fixedHdrSize)
arrWordsHS  = StInt (toInteger arrWordsHdrSize)
arrPtrsHS   = StInt (toInteger arrPtrsHdrSize)
\end{code}

Stix optimisation passes may wish to find out how many times a
given temporary appears in a tree, so as to be able to decide
whether or not to inline the assignment's RHS at usage site(s).

\begin{code}
stixCountTempUses :: Unique -> StixTree -> Int
stixCountTempUses u t 
   = let qq = stixCountTempUses u
     in
     case t of
        StReg reg
           -> case reg of 
                 StixTemp uu pr  -> if u == uu then 1 else 0
                 StixMagicId mid -> 0

        StIndex    pk t1 t2       -> qq t1 + qq t2
        StInd      pk t1          -> qq t1
        StAssign   pk t1 t2       -> qq t1 + qq t2
        StJump     t1             -> qq t1
        StCondJump lbl t1         -> qq t1
        StData     pk ts          -> sum (map qq ts)
        StPrim     op ts          -> sum (map qq ts)
        StCall     nm cconv pk ts -> sum (map qq ts)

        StSegment _      -> 0
        StInt _          -> 0
        StDouble _       -> 0
        StString _       -> 0
        StLitLbl _       -> 0
        StCLbl _         -> 0
        StLabel _        -> 0
        StFunBegin _     -> 0
        StFunEnd _       -> 0
        StFallThrough _  -> 0
        StScratchWord _  -> 0
        StComment _      -> 0


stixSubst :: Unique -> StixTree -> StixTree -> StixTree
stixSubst u new_u in_this_tree
   = stixMapUniques f in_this_tree
     where
        f :: Unique -> Maybe StixTree
        f uu = if uu == u then Just new_u else Nothing


stixMapUniques :: (Unique -> Maybe StixTree) -> StixTree -> StixTree
stixMapUniques f t
   = let qq = stixMapUniques f
     in
     case t of
        StReg reg
           -> case reg of 
                 StixMagicId mid -> t
                 StixTemp uu pr  
                    -> case f uu of
                          Just xx -> xx
                          Nothing -> t

        StIndex    pk t1 t2       -> StIndex    pk (qq t1) (qq t2)
        StInd      pk t1          -> StInd      pk (qq t1)
        StAssign   pk t1 t2       -> StAssign   pk (qq t1) (qq t2)
        StJump     t1             -> StJump     (qq t1)
        StCondJump lbl t1         -> StCondJump lbl (qq t1)
        StData     pk ts          -> StData     pk (map qq ts)
        StPrim     op ts          -> StPrim     op (map qq ts)
        StCall     nm cconv pk ts -> StCall     nm cconv pk (map qq ts)

        StSegment _      -> t
        StInt _          -> t
        StDouble _       -> t
        StString _       -> t
        StLitLbl _       -> t
        StCLbl _         -> t
        StLabel _        -> t
        StFunBegin _     -> t
        StFunEnd _       -> t
        StFallThrough _  -> t
        StScratchWord _  -> t
        StComment _      -> t
\end{code}

\begin{code}
data NatM_State = NatM_State UniqSupply Int
type NatM result = NatM_State -> (result, NatM_State)

mkNatM_State :: UniqSupply -> Int -> NatM_State
mkNatM_State = NatM_State

uniqOfNatM_State  (NatM_State us delta) = us
deltaOfNatM_State (NatM_State us delta) = delta


initNat :: NatM_State -> NatM a -> (a, NatM_State)
initNat init_st m = case m init_st of { (r,st) -> (r,st) }

thenNat :: NatM a -> (a -> NatM b) -> NatM b
thenNat expr cont st
  = case expr st of { (result, st') -> cont result st' }

returnNat :: a -> NatM a
returnNat result st = (result, st)

mapNat :: (a -> NatM b) -> [a] -> NatM [b]
mapNat f []     = returnNat []
mapNat f (x:xs)
  = f x          `thenNat` \ r  ->
    mapNat f xs  `thenNat` \ rs ->
    returnNat (r:rs)

mapAndUnzipNat :: (a -> NatM (b,c))   -> [a] -> NatM ([b],[c])
mapAndUnzipNat f [] = returnNat ([],[])
mapAndUnzipNat f (x:xs)
  = f x		    	`thenNat` \ (r1,  r2)  ->
    mapAndUnzipNat f xs	`thenNat` \ (rs1, rs2) ->
    returnNat (r1:rs1, r2:rs2)


getUniqueNat :: NatM Unique
getUniqueNat (NatM_State us delta)
    = case splitUniqSupply us of
         (us1,us2) -> (uniqFromSupply us1, (NatM_State us2 delta))

getDeltaNat :: NatM Int
getDeltaNat st@(NatM_State us delta)
   = (delta, st)

setDeltaNat :: Int -> NatM ()
setDeltaNat delta (NatM_State us _)
   = ((), NatM_State us delta)
\end{code}
