%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module Stix (
	CodeSegment(..), StixReg(..), StixExpr(..), StixVReg(..),
        StixStmt(..), mkStAssign, StixStmtList,
	pprStixStmts, pprStixStmt, pprStixExpr, pprStixReg,
        stixStmt_CountTempUses, stixStmt_Subst,
        liftStrings, repOfStixExpr,
	DestInfo(..), hasDestInfo,

	stgBaseReg, stgNode, stgSp, stgSpLim, 
        stgHp, stgHpLim, stgHpAlloc, stgTagReg, stgR9, stgR10, 
	stgCurrentTSO, stgCurrentNursery,

	fixedHS, arrWordsHS, arrPtrsHS,

        NatM, initNat, thenNat, returnNat, 
        mapNat, mapAndUnzipNat, mapAccumLNat,
        getUniqueNat, getDeltaNat, setDeltaNat,
        NatM_State, mkNatM_State,
        uniqOfNatM_State, deltaOfNatM_State, importsOfNatM_State,
	addImportNat,

	getUniqLabelNCG, getNatLabelNCG,
        ncgPrimopMoan,

	-- Information about the target arch
        ncg_target_is_32bit
    ) where

#include "HsVersions.h"

import AbsCSyn		( node, tagreg, MagicId(..) )
import AbsCUtils	( magicIdPrimRep )
import ForeignCall	( CCallConv )
import CLabel		( mkAsmTempLabel, CLabel, pprCLabel )
import PrimRep          ( PrimRep(..) )
import MachOp		( MachOp(..), pprMachOp, resultRepOfMachOp )
import Unique           ( Unique )
import SMRep		( fixedHdrSize, arrWordsHdrSize, arrPtrsHdrSize )
import UniqSupply	( UniqSupply, splitUniqSupply, uniqFromSupply,
                          UniqSM, thenUs, returnUs, getUniqueUs )
import Constants	( wORD_SIZE )
import Outputable
import FastTypes
import FastString

import UNSAFE_IO	( unsafePerformIO )

import Ratio		( Rational )
import IO		( hPutStrLn, stderr )
\end{code}

Two types, StixStmt and StixValue, define Stix.

\begin{code}

-- Non-value trees; ones executed for their side-effect.
data StixStmt

  = -- Directive for the assembler to change segment
    StSegment CodeSegment

    -- Assembly-language comments
  | StComment FastString

    -- Assignments are typed to determine size and register placement.
    -- Assign a value to a StixReg
  | StAssignReg PrimRep StixReg StixExpr

    -- Assign a value to memory.  First tree indicates the address to be
    -- assigned to, so there is an implicit dereference here.
  | StAssignMem PrimRep StixExpr StixExpr -- dst, src

    -- A simple assembly label that we might jump to.
  | StLabel CLabel

    -- A function header and footer
  | StFunBegin CLabel
  | StFunEnd CLabel

    -- An unconditional jump. This instruction may or may not jump
    -- out of the register allocation domain (basic block, more or
    -- less).  For correct register allocation when this insn is used
    -- to jump through a jump table, we optionally allow a list of
    -- the exact targets to be attached, so that the allocator can
    -- easily construct the exact flow edges leaving this insn.
    -- Dynamic targets are allowed.
  | StJump DestInfo StixExpr

    -- A fall-through, from slow to fast
  | StFallThrough CLabel

    -- A conditional jump. This instruction can be non-terminal :-)
    -- Only static, local, forward labels are allowed
  | StCondJump CLabel StixExpr

    -- Raw data (as in an info table).
  | StData PrimRep [StixExpr]
    -- String which has been lifted to the top level (sigh).
  | StDataString FastString

    -- A value computed only for its side effects; result is discarded
    -- (A handy trapdoor to allow CCalls with no results to appear as
    -- statements).
  | StVoidable StixExpr


-- Helper fn to make Stix assignment statements where the 
-- lvalue masquerades as a StixExpr.  A kludge that should
-- be done away with.
mkStAssign :: PrimRep -> StixExpr -> StixExpr -> StixStmt
mkStAssign rep (StReg reg) rhs  
   = StAssignReg rep reg rhs
mkStAssign rep (StInd rep' addr) rhs 
   | rep `isCloseEnoughTo` rep'
   = StAssignMem rep addr rhs
   | otherwise
   = --pprPanic "Stix.mkStAssign: mismatched reps" (ppr rep <+> ppr rep')
     --trace ("Stix.mkStAssign: mismatched reps: " ++ showSDoc (ppr rep <+> ppr rep')) (
     StAssignMem rep addr rhs
     --)
     where
        isCloseEnoughTo r1 r2
           = r1 == r2 || (wordIsh r1 && wordIsh r2)
        wordIsh rep
           = rep `elem` [IntRep, WordRep, PtrRep, AddrRep, CodePtrRep, RetRep ]
                        -- determined by looking at PrimRep.showPrimRep

-- Stix trees which denote a value.
data StixExpr
  = -- Literals
    StInt	Integer	    -- ** add Kind at some point
  | StFloat	Rational
  | StDouble	Rational
  | StString	FastString
  | StCLbl	CLabel	    -- labels that we might index into

    -- Abstract registers of various kinds
  | StReg StixReg

    -- A typed offset from a base location
  | StIndex PrimRep StixExpr StixExpr -- kind, base, offset

    -- An indirection from an address to its contents.
  | StInd PrimRep StixExpr

    -- Primitive Operations
  | StMachOp MachOp [StixExpr]

    -- Calls to C functions
  | StCall (Either FastString StixExpr) -- Left: static, Right: dynamic
           CCallConv PrimRep [StixExpr]


-- What's the PrimRep of the value denoted by this StixExpr?
repOfStixExpr :: StixExpr -> PrimRep
repOfStixExpr (StInt _)       = IntRep
repOfStixExpr (StFloat _)     = FloatRep
repOfStixExpr (StDouble _)    = DoubleRep
repOfStixExpr (StString _)    = PtrRep
repOfStixExpr (StCLbl _)      = PtrRep
repOfStixExpr (StReg reg)     = repOfStixReg reg
repOfStixExpr (StIndex _ _ _) = PtrRep
repOfStixExpr (StInd rep _)   = rep
repOfStixExpr (StCall target conv retrep args) = retrep
repOfStixExpr (StMachOp mop args) = resultRepOfMachOp mop


-- used by insnFuture in RegAllocInfo.lhs
data DestInfo
   = NoDestInfo             -- no supplied dests; infer from context
   | DestInfo [CLabel]      -- precisely these dests and no others

hasDestInfo NoDestInfo   = False
hasDestInfo (DestInfo _) = True

pprDests :: DestInfo -> SDoc
pprDests NoDestInfo      = text "NoDestInfo"
pprDests (DestInfo dsts) = brackets (hsep (map pprCLabel dsts))


pprStixStmts :: [StixStmt] -> SDoc
pprStixStmts ts 
  = vcat [
       vcat (map pprStixStmt ts),
       char ' ',
       char ' '
    ]


pprStixExpr :: StixExpr -> SDoc
pprStixExpr t 
   = case t of
       StCLbl lbl       -> pprCLabel lbl
       StInt i          -> (if i < 0 then parens else id) (integer i)
       StFloat rat      -> parens (text "Float" <+> rational rat)
       StDouble	rat     -> parens (text "Double" <+> rational rat)
       StString str     -> parens (text "Str `" <> ftext str <> char '\'')
       StIndex k b o    -> parens (pprStixExpr b <+> char '+' <> 
                                   ppr k <+> pprStixExpr o)
       StInd k t        -> ppr k <> char '[' <> pprStixExpr t <> char ']'
       StReg reg        -> pprStixReg reg
       StMachOp op args -> pprMachOp op 
                           <> parens (hsep (punctuate comma (map pprStixExpr args)))
       StCall fn cc k args
                        -> parens (text "Call" <+> targ <+>
                                   ppr cc <+> ppr k <+> 
                                   hsep (map pprStixExpr args))
                           where
                              targ = case fn of
                                        Left  t_static -> ftext t_static
                                        Right t_dyn    -> parens (pprStixExpr t_dyn)

pprStixStmt :: StixStmt -> SDoc
pprStixStmt t 
   = case t of
       StSegment cseg   -> parens (ppCodeSegment cseg)
       StComment str    -> parens (text "Comment" <+> ftext str)
       StAssignReg pr reg rhs
                        -> pprStixReg reg <> text "  :=" <> ppr pr
                                          <> text "  " <> pprStixExpr rhs
       StAssignMem pr addr rhs
                        -> ppr pr <> char '[' <> pprStixExpr addr <> char ']'
                                  <> text "  :=" <> ppr pr
                                  <> text "  " <> pprStixExpr rhs
       StLabel ll       -> pprCLabel ll <+> char ':'
       StFunBegin ll    -> char ' ' $$ parens (text "FunBegin" <+> pprCLabel ll)
       StFunEnd ll      -> parens (text "FunEnd" <+> pprCLabel ll)
       StJump dsts t    -> parens (text "Jump" <+> pprDests dsts <+> pprStixExpr t)
       StFallThrough ll -> parens (text "FallThru" <+> pprCLabel ll)
       StCondJump l t   -> parens (text "JumpC" <+> pprCLabel l 
                                                <+> pprStixExpr t)
       StData k ds      -> parens (text "Data" <+> ppr k <+>
                                   hsep (map pprStixExpr ds))
       StDataString str -> parens (text "DataString" <+> ppr str)
       StVoidable expr  -> text "(void)" <+> pprStixExpr expr
\end{code}

Stix registers can have two forms.  They {\em may} or {\em may not}
map to real, machine-level registers.

\begin{code}
data StixReg
  = StixMagicId MagicId	-- Regs which are part of the abstract machine model

  | StixTemp StixVReg   -- "Regs" which model local variables (CTemps) in
		        -- the abstract C.

pprStixReg (StixMagicId mid)  = ppMId mid
pprStixReg (StixTemp temp)    = pprStixVReg temp

repOfStixReg (StixTemp (StixVReg u pr)) = pr
repOfStixReg (StixMagicId mid)          = magicIdPrimRep mid

data StixVReg
   = StixVReg Unique PrimRep

pprStixVReg (StixVReg u pr) = hcat [text "VReg(", ppr u, colon, ppr pr, char ')']



ppMId BaseReg              = text "BaseReg"
ppMId (VanillaReg kind n)  = hcat [ppr kind, text "IntReg(", 
                                   int (iBox n), char ')']
ppMId (FloatReg n)         = hcat [text "FltReg(", int (iBox n), char ')']
ppMId (DoubleReg n)        = hcat [text "DblReg(", int (iBox n), char ')']
ppMId (LongReg kind n)     = hcat [ppr kind, text "LongReg(", 
                                   int (iBox n), char ')']
ppMId Sp                   = text "Sp"
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
data CodeSegment 
   = DataSegment 
   | TextSegment 
   | RoDataSegment 
     deriving (Eq, Show)

ppCodeSegment = text . show

type StixStmtList = [StixStmt] -> [StixStmt]
\end{code}

Stix Trees for STG registers:
\begin{code}
stgBaseReg, stgNode, stgSp, stgSpLim, stgHp, stgHpLim :: StixReg

stgBaseReg 	    = StixMagicId BaseReg
stgNode    	    = StixMagicId node
stgTagReg	    = StixMagicId tagreg
stgSp 		    = StixMagicId Sp
stgSpLim	    = StixMagicId SpLim
stgHp		    = StixMagicId Hp
stgHpLim	    = StixMagicId HpLim
stgHpAlloc	    = StixMagicId HpAlloc
stgCurrentTSO	    = StixMagicId CurrentTSO
stgCurrentNursery   = StixMagicId CurrentNursery
stgR9               = StixMagicId (VanillaReg WordRep (_ILIT 9))
stgR10              = StixMagicId (VanillaReg WordRep (_ILIT 10))

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
stixExpr_CountTempUses :: Unique -> StixExpr -> Int
stixExpr_CountTempUses u t 
   = let qs = stixStmt_CountTempUses u
         qe = stixExpr_CountTempUses u
         qr = stixReg_CountTempUses u
     in
     case t of
        StReg      reg            -> qr reg
        StIndex    pk t1 t2       -> qe t1 + qe t2
        StInd      pk t1          -> qe t1
        StMachOp   mop ts         -> sum (map qe ts)
        StCall     (Left nm) cconv pk ts -> sum (map qe ts)
        StCall     (Right f) cconv pk ts -> sum (map qe ts) + qe f
        StInt _          -> 0
        StFloat _        -> 0
        StDouble _       -> 0
        StString _       -> 0
        StCLbl _         -> 0

stixStmt_CountTempUses :: Unique -> StixStmt -> Int
stixStmt_CountTempUses u t 
   = let qe = stixExpr_CountTempUses u
         qr = stixReg_CountTempUses u
         qv = stixVReg_CountTempUses u
     in
     case t of
        StAssignReg pk reg rhs  -> qr reg + qe rhs
        StAssignMem pk addr rhs -> qe addr + qe rhs
        StJump     dsts t1      -> qe t1
        StCondJump lbl t1       -> qe t1
        StData     pk ts        -> sum (map qe ts)
        StVoidable expr  -> qe expr
        StSegment _      -> 0
        StFunBegin _     -> 0
        StFunEnd _       -> 0
        StFallThrough _  -> 0
        StComment _      -> 0
        StLabel _        -> 0
        StDataString _   -> 0

stixReg_CountTempUses u reg
   = case reg of 
        StixTemp vreg    -> stixVReg_CountTempUses u vreg
        StixMagicId mid  -> 0

stixVReg_CountTempUses u (StixVReg uu pr)
   = if u == uu then 1 else 0
\end{code}

If we do decide to inline a temporary binding, the following functions
do the biz.

\begin{code}
stixStmt_Subst :: Unique -> StixExpr -> StixStmt -> StixStmt
stixStmt_Subst u new_u in_this_tree
   = stixStmt_MapUniques f in_this_tree
     where
        f :: Unique -> Maybe StixExpr
        f uu = if uu == u then Just new_u else Nothing


stixExpr_MapUniques :: (Unique -> Maybe StixExpr) -> StixExpr -> StixExpr
stixExpr_MapUniques f t
   = let qe = stixExpr_MapUniques f
         qs = stixStmt_MapUniques f
         qr = stixReg_MapUniques f
     in
     case t of
        StReg reg -> case qr reg of
                     Nothing -> StReg reg
                     Just xx -> xx
        StIndex    pk t1 t2       -> StIndex    pk (qe t1) (qe t2)
        StInd      pk t1          -> StInd      pk (qe t1)
        StMachOp   mop args       -> StMachOp   mop (map qe args)
        StCall     (Left nm) cconv pk ts -> StCall (Left nm) cconv pk (map qe ts)
        StCall     (Right f) cconv pk ts -> StCall (Right (qe f)) cconv pk (map qe ts)
        StInt _          -> t
        StFloat _        -> t
        StDouble _       -> t
        StString _       -> t
        StCLbl _         -> t

stixStmt_MapUniques :: (Unique -> Maybe StixExpr) -> StixStmt -> StixStmt
stixStmt_MapUniques f t
   = let qe = stixExpr_MapUniques f
         qs = stixStmt_MapUniques f
         qr = stixReg_MapUniques f
         qv = stixVReg_MapUniques f
     in
     case t of
        StAssignReg pk reg rhs
           -> case qr reg of
                 Nothing -> StAssignReg pk reg (qe rhs)
                 Just xx -> panic "stixStmt_MapUniques:StAssignReg"
        StAssignMem pk addr rhs   -> StAssignMem pk (qe addr) (qe rhs)
        StJump     dsts t1        -> StJump     dsts (qe t1)
        StCondJump lbl t1         -> StCondJump lbl (qe t1)
        StData     pk ts          -> StData     pk (map qe ts)
        StVoidable expr           -> StVoidable (qe expr)
        StSegment _      -> t
        StLabel _        -> t
        StFunBegin _     -> t
        StFunEnd _       -> t
        StFallThrough _  -> t
        StComment _      -> t
        StDataString _   -> t


stixReg_MapUniques :: (Unique -> Maybe StixExpr) -> StixReg -> Maybe StixExpr
stixReg_MapUniques f reg
   = case reg of
        StixMagicId mid -> Nothing
        StixTemp vreg   -> stixVReg_MapUniques f vreg

stixVReg_MapUniques :: (Unique -> Maybe StixExpr) -> StixVReg -> Maybe StixExpr
stixVReg_MapUniques f (StixVReg uu pr)
   = f uu
\end{code}

\begin{code}
-- Lift StStrings out of top-level StDatas, putting them at the end of
-- the block, and replacing them with StCLbls which refer to the lifted-out strings. 
{- Motivation for this hackery provided by the following bug:
   Stix:
      (DataSegment)
      Bogon.ping_closure :
      (Data P_ Addr.A#_static_info)
      (Data StgAddr (Str `alalal'))
      (Data P_ (0))
   results in:
      .data
              .align 8
      .global Bogon_ping_closure
      Bogon_ping_closure:
              .long   Addr_Azh_static_info
              .long   .Ln1a8
      .Ln1a8:
              .byte   0x61
              .byte   0x6C
              .byte   0x61
              .byte   0x6C
              .byte   0x61
              .byte   0x6C
              .byte   0x00
              .long   0
   ie, the Str is planted in-line, when what we really meant was to place
   a _reference_ to the string there.  liftStrings will lift out all such
   strings in top-level data and place them at the end of the block.

   This is still a rather half-baked solution -- to do the job entirely right
   would mean a complete traversal of all the Stixes, but there's currently no
   real need for it, and it would be slow.  Also, potentially there could be
   literal types other than strings which need lifting out?
-}

liftStrings :: [StixStmt] -> UniqSM [StixStmt]
liftStrings stmts
   = liftStrings_wrk stmts [] []

liftStrings_wrk :: [StixStmt]    -- originals
                -> [StixStmt]    -- (reverse) originals with strings lifted out
                -> [(CLabel, FastString)]   -- lifted strs, and their new labels
                -> UniqSM [StixStmt]

-- First, examine the original trees and lift out strings in top-level StDatas.
liftStrings_wrk (st:sts) acc_stix acc_strs
   = case st of
        StData sz datas
           -> lift datas acc_strs 	`thenUs` \ (datas_done, acc_strs1) ->
              liftStrings_wrk sts ((StData sz datas_done):acc_stix) acc_strs1
        other 
           -> liftStrings_wrk sts (other:acc_stix) acc_strs
     where
        -- Handle a top-level StData
        lift []     acc_strs = returnUs ([], acc_strs)
        lift (d:ds) acc_strs
           = lift ds acc_strs 		`thenUs` \ (ds_done, acc_strs1) ->
             case d of
                StString s 
                   -> getUniqueUs 	`thenUs` \ unq ->
                      let lbl = mkAsmTempLabel unq in
                      returnUs ((StCLbl lbl):ds_done, ((lbl,s):acc_strs1))
                other
                   -> returnUs (other:ds_done, acc_strs1)

-- When we've run out of original trees, emit the lifted strings.
liftStrings_wrk [] acc_stix acc_strs
   = returnUs (reverse acc_stix ++ concatMap f acc_strs)
     where
        f (lbl,str) = [StSegment RoDataSegment, 
                       StLabel lbl, 
                       StDataString str, 
                       StSegment TextSegment]
\end{code}

The NCG's monad.

The monad keeps a UniqSupply, the current stack delta and
a list of imported entities, which is only used for
Darwin (Mac OS X).

\begin{code}
data NatM_State = NatM_State UniqSupply Int [FastString]
type NatM result = NatM_State -> (result, NatM_State)

mkNatM_State :: UniqSupply -> Int -> NatM_State
mkNatM_State us delta = NatM_State us delta []

uniqOfNatM_State  (NatM_State us delta imports) = us
deltaOfNatM_State (NatM_State us delta imports) = delta
importsOfNatM_State (NatM_State us delta imports) = imports

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

mapAccumLNat :: (acc -> x -> NatM (acc, y))
                -> acc
	        -> [x]
	        -> NatM (acc, [y])

mapAccumLNat f b []
  = returnNat (b, [])
mapAccumLNat f b (x:xs)
  = f b x   	        	    `thenNat` \ (b__2, x__2) ->
    mapAccumLNat f b__2 xs   	    `thenNat` \ (b__3, xs__2) ->
    returnNat (b__3, x__2:xs__2)


getUniqueNat :: NatM Unique
getUniqueNat (NatM_State us delta imports)
    = case splitUniqSupply us of
         (us1,us2) -> (uniqFromSupply us1, (NatM_State us2 delta imports))

getDeltaNat :: NatM Int
getDeltaNat st@(NatM_State us delta imports)
   = (delta, st)

setDeltaNat :: Int -> NatM ()
setDeltaNat delta (NatM_State us _ imports)
   = ((), NatM_State us delta imports)

addImportNat :: FastString -> NatM ()
addImportNat imp (NatM_State us delta imports)
   = ((), NatM_State us delta (imp:imports))
\end{code}

Giving up in a not-too-inelegant way.

\begin{code}
ncgPrimopMoan :: String -> SDoc -> a
ncgPrimopMoan msg pp_rep
   = unsafePerformIO (
        hPutStrLn stderr (
        "\n" ++
        "You've fallen across an unimplemented case in GHC's native code generation\n" ++
        "machinery.  You can work around this for the time being by compiling\n" ++ 
        "this module via the C route, by giving the flag -fvia-C.\n" ++
        "The panic below contains information, intended for the GHC implementors,\n" ++
        "about the exact place where GHC gave up.  Please send it to us\n" ++
        "at glasgow-haskell-bugs@haskell.org, so as to encourage us to fix this.\n"
        )
     )
     `seq`
     pprPanic msg pp_rep
\end{code}

Information about the target.

\begin{code}

ncg_target_is_32bit :: Bool
ncg_target_is_32bit | wORD_SIZE == 4 = True
                    | wORD_SIZE == 8 = False

\end{code}
