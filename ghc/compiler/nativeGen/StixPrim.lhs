%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixPrim ( amodeToStix, amodeToStix', foreignCallCode )
where

#include "HsVersions.h"

import MachMisc
import Stix

import PprAbsC		( pprAmode )
import AbsCSyn 		hiding ( spRel )
import AbsCUtils	( getAmodeRep, mixedTypeLocn )
import SMRep		( fixedHdrSize )
import Literal		( Literal(..), word2IntLit )
import MachOp		( MachOp(..) )
import PrimRep		( PrimRep(..), getPrimRepSizeInBytes )
import UniqSupply	( returnUs, thenUs, getUniqueUs, UniqSM )
import Constants	( mIN_INTLIKE, mIN_CHARLIKE, uF_UPDATEE, bLOCK_SIZE,
			  rESERVED_STACK_WORDS )
import CLabel		( mkIntlikeClosureLabel, mkCharlikeClosureLabel,
			  mkMAP_FROZEN_infoLabel, mkEMPTY_MVAR_infoLabel,
			  mkForeignLabel )
import ForeignCall	( ForeignCall(..), CCallSpec(..), CCallTarget(..),
			  CCallConv(..), playSafe )
import Outputable
import FastTypes

#include "NCG.h"
\end{code}

The main honchos here are primCode and foreignCallCode, which handle the guts of COpStmts.

\begin{code}
foreignCallCode
    :: [CAddrMode]  	-- results
    -> ForeignCall    	-- op
    -> [CAddrMode]  	-- args
    -> UniqSM StixStmtList
\end{code}

%************************************************************************
%*									*
\subsubsection{Code for foreign calls}
%*									*
%************************************************************************

First, the dreaded @ccall@.  We can't handle @casm@s.

Usually, this compiles to an assignment, but when the left-hand side
is empty, we just perform the call and ignore the result.

btw Why not let programmer use casm to provide assembly code instead
of C code?  ADR

ToDo: saving/restoring of volatile regs around ccalls.

JRS, 001113: always do the call of suspendThread and resumeThread as a ccall
rather than inheriting the calling convention of the thing which we're really
calling.

\begin{code}
foreignCallCode lhs (CCall (CCallSpec (StaticTarget fn) cconv safety)) rhs

  | not (playSafe safety) 
  = returnUs (\xs -> ccall : xs)

  | otherwise
  = save_thread_state	`thenUs` \ save ->
    load_thread_state	`thenUs` \ load -> 
    getUniqueUs		`thenUs` \ uniq -> 
    let
       id  = StixTemp (StixVReg uniq IntRep)
    
       suspend = StAssignReg IntRep id 
   		 (StCall SLIT("suspendThread") {-no:cconv-} CCallConv
                         IntRep [StReg stgBaseReg])
       resume  = StVoidable 
                 (StCall SLIT("resumeThread") {-no:cconv-} CCallConv
                         VoidRep [StReg id])
    in
    returnUs (\xs -> save (suspend : ccall : resume : load xs))

  where
    args = map amodeCodeForCCall rhs
    amodeCodeForCCall x =
	let base = amodeToStix' x
	in
	    case getAmodeRep x of
	      ArrayRep      -> StIndex PtrRep base arrPtrsHS
	      ByteArrayRep  -> StIndex IntRep base arrWordsHS
	      ForeignObjRep -> StInd PtrRep (StIndex PtrRep base fixedHS)
	      _ -> base

    ccall = case lhs of
      []    -> StVoidable (StCall fn cconv VoidRep args)
      [lhs] -> mkStAssign pk lhs' (StCall fn cconv pk args)
	    where
	       lhs' = amodeToStix lhs
	       pk   = case getAmodeRep lhs of
                        FloatRep  -> FloatRep
                        DoubleRep -> DoubleRep
                        other     -> IntRep

foreignCallCode lhs call rhs
  = ncgPrimopMoan "Native code generator can't handle foreign call" (ppr call)
\end{code}

%************************************************************************
%*									*
\subsubsection{Code for @CAddrMode@s}
%*									*
%************************************************************************

When a character is fetched from a mixed type location, we have to do
an extra cast.  This is reflected in amodeCode', which is for rhs
amodes that might possibly need the extra cast.

\begin{code}
amodeToStix, amodeToStix' :: CAddrMode -> StixExpr

amodeToStix'{-'-} am@(CVal rr CharRep)
  | mixedTypeLocn am = StMachOp MO_NatS_to_32U [amodeToStix am]
  | otherwise        = amodeToStix am
amodeToStix' am 
  = amodeToStix am

-----------
amodeToStix am@(CVal rr CharRep)
  | mixedTypeLocn am
  = StInd IntRep (amodeToStix (CAddr rr))

amodeToStix (CVal rr pk) = StInd pk (amodeToStix (CAddr rr))

amodeToStix (CMem pk addr) = StInd pk (amodeToStix addr)

amodeToStix (CAddr (SpRel off))
  = StIndex PtrRep (StReg stgSp) (StInt (toInteger (iBox off)))

amodeToStix (CAddr (HpRel off))
  = StIndex IntRep (StReg stgHp) (StInt (toInteger (- (iBox off))))

amodeToStix (CAddr (NodeRel off))
  = StIndex IntRep (StReg stgNode) (StInt (toInteger (iBox off)))

amodeToStix (CAddr (CIndex base off pk))
  = StIndex pk (amodeToStix base) (amodeToStix off)

amodeToStix (CReg magic)    = StReg (StixMagicId magic)
amodeToStix (CTemp uniq pk) = StReg (StixTemp (StixVReg uniq pk))

amodeToStix (CLbl      lbl _) = StCLbl lbl

 -- For CharLike and IntLike, we attempt some trivial constant-folding here.

amodeToStix (CCharLike (CLit (MachChar c)))
  = StIndex Word8Rep cHARLIKE_closure (StInt (toInteger off))
  where
    off = charLikeSize * (c - mIN_CHARLIKE)

amodeToStix (CCharLike x)
  = panic "amodeToStix.CCharLike"

amodeToStix (CIntLike (CLit (MachInt i)))
  = StIndex Word8Rep iNTLIKE_closure (StInt (toInteger off))
  where
    off = intLikeSize * (fromInteger (i - mIN_INTLIKE))

amodeToStix (CIntLike x)
  = panic "amodeToStix.CIntLike"

amodeToStix (CLit core)
  = case core of
      MachChar c     -> StInt (toInteger c)
      MachStr s	     -> StString s
      MachAddr a     -> StInt a
      MachInt i      -> StInt i
      MachWord w     -> case word2IntLit core of MachInt iw -> StInt iw
      MachLitLit s _ -> litLitErr
      MachLabel l    -> StCLbl (mkForeignLabel l False{-ToDo: dynamic-})
      MachFloat d    -> StFloat d
      MachDouble d   -> StDouble d
      _ -> panic "amodeToStix:core literal"

amodeToStix (CMacroExpr _ macro [arg])
  = case macro of
      ENTRY_CODE -> amodeToStix arg
      ARG_TAG    -> amodeToStix arg -- just an integer no. of words
      GET_TAG    -> 
#ifdef WORDS_BIGENDIAN
                    StMachOp MO_Nat_And
			[StInd WordRep (StIndex PtrRep (amodeToStix arg)
                                                (StInt (toInteger (-1)))),
			 StInt 65535]
#else
                    StMachOp MO_Nat_Shr
			[StInd WordRep (StIndex PtrRep (amodeToStix arg)
                                                (StInt (toInteger (-1)))),
			 StInt 16]
#endif
      UPD_FRAME_UPDATEE
         -> StInd PtrRep (StIndex PtrRep (amodeToStix arg) 
                                         (StInt (toInteger uF_UPDATEE)))

amodeToStix other
   = pprPanic "StixPrim.amodeToStix" (pprAmode other)

litLitErr 
   = ncgPrimopMoan "native code generator can't handle lit-lits" empty
\end{code}

Sizes of the CharLike and IntLike closures that are arranged as arrays
in the data segment.  (These are in bytes.)

\begin{code}
-- The INTLIKE base pointer

iNTLIKE_closure :: StixExpr
iNTLIKE_closure = StCLbl mkIntlikeClosureLabel

-- The CHARLIKE base

cHARLIKE_closure :: StixExpr
cHARLIKE_closure = StCLbl mkCharlikeClosureLabel

mutArrPtrsFrozen_info = StCLbl mkMAP_FROZEN_infoLabel

-- these are the sizes of charLike and intLike closures, in _bytes_.
charLikeSize = (fixedHdrSize + 1) * (sizeOf PtrRep)
intLikeSize  = (fixedHdrSize + 1) * (sizeOf PtrRep)
\end{code}


\begin{code}
save_thread_state 
   = getUniqueUs   `thenUs` \ tso_uq -> 
     let tso = StixTemp (StixVReg tso_uq ThreadIdRep) in
     returnUs (\xs ->
	StAssignReg ThreadIdRep tso (StReg stgCurrentTSO)
	: StAssignMem PtrRep
             (StMachOp MO_Nat_Add
		       [StReg tso, StInt (toInteger (TSO_SP*BYTES_PER_WORD))])
	     (StReg stgSp)
        : StAssignMem PtrRep 
	     (StMachOp MO_Nat_Add
		       [StReg tso, StInt (toInteger (TSO_SU*BYTES_PER_WORD))])
	     (StReg stgSu)
        : StAssignMem PtrRep
	     (StMachOp MO_Nat_Add
		       [StReg stgCurrentNursery, 
		        StInt (toInteger (BDESCR_FREE * BYTES_PER_WORD))])
             (StMachOp MO_Nat_Add 
                       [StReg stgHp, StInt (toInteger (1 * BYTES_PER_WORD))]) 
        : xs
     )

load_thread_state 
   = getUniqueUs   `thenUs` \ tso_uq -> 
     let tso = StixTemp (StixVReg tso_uq ThreadIdRep) in
     returnUs (\xs ->
	StAssignReg ThreadIdRep tso (StReg stgCurrentTSO)
	: StAssignReg PtrRep 
             stgSp
	     (StInd PtrRep 
                  (StMachOp MO_Nat_Add
                            [StReg tso, StInt (toInteger (TSO_SP*BYTES_PER_WORD))]))
	: StAssignReg PtrRep 
             stgSu
	     (StInd PtrRep 
                  (StMachOp MO_Nat_Add
		            [StReg tso, StInt (toInteger (TSO_SU*BYTES_PER_WORD))]))
	: StAssignReg PtrRep 
             stgSpLim
	     (StMachOp MO_Nat_Add 
                       [StReg tso, 
			StInt (toInteger ((TSO_STACK + rESERVED_STACK_WORDS)
					  *BYTES_PER_WORD))])
	: StAssignReg PtrRep 
             stgHp
	     (StMachOp MO_Nat_Sub 
                       [StInd PtrRep 
                              (StMachOp MO_Nat_Add
  	                                [StReg stgCurrentNursery, 
		                         StInt (toInteger (BDESCR_FREE * BYTES_PER_WORD))]),
	                StInt (toInteger (1 * BYTES_PER_WORD))
	               ]) 
        : StAssignReg PtrRep 
             stgHpLim
	     (StMachOp MO_Nat_Add 
                       [StInd PtrRep 
                              (StMachOp MO_Nat_Add
		                        [StReg stgCurrentNursery, 
		                         StInt (toInteger (BDESCR_START * BYTES_PER_WORD))]),
	                StInt (toInteger (bLOCK_SIZE - (1 * BYTES_PER_WORD)))
	               ]) 
        : xs
     )
\end{code}
