%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixPrim ( amodeToStix, amodeToStix', foreignCallCode )
where

#include "HsVersions.h"

-- import MachMisc
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
			  mkMAP_FROZEN_infoLabel, 
			  mkForeignLabel )
import ForeignCall	( ForeignCall(..), CCallSpec(..), CCallTarget(..),
			  CCallConv(..), playSafe, playThreadSafe )
import Outputable
import Util             ( notNull )
import FastString
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

First, the dreaded @ccall@.

Usually, this compiles to an assignment, but when the left-hand side
is empty, we just perform the call and ignore the result.

ToDo: saving/restoring of volatile regs around ccalls.

JRS, 001113: always do the call of suspendThread and resumeThread as a ccall
rather than inheriting the calling convention of the thing which we're really
calling.

\begin{code}
foreignCallCode lhs call@(CCall (CCallSpec ctarget cconv safety)) rhs

  | not (playSafe safety) 
  = returnUs (\xs -> ccall : xs)

  | otherwise
  = save_thread_state `thenUs` \ save ->
    load_thread_state `thenUs` \ load -> 
    getUniqueUs	      `thenUs` \ uniq -> 
    let
       id  = StixTemp (StixVReg uniq IntRep)
       
       is_threadSafe
        | playThreadSafe safety = 1
	| otherwise             = 0
    
       suspend = StAssignReg IntRep id 
   		 (StCall (Left FSLIT("suspendThread")) {-no:cconv-} CCallConv
                         IntRep [StReg stgBaseReg, StInt is_threadSafe ])
       resume  = StVoidable 
                 (StCall (Left FSLIT("resumeThread")) {-no:cconv-} CCallConv
                         VoidRep [StReg id, StInt is_threadSafe ])
    in
    returnUs (\xs -> save (suspend : ccall : resume : load xs))

  where
    (cargs, stix_target)
        = case ctarget of
             StaticTarget nm -> (rhs, Left nm)
             DynamicTarget |  notNull rhs -- an assertion
                           -> (tail rhs, Right (amodeToStix (head rhs)))

    stix_args = map amodeToStix' cargs

    ccall = case lhs of
      []    -> StVoidable (StCall stix_target cconv VoidRep stix_args)
      [lhs] -> mkStAssign pk lhs' (StCall stix_target cconv pk stix_args)
	    where
	       lhs' = amodeToStix lhs
	       pk   = case getAmodeRep lhs of
                        FloatRep  -> FloatRep
                        DoubleRep -> DoubleRep
                        Int64Rep  -> Int64Rep
                        Word64Rep -> Word64Rep
                        other     -> IntRep

-- a bit late to catch this here..
foreignCallCode _ DNCall{} _
 = panic "foreignCallCode: .NET interop not supported via NCG; compile with -fvia-C"
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
      MachNullAddr   -> StInt 0
      MachInt i      -> StInt i
      MachWord w     -> case word2IntLit core of MachInt iw -> StInt iw
                                                       -- dreadful, but rare.
      MachLabel l (Just x) -> StCLbl (mkForeignLabel (mkFastString (unpackFS l ++ '@':show x)) False)
      MachLabel l _        -> StCLbl (mkForeignLabel l False{-ToDo: dynamic-})
      MachFloat d    -> StFloat d
      MachDouble d   -> StDouble d
      _ -> panic "amodeToStix:core literal"

amodeToStix (CMacroExpr _ macro [arg])
  = let 
	arg_amode = amodeToStix arg
    in	
    case macro of
      ENTRY_CODE -> arg_amode
      ARG_TAG    -> arg_amode -- just an integer no. of words
      GET_TAG    -> 
#ifdef WORDS_BIGENDIAN
                    StMachOp MO_Nat_And
			[StInd WordRep (StIndex PtrRep arg_amode
                                                (StInt (toInteger (-1)))),
			 StInt 65535]
#else
                    StMachOp MO_Nat_Shr
			[StInd WordRep (StIndex PtrRep arg_amode
                                                (StInt (toInteger (-1)))),
			 StInt 16]
#endif
      BYTE_ARR_CTS -> StIndex IntRep arg_amode arrWordsHS
      PTRS_ARR_CTS -> StIndex PtrRep arg_amode arrPtrsHS
      ForeignObj_CLOSURE_DATA -> StInd PtrRep (StIndex PtrRep arg_amode fixedHS)


amodeToStix other
   = pprPanic "StixPrim.amodeToStix" (pprAmode other)
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
charLikeSize = (fixedHdrSize + 1) * (getPrimRepSizeInBytes PtrRep)
intLikeSize  = (fixedHdrSize + 1) * (getPrimRepSizeInBytes PtrRep)
\end{code}


\begin{code}
save_thread_state 
   = getUniqueUs   `thenUs` \ tso_uq -> 
     let tso = StixTemp (StixVReg tso_uq PtrRep) in
     returnUs (\xs ->
	StAssignReg PtrRep tso (StReg stgCurrentTSO)
	: StAssignMem PtrRep
             (StMachOp MO_Nat_Add
		       [StReg tso, StInt (toInteger (TSO_SP*BYTES_PER_WORD))])
	     (StReg stgSp)
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
     let tso = StixTemp (StixVReg tso_uq PtrRep) in
     returnUs (\xs ->
	StAssignReg PtrRep tso (StReg stgCurrentTSO)
	: StAssignReg PtrRep 
             stgSp
	     (StInd PtrRep 
                  (StMachOp MO_Nat_Add
                            [StReg tso, StInt (toInteger (TSO_SP*BYTES_PER_WORD))]))
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
             (StIndex Word8Rep 
                (StInd PtrRep 
                       (StIndex PtrRep (StReg stgCurrentNursery)
                                       (StInt (toInteger BDESCR_START))
                       )
                )
                (StMachOp MO_Nat_Sub
                   [StMachOp MO_NatU_Mul
                      [StInd WordRep 
                             (StIndex PtrRep (StReg stgCurrentNursery)
                                             (StInt (toInteger BDESCR_BLOCKS))),
                       StInt (toInteger bLOCK_SIZE{-in bytes-})
                      ],
                      StInt (1 * BYTES_PER_WORD)
                   ]
                )

             ) 

        : xs
     )
\end{code}
