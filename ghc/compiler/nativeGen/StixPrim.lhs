%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixPrim ( primCode, amodeToStix, amodeToStix' ) where

#include "HsVersions.h"

import MachMisc
import MachRegs
import Stix
import StixInteger

import AbsCSyn 		hiding ( spRel )
import AbsCUtils	( getAmodeRep, mixedTypeLocn )
import SMRep		( fixedHdrSize )
import Literal		( Literal(..), word2IntLit )
import CallConv		( cCallConv )
import PrimOp		( PrimOp(..), CCall(..), CCallTarget(..) )
import PrimRep		( PrimRep(..), isFloatingRep )
import UniqSupply	( returnUs, thenUs, getUniqueUs, UniqSM )
import Constants	( mIN_INTLIKE, uF_UPDATEE, bLOCK_SIZE )
import CLabel		( mkIntlikeClosureLabel, mkCharlikeClosureLabel,
			  mkTopClosureLabel, mkErrorIO_innardsLabel,
			  mkMAP_FROZEN_infoLabel )
import Outputable

import Char	       	( ord, isAlphaNum )

#include "NCG.h"
\end{code}

The main honcho here is primCode, which handles the guts of COpStmts.

\begin{code}
primCode
    :: [CAddrMode]  	-- results
    -> PrimOp 	    	-- op
    -> [CAddrMode]  	-- args
    -> UniqSM StixTreeList
\end{code}

First, the dreaded @ccall@.  We can't handle @casm@s.

Usually, this compiles to an assignment, but when the left-hand side
is empty, we just perform the call and ignore the result.

btw Why not let programmer use casm to provide assembly code instead
of C code?  ADR

The (MP) integer operations are a true nightmare.  Since we don't have
a convenient abstract way of allocating temporary variables on the (C)
stack, we use the space just below HpLim for the @MP_INT@ structures,
and modify our heap check accordingly.

\begin{code}
-- NB: ordering of clauses somewhere driven by
-- the desire to getting sane patt-matching behavior
primCode res@[sr,dr] IntegerNegOp arg@[sa,da]
  = gmpNegate (sr,dr) (sa,da)

primCode [res] IntegerCmpOp args@[sa1,da1, sa2,da2]
  = gmpCompare res (sa1,da1, sa2,da2)

primCode [res] IntegerCmpIntOp args@[sa1,da1,ai]
  = gmpCompareInt res (sa1,da1,ai)

primCode [res] Integer2IntOp arg@[sa,da]
  = gmpInteger2Int res (sa,da)

primCode [res] Integer2WordOp arg@[sa,da]
  = gmpInteger2Word res (sa,da)

primCode [res] Int2AddrOp [arg]
  = simpleCoercion AddrRep res arg

primCode [res] Addr2IntOp [arg]
  = simpleCoercion IntRep res arg

primCode [res] Int2WordOp [arg]
  = simpleCoercion IntRep{-WordRep?-} res arg

primCode [res] Word2IntOp [arg]
  = simpleCoercion IntRep res arg
\end{code}

\begin{code}
primCode [res] SameMutableArrayOp args
  = let
	compare = StPrim AddrEqOp (map amodeToStix args)
	assign = StAssign IntRep (amodeToStix res) compare
    in
    returnUs (\xs -> assign : xs)

primCode res@[_] SameMutableByteArrayOp args
  = primCode res SameMutableArrayOp args

primCode res@[_] SameMutVarOp args
  = primCode res SameMutableArrayOp args

primCode res@[_] SameMVarOp args
  = primCode res SameMutableArrayOp args
\end{code}

Freezing an array of pointers is a double assignment.  We fix the
header of the ``new'' closure because the lhs is probably a better
addressing mode for the indirection (most likely, it's a VanillaReg).

\begin{code}

primCode [lhs] UnsafeFreezeArrayOp [rhs]
  = let
	lhs' = amodeToStix lhs
    	rhs' = amodeToStix rhs
    	header = StInd PtrRep lhs'
	assign = StAssign PtrRep lhs' rhs'
	freeze = StAssign PtrRep header mutArrPtrsFrozen_info
    in
    returnUs (\xs -> assign : freeze : xs)

primCode [lhs] UnsafeFreezeByteArrayOp [rhs]
  = simpleCoercion PtrRep lhs rhs
\end{code}

Returning the size of (mutable) byte arrays is just
an indexing operation.

\begin{code}
primCode [lhs] SizeofByteArrayOp [rhs]
  = let
	lhs' = amodeToStix lhs
    	rhs' = amodeToStix rhs
    	sz   = StIndex IntRep rhs' fixedHS
    	assign = StAssign IntRep lhs' (StInd IntRep sz)
    in
    returnUs (\xs -> assign : xs)

primCode [lhs] SizeofMutableByteArrayOp [rhs]
  = let
	lhs' = amodeToStix lhs
    	rhs' = amodeToStix rhs
    	sz   = StIndex IntRep rhs' fixedHS
    	assign = StAssign IntRep lhs' (StInd IntRep sz)
    in
    returnUs (\xs -> assign : xs)

\end{code}

Most other array primitives translate to simple indexing.

\begin{code}
primCode lhs@[_] IndexArrayOp args
  = primCode lhs ReadArrayOp args

primCode [lhs] ReadArrayOp [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	base = StIndex IntRep obj' arrPtrsHS
    	assign = StAssign PtrRep lhs' (StInd PtrRep (StIndex PtrRep base ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [] WriteArrayOp [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' arrPtrsHS
    	assign = StAssign PtrRep (StInd PtrRep (StIndex PtrRep base ix')) v'
    in
    returnUs (\xs -> assign : xs)

primCode lhs@[_] (IndexByteArrayOp pk) args
  = primCode lhs (ReadByteArrayOp pk) args

-- NB: indexing in "pk" units, *not* in bytes (WDP 95/09)

primCode [lhs] (ReadByteArrayOp pk) [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	base = StIndex IntRep obj' arrWordsHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk base ix'))
    in
    returnUs (\xs -> assign : xs)

primCode lhs@[_] (ReadOffAddrOp pk) args
  = primCode lhs (IndexOffAddrOp pk) args

primCode [lhs] (IndexOffAddrOp pk) [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj' ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [lhs] (IndexOffForeignObjOp pk) [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
	obj'' = StIndex AddrRep obj' fixedHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj'' ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [] (WriteOffAddrOp pk) [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	assign = StAssign pk (StInd pk (StIndex pk obj' ix')) v'
    in
    returnUs (\xs -> assign : xs)

primCode [] (WriteByteArrayOp pk) [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' arrWordsHS
    	assign = StAssign pk (StInd pk (StIndex pk base ix')) v'
    in
    returnUs (\xs -> assign : xs)

primCode [] WriteForeignObjOp [obj, v]
  = let
    	obj' = amodeToStix obj
    	v' = amodeToStix v
	obj'' = StIndex AddrRep obj' (StInt 4711) -- fixedHS
    	assign = StAssign AddrRep (StInd AddrRep obj'') v'
    in
    returnUs (\xs -> assign : xs)
\end{code}

ToDo: saving/restoring of volatile regs around ccalls.

\begin{code}
primCode lhs (CCallOp (CCall (StaticTarget fn) is_asm may_gc cconv)) rhs
  | is_asm = error "ERROR: Native code generator can't handle casm"
  | not may_gc = returnUs (\xs -> ccall : xs)
  | otherwise =
	save_thread_state	`thenUs` \ save ->
	load_thread_state	`thenUs` \ load -> 
	getUniqueUs		`thenUs` \ uniq -> 
	let
	   id  = StReg (StixTemp uniq IntRep)

    	   suspend = StAssign IntRep id 
			(StCall SLIT("suspendThread") cconv IntRep [stgBaseReg])
	   resume  = StCall SLIT("resumeThread") cconv VoidRep [id]
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
	      ForeignObjRep -> StIndex PtrRep base fixedHS
	      _ -> base

    ccall = case lhs of
      [] -> StCall fn cconv VoidRep args
      [lhs] ->
	  let lhs' = amodeToStix lhs
	      pk = if isFloatingRep (getAmodeRep lhs) then DoubleRep else IntRep
	  in
	      StAssign pk lhs' (StCall fn cconv pk args)
\end{code}

DataToTagOp won't work for 64-bit archs, as it is.

\begin{code}
primCode [lhs] DataToTagOp [arg]
  = let lhs'        = amodeToStix lhs
        arg'        = amodeToStix arg
        infoptr     = StInd PtrRep arg'
        word_32     = StInd WordRep (StIndex PtrRep infoptr (StInt (-1)))
        masked_le32 = StPrim SrlOp [word_32, StInt 16]
        masked_be32 = StPrim AndOp [word_32, StInt 65535]
#ifdef WORDS_BIGENDIAN
        masked      = masked_be32
#else
        masked      = masked_le32
#endif
        assign      = StAssign IntRep lhs' masked
    in
    returnUs (\xs -> assign : xs)
\end{code}

MutVars are pretty simple.
#define writeMutVarzh(a,v)       (P_)(((StgMutVar *)(a))->var)=(v)

\begin{code}
primCode [] WriteMutVarOp [aa,vv]
   = let aa_s      = amodeToStix aa
         vv_s      = amodeToStix vv
         var_field = StIndex PtrRep aa_s fixedHS
         assign    = StAssign PtrRep (StInd PtrRep var_field) vv_s
     in
     returnUs (\xs -> assign : xs)

primCode [rr] ReadMutVarOp [aa]
   = let aa_s      = amodeToStix aa
         rr_s      = amodeToStix rr
         var_field = StIndex PtrRep aa_s fixedHS
         assign    = StAssign PtrRep rr_s (StInd PtrRep var_field)
     in
     returnUs (\xs -> assign : xs)
\end{code}

Now the more mundane operations.

\begin{code}
primCode lhs op rhs
  = let
	lhs' = map amodeToStix  lhs
    	rhs' = map amodeToStix' rhs
	pk   = getAmodeRep (head lhs)
    in
    returnUs (\ xs -> simplePrim pk lhs' op rhs' : xs)
\end{code}

\begin{code}
simpleCoercion
      :: PrimRep
      -> CAddrMode
      -> CAddrMode
      -> UniqSM StixTreeList

simpleCoercion pk lhs rhs
  = returnUs (\xs -> StAssign pk (amodeToStix lhs) (amodeToStix rhs) : xs)
\end{code}

Here we try to rewrite primitives into a form the code generator can
understand.  Any primitives not handled here must be handled at the
level of the specific code generator.

\begin{code}
simplePrim
    :: PrimRep		-- Rep of first destination
    -> [StixTree]	-- Destinations
    -> PrimOp
    -> [StixTree]
    -> StixTree
\end{code}

Now look for something more conventional.

\begin{code}
simplePrim pk [lhs] op rest  = StAssign pk lhs (StPrim op rest)
simplePrim pk as    op bs    = simplePrim_error op

simplePrim_error op
    = error ("ERROR: primitive operation `"++show op++"'cannot be handled\nby the native-code generator.  Workaround: use -fvia-C.\n(Perhaps you should report it as a GHC bug, also.)\n")
\end{code}

%---------------------------------------------------------------------

Here we generate the Stix code for CAddrModes.

When a character is fetched from a mixed type location, we have to do
an extra cast.  This is reflected in amodeCode', which is for rhs
amodes that might possibly need the extra cast.

\begin{code}
amodeToStix, amodeToStix' :: CAddrMode -> StixTree

amodeToStix'{-'-} am@(CVal rr CharRep)
    | mixedTypeLocn am = StPrim ChrOp [amodeToStix am]
    | otherwise = amodeToStix am

amodeToStix' am = amodeToStix am

-----------
amodeToStix am@(CVal rr CharRep)
  | mixedTypeLocn am
  = StInd IntRep (amodeToStix (CAddr rr))

amodeToStix (CVal rr pk) = StInd pk (amodeToStix (CAddr rr))

amodeToStix (CAddr (SpRel off))
  = StIndex PtrRep stgSp (StInt (toInteger IBOX(off)))

amodeToStix (CAddr (HpRel off))
  = StIndex IntRep stgHp (StInt (toInteger (- IBOX(off))))

amodeToStix (CAddr (NodeRel off))
  = StIndex IntRep stgNode (StInt (toInteger IBOX(off)))

amodeToStix (CAddr (CIndex base off pk))
  = StIndex pk (amodeToStix base) (amodeToStix off)

amodeToStix (CReg magic)    = StReg (StixMagicId magic)
amodeToStix (CTemp uniq pk) = StReg (StixTemp uniq pk)

amodeToStix (CLbl      lbl _) = StCLbl lbl

 -- For CharLike and IntLike, we attempt some trivial constant-folding here.

amodeToStix (CCharLike (CLit (MachChar c)))
  = StIndex CharRep cHARLIKE_closure (StInt (toInteger off))
  where
    off = charLikeSize * ord c

amodeToStix (CCharLike x)
  = StIndex CharRep cHARLIKE_closure off
  where
    off = StPrim IntMulOp [amodeToStix x, StInt (toInteger charLikeSize)]

amodeToStix (CIntLike (CLit (MachInt i)))
  = StIndex CharRep{-yes,really-} iNTLIKE_closure (StInt (toInteger off))
  where
    off = intLikeSize * (fromInteger (i - mIN_INTLIKE))

amodeToStix (CIntLike x)
  = panic "CIntLike"

amodeToStix (CLit core)
  = case core of
      MachChar c     -> StInt (toInteger (ord c))
      MachStr s	     -> StString s
      MachAddr a     -> StInt a
      MachInt i      -> StInt i
      MachWord w     -> case word2IntLit core of MachInt iw -> StInt iw
      MachLitLit s _ -> litLitToStix (_UNPK_ s)
      MachFloat d    -> StDouble d
      MachDouble d   -> StDouble d
      _ -> panic "amodeToStix:core literal"

amodeToStix (CLitLit s _)
   = litLitToStix (_UNPK_ s)

amodeToStix (CMacroExpr _ macro [arg])
  = case macro of
      ENTRY_CODE -> amodeToStix arg
      ARG_TAG    -> amodeToStix arg -- just an integer no. of words
      GET_TAG    -> 
#ifdef WORDS_BIGENDIAN
                    StPrim AndOp 
			[StInd WordRep (StIndex PtrRep (amodeToStix arg)
                                                (StInt (toInteger (-1)))),
			 StInt 65535]
#else
                    StPrim SrlOp 
			[StInd WordRep (StIndex PtrRep (amodeToStix arg)
                                                (StInt (toInteger (-1)))),
			 StInt 16]
#endif
      UPD_FRAME_UPDATEE
         -> StInd PtrRep (StIndex PtrRep (amodeToStix arg) 
                                         (StInt (toInteger uF_UPDATEE)))
litLitToStix nm
  = error ("\nlitLitToStix: can't handle `" ++ nm ++ "'\n" 
            ++ "suggested workaround: use flag -fvia-C\n")
\end{code}

Sizes of the CharLike and IntLike closures that are arranged as arrays
in the data segment.  (These are in bytes.)

\begin{code}
-- The INTLIKE base pointer

iNTLIKE_closure :: StixTree
iNTLIKE_closure = StCLbl mkIntlikeClosureLabel

-- The CHARLIKE base

cHARLIKE_closure :: StixTree
cHARLIKE_closure = StCLbl mkCharlikeClosureLabel

-- Trees for the ErrorIOPrimOp

topClosure, errorIO :: StixTree

topClosure = StInd PtrRep (StCLbl mkTopClosureLabel)
errorIO = StJump (StInd PtrRep (StCLbl mkErrorIO_innardsLabel))

mutArrPtrsFrozen_info = StCLbl mkMAP_FROZEN_infoLabel

-- these are the sizes of charLike and intLike closures, in _bytes_.
charLikeSize = (fixedHdrSize + 1) * (fromInteger (sizeOf PtrRep))
intLikeSize  = (fixedHdrSize + 1) * (fromInteger (sizeOf PtrRep))
\end{code}


\begin{code}
save_thread_state 
   = getUniqueUs   `thenUs` \tso_uq -> 
     let tso = StReg (StixTemp tso_uq ThreadIdRep) in
     returnUs (\xs ->
	StAssign ThreadIdRep tso stgCurrentTSO :
	StAssign PtrRep
	   (StInd PtrRep (StPrim IntAddOp 
		[tso, StInt (toInteger (TSO_SP*BYTES_PER_WORD))]))
	   stgSp :
	StAssign PtrRep 
	   (StInd PtrRep (StPrim IntAddOp 
		[tso, StInt (toInteger (TSO_SU*BYTES_PER_WORD))]))
	   stgSu :
	StAssign PtrRep 
	   (StInd PtrRep (StPrim IntAddOp 
		[tso, StInt (toInteger (TSO_SPLIM*BYTES_PER_WORD))]))
	   stgSpLim :
	StAssign PtrRep
	   (StInd PtrRep (StPrim IntAddOp
		[stgCurrentNursery, 
		 StInt (toInteger (BDESCR_FREE * BYTES_PER_WORD))]))
	   (StPrim IntAddOp [stgHp, StInt (toInteger (1 * BYTES_PER_WORD))]) :
	xs
     )

load_thread_state 
   = getUniqueUs   `thenUs` \tso_uq -> 
     let tso = StReg (StixTemp tso_uq ThreadIdRep) in
     returnUs (\xs ->
	StAssign ThreadIdRep tso stgCurrentTSO :
	StAssign PtrRep stgSp
	   (StInd PtrRep (StPrim IntAddOp 
		[tso, StInt (toInteger (TSO_SP*BYTES_PER_WORD))])) :
	StAssign PtrRep stgSu
	   (StInd PtrRep (StPrim IntAddOp 
		[tso, StInt (toInteger (TSO_SU*BYTES_PER_WORD))])) :
	StAssign PtrRep stgSpLim
	   (StInd PtrRep (StPrim IntAddOp 
		[tso, StInt (toInteger (TSO_SPLIM*BYTES_PER_WORD))])) :
	StAssign PtrRep stgHp
	   (StPrim IntSubOp [
	      StInd PtrRep (StPrim IntAddOp
		[stgCurrentNursery, 
		 StInt (toInteger (BDESCR_FREE * BYTES_PER_WORD))]),
	      StInt (toInteger (1 * BYTES_PER_WORD))
	    ]) :
	StAssign PtrRep stgHpLim
	   (StPrim IntAddOp [
	      StInd PtrRep (StPrim IntAddOp
		[stgCurrentNursery, 
		 StInt (toInteger (BDESCR_START * BYTES_PER_WORD))]),
	      StInt (toInteger (bLOCK_SIZE - (1 * BYTES_PER_WORD)))
	    ]) :
	xs
     )
\end{code}
