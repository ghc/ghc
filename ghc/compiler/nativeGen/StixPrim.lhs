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
import Const		( Literal(..) )
import CallConv		( cCallConv )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..), isFloatingRep )
import UniqSupply	( returnUs, thenUs, UniqSM )
import Constants	( mIN_INTLIKE )
import Outputable

import Char	       	( ord )
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
primCode res@[ar,sr,dr] IntegerNegOp arg@[aa,sa,da]
  = gmpNegate (ar,sr,dr) (aa,sa,da)
\end{code}

\begin{code}
primCode [res] IntegerCmpOp args@[aa1,sa1,da1, aa2,sa2,da2]
  = gmpCompare res (aa1,sa1,da1, aa2,sa2,da2)

primCode [res] Integer2IntOp arg@[aa,sa,da]
  = gmpInteger2Int res (aa,sa,da)

primCode [res] Integer2WordOp arg@[aa,sa,da]
  = gmpInteger2Word res (aa,sa,da)

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
    	base = StIndex IntRep obj' arrHS
    	assign = StAssign PtrRep lhs' (StInd PtrRep (StIndex PtrRep base ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [] WriteArrayOp [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' arrHS
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
    	base = StIndex IntRep obj' arrHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk base ix'))
    in
    returnUs (\xs -> assign : xs)

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
	obj'' = StIndex PtrRep obj' fixedHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj'' ix'))
    in
    returnUs (\xs -> assign : xs)

primCode [] (WriteByteArrayOp pk) [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' arrHS
    	assign = StAssign pk (StInd pk (StIndex pk base ix')) v'
    in
    returnUs (\xs -> assign : xs)
\end{code}

\begin{code}
--primCode lhs (CCallOp fn is_asm may_gc) rhs
primCode lhs (CCallOp (Left fn) is_asm may_gc cconv) rhs
  | is_asm = error "ERROR: Native code generator can't handle casm"
  | may_gc = error "ERROR: Native code generator can't handle _ccall_GC_\n"
  | otherwise
  = case lhs of
      [] -> returnUs (\xs -> (StCall fn cconv VoidRep args) : xs)
      [lhs] ->
	  let lhs' = amodeToStix lhs
	      pk = if isFloatingRep (getAmodeRep lhs) then DoubleRep else IntRep
	      call = StAssign pk lhs' (StCall fn cconv pk args)
	  in
	      returnUs (\xs -> call : xs)
  where
    args = map amodeCodeForCCall rhs
    amodeCodeForCCall x =
	let base = amodeToStix' x
	in
	    case getAmodeRep x of
	      ArrayRep      -> StIndex PtrRep base arrHS
	      ByteArrayRep  -> StIndex IntRep base arrHS
	      ForeignObjRep -> StIndex PtrRep base fixedHS
	      _ -> base
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

amodeToStix (CReg magic)    = StReg (StixMagicId magic)
amodeToStix (CTemp uniq pk) = StReg (StixTemp uniq pk)

amodeToStix (CLbl      lbl _) = StCLbl lbl

amodeToStix (CTableEntry base off pk)
  = StInd pk (StIndex pk (amodeToStix base) (amodeToStix off))

 -- For CharLike and IntLike, we attempt some trivial constant-folding here.

amodeToStix (CCharLike (CLit (MachChar c)))
  = StLitLbl ((<>) (ptext SLIT("CHARLIKE_closure+")) (int off))
  where
    off = charLikeSize * ord c

amodeToStix (CCharLike x)
  = StIndex PtrRep charLike off
  where
    off = StPrim IntMulOp [amodeToStix x, StInt (toInteger (fixedHdrSize+1))]

amodeToStix (CIntLike (CLit (MachInt i _)))
  = StLitLbl ((<>) (ptext SLIT("INTLIKE_closure+")) (int off))
  where
    off = intLikeSize * (fromInteger (i - mIN_INTLIKE))

amodeToStix (CIntLike x)
  = panic "CIntLike"

 -- A CString is just a (CLit . MachStr)
amodeToStix (CString s) = StString s

amodeToStix (CLit core)
  = case core of
      MachChar c     -> StInt (toInteger (ord c))
      MachStr s	     -> StString s
      MachAddr a     -> StInt a
      MachInt i _    -> StInt (toInteger i)
      MachLitLit s _ -> StLitLit s
      MachFloat d    -> StDouble d
      MachDouble d   -> StDouble d
      _ -> panic "amodeToStix:core literal"

 -- A CLitLit is just a (CLit . MachLitLit)
amodeToStix (CLitLit s _) = StLitLit s

amodeToStix (CMacroExpr _ macro [arg])
  = case macro of
      ENTRY_CODE -> amodeToStix arg
      ARG_TAG    -> amodeToStix arg -- just an integer no. of words
      GET_TAG    -> StPrim SrlOp 
			[StInd WordRep (StPrim IntSubOp [amodeToStix arg,
							 StInt 1]),
			 StInt 16]

-- XXX!!!
-- GET_TAG(info_ptr) is supposed to be  get_itbl(info_ptr)->srt_len,
-- which we've had to hand-code here.
\end{code}

Sizes of the CharLike and IntLike closures that are arranged as arrays
in the data segment.  (These are in bytes.)

\begin{code}
-- The INTLIKE base pointer

intLikePtr :: StixTree

intLikePtr = StInd PtrRep (sStLitLbl SLIT("INTLIKE_closure"))

-- The CHARLIKE base

charLike :: StixTree

charLike = sStLitLbl SLIT("CHARLIKE_closure")

-- Trees for the ErrorIOPrimOp

topClosure, errorIO :: StixTree

topClosure = StInd PtrRep (sStLitLbl SLIT("TopClosure"))
errorIO = StJump (StInd PtrRep (sStLitLbl SLIT("ErrorIO_innards")))

mutArrPtrsFrozen_info = sStLitLbl SLIT("MUT_ARR_PTRS_FROZEN_info")

charLikeSize = (fixedHdrSize + 1) * (fromInteger (sizeOf PtrRep))
intLikeSize  = (fixedHdrSize + 1) * (fromInteger (sizeOf PtrRep))
\end{code}
