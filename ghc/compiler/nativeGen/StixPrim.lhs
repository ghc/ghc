%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixPrim ( primCode, amodeToStix, amodeToStix', foreignCallCode )
  where

#include "HsVersions.h"

import MachMisc
import Stix
import StixInteger

import AbsCSyn 		hiding ( spRel )
import AbsCUtils	( getAmodeRep, mixedTypeLocn )
import SMRep		( fixedHdrSize )
import Literal		( Literal(..), word2IntLit )
import PrimOp		( PrimOp(..) )
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

The main honchos here are primCode anf foreignCallCode, which handle the guts of COpStmts.

\begin{code}
foreignCallCode
    :: [CAddrMode]  	-- results
    -> ForeignCall    	-- op
    -> [CAddrMode]  	-- args
    -> UniqSM StixTreeList

primCode
    :: [CAddrMode]  	-- results
    -> PrimOp 	    	-- op
    -> [CAddrMode]  	-- args
    -> UniqSM StixTreeList
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
  | not (playSafe safety) = returnUs (\xs -> ccall : xs)

  | otherwise
  = save_thread_state	`thenUs` \ save ->
    load_thread_state	`thenUs` \ load -> 
    getUniqueUs		`thenUs` \ uniq -> 
    let
       id  = StReg (StixTemp uniq IntRep)
    
       suspend = StAssign IntRep id 
    		(StCall SLIT("suspendThread") {-no:cconv-} CCallConv
                            IntRep [stgBaseReg])
       resume  = StCall SLIT("resumeThread") {-no:cconv-} CCallConv
                        VoidRep [id]
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
      []    -> StCall fn cconv VoidRep args
      [lhs] -> StAssign pk lhs' (StCall fn cconv pk args)
	    where
	       lhs' = amodeToStix lhs
	       pk   = case getAmodeRep lhs of
                        FloatRep  -> FloatRep
                        DoubleRep -> DoubleRep
                        other     -> IntRep

foreignCallCode lhs call rhs
  = pprPanic "Native code generator can't handle foreign call" (ppr call)
\end{code}


%************************************************************************
%*									*
\subsubsection{Code for primops}
%*									*
%************************************************************************

The (MP) integer operations are a true nightmare.  Since we don't have
a convenient abstract way of allocating temporary variables on the (C)
stack, we use the space just below HpLim for the @MP_INT@ structures,
and modify our heap check accordingly.

\begin{code}
-- NB: ordering of clauses somewhere driven by
-- the desire to getting sane patt-matching behavior

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

primCode [res] AddrToHValueOp [arg]
  = simpleCoercion PtrRep res arg

primCode [res] IntToInt8Op [arg]
  = narrowingCoercion IntRep Int8Rep res arg
primCode [res] IntToInt16Op [arg]
  = narrowingCoercion IntRep Int16Rep res arg
primCode [res] IntToInt32Op [arg]
  = narrowingCoercion IntRep Int32Rep res arg

primCode [res] WordToWord8Op [arg]
  = narrowingCoercion WordRep Word8Rep res arg
primCode [res] WordToWord16Op [arg]
  = narrowingCoercion WordRep Word16Rep res arg
primCode [res] WordToWord32Op [arg]
  = narrowingCoercion WordRep Word32Rep res arg
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
\end{code}

\begin{code}
primCode res@[_] SameMVarOp args
  = primCode res SameMutableArrayOp args

-- #define isEmptyMVarzh(r,a) \
--     r =(I_)((GET_INFO((StgMVar*)(a))) == &stg_EMPTY_MVAR_info )
primCode [res] IsEmptyMVarOp [arg] 
   = let res'     = amodeToStix res
         arg'     = amodeToStix arg
         arg_info = StInd PtrRep arg'
         em_info  = StCLbl mkEMPTY_MVAR_infoLabel
         same     = StPrim IntEqOp [arg_info, em_info]
         assign   = StAssign IntRep res' same
     in
     returnUs (\xs -> assign : xs)

-- #define myThreadIdzh(t) (t = CurrentTSO)
primCode [res] MyThreadIdOp [] 
   = let res' = amodeToStix res
     in  returnUs (\xs -> StAssign ThreadIdRep res' stgCurrentTSO : xs)

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

primCode [] WriteForeignObjOp [obj, v]
  = let
    	obj' = amodeToStix obj
    	v' = amodeToStix v
	obj'' = StIndex AddrRep obj' (StInt 4711) -- fixedHS
    	assign = StAssign AddrRep (StInd AddrRep obj'') v'
    in
    returnUs (\xs -> assign : xs)

-- NB: indexing in "pk" units, *not* in bytes (WDP 95/09)
primCode ls IndexByteArrayOp_Char      rs = primCode_ReadByteArrayOp Word8Rep     ls rs
primCode ls IndexByteArrayOp_WideChar  rs = primCode_ReadByteArrayOp CharRep      ls rs
primCode ls IndexByteArrayOp_Int       rs = primCode_ReadByteArrayOp IntRep       ls rs
primCode ls IndexByteArrayOp_Word      rs = primCode_ReadByteArrayOp WordRep      ls rs
primCode ls IndexByteArrayOp_Addr      rs = primCode_ReadByteArrayOp AddrRep      ls rs
primCode ls IndexByteArrayOp_Float     rs = primCode_ReadByteArrayOp FloatRep     ls rs
primCode ls IndexByteArrayOp_Double    rs = primCode_ReadByteArrayOp DoubleRep    ls rs
primCode ls IndexByteArrayOp_StablePtr rs = primCode_ReadByteArrayOp StablePtrRep ls rs
primCode ls IndexByteArrayOp_Int8      rs = primCode_ReadByteArrayOp Int8Rep      ls rs
primCode ls IndexByteArrayOp_Int16     rs = primCode_ReadByteArrayOp Int16Rep     ls rs
primCode ls IndexByteArrayOp_Int32     rs = primCode_ReadByteArrayOp Int32Rep     ls rs
primCode ls IndexByteArrayOp_Int64     rs = primCode_ReadByteArrayOp Int64Rep     ls rs
primCode ls IndexByteArrayOp_Word8     rs = primCode_ReadByteArrayOp Word8Rep     ls rs
primCode ls IndexByteArrayOp_Word16    rs = primCode_ReadByteArrayOp Word16Rep    ls rs
primCode ls IndexByteArrayOp_Word32    rs = primCode_ReadByteArrayOp Word32Rep    ls rs
primCode ls IndexByteArrayOp_Word64    rs = primCode_ReadByteArrayOp Word64Rep    ls rs

primCode ls ReadByteArrayOp_Char      rs = primCode_ReadByteArrayOp Word8Rep     ls rs
primCode ls ReadByteArrayOp_WideChar  rs = primCode_ReadByteArrayOp CharRep      ls rs
primCode ls ReadByteArrayOp_Int       rs = primCode_ReadByteArrayOp IntRep       ls rs
primCode ls ReadByteArrayOp_Word      rs = primCode_ReadByteArrayOp WordRep      ls rs
primCode ls ReadByteArrayOp_Addr      rs = primCode_ReadByteArrayOp AddrRep      ls rs
primCode ls ReadByteArrayOp_Float     rs = primCode_ReadByteArrayOp FloatRep     ls rs
primCode ls ReadByteArrayOp_Double    rs = primCode_ReadByteArrayOp DoubleRep    ls rs
primCode ls ReadByteArrayOp_StablePtr rs = primCode_ReadByteArrayOp StablePtrRep ls rs
primCode ls ReadByteArrayOp_Int8      rs = primCode_ReadByteArrayOp Int8Rep      ls rs
primCode ls ReadByteArrayOp_Int16     rs = primCode_ReadByteArrayOp Int16Rep     ls rs
primCode ls ReadByteArrayOp_Int32     rs = primCode_ReadByteArrayOp Int32Rep     ls rs
primCode ls ReadByteArrayOp_Int64     rs = primCode_ReadByteArrayOp Int64Rep     ls rs
primCode ls ReadByteArrayOp_Word8     rs = primCode_ReadByteArrayOp Word8Rep     ls rs
primCode ls ReadByteArrayOp_Word16    rs = primCode_ReadByteArrayOp Word16Rep    ls rs
primCode ls ReadByteArrayOp_Word32    rs = primCode_ReadByteArrayOp Word32Rep    ls rs
primCode ls ReadByteArrayOp_Word64    rs = primCode_ReadByteArrayOp Word64Rep    ls rs

primCode ls WriteByteArrayOp_Char      rs = primCode_WriteByteArrayOp Word8Rep     ls rs
primCode ls WriteByteArrayOp_WideChar  rs = primCode_WriteByteArrayOp CharRep      ls rs
primCode ls WriteByteArrayOp_Int       rs = primCode_WriteByteArrayOp IntRep       ls rs
primCode ls WriteByteArrayOp_Word      rs = primCode_WriteByteArrayOp WordRep      ls rs
primCode ls WriteByteArrayOp_Addr      rs = primCode_WriteByteArrayOp AddrRep      ls rs
primCode ls WriteByteArrayOp_Float     rs = primCode_WriteByteArrayOp FloatRep     ls rs
primCode ls WriteByteArrayOp_Double    rs = primCode_WriteByteArrayOp DoubleRep    ls rs
primCode ls WriteByteArrayOp_StablePtr rs = primCode_WriteByteArrayOp StablePtrRep ls rs
primCode ls WriteByteArrayOp_Int8      rs = primCode_WriteByteArrayOp Int8Rep      ls rs
primCode ls WriteByteArrayOp_Int16     rs = primCode_WriteByteArrayOp Int16Rep     ls rs
primCode ls WriteByteArrayOp_Int32     rs = primCode_WriteByteArrayOp Int32Rep     ls rs
primCode ls WriteByteArrayOp_Int64     rs = primCode_WriteByteArrayOp Int64Rep     ls rs
primCode ls WriteByteArrayOp_Word8     rs = primCode_WriteByteArrayOp Word8Rep     ls rs
primCode ls WriteByteArrayOp_Word16    rs = primCode_WriteByteArrayOp Word16Rep    ls rs
primCode ls WriteByteArrayOp_Word32    rs = primCode_WriteByteArrayOp Word32Rep    ls rs
primCode ls WriteByteArrayOp_Word64    rs = primCode_WriteByteArrayOp Word64Rep    ls rs

primCode ls IndexOffAddrOp_Char      rs = primCode_IndexOffAddrOp Word8Rep     ls rs
primCode ls IndexOffAddrOp_WideChar  rs = primCode_IndexOffAddrOp CharRep      ls rs
primCode ls IndexOffAddrOp_Int       rs = primCode_IndexOffAddrOp IntRep       ls rs
primCode ls IndexOffAddrOp_Word      rs = primCode_IndexOffAddrOp WordRep      ls rs
primCode ls IndexOffAddrOp_Addr      rs = primCode_IndexOffAddrOp AddrRep      ls rs
primCode ls IndexOffAddrOp_Float     rs = primCode_IndexOffAddrOp FloatRep     ls rs
primCode ls IndexOffAddrOp_Double    rs = primCode_IndexOffAddrOp DoubleRep    ls rs
primCode ls IndexOffAddrOp_StablePtr rs = primCode_IndexOffAddrOp StablePtrRep ls rs
primCode ls IndexOffAddrOp_Int8      rs = primCode_IndexOffAddrOp Int8Rep      ls rs
primCode ls IndexOffAddrOp_Int16     rs = primCode_IndexOffAddrOp Int16Rep     ls rs
primCode ls IndexOffAddrOp_Int32     rs = primCode_IndexOffAddrOp Int32Rep     ls rs
primCode ls IndexOffAddrOp_Int64     rs = primCode_IndexOffAddrOp Int64Rep     ls rs
primCode ls IndexOffAddrOp_Word8     rs = primCode_IndexOffAddrOp Word8Rep     ls rs
primCode ls IndexOffAddrOp_Word16    rs = primCode_IndexOffAddrOp Word16Rep    ls rs
primCode ls IndexOffAddrOp_Word32    rs = primCode_IndexOffAddrOp Word32Rep    ls rs
primCode ls IndexOffAddrOp_Word64    rs = primCode_IndexOffAddrOp Word64Rep    ls rs

primCode ls IndexOffForeignObjOp_Char      rs = primCode_IndexOffForeignObjOp Word8Rep     ls rs
primCode ls IndexOffForeignObjOp_WideChar  rs = primCode_IndexOffForeignObjOp CharRep      ls rs
primCode ls IndexOffForeignObjOp_Int       rs = primCode_IndexOffForeignObjOp IntRep       ls rs
primCode ls IndexOffForeignObjOp_Word      rs = primCode_IndexOffForeignObjOp WordRep      ls rs
primCode ls IndexOffForeignObjOp_Addr      rs = primCode_IndexOffForeignObjOp AddrRep      ls rs
primCode ls IndexOffForeignObjOp_Float     rs = primCode_IndexOffForeignObjOp FloatRep     ls rs
primCode ls IndexOffForeignObjOp_Double    rs = primCode_IndexOffForeignObjOp DoubleRep    ls rs
primCode ls IndexOffForeignObjOp_StablePtr rs = primCode_IndexOffForeignObjOp StablePtrRep ls rs
primCode ls IndexOffForeignObjOp_Int8      rs = primCode_IndexOffForeignObjOp Int8Rep      ls rs
primCode ls IndexOffForeignObjOp_Int16     rs = primCode_IndexOffForeignObjOp Int16Rep     ls rs
primCode ls IndexOffForeignObjOp_Int32     rs = primCode_IndexOffForeignObjOp Int32Rep     ls rs
primCode ls IndexOffForeignObjOp_Int64     rs = primCode_IndexOffForeignObjOp Int64Rep     ls rs
primCode ls IndexOffForeignObjOp_Word8     rs = primCode_IndexOffForeignObjOp Word8Rep     ls rs
primCode ls IndexOffForeignObjOp_Word16    rs = primCode_IndexOffForeignObjOp Word16Rep    ls rs
primCode ls IndexOffForeignObjOp_Word32    rs = primCode_IndexOffForeignObjOp Word32Rep    ls rs
primCode ls IndexOffForeignObjOp_Word64    rs = primCode_IndexOffForeignObjOp Word64Rep    ls rs

primCode ls ReadOffAddrOp_Char      rs = primCode_IndexOffAddrOp Word8Rep     ls rs
primCode ls ReadOffAddrOp_WideChar  rs = primCode_IndexOffAddrOp CharRep      ls rs
primCode ls ReadOffAddrOp_Int       rs = primCode_IndexOffAddrOp IntRep       ls rs
primCode ls ReadOffAddrOp_Word      rs = primCode_IndexOffAddrOp WordRep      ls rs
primCode ls ReadOffAddrOp_Addr      rs = primCode_IndexOffAddrOp AddrRep      ls rs
primCode ls ReadOffAddrOp_Float     rs = primCode_IndexOffAddrOp FloatRep     ls rs
primCode ls ReadOffAddrOp_Double    rs = primCode_IndexOffAddrOp DoubleRep    ls rs
primCode ls ReadOffAddrOp_StablePtr rs = primCode_IndexOffAddrOp StablePtrRep ls rs
primCode ls ReadOffAddrOp_Int8      rs = primCode_IndexOffAddrOp Int8Rep      ls rs
primCode ls ReadOffAddrOp_Int16     rs = primCode_IndexOffAddrOp Int16Rep     ls rs
primCode ls ReadOffAddrOp_Int32     rs = primCode_IndexOffAddrOp Int32Rep     ls rs
primCode ls ReadOffAddrOp_Int64     rs = primCode_IndexOffAddrOp Int64Rep     ls rs
primCode ls ReadOffAddrOp_Word8     rs = primCode_IndexOffAddrOp Word8Rep     ls rs
primCode ls ReadOffAddrOp_Word16    rs = primCode_IndexOffAddrOp Word16Rep    ls rs
primCode ls ReadOffAddrOp_Word32    rs = primCode_IndexOffAddrOp Word32Rep    ls rs
primCode ls ReadOffAddrOp_Word64    rs = primCode_IndexOffAddrOp Word64Rep    ls rs

primCode ls WriteOffAddrOp_Char      rs = primCode_WriteOffAddrOp Word8Rep     ls rs
primCode ls WriteOffAddrOp_WideChar  rs = primCode_WriteOffAddrOp CharRep      ls rs
primCode ls WriteOffAddrOp_Int       rs = primCode_WriteOffAddrOp IntRep       ls rs
primCode ls WriteOffAddrOp_Word      rs = primCode_WriteOffAddrOp WordRep      ls rs
primCode ls WriteOffAddrOp_Addr      rs = primCode_WriteOffAddrOp AddrRep      ls rs
primCode ls WriteOffAddrOp_Float     rs = primCode_WriteOffAddrOp FloatRep     ls rs
primCode ls WriteOffAddrOp_Double    rs = primCode_WriteOffAddrOp DoubleRep    ls rs
primCode ls WriteOffAddrOp_StablePtr rs = primCode_WriteOffAddrOp StablePtrRep ls rs
primCode ls WriteOffAddrOp_Int8      rs = primCode_WriteOffAddrOp Int8Rep      ls rs
primCode ls WriteOffAddrOp_Int16     rs = primCode_WriteOffAddrOp Int16Rep     ls rs
primCode ls WriteOffAddrOp_Int32     rs = primCode_WriteOffAddrOp Int32Rep     ls rs
primCode ls WriteOffAddrOp_Int64     rs = primCode_WriteOffAddrOp Int64Rep     ls rs
primCode ls WriteOffAddrOp_Word8     rs = primCode_WriteOffAddrOp Word8Rep     ls rs
primCode ls WriteOffAddrOp_Word16    rs = primCode_WriteOffAddrOp Word16Rep    ls rs
primCode ls WriteOffAddrOp_Word32    rs = primCode_WriteOffAddrOp Word32Rep    ls rs
primCode ls WriteOffAddrOp_Word64    rs = primCode_WriteOffAddrOp Word64Rep    ls rs

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

ForeignObj# primops.

\begin{code}
primCode [rr] ForeignObjToAddrOp [fo]
  = let code =  StAssign AddrRep (amodeToStix rr)
		   (StInd AddrRep 
			(StIndex PtrRep (amodeToStix fo) fixedHS))
    in
    returnUs (\xs -> code : xs)

primCode [] TouchOp [_] = returnUs id
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

Helper fns for some array ops.

\begin{code}
primCode_ReadByteArrayOp pk [lhs] [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	base = StIndex IntRep obj' arrWordsHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk base ix'))
    in
    returnUs (\xs -> assign : xs)


primCode_IndexOffAddrOp pk [lhs] [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj' ix'))
    in
    returnUs (\xs -> assign : xs)


primCode_IndexOffForeignObjOp pk [lhs] [obj, ix]
  = let
	lhs' = amodeToStix lhs
    	obj' = amodeToStix obj
    	ix' = amodeToStix ix
	obj'' = StIndex AddrRep obj' fixedHS
    	assign = StAssign pk lhs' (StInd pk (StIndex pk obj'' ix'))
    in
    returnUs (\xs -> assign : xs)


primCode_WriteOffAddrOp pk [] [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	assign = StAssign pk (StInd pk (StIndex pk obj' ix')) v'
    in
    returnUs (\xs -> assign : xs)


primCode_WriteByteArrayOp pk [] [obj, ix, v]
  = let
	obj' = amodeToStix obj
    	ix' = amodeToStix ix
    	v' = amodeToStix v
    	base = StIndex IntRep obj' arrWordsHS
    	assign = StAssign pk (StInd pk (StIndex pk base ix')) v'
    in
    returnUs (\xs -> assign : xs)

\end{code}

\begin{code}
simpleCoercion
      :: PrimRep
      -> CAddrMode
      -> CAddrMode
      -> UniqSM StixTreeList

simpleCoercion pk lhs rhs
  = returnUs (\xs -> StAssign pk (amodeToStix lhs) (amodeToStix rhs) : xs)


-- Rewrite a narrowing coercion into a pair of shifts.
narrowingCoercion
      :: PrimRep   -> PrimRep
      -> CAddrMode -> CAddrMode
      -> UniqSM StixTreeList

narrowingCoercion pks pkd dst src
  | szd > szs 
  = panic "StixPrim.narrowingCoercion"
  | szd == szs
  = returnUs (\xs -> StAssign pkd dst' src' : xs)
  | otherwise
  = returnUs (\xs -> assign : xs)
    where 
          szs       = getPrimRepSizeInBytes pks
          szd       = getPrimRepSizeInBytes pkd
          src'      = amodeToStix src
          dst'      = amodeToStix dst
          shift_amt = fromIntegral (8 * (szs - szd))

          assign
             = StAssign pkd dst'
                  (StPrim (if signed then ISraOp else SrlOp) 
                     [StPrim SllOp [src', StInt shift_amt],
                      StInt shift_amt])
          signed 
             = case pkd of 
                  Int8Rep -> True; Int16Rep -> True
                  Int32Rep -> True; Int64Rep -> True; IntRep -> True
                  Word8Rep -> False; Word16Rep -> False
                  Word32Rep -> False; Word64Rep -> False; WordRep -> False
                  other -> pprPanic "StixPrim.narrowingCoercion" (ppr pkd)
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
  = StIndex PtrRep stgSp (StInt (toInteger (iBox off)))

amodeToStix (CAddr (HpRel off))
  = StIndex IntRep stgHp (StInt (toInteger (- (iBox off))))

amodeToStix (CAddr (NodeRel off))
  = StIndex IntRep stgNode (StInt (toInteger (iBox off)))

amodeToStix (CAddr (CIndex base off pk))
  = StIndex pk (amodeToStix base) (amodeToStix off)

amodeToStix (CReg magic)    = StReg (StixMagicId magic)
amodeToStix (CTemp uniq pk) = StReg (StixTemp uniq pk)

amodeToStix (CLbl      lbl _) = StCLbl lbl

 -- For CharLike and IntLike, we attempt some trivial constant-folding here.

amodeToStix (CCharLike (CLit (MachChar c)))
  = StIndex Word8Rep cHARLIKE_closure (StInt (toInteger off))
  where
    off = charLikeSize * (c - mIN_CHARLIKE)

amodeToStix (CCharLike x)
  = panic "CCharLike"

amodeToStix (CIntLike (CLit (MachInt i)))
  = StIndex Word8Rep iNTLIKE_closure (StInt (toInteger off))
  where
    off = intLikeSize * (fromInteger (i - mIN_INTLIKE))

amodeToStix (CIntLike x)
  = panic "CIntLike"

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

litLitErr = 
  panic "native code generator can't compile lit-lits, use -fvia-C"
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

mutArrPtrsFrozen_info = StCLbl mkMAP_FROZEN_infoLabel

-- these are the sizes of charLike and intLike closures, in _bytes_.
charLikeSize = (fixedHdrSize + 1) * (sizeOf PtrRep)
intLikeSize  = (fixedHdrSize + 1) * (sizeOf PtrRep)
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
	   (StPrim IntAddOp [tso, 
			     StInt (toInteger ((TSO_STACK + rESERVED_STACK_WORDS)
					       *BYTES_PER_WORD))]) :
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
