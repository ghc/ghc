%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixInteger ( 
	gmpCompare, 
        gmpCompareInt,
	gmpInteger2Int, 
	gmpInteger2Word,
	gmpNegate
	) where

#include "HsVersions.h"

import {-# SOURCE #-} StixPrim ( amodeToStix )
import MachMisc
import MachRegs

import AbsCSyn		hiding (spRel) -- bits and bobs..
import Const		( Literal(..) )
import CallConv		( cCallConv )
import OrdList		( OrdList )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import SMRep		( arrWordsHdrSize )
import Stix		( sStLitLbl, StixTree(..), StixTreeList, arrWordsHS )
import UniqSupply	( returnUs, thenUs, UniqSM )
\end{code}

Although gmpCompare doesn't allocate space, it does temporarily use
some space just beyond the heap pointer.  This is safe, because the
enclosing routine has already guaranteed that this space will be
available.  (See ``primOpHeapRequired.'')

\begin{code}
stgArrWords__words        :: StixTree -> StixTree
stgArrWords__BYTE_ARR_CTS :: StixTree -> StixTree

stgArrWords__BYTE_ARR_CTS arr 
   = StIndex WordRep arr arrWordsHS
stgArrWords__words        arr 
   = case arrWordsHS of 
        StInt i -> StInd WordRep (StIndex PtrRep arr (StInt (i-1)))

gmpCompare
    :: CAddrMode    	    -- result (boolean)
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- alloc hp + 2 arguments (2 parts each)
    -> UniqSM StixTreeList

gmpCompare res args@(csa1,cda1, csa2,cda2)
  = let
	result	= amodeToStix res
	sa1	= amodeToStix csa1
	sa2	= amodeToStix csa2
	aa1	= stgArrWords__words (amodeToStix cda1)
	aa2	= stgArrWords__words (amodeToStix cda2)
	da1	= stgArrWords__BYTE_ARR_CTS (amodeToStix cda1)
	da2	= stgArrWords__BYTE_ARR_CTS (amodeToStix cda2)

    	(a1,a2,a3) = toStruct scratch1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct scratch2 (aa2,sa2,da2)
    	mpz_cmp = StCall SLIT("mpz_cmp") cCallConv IntRep [scratch1, scratch2]
    	r1 = StAssign IntRep result mpz_cmp
    in
    returnUs (\xs -> a1 : a2 : a3 : a4 : a5 : a6 : r1 : xs)


gmpCompareInt
    :: CAddrMode    	    -- result (boolean)
    -> (CAddrMode,CAddrMode,CAddrMode)
    -> UniqSM StixTreeList  -- alloc hp + 1 arg (??)

gmpCompareInt res args@(csa1,cda1, cai)
  = let
	result	 = amodeToStix res
	sa1	 = amodeToStix csa1
	aa1	 = stgArrWords__words (amodeToStix cda1)
	da1	 = stgArrWords__BYTE_ARR_CTS (amodeToStix cda1)
        ai       = amodeToStix cai
    	(a1,a2,a3) = toStruct scratch1 (aa1,sa1,da1)
    	mpz_cmp_si = StCall SLIT("mpz_cmp_si") cCallConv IntRep [scratch1, ai]
    	r1 = StAssign IntRep result mpz_cmp_si
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)
\end{code}

\begin{code}
gmpInteger2Int
    :: CAddrMode    	    -- result
    -> (CAddrMode,CAddrMode) -- alloc hp + argument (2 parts)
    -> UniqSM StixTreeList

gmpInteger2Int res args@(csa,cda)
  = let
	result	= amodeToStix res
	sa	= amodeToStix csa
	aa	= stgArrWords__words (amodeToStix cda)
	da	= stgArrWords__BYTE_ARR_CTS (amodeToStix cda)

    	(a1,a2,a3) = toStruct scratch1 (aa,sa,da)
    	mpz_get_si = StCall SLIT("mpz_get_si") cCallConv IntRep [scratch1]
    	r1 = StAssign IntRep result mpz_get_si
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

gmpInteger2Word
    :: CAddrMode    	    -- result
    -> (CAddrMode,CAddrMode) -- alloc hp + argument (2 parts)
    -> UniqSM StixTreeList

gmpInteger2Word res args@(csa,cda)
  = let
	result	= amodeToStix res
	sa	= amodeToStix csa
	aa	= stgArrWords__words (amodeToStix cda)
	da	= stgArrWords__BYTE_ARR_CTS (amodeToStix cda)

    	(a1,a2,a3) = toStruct scratch1 (aa,sa,da)
    	mpz_get_ui = StCall SLIT("mpz_get_ui") cCallConv IntRep [scratch1]
    	r1 = StAssign WordRep result mpz_get_ui
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

gmpNegate
    :: (CAddrMode,CAddrMode) -- result
    -> (CAddrMode,CAddrMode) -- argument (2 parts)
    -> UniqSM StixTreeList

gmpNegate (rcs, rcd) args@(cs, cd)
  = let
	s	= amodeToStix cs
	a	= stgArrWords__words (amodeToStix cd)
	d	= stgArrWords__BYTE_ARR_CTS (amodeToStix cd)
	rs	= amodeToStix rcs
	ra	= stgArrWords__words (amodeToStix rcd)
	rd	= stgArrWords__BYTE_ARR_CTS (amodeToStix rcd)
	a1      = StAssign IntRep ra a
	a2      = StAssign IntRep rs (StPrim IntNegOp [s])
	a3	= StAssign PtrRep rd d
    in
    returnUs (\xs -> a1 : a2 : a3 : xs)
\end{code}

Support for the Gnu GMP multi-precision package.

\begin{code}
-- size (in words) of __MP_INT
mpIntSize = 3 :: Int

mpAlloc, mpSize, mpData :: StixTree -> StixTree
mpAlloc base = StInd IntRep base
mpSize base = StInd IntRep (StIndex IntRep base (StInt 1))
mpData base = StInd PtrRep (StIndex IntRep base (StInt 2))
\end{code}

\begin{code}
toStruct
    :: StixTree
    -> (StixTree, StixTree, StixTree)
    -> (StixTree, StixTree, StixTree)

toStruct str (alloc,size,arr)
  = let
    	f1 = StAssign IntRep (mpAlloc str) alloc
    	f2 = StAssign IntRep (mpSize str) size
    	f3 = StAssign PtrRep (mpData str) arr
    in
    (f1, f2, f3)

scratch1 = StScratchWord 0
scratch2 = StScratchWord mpIntSize
\end{code}

