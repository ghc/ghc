%
% (c) The AQUA Project, Glasgow University, 1993-1998
%

\begin{code}
module StixInteger ( 
	gmpCompare, 
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
import SMRep		( arrHdrSize )
import Stix		( sStLitLbl, StixTree(..), StixTreeList )
import UniqSupply	( returnUs, thenUs, UniqSM )
\end{code}

Although gmpCompare doesn't allocate space, it does temporarily use
some space just beyond the heap pointer.  This is safe, because the
enclosing routine has already guaranteed that this space will be
available.  (See ``primOpHeapRequired.'')

\begin{code}
gmpCompare
    :: CAddrMode    	    -- result (boolean)
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- alloc hp + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpCompare res args@(caa1,csa1,cda1, caa2,csa2,cda2)
  = let
	result	= amodeToStix res
	scratch1 = scratch_space
	scratch2 = StIndex IntRep scratch_space (StInt (toInteger mpIntSize))
	aa1	= amodeToStix caa1
	sa1	= amodeToStix csa1
	da1	= amodeToStix cda1
	aa2	= amodeToStix caa2
	sa2	= amodeToStix csa2
	da2	= amodeToStix cda2

    	(a1,a2,a3) = toStruct scratch1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct scratch2 (aa2,sa2,da2)
    	mpz_cmp = StCall SLIT("mpz_cmp") cCallConv IntRep [scratch1, scratch2]
    	r1 = StAssign IntRep result mpz_cmp
    in
    returnUs (\xs -> a1 : a2 : a3 : a4 : a5 : a6 : r1 : xs)
\end{code}

\begin{code}
gmpInteger2Int
    :: CAddrMode    	    -- result
    -> (CAddrMode,CAddrMode,CAddrMode) -- alloc hp + argument (3 parts)
    -> UniqSM StixTreeList

gmpInteger2Int res args@(caa,csa,cda)
  = let
	result	= amodeToStix res
	aa	= amodeToStix caa
	sa	= amodeToStix csa
	da	= amodeToStix cda

    	(a1,a2,a3) = toStruct scratch_space (aa,sa,da)
    	mpz_get_si = StCall SLIT("mpz_get_si") cCallConv IntRep [scratch_space]
    	r1 = StAssign IntRep result mpz_get_si
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

gmpInteger2Word
    :: CAddrMode    	    -- result
    -> (CAddrMode,CAddrMode,CAddrMode) -- alloc hp + argument (3 parts)
    -> UniqSM StixTreeList

gmpInteger2Word res args@(caa,csa,cda)
  = let
	result	= amodeToStix res
	aa	= amodeToStix caa
	sa	= amodeToStix csa
	da	= amodeToStix cda

    	(a1,a2,a3) = toStruct scratch_space (aa,sa,da)
    	mpz_get_ui = StCall SLIT("mpz_get_ui") cCallConv IntRep [scratch_space]
    	r1 = StAssign WordRep result mpz_get_ui
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

gmpNegate
    :: (CAddrMode,CAddrMode,CAddrMode) -- result
    -> (CAddrMode,CAddrMode,CAddrMode) -- argument (3 parts)
    -> UniqSM StixTreeList

gmpNegate (rca, rcs, rcd) args@(ca, cs, cd)
  = let
	a	= amodeToStix ca
	s	= amodeToStix cs
	d	= amodeToStix cd
	ra	= amodeToStix rca
	rs	= amodeToStix rcs
	rd	= amodeToStix rcd
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
    	f3 = StAssign PtrRep (mpData str) 
		(StIndex PtrRep arr (StInt (toInteger arrHdrSize)))
    in
    (f1, f2, f3)

scratch_space = sStLitLbl SLIT("stg_scratch_space")
\end{code}

