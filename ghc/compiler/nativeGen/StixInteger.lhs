%
% (c) The AQUA Project, Glasgow University, 1993-1996
%

\begin{code}
module StixInteger (
	gmpTake1Return1, gmpTake2Return1, gmpTake2Return2, gmpCompare,
	gmpInteger2Int, gmpInteger2Word,
	gmpInt2Integer, gmpString2Integer,
	encodeFloatingKind, decodeFloatingKind
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} StixPrim ( amodeToStix )
import MachMisc
import MachRegs

import AbsCSyn		-- bits and bobs...
import Constants	( mIN_MP_INT_SIZE )
import Literal		( Literal(..) )
import OrdList		( OrdList )
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import SMRep		( SMRep(..), SMSpecRepKind, SMUpdateKind )
import Stix		( getUniqLabelNCG, sStLitLbl, stgHp, stgHpLim,
			  StixTree(..), StixTreeList,
			  CodeSegment, StixReg
			)
import StixMacro	( macroCode, heapCheck )
import UniqSupply	( returnUs, thenUs, UniqSM )
import Util		( panic )
\end{code}

\begin{code}
gmpTake1Return1
    :: (CAddrMode,CAddrMode,CAddrMode)  -- result (3 parts)
    -> FAST_STRING			-- function name
    -> (CAddrMode, CAddrMode,CAddrMode,CAddrMode)
					-- argument (4 parts)
    -> UniqSM StixTreeList

argument1 = mpStruct 1 -- out here to avoid CAF (sigh)
argument2 = mpStruct 2
result2 = mpStruct 2
result3 = mpStruct 3
result4 = mpStruct 4
init2 = StCall SLIT("mpz_init") VoidRep [result2]
init3 = StCall SLIT("mpz_init") VoidRep [result3]
init4 = StCall SLIT("mpz_init") VoidRep [result4]

gmpTake1Return1 res@(car,csr,cdr) rtn arg@(clive,caa,csa,cda)
  = let
	ar	= amodeToStix car
	sr	= amodeToStix csr
	dr	= amodeToStix cdr
    	liveness= amodeToStix clive
	aa	= amodeToStix caa
	sa	= amodeToStix csa
	da	= amodeToStix cda

    	space = mpSpace 2 1 [sa]
    	oldHp = StIndex PtrRep stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc Hp
    	save = StAssign PtrRep safeHp oldHp
    	(a1,a2,a3) = toStruct argument1 (aa,sa,da)
    	mpz_op = StCall rtn VoidRep [result2, argument1]
    	restore = StAssign PtrRep stgHp safeHp
    	(r1,r2,r3) = fromStruct result2 (ar,sr,dr)
    in
    heapCheck liveness space (StInt 0) `thenUs` \ heap_chk ->

    returnUs (heap_chk .
	(\xs -> a1 : a2 : a3 : save : init2 : mpz_op : r1 : r2 : r3 : restore : xs))

gmpTake2Return1
    :: (CAddrMode,CAddrMode,CAddrMode)	-- result (3 parts)
    -> FAST_STRING    	    		-- function name
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
					-- liveness + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpTake2Return1 res@(car,csr,cdr) rtn args@(clive, caa1,csa1,cda1, caa2,csa2,cda2)
  = let
	ar	= amodeToStix car
	sr	= amodeToStix csr
	dr	= amodeToStix cdr
    	liveness= amodeToStix clive
	aa1	= amodeToStix caa1
	sa1	= amodeToStix csa1
	da1	= amodeToStix cda1
	aa2	= amodeToStix caa2
	sa2	= amodeToStix csa2
	da2	= amodeToStix cda2

    	space = mpSpace 3 1 [sa1, sa2]
    	oldHp = StIndex PtrRep stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc Hp
    	save = StAssign PtrRep safeHp oldHp
    	(a1,a2,a3) = toStruct argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct argument2 (aa2,sa2,da2)
    	mpz_op = StCall rtn VoidRep [result3, argument1, argument2]
    	restore = StAssign PtrRep stgHp safeHp
    	(r1,r2,r3) = fromStruct result3 (ar,sr,dr)
    in
    heapCheck liveness space (StInt 0) `thenUs` \ heap_chk ->

    returnUs (heap_chk .
	(\xs -> a1 : a2 : a3 : a4 : a5 : a6
		    : save : init3 : mpz_op : r1 : r2 : r3 : restore : xs))

gmpTake2Return2
    :: (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- 2 results (3 parts each)
    -> FAST_STRING    	    -- function name
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- liveness + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpTake2Return2 res@(car1,csr1,cdr1, car2,csr2,cdr2)
		rtn args@(clive, caa1,csa1,cda1, caa2,csa2,cda2)
  = let
	ar1	= amodeToStix car1
	sr1	= amodeToStix csr1
	dr1	= amodeToStix cdr1
	ar2	= amodeToStix car2
	sr2	= amodeToStix csr2
	dr2	= amodeToStix cdr2
    	liveness= amodeToStix clive
	aa1	= amodeToStix caa1
	sa1	= amodeToStix csa1
	da1	= amodeToStix cda1
	aa2	= amodeToStix caa2
	sa2	= amodeToStix csa2
	da2	= amodeToStix cda2

    	space = StPrim IntMulOp [mpSpace 2 1 [sa1, sa2], StInt 2]
    	oldHp = StIndex PtrRep stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc Hp
    	save = StAssign PtrRep safeHp oldHp
    	(a1,a2,a3) = toStruct argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct argument2 (aa2,sa2,da2)
    	mpz_op = StCall rtn VoidRep [result3, result4, argument1, argument2]
    	restore = StAssign PtrRep stgHp safeHp
    	(r1,r2,r3) = fromStruct result3 (ar1,sr1,dr1)
    	(r4,r5,r6) = fromStruct result4 (ar2,sr2,dr2)

    in
    heapCheck liveness space (StInt 0) `thenUs` \ heap_chk ->

    returnUs (heap_chk .
	(\xs -> a1 : a2 : a3 : a4 : a5 : a6
		    : save : init3 : init4 : mpz_op
		    : r1 : r2 : r3 : r4 : r5 : r6 : restore : xs))
\end{code}

Although gmpCompare doesn't allocate space, it does temporarily use
some space just beyond the heap pointer.  This is safe, because the
enclosing routine has already guaranteed that this space will be
available.  (See ``primOpHeapRequired.'')

\begin{code}
gmpCompare
    :: CAddrMode    	    -- result (boolean)
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- alloc hp + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpCompare res args@(chp, caa1,csa1,cda1, caa2,csa2,cda2)
  = let
	result	= amodeToStix res
	hp	= amodeToStix chp
	aa1	= amodeToStix caa1
	sa1	= amodeToStix csa1
	da1	= amodeToStix cda1
	aa2	= amodeToStix caa2
	sa2	= amodeToStix csa2
	da2	= amodeToStix cda2

    	argument1 = hp
    	argument2 = StIndex IntRep hp (StInt (toInteger mpIntSize))
    	(a1,a2,a3) = toStruct argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct argument2 (aa2,sa2,da2)
    	mpz_cmp = StCall SLIT("mpz_cmp") IntRep [argument1, argument2]
    	r1 = StAssign IntRep result mpz_cmp
    in
    returnUs (\xs -> a1 : a2 : a3 : a4 : a5 : a6 : r1 : xs)
\end{code}

See the comment above regarding the heap check (or lack thereof).

\begin{code}
gmpInteger2Int
    :: CAddrMode    	    -- result
    -> (CAddrMode, CAddrMode,CAddrMode,CAddrMode) -- alloc hp + argument (3 parts)
    -> UniqSM StixTreeList

gmpInteger2Int res args@(chp, caa,csa,cda)
  = let
	result	= amodeToStix res
	hp	= amodeToStix chp
	aa	= amodeToStix caa
	sa	= amodeToStix csa
	da	= amodeToStix cda

    	(a1,a2,a3) = toStruct hp (aa,sa,da)
    	mpz_get_si = StCall SLIT("mpz_get_si") IntRep [hp]
    	r1 = StAssign IntRep result mpz_get_si
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

gmpInteger2Word
    :: CAddrMode    	    -- result
    -> (CAddrMode, CAddrMode,CAddrMode,CAddrMode) -- alloc hp + argument (3 parts)
    -> UniqSM StixTreeList

gmpInteger2Word res args@(chp, caa,csa,cda)
  = let
	result	= amodeToStix res
	hp	= amodeToStix chp
	aa	= amodeToStix caa
	sa	= amodeToStix csa
	da	= amodeToStix cda

    	(a1,a2,a3) = toStruct hp (aa,sa,da)
    	mpz_get_ui = StCall SLIT("mpz_get_ui") IntRep [hp]
    	r1 = StAssign WordRep result mpz_get_ui
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

arrayOfData_info = sStLitLbl SLIT("ArrayOfData_info")

--------------
gmpInt2Integer
    :: (CAddrMode, CAddrMode, CAddrMode) -- result (3 parts)
    -> (CAddrMode, CAddrMode)	-- allocated heap, Int to convert
    -> UniqSM StixTreeList

gmpInt2Integer res@(car,csr,cdr) args@(chp, n)
  = getUniqLabelNCG			`thenUs` \ zlbl ->
    getUniqLabelNCG			`thenUs` \ nlbl ->
    getUniqLabelNCG			`thenUs` \ jlbl ->
    let
	ar  = amodeToStix car
	sr  = amodeToStix csr
	dr  = amodeToStix cdr
	hp  = amodeToStix chp
	i   = amodeToStix n

    	h1 = StAssign PtrRep (StInd PtrRep hp) arrayOfData_info
    	size = varHdrSizeInWords (DataRep 0) + mIN_MP_INT_SIZE
    	h2 = StAssign IntRep (StInd IntRep (StIndex IntRep hp (StInt 1)))
			      (StInt (toInteger size))
	cts = StInd IntRep (StIndex IntRep hp dataHS)
	test1 = StPrim IntEqOp [i, StInt 0]
	test2 = StPrim IntLtOp [i, StInt 0]
	cjmp1 = StCondJump zlbl test1
	cjmp2 = StCondJump nlbl test2
    	-- positive
	p1 = StAssign IntRep cts i
	p2 = StAssign IntRep sr (StInt 1)
	p3 = StJump (StCLbl jlbl)
    	-- negative
	n0 = StLabel nlbl
	n1 = StAssign IntRep cts (StPrim IntNegOp [i])
	n2 = StAssign IntRep sr (StInt (-1))
	n3 = StJump (StCLbl jlbl)
    	-- zero
	z0 = StLabel zlbl
	z1 = StAssign IntRep sr (StInt 0)
	-- everybody
	a0 = StLabel jlbl
	a1 = StAssign IntRep ar (StInt 1)
	a2 = StAssign PtrRep dr hp
    in
    returnUs (\xs ->
	case n of
	    CLit (MachInt c _) ->
		if c == 0 then     h1 : h2 : z1 : a1 : a2 : xs
		else if c > 0 then h1 : h2 : p1 : p2 : a1 : a2 : xs
		else               h1 : h2 : n1 : n2 : a1 : a2 : xs
	    _                ->    h1 : h2 : cjmp1 : cjmp2 : p1 : p2 : p3
				      : n0 : n1 : n2 : n3 : z0 : z1
				      : a0 : a1 : a2 : xs)

gmpString2Integer
    :: (CAddrMode, CAddrMode, CAddrMode)    -- result (3 parts)
    -> (CAddrMode, CAddrMode)		    -- liveness, string
    -> UniqSM StixTreeList

gmpString2Integer res@(car,csr,cdr) (liveness, str)
  = getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	ar = amodeToStix car
	sr = amodeToStix csr
	dr = amodeToStix cdr

    	len = case str of
    	    (CString s) -> _LENGTH_ s
    	    (CLit (MachStr s)) -> _LENGTH_ s
    	    _ -> panic "String2Integer"
    	space = len `quot` 8 + 17 + mpIntSize +
    	    varHdrSizeInWords (DataRep 0) + fixedHdrSizeInWords
    	oldHp = StIndex PtrRep stgHp (StInt (toInteger (-space)))
    	safeHp = saveLoc Hp
    	save = StAssign PtrRep safeHp oldHp
    	result = StIndex IntRep stgHpLim (StInt (toInteger (-mpIntSize)))
    	set_str = StCall SLIT("mpz_init_set_str") IntRep
    	    [result, amodeToStix str, StInt 10]
    	test = StPrim IntEqOp [set_str, StInt 0]
    	cjmp = StCondJump ulbl test
    	abort = StCall SLIT("abort") VoidRep []
    	join = StLabel ulbl
    	restore = StAssign PtrRep stgHp safeHp
    	(a1,a2,a3) = fromStruct result (ar,sr,dr)
    in
    macroCode HEAP_CHK [liveness, mkIntCLit space, mkIntCLit_0]
						    `thenUs` \ heap_chk ->

    returnUs (heap_chk .
	(\xs -> save : cjmp : abort : join : a1 : a2 : a3 : restore : xs))

mkIntCLit_0 = mkIntCLit 0 -- out here to avoid CAF (sigh)

encodeFloatingKind
    :: PrimRep
    -> CAddrMode  	-- result
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
		-- heap pointer for result, integer argument (3 parts), exponent
    -> UniqSM StixTreeList

encodeFloatingKind pk res args@(chp, caa,csa,cda, cexpon)
  = let
	result  = amodeToStix res
	hp	= amodeToStix chp
	aa	= amodeToStix caa
	sa	= amodeToStix csa
	da	= amodeToStix cda
	expon	= amodeToStix cexpon

	pk' = if sizeOf FloatRep == sizeOf DoubleRep
	      then DoubleRep
	      else pk
    	(a1,a2,a3) = toStruct hp (aa,sa,da)
    	fn = case pk' of
    	    FloatRep -> SLIT("__encodeFloat")
    	    DoubleRep -> SLIT("__encodeDouble")
    	    _ -> panic "encodeFloatingKind"
    	encode = StCall fn pk' [hp, expon]
    	r1 = StAssign pk' result encode
    in
    returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

decodeFloatingKind
    :: PrimRep
    -> (CAddrMode, CAddrMode, CAddrMode, CAddrMode)
			-- exponent result, integer result (3 parts)
    -> (CAddrMode, CAddrMode)
			-- heap pointer for exponent, floating argument
    -> UniqSM StixTreeList

decodeFloatingKind pk res@(cexponr,car,csr,cdr) args@(chp, carg)
  = let
	exponr	= amodeToStix cexponr
	ar	= amodeToStix car
	sr	= amodeToStix csr
	dr	= amodeToStix cdr
	hp	= amodeToStix chp
	arg	= amodeToStix carg

	pk' = if sizeOf FloatRep == sizeOf DoubleRep
	      then DoubleRep
	      else pk
	setup = StAssign PtrRep mpData_mantissa (StIndex IntRep hp (StInt 1))
    	fn = case pk' of
    	    FloatRep -> SLIT("__decodeFloat")
    	    DoubleRep -> SLIT("__decodeDouble")
    	    _ -> panic "decodeFloatingKind"
    	decode = StCall fn VoidRep [mantissa, hp, arg]
    	(a1,a2,a3) = fromStruct mantissa (ar,sr,dr)
    	a4 = StAssign IntRep exponr (StInd IntRep hp)
    in
    returnUs (\xs -> setup : decode : a1 : a2 : a3 : a4 : xs)

mantissa = mpStruct 1 -- out here to avoid CAF (sigh)
mpData_mantissa = mpData mantissa
\end{code}

Support for the Gnu GMP multi-precision package.

\begin{code}
mpIntSize = 3 :: Int

mpAlloc, mpSize, mpData :: StixTree -> StixTree
mpAlloc base = StInd IntRep base
mpSize base = StInd IntRep (StIndex IntRep base (StInt 1))
mpData base = StInd PtrRep (StIndex IntRep base (StInt 2))

mpSpace
    :: Int  	    	-- gmp structures needed
    -> Int  	    	-- number of results
    -> [StixTree]	-- sizes to add for estimating result size
    -> StixTree  	-- total space

mpSpace gmp res sizes
  = foldr sum (StPrim IntAddOp [fixed, hdrs]) sizes
  where
    sum x y = StPrim IntAddOp [StPrim IntAbsOp [x], y]
    fixed = StInt (toInteger (17 * res + gmp * mpIntSize))
    hdrs = StPrim IntMulOp [dataHS, StInt (toInteger res)]
\end{code}

We don't have a truly portable way of allocating local temporaries, so
we cheat and use space at the end of the heap.  (Thus, negative
offsets from HpLim are our temporaries.)  Note that you must have
performed a heap check which includes the space needed for these
temporaries before you use them.

\begin{code}
mpStruct :: Int -> StixTree
mpStruct n = StIndex IntRep stgHpLim (StInt (toInteger (-(n * mpIntSize))))

toStruct
    :: StixTree
    -> (StixTree, StixTree, StixTree)
    -> (StixTree, StixTree, StixTree)

toStruct str (alloc,size,arr)
  = let
    	f1 = StAssign IntRep (mpAlloc str) alloc
    	f2 = StAssign IntRep (mpSize str) size
    	f3 = StAssign PtrRep (mpData str) (StIndex PtrRep arr dataHS)
    in
    (f1, f2, f3)

fromStruct
    :: StixTree
    -> (StixTree, StixTree, StixTree)
    -> (StixTree, StixTree, StixTree)

fromStruct str (alloc,size,arr)
  = let
    	e1 = StAssign IntRep alloc (mpAlloc str)
    	e2 = StAssign IntRep size (mpSize str)
    	e3 = StAssign PtrRep arr (StIndex PtrRep (mpData str)
						 (StPrim IntNegOp [dataHS]))
    in
    (e1, e2, e3)
\end{code}

