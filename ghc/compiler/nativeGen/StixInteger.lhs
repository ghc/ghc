%
% (c) The AQUA Project, Glasgow University, 1993-1995
%

\begin{code}
#include "HsVersions.h"

module StixInteger (
	gmpTake1Return1, gmpTake2Return1, gmpTake2Return2,
	gmpCompare, gmpInteger2Int, gmpInt2Integer, gmpString2Integer,
	encodeFloatingKind, decodeFloatingKind
    ) where

IMPORT_Trace	-- ToDo: rm debugging

import AbsCSyn
import CgCompInfo   ( mIN_MP_INT_SIZE )
import MachDesc
import Pretty
import PrelInfo	    ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import SMRep	    ( SMRep(..), SMSpecRepKind, SMUpdateKind(..) )
import Stix
import UniqSupply
import Util

\end{code}

\begin{code}

gmpTake1Return1
    :: Target
    -> (CAddrMode,CAddrMode,CAddrMode)  -- result (3 parts)
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

-- hacking with Uncle Will:
#define target_STRICT target@(Target _ _ _ _ _ _ _ _)

gmpTake1Return1 target_STRICT res@(car,csr,cdr) rtn arg@(clive,caa,csa,cda) =
    let
	a2stix  = amodeToStix target
	data_hs = dataHS target

	ar	= a2stix car
	sr	= a2stix csr
	dr	= a2stix cdr
    	liveness= a2stix clive
	aa	= a2stix caa
	sa	= a2stix csa
	da	= a2stix cda

    	space = mpSpace data_hs 2 1 [sa]
    	oldHp = StIndex PtrRep stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc target Hp
    	save = StAssign PtrRep safeHp oldHp
    	(a1,a2,a3) = toStruct data_hs argument1 (aa,sa,da)
    	mpz_op = StCall rtn VoidRep [result2, argument1]
    	restore = StAssign PtrRep stgHp safeHp
    	(r1,r2,r3) = fromStruct data_hs result2 (ar,sr,dr)
    in
    	heapCheck target liveness space (StInt 0) `thenUs` \ heap_chk ->

    	returnUs (heap_chk .
    	    (\xs -> a1 : a2 : a3 : save : init2 : mpz_op : r1 : r2 : r3 : restore : xs))

gmpTake2Return1
    :: Target
    -> (CAddrMode,CAddrMode,CAddrMode)	-- result (3 parts)
    -> FAST_STRING    	    		-- function name
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
					-- liveness + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpTake2Return1 target_STRICT res@(car,csr,cdr) rtn args@(clive, caa1,csa1,cda1, caa2,csa2,cda2) =
    let
	a2stix  = amodeToStix target
	data_hs = dataHS target

	ar	= a2stix car
	sr	= a2stix csr
	dr	= a2stix cdr
    	liveness= a2stix clive
	aa1	= a2stix caa1
	sa1	= a2stix csa1
	da1	= a2stix cda1
	aa2	= a2stix caa2
	sa2	= a2stix csa2
	da2	= a2stix cda2

    	space = mpSpace data_hs 3 1 [sa1, sa2]
    	oldHp = StIndex PtrRep stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc target Hp
    	save = StAssign PtrRep safeHp oldHp
    	(a1,a2,a3) = toStruct data_hs argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct data_hs argument2 (aa2,sa2,da2)
    	mpz_op = StCall rtn VoidRep [result3, argument1, argument2]
    	restore = StAssign PtrRep stgHp safeHp
    	(r1,r2,r3) = fromStruct data_hs result3 (ar,sr,dr)
    in
    	heapCheck target liveness space (StInt 0) `thenUs` \ heap_chk ->

    	returnUs (heap_chk .
    	    (\xs -> a1 : a2 : a3 : a4 : a5 : a6
    	    	    	: save : init3 : mpz_op : r1 : r2 : r3 : restore : xs))

gmpTake2Return2
    :: Target
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- 2 results (3 parts each)
    -> FAST_STRING    	    -- function name
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- liveness + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpTake2Return2 target_STRICT res@(car1,csr1,cdr1, car2,csr2,cdr2)
		rtn args@(clive, caa1,csa1,cda1, caa2,csa2,cda2) =
    let
	a2stix  = amodeToStix target
	data_hs = dataHS target

	ar1	= a2stix car1
	sr1	= a2stix csr1
	dr1	= a2stix cdr1
	ar2	= a2stix car2
	sr2	= a2stix csr2
	dr2	= a2stix cdr2
    	liveness= a2stix clive
	aa1	= a2stix caa1
	sa1	= a2stix csa1
	da1	= a2stix cda1
	aa2	= a2stix caa2
	sa2	= a2stix csa2
	da2	= a2stix cda2

    	space = StPrim IntMulOp [mpSpace data_hs 2 1 [sa1, sa2], StInt 2]
    	oldHp = StIndex PtrRep stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc target Hp
    	save = StAssign PtrRep safeHp oldHp
    	(a1,a2,a3) = toStruct data_hs argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct data_hs argument2 (aa2,sa2,da2)
    	mpz_op = StCall rtn VoidRep [result3, result4, argument1, argument2]
    	restore = StAssign PtrRep stgHp safeHp
    	(r1,r2,r3) = fromStruct data_hs result3 (ar1,sr1,dr1)
    	(r4,r5,r6) = fromStruct data_hs result4 (ar2,sr2,dr2)

    in
    	heapCheck target liveness space (StInt 0) `thenUs` \ heap_chk ->

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
    :: Target
    -> CAddrMode    	    -- result (boolean)
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
    		  	    -- alloc hp + 2 arguments (3 parts each)
    -> UniqSM StixTreeList

gmpCompare target_STRICT res args@(chp, caa1,csa1,cda1, caa2,csa2,cda2) =
    let
	a2stix  = amodeToStix target
	data_hs = dataHS target

	result	= a2stix res
	hp	= a2stix chp
	aa1	= a2stix caa1
	sa1	= a2stix csa1
	da1	= a2stix cda1
	aa2	= a2stix caa2
	sa2	= a2stix csa2
	da2	= a2stix cda2

    	argument1 = hp
    	argument2 = StIndex IntRep hp (StInt (toInteger mpIntSize))
    	(a1,a2,a3) = toStruct data_hs argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct data_hs argument2 (aa2,sa2,da2)
    	mpz_cmp = StCall SLIT("mpz_cmp") IntRep [argument1, argument2]
    	r1 = StAssign IntRep result mpz_cmp
    in
    	returnUs (\xs -> a1 : a2 : a3 : a4 : a5 : a6 : r1 : xs)

\end{code}

See the comment above regarding the heap check (or lack thereof).

\begin{code}

gmpInteger2Int
    :: Target
    -> CAddrMode    	    -- result
    -> (CAddrMode, CAddrMode,CAddrMode,CAddrMode) -- alloc hp + argument (3 parts)
    -> UniqSM StixTreeList

gmpInteger2Int target_STRICT res args@(chp, caa,csa,cda) =
    let
	a2stix  = amodeToStix target
	data_hs = dataHS target

	result	= a2stix res
	hp	= a2stix chp
	aa	= a2stix caa
	sa	= a2stix csa
	da	= a2stix cda

    	(a1,a2,a3) = toStruct data_hs hp (aa,sa,da)
    	mpz_get_si = StCall SLIT("mpz_get_si") IntRep [hp]
    	r1 = StAssign IntRep result mpz_get_si
    in
    	returnUs (\xs -> a1 : a2 : a3 : r1 : xs)

arrayOfData_info = sStLitLbl SLIT("ArrayOfData_info")

gmpInt2Integer
    :: Target
    -> (CAddrMode, CAddrMode, CAddrMode) -- result (3 parts)
    -> (CAddrMode, CAddrMode)	-- allocated heap, Int to convert
    -> UniqSM StixTreeList

gmpInt2Integer target_STRICT res@(car,csr,cdr) args@(chp, n) =
    getUniqLabelNCG			`thenUs` \ zlbl ->
    getUniqLabelNCG			`thenUs` \ nlbl ->
    getUniqLabelNCG			`thenUs` \ jlbl ->
    let
	a2stix = amodeToStix target

	ar  = a2stix car
	sr  = a2stix csr
	dr  = a2stix cdr
	hp  = a2stix chp
	i   = a2stix n

    	h1 = StAssign PtrRep (StInd PtrRep hp) arrayOfData_info
    	size = varHeaderSize target (DataRep 0) + mIN_MP_INT_SIZE
    	h2 = StAssign IntRep (StInd IntRep (StIndex IntRep hp (StInt 1)))
			      (StInt (toInteger size))
	cts = StInd IntRep (StIndex IntRep hp (dataHS target))
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
    :: Target
    -> (CAddrMode, CAddrMode, CAddrMode)    -- result (3 parts)
    -> (CAddrMode, CAddrMode)		    -- liveness, string
    -> UniqSM StixTreeList

gmpString2Integer target_STRICT res@(car,csr,cdr) (liveness, str) =
    getUniqLabelNCG					`thenUs` \ ulbl ->
    let
	a2stix  = amodeToStix target
	data_hs = dataHS target

	ar = a2stix car
	sr = a2stix csr
	dr = a2stix cdr

    	len = case str of
    	    (CString s) -> _LENGTH_ s
    	    (CLit (MachStr s)) -> _LENGTH_ s
    	    _ -> panic "String2Integer"
    	space = len `quot` 8 + 17 + mpIntSize +
    	    varHeaderSize target (DataRep 0) + fixedHeaderSize target
    	oldHp = StIndex PtrRep stgHp (StInt (toInteger (-space)))
    	safeHp = saveLoc target Hp
    	save = StAssign PtrRep safeHp oldHp
    	result = StIndex IntRep stgHpLim (StInt (toInteger (-mpIntSize)))
    	set_str = StCall SLIT("mpz_init_set_str") IntRep
    	    [result, a2stix str, StInt 10]
    	test = StPrim IntEqOp [set_str, StInt 0]
    	cjmp = StCondJump ulbl test
    	abort = StCall SLIT("abort") VoidRep []
    	join = StLabel ulbl
    	restore = StAssign PtrRep stgHp safeHp
    	(a1,a2,a3) = fromStruct data_hs result (ar,sr,dr)
    in
    	macroCode target HEAP_CHK [liveness, mkIntCLit space, mkIntCLit_0]
    	    	    	    	    	    	    	`thenUs` \ heap_chk ->

    	returnUs (heap_chk .
    	    (\xs -> save : cjmp : abort : join : a1 : a2 : a3 : restore : xs))

mkIntCLit_0 = mkIntCLit 0 -- out here to avoid CAF (sigh)

encodeFloatingKind
    :: PrimRep
    -> Target
    -> CAddrMode  	-- result
    -> (CAddrMode,CAddrMode,CAddrMode,CAddrMode,CAddrMode)
		-- heap pointer for result, integer argument (3 parts), exponent
    -> UniqSM StixTreeList

encodeFloatingKind pk target_STRICT res args@(chp, caa,csa,cda, cexpon) =
    let
	a2stix  = amodeToStix target
	size_of = sizeof target
	data_hs = dataHS target

	result  = a2stix res
	hp	= a2stix chp
	aa	= a2stix caa
	sa	= a2stix csa
	da	= a2stix cda
	expon	= a2stix cexpon

	pk' = if size_of FloatRep == size_of DoubleRep
	      then DoubleRep
	      else pk
    	(a1,a2,a3) = toStruct data_hs hp (aa,sa,da)
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
    -> Target
    -> (CAddrMode, CAddrMode, CAddrMode, CAddrMode)
			-- exponent result, integer result (3 parts)
    -> (CAddrMode, CAddrMode)
			-- heap pointer for exponent, floating argument
    -> UniqSM StixTreeList

decodeFloatingKind pk target_STRICT res@(cexponr,car,csr,cdr) args@(chp, carg) =
    let
	a2stix  = amodeToStix target
	size_of = sizeof target
	data_hs = dataHS target

	exponr	= a2stix cexponr
	ar	= a2stix car
	sr	= a2stix csr
	dr	= a2stix cdr
	hp	= a2stix chp
	arg	= a2stix carg

	pk' = if size_of FloatRep == size_of DoubleRep
	      then DoubleRep
	      else pk
	setup = StAssign PtrRep mpData_mantissa (StIndex IntRep hp (StInt 1))
    	fn = case pk' of
    	    FloatRep -> SLIT("__decodeFloat")
    	    DoubleRep -> SLIT("__decodeDouble")
    	    _ -> panic "decodeFloatingKind"
    	decode = StCall fn VoidRep [mantissa, hp, arg]
    	(a1,a2,a3) = fromStruct data_hs mantissa (ar,sr,dr)
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
    :: StixTree		-- dataHs from Target
    -> Int  	    	-- gmp structures needed
    -> Int  	    	-- number of results
    -> [StixTree]	-- sizes to add for estimating result size
    -> StixTree  	-- total space

mpSpace data_hs gmp res sizes =
    foldr sum (StPrim IntAddOp [fixed, hdrs]) sizes
  where
    sum x y = StPrim IntAddOp [StPrim IntAbsOp [x], y]
    fixed = StInt (toInteger (17 * res + gmp * mpIntSize))
    hdrs = StPrim IntMulOp [data_hs, StInt (toInteger res)]

\end{code}

We don't have a truly portable way of allocating local temporaries, so we
cheat and use space at the end of the heap.  (Thus, negative offsets from
HpLim are our temporaries.)  Note that you must have performed a heap check
which includes the space needed for these temporaries before you use them.

\begin{code}
mpStruct :: Int -> StixTree
mpStruct n = StIndex IntRep stgHpLim (StInt (toInteger (-(n * mpIntSize))))

toStruct
    :: StixTree		-- dataHS, from Target
    -> StixTree
    -> (StixTree, StixTree, StixTree)
    -> (StixTree, StixTree, StixTree)

toStruct data_hs str (alloc,size,arr) =
    let
    	f1 = StAssign IntRep (mpAlloc str) alloc
    	f2 = StAssign IntRep (mpSize str) size
    	f3 = StAssign PtrRep (mpData str) (StIndex PtrRep arr data_hs)
    in
    	(f1, f2, f3)

fromStruct
    :: StixTree		-- dataHS, from Target
    -> StixTree
    -> (StixTree, StixTree, StixTree)
    -> (StixTree, StixTree, StixTree)

fromStruct data_hs str (alloc,size,arr) =
    let
    	e1 = StAssign IntRep alloc (mpAlloc str)
    	e2 = StAssign IntRep size (mpSize str)
    	e3 = StAssign PtrRep arr (StIndex PtrRep (mpData str)
    	    	    	    	    	    	   (StPrim IntNegOp [data_hs]))
    in
    	(e1, e2, e3)
\end{code}

