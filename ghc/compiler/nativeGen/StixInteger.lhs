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
import AbsPrel	    ( PrimOp(..)
		      IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
		    )
import SMRep	    ( SMRep(..), SMSpecRepKind, SMUpdateKind(..) )
import Stix
import SplitUniq
import Unique
import Util

\end{code}

\begin{code}

gmpTake1Return1 
    :: Target 
    -> [CAddrMode]  	    -- result (3 parts)
    -> FAST_STRING    	    -- function name
    -> [CAddrMode]  	    -- argument (3 parts)
    -> SUniqSM StixTreeList

argument1 = mpStruct 1 -- out here to avoid CAF (sigh)
argument2 = mpStruct 2
result2 = mpStruct 2
result3 = mpStruct 3
result4 = mpStruct 4
init2 = StCall SLIT("mpz_init") VoidKind [result2]
init3 = StCall SLIT("mpz_init") VoidKind [result3]
init4 = StCall SLIT("mpz_init") VoidKind [result4]

gmpTake1Return1 target res rtn arg =
    let	[ar,sr,dr] = map (amodeToStix target) res
    	[liveness, aa,sa,da] = map (amodeToStix target) arg
    	space = mpSpace target 2 1 [sa]
    	oldHp = StIndex PtrKind stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc target Hp
    	save = StAssign PtrKind safeHp oldHp
    	(a1,a2,a3) = toStruct target argument1 (aa,sa,da)
    	mpz_op = StCall rtn VoidKind [result2, argument1]
    	restore = StAssign PtrKind stgHp safeHp
    	(r1,r2,r3) = fromStruct target result2 (ar,sr,dr)
    in
    	heapCheck target liveness space (StInt 0)
    	    	    	    	    	    	    	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk . 
    	    (\xs -> a1 : a2 : a3 : save : init2 : mpz_op : r1 : r2 : r3 : restore : xs))

gmpTake2Return1 
    :: Target 
    -> [CAddrMode]  	    -- result (3 parts)
    -> FAST_STRING    	    -- function name
    -> [CAddrMode]  	    -- arguments (3 parts each)
    -> SUniqSM StixTreeList

gmpTake2Return1 target res rtn args =
    let	[ar,sr,dr] = map (amodeToStix target) res
    	[liveness, aa1,sa1,da1, aa2,sa2,da2] = map (amodeToStix target) args
    	space = mpSpace target 3 1 [sa1, sa2]
    	oldHp = StIndex PtrKind stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc target Hp
    	save = StAssign PtrKind safeHp oldHp
    	(a1,a2,a3) = toStruct target argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct target argument2 (aa2,sa2,da2)
    	mpz_op = StCall rtn VoidKind [result3, argument1, argument2]
    	restore = StAssign PtrKind stgHp safeHp
    	(r1,r2,r3) = fromStruct target result3 (ar,sr,dr)
    in
    	heapCheck target liveness space (StInt 0)
    	    	    	    	    	    	    	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk .
    	    (\xs -> a1 : a2 : a3 : a4 : a5 : a6 
    	    	    	: save : init3 : mpz_op : r1 : r2 : r3 : restore : xs))

gmpTake2Return2
    :: Target 
    -> [CAddrMode]  	    -- results (3 parts each)
    -> FAST_STRING    	    -- function name
    -> [CAddrMode]  	    -- arguments (3 parts each)
    -> SUniqSM StixTreeList

gmpTake2Return2 target res rtn args =
    let	[ar1,sr1,dr1, ar2,sr2,dr2] = map (amodeToStix target) res
    	[liveness, aa1,sa1,da1, aa2,sa2,da2] = map (amodeToStix target) args
    	space = StPrim IntMulOp [mpSpace target 2 1 [sa1, sa2], StInt 2]
    	oldHp = StIndex PtrKind stgHp (StPrim IntNegOp [space])
    	safeHp = saveLoc target Hp
    	save = StAssign PtrKind safeHp oldHp
    	(a1,a2,a3) = toStruct target argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct target argument2 (aa2,sa2,da2)
    	mpz_op = StCall rtn VoidKind [result3, result4, argument1, argument2]
    	restore = StAssign PtrKind stgHp safeHp
    	(r1,r2,r3) = fromStruct target result3 (ar1,sr1,dr1)
    	(r4,r5,r6) = fromStruct target result4 (ar2,sr2,dr2)

    in
    	heapCheck target liveness space (StInt 0)
    	    	    	    	    	    	    	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk .
    	    (\xs -> a1 : a2 : a3 : a4 : a5 : a6 
    	    	    	: save : init3 : init4 : mpz_op 
    	    	    	: r1 : r2 : r3 : r4 : r5 : r6 : restore : xs))

\end{code}

Although gmpCompare doesn't allocate space, it does temporarily use some
space just beyond the heap pointer.  This is safe, because the enclosing
routine has already guaranteed that this space will be available.  
(See ``primOpHeapRequired.'')

\begin{code}

gmpCompare 
    :: Target 
    -> CAddrMode    	    -- result (boolean)
    -> [CAddrMode]  	    -- arguments (3 parts each)
    -> SUniqSM StixTreeList

gmpCompare target res args =
    let	result = amodeToStix target res
    	[hp, aa1,sa1,da1, aa2,sa2,da2] = map (amodeToStix target) args
    	argument1 = hp
    	argument2 = StIndex IntKind hp (StInt (toInteger mpIntSize))
    	(a1,a2,a3) = toStruct target argument1 (aa1,sa1,da1)
    	(a4,a5,a6) = toStruct target argument2 (aa2,sa2,da2)
    	mpz_cmp = StCall SLIT("mpz_cmp") IntKind [argument1, argument2]
    	r1 = StAssign IntKind result mpz_cmp
    in
    	returnSUs (\xs -> a1 : a2 : a3 : a4 : a5 : a6 : r1 : xs)

\end{code}

See the comment above regarding the heap check (or lack thereof).

\begin{code}

gmpInteger2Int 
    :: Target 
    -> CAddrMode    	    -- result
    -> [CAddrMode]  	    -- argument (3 parts)
    -> SUniqSM StixTreeList

gmpInteger2Int target res args =
    let	result = amodeToStix target res
    	[hp, aa,sa,da] = map (amodeToStix target) args
    	(a1,a2,a3) = toStruct target hp (aa,sa,da)
    	mpz_get_si = StCall SLIT("mpz_get_si") IntKind [hp]
    	r1 = StAssign IntKind result mpz_get_si
    in
    	returnSUs (\xs -> a1 : a2 : a3 : r1 : xs)

arrayOfData_info = sStLitLbl SLIT("ArrayOfData_info")

gmpInt2Integer 
    :: Target 
    -> [CAddrMode]  	    -- result (3 parts)
    -> [CAddrMode]  	    -- allocated heap, int to convert
    -> SUniqSM StixTreeList

gmpInt2Integer target res args@[_, n] =
    getUniqLabelNCG					`thenSUs` \ zlbl ->
    getUniqLabelNCG					`thenSUs` \ nlbl ->
    getUniqLabelNCG					`thenSUs` \ jlbl ->
    let	[ar,sr,dr] = map (amodeToStix target) res
        [hp, i] = map (amodeToStix target) args
    	h1 = StAssign PtrKind (StInd PtrKind hp) arrayOfData_info
    	size = varHeaderSize target (DataRep 0) + mIN_MP_INT_SIZE
    	h2 = StAssign IntKind (StInd IntKind (StIndex IntKind hp (StInt 1)))
                              (StInt (toInteger size))
        cts = StInd IntKind (StIndex IntKind hp (dataHS target))
        test1 = StPrim IntEqOp [i, StInt 0]
        test2 = StPrim IntLtOp [i, StInt 0]
        cjmp1 = StCondJump zlbl test1
        cjmp2 = StCondJump nlbl test2
    	-- positive
        p1 = StAssign IntKind cts i
        p2 = StAssign IntKind sr (StInt 1)
        p3 = StJump (StCLbl jlbl)
    	-- negative
        n0 = StLabel nlbl
        n1 = StAssign IntKind cts (StPrim IntNegOp [i])
        n2 = StAssign IntKind sr (StInt (-1))
        n3 = StJump (StCLbl jlbl)
    	-- zero
        z0 = StLabel zlbl
        z1 = StAssign IntKind sr (StInt 0)
        -- everybody
        a0 = StLabel jlbl
        a1 = StAssign IntKind ar (StInt 1)
        a2 = StAssign PtrKind dr hp
    in
    	returnSUs (\xs -> 
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
    -> [CAddrMode]  	    -- result (3 parts)
    -> [CAddrMode]  	    -- liveness, string
    -> SUniqSM StixTreeList

gmpString2Integer target res [liveness, str] =
    getUniqLabelNCG					`thenSUs` \ ulbl ->
    let	[ar,sr,dr] = map (amodeToStix target) res
    	len = case str of
    	    (CString s) -> _LENGTH_ s
    	    (CLit (MachStr s)) -> _LENGTH_ s
    	    _ -> panic "String2Integer"
    	space = len `quot` 8 + 17 + mpIntSize +
    	    varHeaderSize target (DataRep 0) + fixedHeaderSize target
    	oldHp = StIndex PtrKind stgHp (StInt (toInteger (-space)))
    	safeHp = saveLoc target Hp
    	save = StAssign PtrKind safeHp oldHp
    	result = StIndex IntKind stgHpLim (StInt (toInteger (-mpIntSize)))
    	set_str = StCall SLIT("mpz_init_set_str") IntKind
    	    [result, amodeToStix target str, StInt 10]
    	test = StPrim IntEqOp [set_str, StInt 0]
    	cjmp = StCondJump ulbl test
    	abort = StCall SLIT("abort") VoidKind []
    	join = StLabel ulbl
    	restore = StAssign PtrKind stgHp safeHp
    	(a1,a2,a3) = fromStruct target result (ar,sr,dr)
    in
    	macroCode target HEAP_CHK [liveness, mkIntCLit space, mkIntCLit_0]
    	    	    	    	    	    	    	`thenSUs` \ heap_chk ->

    	returnSUs (heap_chk .
    	    (\xs -> save : cjmp : abort : join : a1 : a2 : a3 : restore : xs))

mkIntCLit_0 = mkIntCLit 0 -- out here to avoid CAF (sigh)

encodeFloatingKind 
    :: PrimKind 
    -> Target 
    -> [CAddrMode]  	-- result
    -> [CAddrMode]  	-- heap pointer for result, integer argument (3 parts), exponent
    -> SUniqSM StixTreeList

encodeFloatingKind pk target [res] args =
    let	result = amodeToStix target res
    	[hp, aa,sa,da, expon] = map (amodeToStix target) args
        pk' = if sizeof target FloatKind == sizeof target DoubleKind then DoubleKind
              else pk
    	(a1,a2,a3) = toStruct target hp (aa,sa,da)
    	fn = case pk' of
    	    FloatKind -> SLIT("__encodeFloat")
    	    DoubleKind -> SLIT("__encodeDouble")
    	    _ -> panic "encodeFloatingKind"
    	encode = StCall fn pk' [hp, expon]
    	r1 = StAssign pk' result encode
    in
    	returnSUs (\xs -> a1 : a2 : a3 : r1 : xs)

decodeFloatingKind 
    :: PrimKind 
    -> Target 
    -> [CAddrMode]  	    -- exponent result, integer result (3 parts)
    -> [CAddrMode]  	    -- heap pointer for exponent, floating argument
    -> SUniqSM StixTreeList

decodeFloatingKind pk target res args =
    let	[exponr,ar,sr,dr] = map (amodeToStix target) res
        [hp, arg] = map (amodeToStix target) args
        pk' = if sizeof target FloatKind == sizeof target DoubleKind then DoubleKind
              else pk
        setup = StAssign PtrKind mpData_mantissa (StIndex IntKind hp (StInt 1))
    	fn = case pk' of
    	    FloatKind -> SLIT("__decodeFloat")
    	    DoubleKind -> SLIT("__decodeDouble")
    	    _ -> panic "decodeFloatingKind"
    	decode = StCall fn VoidKind [mantissa, hp, arg]
    	(a1,a2,a3) = fromStruct target mantissa (ar,sr,dr)
    	a4 = StAssign IntKind exponr (StInd IntKind hp)
    in
    	returnSUs (\xs -> setup : decode : a1 : a2 : a3 : a4 : xs)

mantissa = mpStruct 1 -- out here to avoid CAF (sigh)
mpData_mantissa = mpData mantissa
\end{code}

Support for the Gnu GMP multi-precision package.

\begin{code}

mpIntSize = 3 :: Int

mpAlloc, mpSize, mpData :: StixTree -> StixTree
mpAlloc base = StInd IntKind base
mpSize base = StInd IntKind (StIndex IntKind base (StInt 1))
mpData base = StInd PtrKind (StIndex IntKind base (StInt 2))

mpSpace 
    :: Target
    -> Int  	    	-- gmp structures needed
    -> Int  	    	-- number of results
    -> [StixTree]	-- sizes to add for estimating result size
    -> StixTree  	-- total space

mpSpace target gmp res sizes = 
    foldr sum (StPrim IntAddOp [fixed, hdrs]) sizes
  where
    sum x y = StPrim IntAddOp [StPrim IntAbsOp [x], y]
    fixed = StInt (toInteger (17 * res + gmp * mpIntSize))
    hdrs = StPrim IntMulOp [dataHS target, StInt (toInteger res)]

\end{code}

We don't have a truly portable way of allocating local temporaries, so we
cheat and use space at the end of the heap.  (Thus, negative offsets from
HpLim are our temporaries.)  Note that you must have performed a heap check
which includes the space needed for these temporaries before you use them.

\begin{code}

mpStruct :: Int -> StixTree
mpStruct n = StIndex IntKind stgHpLim (StInt (toInteger (-(n * mpIntSize))))

toStruct 
    :: Target
    -> StixTree 
    -> (StixTree, StixTree, StixTree) 
    -> (StixTree, StixTree, StixTree) 

toStruct target str (alloc,size,arr) =
    let
    	f1 = StAssign IntKind (mpAlloc str) alloc
    	f2 = StAssign IntKind (mpSize str) size
    	f3 = StAssign PtrKind (mpData str) (StIndex PtrKind arr (dataHS target))
    in
    	(f1, f2, f3)

fromStruct 
    :: Target
    -> StixTree 
    -> (StixTree, StixTree, StixTree) 
    -> (StixTree, StixTree, StixTree) 

fromStruct target str (alloc,size,arr) =
    let
    	e1 = StAssign IntKind alloc (mpAlloc str)
    	e2 = StAssign IntKind size (mpSize str)
    	e3 = StAssign PtrKind arr (StIndex PtrKind (mpData str) 
    	    	    	    	    	    	   (StPrim IntNegOp [dataHS target]))
    in
    	(e1, e2, e3)


\end{code}

