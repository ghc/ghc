%
% (c) The GRASP/AQUA Project, Glasgow University, 1994-1996
%     Hans Wolfgang Loidl
%
% ---------------------------------------------------------------------------

\section[Costs]{Evaluating the costs of computing some abstract C code}

This module   provides all necessary  functions for   computing for a given
abstract~C Program the costs of executing that program. This is done by the
exported function:

\begin{quote}
 {\verb type CostRes = (Int, Int, Int, Int, Int)}
 {\verb costs :: AbstractC -> CostRes }
\end{quote}

The meaning of the result tuple is:
\begin{itemize}
 \item The first component ({\tt i}) counts the number of integer,
   arithmetic and bit-manipulating instructions.
 \item The second component ({\tt b}) counts the number of branches (direct
   branches as well as indirect ones).
 \item The third component ({\tt l}) counts the number of load instructions.
 \item The fourth component ({\tt s}) counts the number of store
   instructions.
 \item The fifth component ({\tt f}) counts the number of floating point
   instructions.
\end{itemize}

This function is needed in GrAnSim for parallelism.

These are first suggestions for scaling the costs. But, this scaling should be done in the RTS rather than the compiler (this really should be tunable!):

\begin{pseudocode}

#define LOAD_COSTS		2
#define STORE_COSTS		2
#define INT_ARITHM_COSTS	1
#define GMP_ARITHM_COSTS	3 {- any clue for GMP costs ? -}
#define FLOAT_ARITHM_COSTS	3 {- any clue for float costs ? -}
#define BRANCH_COSTS		2

\end{pseudocode}

\begin{code}
#define ACCUM_COSTS(i,b,l,s,f)	(i+b+l+s+f)

#define NUM_REGS		10 {- PprAbsCSyn.lhs -}	      {- runtime/c-as-asm/CallWrap_C.lc -}
#define RESTORE_COSTS		(Cost (0, 0, NUM_REGS, 0, 0)  :: CostRes)
#define SAVE_COSTS		(Cost (0, 0, 0, NUM_REGS, 0)  :: CostRes)
#define CCALL_COSTS_GUESS	(Cost (50, 0, 0, 0, 0)	      :: CostRes)

module Costs( costs,
	      addrModeCosts, CostRes(Cost), nullCosts, Side(..)
    ) where

#include "HsVersions.h"

import AbsCSyn
import PrimOp		( primOpNeedsWrapper, PrimOp(..) )
import Util		( trace )

-- --------------------------------------------------------------------------
newtype CostRes = Cost (Int, Int, Int, Int, Int)
	       deriving (Text)

nullCosts    = Cost (0, 0, 0, 0, 0) :: CostRes
initHdrCosts = Cost (2, 0, 0, 1, 0) :: CostRes
errorCosts   = Cost (-1, -1, -1, -1, -1)  -- just for debugging

oneArithm = Cost (1, 0, 0, 0, 0) :: CostRes

instance Eq CostRes where
 (==) t1 t2 = i && b && l && s && f
	     where (i,b,l,s,f) = binOp' (==) t1 t2

instance Num CostRes where
 (+) = binOp (+)
 (-) = binOp (-)
 (*) = binOp (*)
 negate	 = mapOp negate
 abs	 = mapOp abs
 signum	 = mapOp signum

mapOp :: (Int -> Int) -> CostRes -> CostRes
mapOp g ( Cost (i, b, l, s, f) )  = Cost (g i, g b, g l, g s, g f)

foldrOp :: (Int -> a -> a) -> a -> CostRes -> a
foldrOp o x  ( Cost (i1, b1, l1, s1, f1) )   =
	i1 `o` ( b1 `o` ( l1 `o` ( s1 `o` ( f1 `o` x))))

binOp :: (Int -> Int -> Int) -> CostRes -> CostRes -> CostRes
binOp o ( Cost (i1, b1, l1, s1, f1) ) ( Cost  (i2, b2, l2, s2, f2) )  =
	( Cost (i1 `o` i2, b1 `o` b2, l1 `o` l2, s1 `o` s2, f1 `o` f2) )

binOp' :: (Int -> Int -> a) -> CostRes -> CostRes -> (a,a,a,a,a)
binOp' o ( Cost (i1, b1, l1, s1, f1) ) ( Cost  (i2, b2, l2, s2, f2) )  =
	 (i1 `o` i2, b1 `o` b2, l1 `o` l2, s1 `o` s2, f1 `o` f2)

-- --------------------------------------------------------------------------

data Side = Lhs | Rhs
	    deriving (Eq)

-- --------------------------------------------------------------------------

costs :: AbstractC -> CostRes

costs absC =
  case absC of
   AbsCNop			->  nullCosts

   AbsCStmts absC1 absC2	-> costs absC1 + costs absC2

   CAssign (CReg _) (CReg _)	-> Cost (1,0,0,0,0)   -- typ.: mov %reg1,%reg2

   CAssign (CReg _) (CTemp _ _) -> Cost (1,0,0,0,0)

   CAssign (CReg _) (CAddr _)	-> Cost (1,0,0,0,0)  -- typ.: add %reg1,<adr>,%reg2

   CAssign target_m source_m	-> addrModeCosts target_m Lhs +
				   addrModeCosts source_m Rhs

   CJump (CLbl _  _)		-> Cost (0,1,0,0,0)  -- no ld for call necessary

   CJump mode			-> addrModeCosts mode Rhs +
				   Cost (0,1,0,0,0)

   CFallThrough mode  -> addrModeCosts mode Rhs +		-- chu' 0.24
			 Cost (0,1,0,0,0)

   CReturn mode info  -> case info of
			  DirectReturn -> addrModeCosts mode Rhs +
					  Cost (0,1,0,0,0)

			    -- i.e. ld address to reg and call reg

			  DynamicVectoredReturn mode' ->
					addrModeCosts mode Rhs +
					addrModeCosts mode' Rhs +
					Cost (0,1,1,0,0)

			    {- generates code like this:
				JMP_(<mode>)[RVREL(<mode'>)];
			       i.e. 1 possb ld for mode'
				    1 ld for RVREL
				    1 possb ld for mode
				    1 call				-}

			  StaticVectoredReturn _ -> addrModeCosts mode Rhs +
						  Cost (0,1,1,0,0)

			    -- as above with mode' fixed to CLit
			    -- typically 2 ld + 1 call; 1st ld due
			    -- to CVal as mode

   CSwitch mode alts absC     -> nullCosts
				 {- for handling costs of all branches of
				    a CSwitch see PprAbsC.
				    Basically:
				     Costs for branch =
					Costs before CSwitch +
					addrModeCosts of head +
					Costs for 1 cond branch +
					Costs for body of branch
				 -}

   CCodeBlock _ absC	      -> costs absC

   CInitHdr cl_info reg_rel cost_centre inplace_upd -> initHdrCosts

			{- This is more fancy but superflous: The addr modes
			   are fixed and so the costs are const!

			argCosts + initHdrCosts
			where argCosts = addrModeCosts (CAddr reg_rel) Rhs +
					 addrModeCosts base_lbl +    -- CLbl!
					 3*addrModeCosts (mkIntCLit 1{- any val -})
			-}
			{- this extends to something like
			    SET_SPEC_HDR(...)
			   For costing the args of this macro
			   see PprAbsC.lhs where args are inserted -}

   COpStmt modes_res primOp modes_args _ _ ->
	{-
	   let
		n = length modes_res
	   in
		(0, 0, n, n, 0) +
		primOpCosts primOp +
		if primOpNeedsWrapper primOp then SAVE_COSTS + RESTORE_COSTS
					     else nullCosts
	   -- ^^HWL
	-}
	foldl (+) nullCosts [addrModeCosts mode Lhs | mode <- modes_res]  +
	foldl (+) nullCosts [addrModeCosts mode Rhs | mode <- modes_args]  +
	primOpCosts primOp +
	if primOpNeedsWrapper primOp then SAVE_COSTS + RESTORE_COSTS
				     else nullCosts

   CSimultaneous absC	     -> costs absC

   CMacroStmt	macro modes  -> stmtMacroCosts macro modes

   CCallProfCtrMacro   _ _   -> nullCosts
				  {- we don't count profiling in GrAnSim -}

   CCallProfCCMacro    _ _   -> nullCosts
				  {- we don't count profiling in GrAnSim -}

  -- *** the next three [or so...] are DATA (those above are CODE) ***
  -- as they are data rather than code they all have nullCosts	       -- HWL

   CStaticClosure _ _ _ _    -> nullCosts

   CClosureInfoAndCode _ _ _ _ _ _ -> nullCosts

   CRetVector _ _ _	     -> nullCosts

   CRetUnVector _ _	     -> nullCosts

   CFlatRetVector _ _	     -> nullCosts

   CCostCentreDecl _ _	     -> nullCosts

   CClosureUpdInfo _	     -> nullCosts

   CSplitMarker		     -> nullCosts

-- ---------------------------------------------------------------------------

addrModeCosts :: CAddrMode -> Side -> CostRes

-- addrModeCosts _ _ = nullCosts

addrModeCosts addr_mode side =
  let
    lhs = side == Lhs
  in
  case addr_mode of
    CVal _ _ -> if lhs then Cost (0, 0, 0, 1, 0)
		       else Cost (0, 0, 1, 0, 0)

    CAddr _  -> if lhs then Cost (0, 0, 0, 1, 0)  -- ??unchecked
		       else Cost (0, 0, 1, 0, 0)

    CReg _   -> nullCosts	 {- loading from, storing to reg is free ! -}
				 {- for costing CReg->Creg ops see special -}
				 {- case in costs fct -}
    CTableEntry base_mode offset_mode kind ->
		addrModeCosts base_mode side +
		addrModeCosts offset_mode side +
		Cost (1,0,1,0,0)

    CTemp _ _  -> nullCosts	{- if lhs then Cost (0, 0, 0, 1, 0)
					  else Cost (0, 0, 1, 0, 0)  -}
	-- ``Temporaries'' correspond to local variables in C, and registers in
	-- native code.
	-- I assume they can be somewhat optimized by gcc -- HWL

    CLbl _ _   -> if lhs then Cost (0, 0, 0, 1, 0)
			 else Cost (2, 0, 0, 0, 0)
		  -- Rhs: typically: sethi %hi(lbl),%tmp_reg
		  --		     or	   %tmp_reg,%lo(lbl),%target_reg

    CUnVecLbl _ _ -> if lhs then Cost (0, 0, 0, 1, 0)
			    else Cost (2, 0, 0, 0, 0)
		     -- same as CLbl

    --	Check the following 3 (checked form CLit on)

    CCharLike mode -> if lhs then Cost (0, 0, 0, 1, 0)
			     else Cost (0, 0, 1, 0, 0)

    CIntLike mode  -> if lhs then Cost (0, 0, 0, 1, 0)
			     else Cost (0, 0, 1, 0, 0)

    CString _	   -> if lhs then Cost (0, 0, 0, 1, 0)
			     else Cost (0, 0, 1, 0, 0)

    CLit    _	   -> if lhs then nullCosts	       -- should never occur
			     else Cost (1, 0, 0, 0, 0) -- typ.: mov lit,%reg

    CLitLit _  _   -> if lhs then nullCosts
			     else Cost (1, 0, 0, 0, 0)
		      -- same es CLit

    COffset _	   -> if lhs then nullCosts
			     else Cost (1, 0, 0, 0, 0)
		      -- same es CLit

    CCode absC	   -> costs absC

    CLabelledCode _ absC  ->  costs absC

    CJoinPoint _ _	  -> if lhs then Cost (0, 0, 0, 1, 0)
				    else Cost (0, 0, 1, 0, 0)

    CMacroExpr _ macro mode_list -> exprMacroCosts side macro mode_list

    CCostCentre _ _ -> nullCosts

-- ---------------------------------------------------------------------------

exprMacroCosts :: Side -> CExprMacro -> [CAddrMode] -> CostRes

exprMacroCosts side macro mode_list =
  let
    arg_costs = foldl (+) nullCosts
		      (map (\ x -> addrModeCosts x Rhs) mode_list)
  in
  arg_costs +
  case macro of
    INFO_PTR   -> if side == Lhs then Cost (0, 0, 0, 1, 0)
				 else Cost (0, 0, 1, 0, 0)
    ENTRY_CODE -> nullCosts
    INFO_TAG   -> if side == Lhs then Cost (0, 0, 0, 1, 0)
				 else Cost (0, 0, 1, 0, 0)
    EVAL_TAG   -> if side == Lhs then Cost (1, 0, 0, 1, 0)
				 else Cost (1, 0, 1, 0, 0)
		  -- costs of INFO_TAG + (1,0,0,0,0)

-- ---------------------------------------------------------------------------

stmtMacroCosts :: CStmtMacro -> [CAddrMode] -> CostRes

stmtMacroCosts macro modes =
  let
    arg_costs =	  foldl (+) nullCosts
			[addrModeCosts mode Rhs | mode <- modes]
  in
  case macro of
    ARGS_CHK_A_LOAD_NODE  ->  Cost (2, 1, 0, 0, 0)	 {- StgMacros.lh  -}
		-- p=probability of PAP (instead of AP): + p*(3,1,0,0,0)
    ARGS_CHK_A		  ->  Cost (2, 1, 0, 0, 0)	 {- StgMacros.lh  -}
		-- p=probability of PAP (instead of AP): + p*(0,1,0,0,0)
    ARGS_CHK_B_LOAD_NODE  ->  Cost (2, 1, 0, 0, 0)	 {- StgMacros.lh  -}
    ARGS_CHK_B		  ->  Cost (2, 1, 0, 0, 0)	 {- StgMacros.lh  -}
    HEAP_CHK		  ->  Cost (2, 1, 0, 0, 0)	 {- StgMacros.lh  -}
    -- STK_CHK		     ->	 (2, 1, 0, 0, 0)       {- StgMacros.lh	-}
    STK_CHK		  ->  Cost (0, 0, 0, 0, 0)	 {- StgMacros.lh  -}
    UPD_CAF		  ->  Cost (7, 0, 1, 3, 0)	 {- SMupdate.lh	 -}
    UPD_IND		  ->  Cost (8, 2, 2, 0, 0)	 {- SMupdate.lh
				updatee in old-gen: Cost (4, 1, 1, 0, 0)
				updatee in new-gen: Cost (4, 1, 1, 0, 0)
				NB: we include costs fo checking if there is
				    a BQ, but we omit costs for awakening BQ
				    (these probably differ between old-gen and
				    new gen) -}
    UPD_INPLACE_NOPTRS	  ->  Cost (13, 3, 3, 2, 0)	  {- SMupdate.lh
				common for both:    Cost (4, 1, 1, 0, 0)
				updatee in old-gen: Cost (14, 3, 2, 4, 0)
				updatee in new-gen: Cost (4, 1, 1, 0, 0)   -}
    UPD_INPLACE_PTRS	  ->  Cost (13, 3, 3, 2, 0)	  {- SMupdate.lh
				common for both:    Cost (4, 1, 1, 0, 0)
				updatee in old-gen: Cost (14, 3, 2, 4, 0)
				updatee in new-gen: Cost (4, 1, 1, 0, 0)   -}

    UPD_BH_UPDATABLE	  ->  Cost (3, 0, 0, 1, 0)	 {- SMupdate.lh	 -}
    UPD_BH_SINGLE_ENTRY	  ->  Cost (3, 0, 0, 1, 0)	 {- SMupdate.lh	 -}
    PUSH_STD_UPD_FRAME	  ->  Cost (3, 0, 0, 4, 0)	 {- SMupdate.lh	 -}
    POP_STD_UPD_FRAME	  ->  Cost (1, 0, 3, 0, 0)	 {- SMupdate.lh	 -}
    SET_TAG		  ->  nullCosts		    {- COptRegs.lh -}
    GRAN_FETCH			->  nullCosts	  {- GrAnSim bookkeeping -}
    GRAN_RESCHEDULE		->  nullCosts	  {- GrAnSim bookkeeping -}
    GRAN_FETCH_AND_RESCHEDULE	->  nullCosts	  {- GrAnSim bookkeeping -}
    GRAN_YIELD		        ->  nullCosts	  {- GrAnSim bookkeeping -- added SOF -}
    THREAD_CONTEXT_SWITCH	->  nullCosts	  {- GrAnSim bookkeeping -}
    _ -> trace ("Costs.stmtMacroCosts: "++show macro) nullCosts

-- ---------------------------------------------------------------------------

floatOps :: [PrimOp]
floatOps =
  [   FloatGtOp	 , FloatGeOp  , FloatEqOp  , FloatNeOp	, FloatLtOp  , FloatLeOp
    , DoubleGtOp , DoubleGeOp , DoubleEqOp , DoubleNeOp , DoubleLtOp , DoubleLeOp
    , FloatAddOp , FloatSubOp , FloatMulOp , FloatDivOp , FloatNegOp
    , Float2IntOp , Int2FloatOp
    , FloatExpOp   , FloatLogOp	  , FloatSqrtOp
    , FloatSinOp   , FloatCosOp	  , FloatTanOp
    , FloatAsinOp  , FloatAcosOp  , FloatAtanOp
    , FloatSinhOp  , FloatCoshOp  , FloatTanhOp
    , FloatPowerOp
    , DoubleAddOp , DoubleSubOp , DoubleMulOp , DoubleDivOp , DoubleNegOp
    , Double2IntOp , Int2DoubleOp
    , Double2FloatOp , Float2DoubleOp
    , DoubleExpOp   , DoubleLogOp   , DoubleSqrtOp
    , DoubleSinOp   , DoubleCosOp   , DoubleTanOp
    , DoubleAsinOp  , DoubleAcosOp  , DoubleAtanOp
    , DoubleSinhOp  , DoubleCoshOp  , DoubleTanhOp
    , DoublePowerOp
    , FloatEncodeOp  , FloatDecodeOp
    , DoubleEncodeOp , DoubleDecodeOp
  ]

gmpOps :: [PrimOp]
gmpOps	=
  [   IntegerAddOp , IntegerSubOp , IntegerMulOp
    , IntegerQuotRemOp , IntegerDivModOp , IntegerNegOp
    , IntegerCmpOp
    , Integer2IntOp  , Int2IntegerOp
    , Addr2IntegerOp
  ]


abs_costs = nullCosts   -- NB:  This is normal STG code with costs already 
			--	included; no need to add costs again.

umul_costs = Cost (21,4,0,0,0)	   -- due to spy counts
rem_costs =  Cost (30,15,0,0,0)	   -- due to spy counts
div_costs =  Cost (30,15,0,0,0)	   -- due to spy counts

primOpCosts :: PrimOp -> CostRes

-- Special cases

primOpCosts (CCallOp _ _ _ _ _ _) = SAVE_COSTS + RESTORE_COSTS  	
	                          -- don't guess costs of ccall proper
                                  -- for exact costing use a GRAN_EXEC
                                  -- in the C code

-- Usually 3 mov instructions are needed to get args and res in right place.

primOpCosts IntMulOp  = Cost (3, 1, 0, 0, 0)  + umul_costs
primOpCosts IntQuotOp = Cost (3, 1, 0, 0, 0)  + div_costs
primOpCosts IntRemOp  = Cost (3, 1, 0, 0, 0)  + rem_costs
primOpCosts IntNegOp  = Cost (1, 1, 0, 0, 0) -- translates into 1 sub
primOpCosts IntAbsOp  = Cost (0, 1, 0, 0, 0) -- abs closure already costed

primOpCosts FloatGtOp  = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts FloatGeOp  = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts FloatEqOp  = Cost (0, 0, 0, 0, 2) -- cheap f-comp
primOpCosts FloatNeOp  = Cost (0, 0, 0, 0, 2) -- cheap f-comp
primOpCosts FloatLtOp  = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts FloatLeOp  = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts DoubleGtOp = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts DoubleGeOp = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts DoubleEqOp = Cost (0, 0, 0, 0, 2) -- cheap f-comp
primOpCosts DoubleNeOp = Cost (0, 0, 0, 0, 2) -- cheap f-comp
primOpCosts DoubleLtOp = Cost (2, 0, 0, 0, 2) -- expensive f-comp
primOpCosts DoubleLeOp = Cost (2, 0, 0, 0, 2) -- expensive f-comp

primOpCosts FloatExpOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatLogOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatSqrtOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatSinOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatCosOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatTanOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatAsinOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatAcosOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatAtanOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatSinhOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatCoshOp	  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatTanhOp	  = Cost (2, 1, 4, 4, 3)
--primOpCosts FloatAsinhOp  = Cost (2, 1, 4, 4, 3)
--primOpCosts FloatAcoshOp  = Cost (2, 1, 4, 4, 3)
--primOpCosts FloatAtanhOp  = Cost (2, 1, 4, 4, 3)
primOpCosts FloatPowerOp  = Cost (2, 1, 4, 4, 3)

{- There should be special handling of the Array PrimOps in here   HWL -}

primOpCosts primOp
  | primOp `elem` floatOps = Cost (0, 0, 0, 0, 1)  :: CostRes
  | primOp `elem` gmpOps   = Cost (30, 5, 10, 10, 0) :: CostRes  -- GUESS; check it
  | otherwise		   = Cost (1, 0, 0, 0, 0)

-- ---------------------------------------------------------------------------
{- HWL: currently unused

costsByKind :: PrimRep -> Side -> CostRes

-- The following PrimKinds say that the data is already in a reg

costsByKind CharRep	_ = nullCosts
costsByKind IntRep	_ = nullCosts
costsByKind WordRep	_ = nullCosts
costsByKind AddrRep	_ = nullCosts
costsByKind FloatRep	_ = nullCosts
costsByKind DoubleRep	_ = nullCosts
-}
-- ---------------------------------------------------------------------------
\end{code}

This is the data structure of {\tt PrimOp} copied from prelude/PrimOp.lhs.
I include here some comments about the estimated costs for these @PrimOps@.
Compare with the @primOpCosts@ fct above.  -- HWL

\begin{pseudocode}
data PrimOp
    -- I assume all these basic comparisons take just one ALU instruction
    -- Checked that for Char, Int; Word, Addr should be the same as Int.

    = CharGtOp	 | CharGeOp   | CharEqOp   | CharNeOp	| CharLtOp   | CharLeOp
    | IntGtOp	 | IntGeOp    | IntEqOp	   | IntNeOp	| IntLtOp    | IntLeOp
    | WordGtOp	 | WordGeOp   | WordEqOp   | WordNeOp	| WordLtOp   | WordLeOp
    | AddrGtOp	 | AddrGeOp   | AddrEqOp   | AddrNeOp	| AddrLtOp   | AddrLeOp

    -- Analogously, these take one FP unit instruction
    -- Haven't checked that, yet.

    | FloatGtOp	 | FloatGeOp  | FloatEqOp  | FloatNeOp	| FloatLtOp  | FloatLeOp
    | DoubleGtOp | DoubleGeOp | DoubleEqOp | DoubleNeOp | DoubleLtOp | DoubleLeOp

    -- 1 ALU op; unchecked
    | OrdOp | ChrOp

    -- these just take 1 ALU op; checked
    | IntAddOp | IntSubOp

    -- but these take more than that; see special cases in primOpCosts
    -- I counted the generated ass. instructions for these -> checked
    | IntMulOp | IntQuotOp
    | IntRemOp | IntNegOp | IntAbsOp

    -- Rest is unchecked so far -- HWL

    -- Word#-related ops:
    | AndOp   | OrOp  | NotOp | XorOp | ShiftLOp | ShiftROp
    | Int2WordOp | Word2IntOp -- casts

    -- Addr#-related ops:
    | Int2AddrOp | Addr2IntOp -- casts

    -- Float#-related ops:
    | FloatAddOp | FloatSubOp | FloatMulOp | FloatDivOp | FloatNegOp
    | Float2IntOp | Int2FloatOp

    | FloatExpOp   | FloatLogOp	  | FloatSqrtOp
    | FloatSinOp   | FloatCosOp	  | FloatTanOp
    | FloatAsinOp  | FloatAcosOp  | FloatAtanOp
    | FloatSinhOp  | FloatCoshOp  | FloatTanhOp
    -- not all machines have these available conveniently:
    -- | FloatAsinhOp | FloatAcoshOp | FloatAtanhOp
    | FloatPowerOp -- ** op

    -- Double#-related ops:
    | DoubleAddOp | DoubleSubOp | DoubleMulOp | DoubleDivOp | DoubleNegOp
    | Double2IntOp | Int2DoubleOp
    | Double2FloatOp | Float2DoubleOp

    | DoubleExpOp   | DoubleLogOp   | DoubleSqrtOp
    | DoubleSinOp   | DoubleCosOp   | DoubleTanOp
    | DoubleAsinOp  | DoubleAcosOp  | DoubleAtanOp
    | DoubleSinhOp  | DoubleCoshOp  | DoubleTanhOp
    -- not all machines have these available conveniently:
    -- | DoubleAsinhOp | DoubleAcoshOp | DoubleAtanhOp
    | DoublePowerOp -- ** op

    -- Integer (and related...) ops:
    -- slightly weird -- to match GMP package.
    | IntegerAddOp | IntegerSubOp | IntegerMulOp
    | IntegerQuotRemOp | IntegerDivModOp | IntegerNegOp

    | IntegerCmpOp

    | Integer2IntOp  | Int2IntegerOp
    | Addr2IntegerOp -- "Addr" is *always* a literal string
    -- ?? gcd, etc?

    | FloatEncodeOp  | FloatDecodeOp
    | DoubleEncodeOp | DoubleDecodeOp

    -- primitive ops for primitive arrays

    | NewArrayOp
    | NewByteArrayOp PrimRep

    | SameMutableArrayOp
    | SameMutableByteArrayOp

    | ReadArrayOp | WriteArrayOp | IndexArrayOp -- for arrays of Haskell ptrs

    | ReadByteArrayOp	PrimRep
    | WriteByteArrayOp	PrimRep
    | IndexByteArrayOp	PrimRep
    | IndexOffAddrOp	PrimRep
	-- PrimRep can be one of {Char,Int,Addr,Float,Double}Kind.
	-- This is just a cheesy encoding of a bunch of ops.
	-- Note that ForeignObjRep is not included -- the only way of
	-- creating a ForeignObj is with a ccall or casm.

    | UnsafeFreezeArrayOp | UnsafeFreezeByteArrayOp

    | MakeStablePtrOp | DeRefStablePtrOp
\end{pseudocode}

A special ``trap-door'' to use in making calls direct to C functions:
Note: From GrAn point of view, CCall is probably very expensive 
      The programmer can specify the costs of the Ccall by inserting
      a GRAN_EXEC(a,b,l,s,f) at the end of the C- code, specifing the
      number or arithm., branch, load, store and floating point instructions
      -- HWL

\begin{pseudocode}
    | CCallOp	String	-- An "unboxed" ccall# to this named function
		Bool	-- True <=> really a "casm"
		Bool	-- True <=> might invoke Haskell GC
		[Type]	-- Unboxed argument; the state-token
			-- argument will have been put *first*
		Type	-- Return type; one of the "StateAnd<blah>#" types

    -- (... to be continued ... )
\end{pseudocode}
