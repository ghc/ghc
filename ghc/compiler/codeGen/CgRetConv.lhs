%
% (c) The GRASP Project, Glasgow University, 1992-1995
%
\section[CgRetConv]{Return conventions for the code generator}

The datatypes and functions here encapsulate what there is to know
about return conventions.

\begin{code}
#include "HsVersions.h"

module CgRetConv (
	CtrlReturnConvention(..), DataReturnConvention(..),

	ctrlReturnConvAlg,
	dataReturnConvAlg,

	dataReturnConvPrim,

	assignPrimOpResultRegs,
	makePrimOpArgsRobust,
	assignRegs

	-- and to make the interface self-sufficient...
    ) where

import Ubiq{-uitous-}
import AbsCLoop		-- paranoia checking

import AbsCSyn		-- quite a few things
import AbsCUtils	( mkAbstractCs, getAmodeRep,
			  amodeCanSurviveGC
			)
import CgCompInfo	( mAX_FAMILY_SIZE_FOR_VEC_RETURNS,
			  mAX_Vanilla_REG, mAX_Float_REG,
			  mAX_Double_REG
			)
import CmdLineOpts	( opt_ReturnInRegsThreshold )
import Id		( isDataCon, dataConSig,
			  DataCon(..), GenId{-instance Eq-}
			)
import Maybes		( catMaybes )
import PprStyle		( PprStyle(..) )
import PprType		( TyCon{-instance Outputable-} )
import PrimOp		( primOpCanTriggerGC,
			  getPrimOpResultInfo, PrimOpResultInfo(..),
			  PrimOp{-instance Outputable-}
			)
import PrimRep		( isFloatingRep, PrimRep(..) )
import TyCon		( tyConDataCons, tyConFamilySize )
import Type		( typePrimRep )
import Util		( zipWithEqual, mapAccumL, isn'tIn,
			  pprError, pprTrace, panic, assertPanic
			)
\end{code}

%************************************************************************
%*									*
\subsection[CgRetConv-possibilities]{Data types that encode possible return conventions}
%*									*
%************************************************************************

A @CtrlReturnConvention@ says how {\em control} is returned.
\begin{code}
data CtrlReturnConvention
  = VectoredReturn	Int	-- size of the vector table (family size)
  | UnvectoredReturn    Int 	-- family size
\end{code}

A @DataReturnConvention@ says how the data for a particular
data-constructor is returned.
\begin{code}
data DataReturnConvention
  = ReturnInHeap
  | ReturnInRegs	[MagicId]
\end{code}
The register assignment given by a @ReturnInRegs@ obeys three rules:
\begin{itemize}
\item   R1 is dead.
\item   R2 points to the info table for the phantom constructor
\item	The list of @MagicId@ is in the same order as the arguments
	to the constructor.
\end{itemize}


%************************************************************************
%*									*
\subsection[CgRetConv-algebraic]{Return conventions for algebraic datatypes}
%*									*
%************************************************************************

\begin{code}
ctrlReturnConvAlg :: TyCon -> CtrlReturnConvention

ctrlReturnConvAlg tycon
  = case (tyConFamilySize tycon) of
      0 -> pprTrace "ctrlReturnConvAlg:" (ppr PprDebug tycon) $
	   UnvectoredReturn 0 -- e.g., w/ "data Bin"

      size -> -- we're supposed to know...
	if (size > (1::Int) && size <= mAX_FAMILY_SIZE_FOR_VEC_RETURNS) then
	    VectoredReturn size
	else
	    UnvectoredReturn size
\end{code}

@dataReturnConvAlg@ determines the return conventions from the
(possibly specialised) data constructor.

(See also @getDataConReturnConv@ (in @Id@).)  We grab the types
of the data constructor's arguments.  We feed them and a list of
available registers into @assign_reg@, which sequentially assigns
registers of the appropriate types to the arguments, based on the
types.	If @assign_reg@ runs out of a particular kind of register,
then it gives up, returning @ReturnInHeap@.

\begin{code}
dataReturnConvAlg :: DataCon -> DataReturnConvention

dataReturnConvAlg data_con
  = ASSERT(isDataCon data_con)
    case leftover_kinds of
	[]    ->	ReturnInRegs reg_assignment
	other ->	ReturnInHeap	-- Didn't fit in registers
  where
    (_, _, arg_tys, _) = dataConSig data_con

    (reg_assignment, leftover_kinds)
      = assignRegs [node, infoptr] -- taken...
		   (map typePrimRep arg_tys)
\end{code}

%************************************************************************
%*									*
\subsection[CgRetConv-prim]{Return conventions for primitive datatypes}
%*									*
%************************************************************************

WARNING! If you add a return convention which can return a pointer,
make sure you alter CgCase (cgPrimDefault) to generate the right sort
of heap check!
\begin{code}
dataReturnConvPrim :: PrimRep -> MagicId

dataReturnConvPrim IntRep	= VanillaReg IntRep  ILIT(1)
dataReturnConvPrim WordRep	= VanillaReg WordRep ILIT(1)
dataReturnConvPrim AddrRep	= VanillaReg AddrRep ILIT(1)
dataReturnConvPrim CharRep	= VanillaReg CharRep ILIT(1)
dataReturnConvPrim FloatRep	= FloatReg  ILIT(1)
dataReturnConvPrim DoubleRep	= DoubleReg ILIT(1)
dataReturnConvPrim VoidRep	= VoidReg

-- Return a primitive-array pointer in the usual register:
dataReturnConvPrim ArrayRep     = VanillaReg ArrayRep ILIT(1)
dataReturnConvPrim ByteArrayRep = VanillaReg ByteArrayRep ILIT(1)

dataReturnConvPrim StablePtrRep = VanillaReg StablePtrRep ILIT(1)
dataReturnConvPrim ForeignObjRep = VanillaReg ForeignObjRep ILIT(1)

#ifdef DEBUG
dataReturnConvPrim PtrRep	= panic "dataReturnConvPrim: PtrRep"
dataReturnConvPrim _		= panic "dataReturnConvPrim: other"
#endif
\end{code}

%********************************************************
%*							*
\subsection[primop-stuff]{Argument and return conventions for Prim Ops}
%*							*
%********************************************************

\begin{code}
assignPrimOpResultRegs
    :: PrimOp		-- The constructors in canonical order
    -> [MagicId]	-- The return regs all concatenated to together,
			-- (*including* one for the tag if necy)

assignPrimOpResultRegs op
 = case (getPrimOpResultInfo op) of

	ReturnsPrim kind -> [dataReturnConvPrim kind]

	ReturnsAlg tycon
	  -> let
		cons	    = tyConDataCons tycon
		result_regs = concat (map get_return_regs cons)
	     in
	     -- As R1 is dead, it can hold the tag if necessary
	     case cons of
		[_]   -> result_regs
		other -> (VanillaReg IntRep ILIT(1)) : result_regs
  where
    get_return_regs con
      = case (dataReturnConvAlg con) of
	  ReturnInRegs regs -> regs
	  ReturnInHeap	    -> panic "getPrimOpAlgResultRegs"
\end{code}

@assignPrimOpArgsRobust@ is used only for primitive ops which may
trigger GC. [MAYBE (WDP 94/05)] For these, we pass all (nonRobust)
arguments in registers.  This function assigns them and tells us which
of those registers are now live (because we've shoved a followable
argument into it).

Bug: it is assumed that robust amodes cannot contain pointers.  This
seems reasonable but isn't true.  For example, \tr{Array#}'s
\tr{ForeignObj#}'s are pointers.  (This is only known to bite on
\tr{_ccall_GC_} with a ForeignObj argument.)

See after for some ADR comments...

\begin{code}
makePrimOpArgsRobust
	:: PrimOp
	-> [CAddrMode]		-- Arguments
	-> ([CAddrMode],	-- Arg registers
	    Int,		-- Liveness mask
	    AbstractC)		-- Simultaneous assignments to assign args to regs

makePrimOpArgsRobust op arg_amodes
  = ASSERT (primOpCanTriggerGC op)
    let
	non_robust_amodes = filter (not . amodeCanSurviveGC) arg_amodes
    	arg_kinds = map getAmodeRep non_robust_amodes

	(arg_regs, extra_args)
	  = assignRegs [{-nothing live-}] arg_kinds

		-- Check that all the args fit before returning arg_regs
	final_arg_regs = case extra_args of
			   []    -> arg_regs
			   other -> pprError "Cannot allocate enough registers for primop (try rearranging code or reducing number of arguments?)" (ppr PprDebug op)

	arg_assts
	  = mkAbstractCs (zipWithEqual "assign_to_reg" assign_to_reg final_arg_regs non_robust_amodes)

	assign_to_reg reg_id amode = CAssign (CReg reg_id) amode

    	safe_arg regs arg
		| amodeCanSurviveGC arg = (regs, arg)
    		| otherwise    		= (tail regs, CReg (head regs))
    	safe_amodes = snd (mapAccumL safe_arg final_arg_regs arg_amodes)

	liveness_mask = mkLiveRegsMask final_arg_regs
    in
    (safe_amodes, liveness_mask, arg_assts)
\end{code}

%************************************************************************
%*									*
\subsubsection[CgRetConv-regs]{Register assignment}
%*									*
%************************************************************************

How to assign registers.
Registers are assigned in order.

If we run out, we don't attempt to assign
any further registers (even though we might have run out of only one kind of
register); we just return immediately with the left-overs specified.

\begin{code}
assignRegs  :: [MagicId]	-- Unavailable registers
	    -> [PrimRep]	-- Arg or result kinds to assign
	    -> ([MagicId],	-- Register assignment in same order
				-- for *initial segment of* input list
		[PrimRep])-- leftover kinds

assignRegs regs_in_use kinds
 = assign_reg kinds [] (mkRegTbl regs_in_use)
 where

    assign_reg	:: [PrimRep]  -- arg kinds being scrutinized
		-> [MagicId]	    -- accum. regs assigned so far (reversed)
		-> ([Int], [Int], [Int])
			-- regs still avail: Vanilla, Float, Double
		-> ([MagicId], [PrimRep])

    assign_reg (VoidRep:ks) acc supply
	= assign_reg ks (VoidReg:acc) supply -- one VoidReg is enough for everybody!

    assign_reg (FloatRep:ks) acc (vanilla_rs, IBOX(f):float_rs, double_rs)
	= assign_reg ks (FloatReg f:acc) (vanilla_rs, float_rs, double_rs)

    assign_reg (DoubleRep:ks) acc (vanilla_rs, float_rs, IBOX(d):double_rs)
	= assign_reg ks (DoubleReg d:acc) (vanilla_rs, float_rs, double_rs)

    assign_reg (k:ks) acc (IBOX(v):vanilla_rs, float_rs, double_rs)
	| not (isFloatingRep k)
	= assign_reg ks (VanillaReg k v:acc) (vanilla_rs, float_rs, double_rs)

    -- The catch-all.  It can happen because either
    --	(a) we've assigned all the regs so leftover_ks is []
    --  (b) we couldn't find a spare register in the appropriate supply
    --  or, I suppose,
    --  (c) we came across a Kind we couldn't handle (this one shouldn't happen)
    assign_reg leftover_ks acc _ = (reverse acc, leftover_ks)
\end{code}

Register supplies.  Vanilla registers can contain pointers, Ints, Chars.

\begin{code}
vanillaRegNos :: [Int]
vanillaRegNos	= [1 .. mAX_Vanilla_REG]
\end{code}

Floats and doubles have separate register supplies.

\begin{code}
floatRegNos, doubleRegNos :: [Int]
floatRegNos	= [1 .. mAX_Float_REG]
doubleRegNos	= [1 .. mAX_Double_REG]

mkRegTbl :: [MagicId] -> ([Int], [Int], [Int])

mkRegTbl regs_in_use
  = (ok_vanilla, ok_float, ok_double)
  where
    ok_vanilla = catMaybes (map (select (VanillaReg VoidRep)) (taker vanillaRegNos))
    ok_float   = catMaybes (map (select FloatReg)	       floatRegNos)
    ok_double  = catMaybes (map (select DoubleReg)	       doubleRegNos)

    taker :: [Int] -> [Int]
    taker rs
      = case (opt_ReturnInRegsThreshold) of
	  Nothing -> rs -- no flag set; use all of them
	  Just  n -> take n rs

    select :: (FAST_INT -> MagicId) -> Int{-cand-} -> Maybe Int
	-- one we've unboxed the Int, we make a MagicId
	-- and see if it is already in use; if not, return its number.

    select mk_reg_fun cand@IBOX(i)
      = let
	    reg = mk_reg_fun i
	in
	if reg `not_elem` regs_in_use
	then Just cand
	else Nothing
      where
	not_elem = isn'tIn "mkRegTbl"
\end{code}
