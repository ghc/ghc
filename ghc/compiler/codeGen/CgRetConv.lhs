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

	mkLiveRegsBitMask, noLiveRegsMask,

	dataReturnConvPrim,

	assignPrimOpResultRegs,
	makePrimOpArgsRobust,
	assignRegs,

	-- and to make the interface self-sufficient...
	MagicId, PrimKind, Id, CLabel, TyCon
    ) where

import AbsCSyn

import AbsPrel		( PrimOp(..), PrimOpResultInfo(..), primOpCanTriggerGC,
			  getPrimOpResultInfo, PrimKind
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( getTyConFamilySize, kindFromType, getTyConDataCons,
			  TyVarTemplate, TyCon, Class,
			  TauType(..), ThetaType(..), UniType
			  IF_ATTACK_PRAGMAS(COMMA cmpTyCon COMMA cmpClass)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import CgCompInfo	-- various things

import Id		( Id, getDataConSig, fIRST_TAG, isDataCon,
			  DataCon(..), ConTag(..)
			)
import Maybes		( catMaybes, Maybe(..) )
import PrimKind
import Util
import Pretty
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
  = case (getTyConFamilySize tycon) of
      Nothing -> -- pprPanic "ctrlReturnConvAlg:" (ppr PprDebug tycon)
		 UnvectoredReturn 0 -- e.g., w/ "data Bin"

      Just size -> -- we're supposed to know...
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
    (_, _, arg_tys, _) = getDataConSig data_con
    (reg_assignment, leftover_kinds) = assignRegs [node,infoptr] 
    	    	    	    	    	    	  (map kindFromType arg_tys)
\end{code}

\begin{code}
noLiveRegsMask :: Int	-- Mask indicating nothing live
noLiveRegsMask = 0

mkLiveRegsBitMask
	:: [MagicId]	-- Candidate live regs; depends what they have in them
	-> Int

mkLiveRegsBitMask regs
  = foldl do_reg noLiveRegsMask regs
  where
    do_reg acc (VanillaReg kind reg_no)
      | isFollowableKind kind
      = acc + (reg_tbl !! IBOX(reg_no _SUB_ ILIT(1)))

    do_reg acc anything_else = acc

    reg_tbl -- ToDo: mk Array!
      = [lIVENESS_R1, lIVENESS_R2, lIVENESS_R3, lIVENESS_R4,
	 lIVENESS_R5, lIVENESS_R6, lIVENESS_R7, lIVENESS_R8]

{-
-- Completely opaque code.  ADR
-- What's wrong with: (untested)

mkLiveRegsBitMask regs
  = foldl (+) noLiveRegsMask (map liveness_bit regs)
  where
    liveness_bit (VanillaReg kind reg_no)
      | isFollowableKind kind
      = reg_tbl !! (reg_no - 1)

    liveness_bit anything_else 
      = noLiveRegsBitMask

    reg_tbl
      = [lIVENESS_R1, lIVENESS_R2, lIVENESS_R3, lIVENESS_R4,
	 lIVENESS_R5, lIVENESS_R6, lIVENESS_R7, lIVENESS_R8]
-}
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
dataReturnConvPrim :: PrimKind -> MagicId

#ifndef DPH
dataReturnConvPrim IntKind	= VanillaReg IntKind  ILIT(1)
dataReturnConvPrim WordKind	= VanillaReg WordKind ILIT(1)
dataReturnConvPrim AddrKind	= VanillaReg AddrKind ILIT(1)
dataReturnConvPrim CharKind	= VanillaReg CharKind ILIT(1)
dataReturnConvPrim FloatKind	= FloatReg  ILIT(1)
dataReturnConvPrim DoubleKind	= DoubleReg ILIT(1)
dataReturnConvPrim VoidKind	= VoidReg

-- Return a primitive-array pointer in the usual register:
dataReturnConvPrim ArrayKind     = VanillaReg ArrayKind ILIT(1)
dataReturnConvPrim ByteArrayKind = VanillaReg ByteArrayKind ILIT(1)

dataReturnConvPrim StablePtrKind = VanillaReg StablePtrKind ILIT(1)
dataReturnConvPrim MallocPtrKind = VanillaReg MallocPtrKind ILIT(1)

dataReturnConvPrim PtrKind	= panic "dataReturnConvPrim: PtrKind"
dataReturnConvPrim _		= panic "dataReturnConvPrim: other"

#else
dataReturnConvPrim VoidKind	= VoidReg
dataReturnConvPrim PtrKind	= panic "dataReturnConvPrim: PtrKind"
dataReturnConvPrim kind         = DataReg kind 2 -- Don't Hog a Modifier reg.
#endif {- Data Parallel Haskell -}
\end{code}


%********************************************************
%*							*
\subsection[primop-stuff]{Argument and return conventions for Prim Ops}
%*							*
%********************************************************

\begin{code}
assignPrimOpResultRegs
    :: PrimOp	-- The constructors in canonical order
    -> [MagicId]	-- The return regs all concatenated to together,
			-- (*including* one for the tag if necy)

assignPrimOpResultRegs op
 = case (getPrimOpResultInfo op) of

	ReturnsPrim kind -> [dataReturnConvPrim kind]

	ReturnsAlg tycon -> let cons	    = getTyConDataCons tycon
				result_regs = concat (map get_return_regs cons)
			    in
				-- Since R1 is dead, it can hold the tag if necessary
			    case cons of
				[_]   -> result_regs
				other -> (VanillaReg IntKind ILIT(1)) : result_regs

 where
   get_return_regs con = case (dataReturnConvAlg con) of
			      ReturnInHeap	-> panic "getPrimOpAlgResultRegs"
			      ReturnInRegs regs -> regs
\end{code}

@assignPrimOpArgsRobust@ is used only for primitive ops which may
trigger GC. [MAYBE (WDP 94/05)] For these, we pass all (nonRobust)
arguments in registers.  This function assigns them and tells us which
of those registers are now live (because we've shoved a followable
argument into it).

Bug: it is assumed that robust amodes cannot contain pointers.  This
seems reasonable but isn't true.  For example, \tr{Array#}'s
\tr{MallocPtr#}'s are pointers.  (This is only known to bite on
\tr{_ccall_GC_} with a MallocPtr argument.)

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
    	arg_kinds = map getAmodeKind non_robust_amodes

	(arg_regs, extra_args) = assignRegs [{-nothing live-}] arg_kinds

		-- Check that all the args fit before returning arg_regs
	final_arg_regs = case extra_args of
			   []    -> arg_regs
			   other -> error ("Cannot allocate enough registers for primop (try rearranging code or reducing number of arguments?) " ++ ppShow 80 (ppr PprDebug op))

	arg_assts = mkAbstractCs (zipWith assign_to_reg arg_regs non_robust_amodes)
	assign_to_reg reg_id amode = CAssign (CReg reg_id) amode

    	safe_arg regs arg 
		| amodeCanSurviveGC arg = (regs, arg) 
    		| otherwise    		= (tail regs, CReg (head regs))
    	safe_amodes = snd (mapAccumL safe_arg arg_regs arg_amodes)

	liveness_mask = mkLiveRegsBitMask arg_regs
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
	    -> [PrimKind]	-- Arg or result kinds to assign
	    -> ([MagicId],	-- Register assignment in same order
				-- for *initial segment of* input list
		[PrimKind])-- leftover kinds

#ifndef DPH
assignRegs regs_in_use kinds
 = assign_reg kinds [] (mkRegTbl regs_in_use)
 where

    assign_reg	:: [PrimKind]  -- arg kinds being scrutinized
		-> [MagicId]	    -- accum. regs assigned so far (reversed)
		-> ([Int], [Int], [Int])
			-- regs still avail: Vanilla, Float, Double
		-> ([MagicId], [PrimKind])

    assign_reg (VoidKind:ks) acc supply
	= assign_reg ks (VoidReg:acc) supply -- one VoidReg is enough for everybody!

    assign_reg (FloatKind:ks) acc (vanilla_rs, IBOX(f):float_rs, double_rs)
	= assign_reg ks (FloatReg f:acc) (vanilla_rs, float_rs, double_rs)

    assign_reg (DoubleKind:ks) acc (vanilla_rs, float_rs, IBOX(d):double_rs)
	= assign_reg ks (DoubleReg d:acc) (vanilla_rs, float_rs, double_rs)

    assign_reg (k:ks) acc (IBOX(v):vanilla_rs, float_rs, double_rs)
	| not (isFloatingKind k)
	= assign_reg ks (VanillaReg k v:acc) (vanilla_rs, float_rs, double_rs)

    -- The catch-all.  It can happen because either
    --	(a) we've assigned all the regs so leftover_ks is []
    --  (b) we couldn't find a spare register in the appropriate supply
    --  or, I suppose,
    --  (c) we came across a Kind we couldn't handle (this one shouldn't happen)
    assign_reg leftover_ks acc _ = (reverse acc, leftover_ks)
#else
assignRegs node_using_Ret1 kinds
 = if node_using_Ret1
   then assign_reg kinds [] (tail vanillaRegNos) (tail datRegNos)
   else assign_reg kinds [] vanillaRegNos        (tail datRegNos)
 where
    assign_reg:: [PrimKind]  -- arg kinds being scrutinized
	      -> [MagicId]	  -- accum. regs assigned so far (reversed)
	      -> [Int]	   -- Vanilla Regs (ptr, int, char, float or double)
	      -> [Int]	   -- Data Regs    (     int, char, float or double)
	      -> ([MagicId], [PrimKind])

    assign_reg (k:ks) acc (IBOX(p):ptr_regs) dat_regs
      | isFollowableKind k	 
      = assign_reg ks (VanillaReg k p:acc) ptr_regs dat_regs

    assign_reg (CharKind:ks) acc ptr_regs (d:dat_regs)
      = assign_reg ks (DataReg CharKind d:acc) ptr_regs dat_regs

    assign_reg (IntKind:ks) acc ptr_regs (d:dat_regs)
      = assign_reg ks (DataReg IntKind d:acc) ptr_regs dat_regs

    assign_reg (WordKind:ks) acc ptr_regs (d:dat_regs)
      = assign_reg ks (DataReg WordKind d:acc) ptr_regs dat_regs

    assign_reg (AddrKind:ks) acc ptr_regs (d:dat_regs)
      = assign_reg ks (DataReg AddrKind d:acc) ptr_regs dat_regs

    assign_reg (FloatKind:ks) acc ptr_regs (d:dat_regs)
      = assign_reg ks (DataReg FloatKind d:acc) ptr_regs dat_regs

    -- Notice how doubles take up two data registers....
    assign_reg (DoubleKind:ks)   acc ptr_regs (IBOX(d1):d2:dat_regs)
      = assign_reg ks (DoubleReg d1:acc) ptr_regs dat_regs

    assign_reg (VoidKind:ks) acc ptr_regs dat_regs
      = assign_reg ks (VoidReg:acc) ptr_regs dat_regs

    -- The catch-all.  It can happen because either
    --	(a) we've assigned all the regs so leftover_ks is []
    --  (b) we couldn't find a spare register in the appropriate supply
    --  or, I suppose,
    --  (c) we came across a Kind we couldn't handle (this one shouldn't happen)
    --  ToDo Maybe when dataReg becomes empty, we can start using the
    --       vanilla registers ????
    assign_reg leftover_ks acc _ _ = (reverse acc, leftover_ks)
#endif {- Data Parallel Haskell -}
\end{code}

Register supplies.  Vanilla registers can contain pointers, Ints, Chars.

\begin{code}
vanillaRegNos :: [Int]
vanillaRegNos	= [1 .. mAX_Vanilla_REG]
\end{code}

Only a subset of the registers on the DAP can be used to hold pointers (and most
of these are taken up with things like the heap pointer and stack pointers). 
However the resulting registers can hold integers, floats or chars. We therefore
allocate pointer like things into the @vanillaRegNos@ (and Ints Chars or Floats
if the remaining registers are empty). See NOTE.regsiterMap for an outline of
the global and local register allocation scheme.

\begin{code}
#ifdef DPH
datRegNos ::[Int]		
datRegNos = [1..mAX_Data_REG]		-- For Ints, Floats, Doubles or Chars
#endif {- Data Parallel Haskell -}
\end{code}

Floats and doubles have separate register supplies.

\begin{code}
#ifndef DPH
floatRegNos, doubleRegNos :: [Int]
floatRegNos	= [1 .. mAX_Float_REG]
doubleRegNos	= [1 .. mAX_Double_REG]

mkRegTbl :: [MagicId] -> ([Int], [Int], [Int])
mkRegTbl regs_in_use = (ok_vanilla, ok_float, ok_double)
  where
    ok_vanilla = catMaybes (map (select (VanillaReg VoidKind)) vanillaRegNos)
    ok_float   = catMaybes (map (select FloatReg)	       floatRegNos)
    ok_double  = catMaybes (map (select DoubleReg)	       doubleRegNos)

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

#endif {- Data Parallel Haskell -}
\end{code}
