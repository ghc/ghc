%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
% $Id: CgRetConv.lhs,v 1.20 2000/03/23 17:45:19 simonpj Exp $
%
\section[CgRetConv]{Return conventions for the code generator}

The datatypes and functions here encapsulate what there is to know
about return conventions.

\begin{code}
module CgRetConv (
	CtrlReturnConvention(..),
	ctrlReturnConvAlg,
	dataReturnConvPrim,
	assignRegs, assignAllRegs
    ) where

#include "HsVersions.h"

import AbsCSyn		-- quite a few things
import Constants	( mAX_FAMILY_SIZE_FOR_VEC_RETURNS,
			  mAX_Vanilla_REG, mAX_Float_REG,
			  mAX_Double_REG, mAX_Long_REG
			)
import CmdLineOpts	( opt_UseVanillaRegs, opt_UseFloatRegs,
			  opt_UseDoubleRegs, opt_UseLongRegs
			)
import Maybes		( catMaybes )
import DataCon		( DataCon )
import PrimOp		( PrimOp{-instance Outputable-} )
import PrimRep		( isFloatingRep, PrimRep(..), is64BitRep )
import TyCon		( TyCon, tyConDataCons, tyConFamilySize )
import Type		( Type, typePrimRep, isUnLiftedType, 
			  splitAlgTyConApp_maybe )
import Util		( isn'tIn )

import Outputable
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

%************************************************************************
%*									*
\subsection[CgRetConv-algebraic]{Return conventions for algebraic datatypes}
%*									*
%************************************************************************

\begin{code}
ctrlReturnConvAlg :: TyCon -> CtrlReturnConvention

ctrlReturnConvAlg tycon
  = case (tyConFamilySize tycon) of
      0 -> panic "ctrlRetConvAlg"
      size -> -- we're supposed to know...
	if (size > (1::Int) && size <= mAX_FAMILY_SIZE_FOR_VEC_RETURNS) then
	    VectoredReturn size
	else
	    UnvectoredReturn size
\end{code}

%************************************************************************
%*									*
\subsection[CgRetConv-prim]{Return conventions for primitive datatypes}
%*									*
%************************************************************************

\begin{code}
dataReturnConvPrim :: PrimRep -> MagicId

dataReturnConvPrim IntRep	= VanillaReg IntRep  ILIT(1)
dataReturnConvPrim WordRep	= VanillaReg WordRep ILIT(1)
dataReturnConvPrim Int64Rep	= LongReg Int64Rep  ILIT(1)
dataReturnConvPrim Word64Rep	= LongReg Word64Rep ILIT(1)
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
dataReturnConvPrim WeakPtrRep   = VanillaReg WeakPtrRep ILIT(1)

#ifdef DEBUG
dataReturnConvPrim PtrRep	= panic "dataReturnConvPrim: PtrRep"
dataReturnConvPrim _		= panic "dataReturnConvPrim: other"
#endif
\end{code}

%************************************************************************
%*									*
\subsubsection[CgRetConv-regs]{Register assignment}
%*									*
%************************************************************************

How to assign registers for 

	1) Calling a fast entry point.
	2) Returning an unboxed tuple.
	3) Invoking an out-of-line PrimOp.

Registers are assigned in order.

If we run out, we don't attempt to assign any further registers (even
though we might have run out of only one kind of register); we just
return immediately with the left-overs specified.

The alternative version @assignAllRegs@ uses the complete set of
registers, including those that aren't mapped to real machine
registers.  This is used for calling special RTS functions and PrimOps
which expect their arguments to always be in the same registers.

\begin{code}
assignRegs, assignAllRegs
	:: [MagicId]	-- Unavailable registers
	-> [PrimRep]	-- Arg or result kinds to assign
	-> ([MagicId],	-- Register assignment in same order
				-- for *initial segment of* input list
	    [PrimRep])-- leftover kinds

assignRegs regs_in_use kinds
 = assign_reg kinds [] (mkRegTbl regs_in_use)

assignAllRegs regs_in_use kinds
 = assign_reg kinds [] (mkRegTbl_allRegs regs_in_use)

assign_reg 
	:: [PrimRep]              -- arg kinds being scrutinized
	-> [MagicId]		  -- accum. regs assigned so far (reversed)
	-> AvailRegs		  -- regs still avail: Vanilla, Float, Double, longs
	-> ([MagicId], [PrimRep])

assign_reg (VoidRep:ks) acc supply
	= assign_reg ks (VoidReg:acc) supply 
	-- one VoidReg is enough for everybody!

assign_reg (FloatRep:ks) acc (vanilla_rs, IBOX(f):float_rs, double_rs, long_rs)
	= assign_reg ks (FloatReg f:acc) (vanilla_rs, float_rs, double_rs, long_rs)

assign_reg (DoubleRep:ks) acc (vanilla_rs, float_rs, IBOX(d):double_rs, long_rs)
	= assign_reg ks (DoubleReg d:acc) (vanilla_rs, float_rs, double_rs, long_rs)

assign_reg (Word64Rep:ks) acc (vanilla_rs, float_rs, double_rs, IBOX(u):long_rs)
	= assign_reg ks (LongReg Word64Rep u:acc) (vanilla_rs, float_rs, double_rs, long_rs)

assign_reg (Int64Rep:ks) acc (vanilla_rs, float_rs, double_rs, IBOX(l):long_rs)
	= assign_reg ks (LongReg Int64Rep l:acc) (vanilla_rs, float_rs, double_rs, long_rs)

assign_reg (k:ks) acc (IBOX(v):vanilla_rs, float_rs, double_rs, long_rs)
	| not (isFloatingRep k || is64BitRep k)
	= assign_reg ks (VanillaReg k v:acc) (vanilla_rs, float_rs, double_rs, long_rs)

-- The catch-all.  It can happen because either
--	(a) we've assigned all the regs so leftover_ks is []
--  (b) we couldn't find a spare register in the appropriate supply
--  or, I suppose,
--  (c) we came across a Kind we couldn't handle (this one shouldn't happen)
assign_reg leftover_ks acc _ = (reverse acc, leftover_ks)

\end{code}

Register supplies.  Vanilla registers can contain pointers, Ints, Chars.
Floats and doubles have separate register supplies.

We take these register supplies from the *real* registers, i.e. those
that are guaranteed to map to machine registers.

\begin{code}
vanillaRegNos, floatRegNos, doubleRegNos, longRegNos :: [Int]
vanillaRegNos	 = regList opt_UseVanillaRegs
floatRegNos	 = regList opt_UseFloatRegs
doubleRegNos	 = regList opt_UseDoubleRegs
longRegNos       = regList opt_UseLongRegs

allVanillaRegNos, allFloatRegNos, allDoubleRegNos, allLongRegNos :: [Int]
allVanillaRegNos = regList mAX_Vanilla_REG
allFloatRegNos	 = regList mAX_Float_REG
allDoubleRegNos	 = regList mAX_Double_REG
allLongRegNos	 = regList mAX_Long_REG

regList 0 = []
regList n = [1 .. n]

type AvailRegs = ( [Int]   -- available vanilla regs.
		 , [Int]   -- floats
		 , [Int]   -- doubles
		 , [Int]   -- longs (int64 and word64)
		 )

mkRegTbl :: [MagicId] -> AvailRegs
mkRegTbl regs_in_use
  = mkRegTbl' regs_in_use vanillaRegNos floatRegNos doubleRegNos longRegNos

mkRegTbl_allRegs :: [MagicId] -> AvailRegs
mkRegTbl_allRegs regs_in_use
  = mkRegTbl' regs_in_use allVanillaRegNos allFloatRegNos allDoubleRegNos allLongRegNos

mkRegTbl' regs_in_use vanillas floats doubles longs
  = (ok_vanilla, ok_float, ok_double, ok_long)
  where
    ok_vanilla = catMaybes (map (select (VanillaReg VoidRep))  vanillas)
    ok_float   = catMaybes (map (select FloatReg)	       floats)
    ok_double  = catMaybes (map (select DoubleReg)	       doubles)
    ok_long    = catMaybes (map (select (LongReg Int64Rep))    longs)   
				    -- rep isn't looked at, hence we can use any old rep.

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
