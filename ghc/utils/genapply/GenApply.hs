{-# OPTIONS -cpp #-}
module Main(main) where

#include "../../includes/config.h"
#include "../../includes/MachRegs.h"

#if __GLASGOW_HASKELL__ >= 504
import Text.PrettyPrint
import Data.Word
import Data.Bits
import Data.List	( intersperse )
import Data.Char	( toUpper )
#else
import Bits
import Word
import Pretty
import List		( intersperse )
import Char		( toUpper )
#endif


-- -----------------------------------------------------------------------------
-- Argument kinds (rougly equivalent to PrimRep)

data ArgRep 
  = N		-- non-ptr
  | P		-- ptr
  | V		-- void
  | F		-- float
  | D		-- double
  | L		-- long (64-bit)

-- size of a value in *words*
argSize :: ArgRep -> Int
argSize N = 1
argSize P = 1
argSize V = 0
argSize F = 1
argSize D = (SIZEOF_DOUBLE `quot` SIZEOF_VOID_P :: Int)
argSize L = (8 `quot` SIZEOF_VOID_P :: Int)

showArg :: ArgRep -> Char
showArg N = 'n'
showArg P = 'p'
showArg V = 'v'
showArg F = 'f'
showArg D = 'd'
showArg L = 'l'

-- is a value a pointer?
isPtr :: ArgRep -> Bool
isPtr P = True
isPtr _ = False

-- -----------------------------------------------------------------------------
-- Registers

type Reg = String

availableRegs :: ([Reg],[Reg],[Reg],[Reg])
availableRegs = 
  ( vanillaRegs MAX_REAL_VANILLA_REG,
    floatRegs   MAX_REAL_FLOAT_REG,
    doubleRegs  MAX_REAL_DOUBLE_REG,
    longRegs    MAX_REAL_LONG_REG
  )

vanillaRegs, floatRegs, doubleRegs, longRegs :: Int -> [Reg]
vanillaRegs n = [ "R" ++ show m ++ ".w" | m <- [2..n] ]  -- never use R1
floatRegs   n = [ "F" ++ show m | m <- [1..n] ]
doubleRegs  n = [ "D" ++ show m | m <- [1..n] ]
longRegs    n = [ "L" ++ show m | m <- [1..n] ]

-- -----------------------------------------------------------------------------
-- Loading/saving register arguments to the stack

loadRegArgs :: Int -> [ArgRep] -> (Doc,Int)
loadRegArgs sp args = (vcat (map (uncurry assign_stk_to_reg) reg_locs), sp')
 where
  (reg_locs, sp') = assignRegs sp args

-- a bit like assignRegs in CgRetConv.lhs
assignRegs
	:: Int			-- Sp of first arg
	-> [ArgRep]		-- args
	-> ([(Reg,Int)], Int)	-- Sp and rest of args
assignRegs sp args = assign sp args availableRegs []

assign sp [] regs doc = (doc, sp)
assign sp (V : args) regs doc = assign sp args regs doc
assign sp (arg : args) regs doc
 = case findAvailableReg arg regs of
    Just (reg, regs') -> assign (sp + argSize arg)  args regs' 
	    		    ((reg, sp) : doc)
    Nothing -> (doc, sp)

findAvailableReg N (vreg:vregs, fregs, dregs, lregs) =
  Just (vreg, (vregs,fregs,dregs,lregs))
findAvailableReg P (vreg:vregs, fregs, dregs, lregs) =
  Just (vreg, (vregs,fregs,dregs,lregs))
findAvailableReg F (vregs, freg:fregs, dregs, lregs) =
  Just (freg, (vregs,fregs,dregs,lregs))
findAvailableReg D (vregs, fregs, dreg:dregs, lregs) =
  Just (dreg, (vregs,fregs,dregs,lregs))
findAvailableReg L (vregs, fregs, dregs, lreg:lregs) =
  Just (lreg, (vregs,fregs,dregs,lregs))
findAvailableReg _ _ = Nothing

assign_reg_to_stk reg@('F':_) sp
   = text "ASSIGN_FLT(Sp+" <> int sp <> comma <> text reg <> text ");"
assign_reg_to_stk reg@('D':_) sp
   = text "ASSIGN_DBL(Sp+" <> int sp <> comma <> text reg <> text ");"
assign_reg_to_stk reg@('L':_) sp
   = text "ASSIGN_Word64(Sp+" <> int sp <> comma <> text reg <> text ");"
assign_reg_to_stk reg sp
   = text "Sp[" <> int sp <> text "] = " <> text reg <> semi

assign_stk_to_reg reg@('F':_) sp
   = text reg <> text " = "  <> text "PK_FLT(Sp+" <> int sp <> text ");"
assign_stk_to_reg reg@('D':_) sp
   = text reg <> text " = "  <> text "PK_DBL(Sp+" <> int sp <> text ");"
assign_stk_to_reg reg@('L':_) sp
   = text reg <> text " = "  <> text "PK_Word64(Sp+" <> int sp <> text ");"
assign_stk_to_reg reg sp
   = text reg <> text " = Sp[" <> int sp <> text "];"


-- make a ptr/non-ptr bitmap from a list of argument types
mkBitmap :: [ArgRep] -> Word32
mkBitmap args = foldr f 0 args
 where f arg bm | isPtr arg = bm `shiftL` 1
		| otherwise = (bm `shiftL` size) .|. ((1 `shiftL` size) - 1)
	        where size = argSize arg

-- -----------------------------------------------------------------------------
-- Generating the application functions

-- A SUBTLE POINT about stg_ap functions (can't think of a better
-- place to put this comment --SDM):
--
-- The entry convention to an stg_ap_ function is as follows: all the
-- arguments are on the stack (we might revisit this at some point,
-- but it doesn't make any difference on x86), and THERE IS AN EXTRA
-- EMPTY STACK SLOT at the top of the stack.  
--
-- Why?  Because in several cases, stg_ap_* will need an extra stack
-- slot, eg. to push a return address in the THUNK case, and this is a
-- way of pushing the stack check up into the caller which is probably
-- doing one anyway.  Allocating the extra stack slot in the caller is
-- also probably free, because it will be adjusting Sp after pushing
-- the args anyway (this might not be true of register-rich machines
-- when we start passing args to stg_ap_* in regs).

mkApplyRetName args
  = text "stg_ap_" <> text (map showArg args) <> text "_ret"

mkApplyInfoName args
  = text "stg_ap_" <> text (map showArg args) <> text "_info"

genMkPAP macro jump stack_apply is_pap args all_args_size fun_info_label
  =  smaller_arity_cases
  $$ exact_arity_case
  $$ larger_arity_case
	
  where
    n_args = length args

    -- offset of args on the stack, see large comment above.
    arg_sp_offset = 1

-- The SMALLER ARITY cases:
--	if (arity == 1) {
--	    Sp[0] = Sp[1];
--	    Sp[1] = (W_)&stg_ap_1_info;
--	    JMP_(GET_ENTRY(R1.cl));

    smaller_arity_cases = vcat [ smaller_arity i | i <- [1..n_args-1] ]

    smaller_arity arity
        =  text "if (arity == " <> int arity <> text ") {" $$
	   let
	     (reg_doc, sp')
		| stack_apply = (empty, arg_sp_offset)
		| otherwise   = loadRegArgs arg_sp_offset these_args
	   in
           nest 4 (vcat [
	     reg_doc,
	     vcat [ shuffle_down j | j <- [sp'..these_args_size] ],
	     text "Sp[" <> int these_args_size <>  text "] = (W_)&" <>
                mkApplyInfoName rest_args <> semi,
	     text "Sp += " <> int (sp' -  1) <> semi,
		-- for a PAP, we have to arrange that the stack contains a
		-- return address in the even that stg_PAP_entry fails its
		-- heap check.  See stg_PAP_entry in Apply.hc for details.
	     if is_pap 
		then text "R2.w = (W_)&" <> mkApplyInfoName these_args <> semi
		else empty,
	     text "JMP_" <> parens (text jump) <> semi
	    ]) $$
 	   text "}"
	where
		(these_args, rest_args) = splitAt arity args
	        these_args_size = sum (map argSize these_args)
		
		shuffle_down i = 
		  text "Sp[" <> int (i-1) <> text "] = Sp["
		     <> int i <> text "];"

-- The EXACT ARITY case
--
--	if (arity == 1) {
--	    Sp++;
--	    JMP_(GET_ENTRY(R1.cl));

    exact_arity_case 
	= text "if (arity == " <> int n_args <> text ") {" $$
	  let
	     (reg_doc, sp')
		| stack_apply = (empty, arg_sp_offset)
		| otherwise   = loadRegArgs arg_sp_offset args
	  in
	  nest 4 (vcat [
	    reg_doc,
	    text "Sp += " <> int sp' <> semi,
	    if is_pap 
		then text "R2.w = (W_)&" <> fun_info_label <> semi
		else empty,
	    text "JMP_" <> parens (text jump) <> semi
	  ])

-- The LARGER ARITY cases:
--
--	} else /* arity > 1 */ {
--	    BUILD_PAP(1,0,(W_)&stg_ap_v_info);
--	}

    larger_arity_case = 
	   text "} else {" $$
	   nest 4 (
		text macro <> char '(' <> int n_args <> comma <> 
					int all_args_size <>  
					text ",(W_)&" <> fun_info_label <>
					text ");"
	   ) $$
	   char '}'

-- -----------------------------------------------------------------------------
-- generate an apply function

-- args is a list of 'p', 'n', 'f', 'd' or 'l'

genApply args =
   let
    fun_ret_label  = mkApplyRetName args
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map argSize args)
   in
    vcat [
      text "INFO_TABLE_RET(" <> fun_info_label <> text "," <>
	fun_ret_label <> text "," <>
        text "MK_SMALL_BITMAP(" <> int all_args_size <> text "/*framsize*/," <>
	int (fromIntegral (mkBitmap args)) <> text "/*bitmap*/), " <>
        text "0,0,0,RET_SMALL,,EF_,0,0);",
      text "",
      text "F_ " <> fun_ret_label <> text "( void )\n{",
      nest 4 (vcat [
       text "StgInfoTable *info;",
       text "nat arity;",

--    if fast == 1:
--        print "static void *lbls[] ="
--        print "  { [FUN]             &&fun_lbl,"
--        print "    [FUN_1_0]         &&fun_lbl,"
--        print "    [FUN_0_1]	      &&fun_lbl,"
--        print "    [FUN_2_0]	      &&fun_lbl,"
--        print "    [FUN_1_1]	      &&fun_lbl,"
--        print "    [FUN_0_2]	      &&fun_lbl,"
--        print "    [FUN_STATIC]      &&fun_lbl,"
--        print "    [PAP]             &&pap_lbl,"
--        print "    [THUNK]           &&thunk_lbl,"
--        print "    [THUNK_1_0]	      &&thunk_lbl,"
--        print "    [THUNK_0_1]	      &&thunk_lbl,"
--        print "    [THUNK_2_0]	      &&thunk_lbl,"
--        print "    [THUNK_1_1]	      &&thunk_lbl,"
--        print "    [THUNK_0_2]	      &&thunk_lbl,"
--        print "    [THUNK_STATIC]    &&thunk_lbl,"
--        print "    [THUNK_SELECTOR]  &&thunk_lbl,"
--        print "    [IND]	      &&ind_lbl,"
--        print "    [IND_OLDGEN]      &&ind_lbl,"
--        print "    [IND_STATIC]      &&ind_lbl,"
--        print "    [IND_PERM]	      &&ind_lbl,"
--        print "    [IND_OLDGEN_PERM] &&ind_lbl"
--        print "  };"
    
       text "FB_",
       text "",
       text "IF_DEBUG(apply,fprintf(stderr, \"" <> fun_ret_label <> 
	  text "... \"); printClosure(R1.cl));",

       text "IF_DEBUG(sanity,checkStackFrame(Sp+" <> int (1 + all_args_size)
	<> text "));",

--       text "IF_DEBUG(sanity,checkStackChunk(Sp+" <> int (1 + all_args_size) <>
--	  text ", CurrentTSO->stack + CurrentTSO->stack_size));",
    
       text "TICK_SLOW_CALL(" <> int (length args) <> text ");",

       let do_assert [] _ = []
	   do_assert (arg:args) offset
		| isPtr arg = this : rest
		| otherwise = rest
	        where this = text "ASSERT(LOOKS_LIKE_CLOSURE_PTR(Sp[" 
				 <> int offset <> text "]));"
		      rest = do_assert args (offset + argSize arg)
       in
       vcat (do_assert args 1),
	 
       text  "again:",
       text  "info = get_itbl(R1.cl);",

--    if fast == 1:
--        print "    goto *lbls[info->type];";
--    else:
        text "switch (info->type) {" $$
	 nest 4 (vcat [

--    if fast == 1:
--        print "    bco_lbl:"
--    else:
	text "case BCO:",
	nest 4 (vcat [
	  text "arity = ((StgBCO *)R1.p)->arity;",
	  text "ASSERT(arity > 0);",
	  genMkPAP "BUILD_PAP" "stg_BCO_entry" 
		True{-stack apply-} False{-not a PAP-}
		args all_args_size fun_info_label
	 ]),

--    if fast == 1:
--        print "    fun_lbl:"
--    else:
        text "case FUN:",
        text "case FUN_1_0:",
        text "case FUN_0_1:",
        text "case FUN_2_0:",
        text "case FUN_1_1:",
        text "case FUN_0_2:",
        text "case FUN_STATIC:",
	nest 4 (vcat [
	  text "arity = itbl_to_fun_itbl(info)->arity;",
	  text "ASSERT(arity > 0);",
          genMkPAP "BUILD_PAP" "GET_ENTRY(R1.cl)" 
		False{-reg apply-} False{-not a PAP-}
		args all_args_size fun_info_label
	 ]),

--    if fast == 1:
--        print "    pap_lbl:"
--    else:

	text "case PAP:",
	nest 4 (vcat [
 	  text "arity = ((StgPAP *)R1.p)->arity;",
	  text "ASSERT(arity > 0);",
	  genMkPAP "NEW_PAP" "stg_PAP_entry" 
		True{-stack apply-} True{-is a PAP-}
		args all_args_size fun_info_label
	 ]),

	text "",

--    if fast == 1:
--        print "    thunk_lbl:"
--    else:
	text "case AP:",
	text "case AP_STACK:",
	text "case CAF_BLACKHOLE:",
	text "case BLACKHOLE:",
	text "case BLACKHOLE_BQ:",
	text "case SE_BLACKHOLE:",
	text "case SE_CAF_BLACKHOLE:",
        text "case THUNK:",
        text "case THUNK_1_0:",
        text "case THUNK_0_1:",
        text "case THUNK_2_0:",
        text "case THUNK_1_1:",
        text "case THUNK_0_2:",
        text "case THUNK_STATIC:",
        text "case THUNK_SELECTOR:",
	nest 4 (vcat [
	  text "Sp[0] = (W_)&" <> fun_info_label <> text ";",
	  text "JMP_(GET_ENTRY(R1.cl));",
	  text ""
	 ]),

--    if fast == 1:
--        print "    ind_lbl:"
--    else:
        text "case IND:",
        text "case IND_OLDGEN:",
        text "case IND_STATIC:",
        text "case IND_PERM:",
        text "case IND_OLDGEN_PERM:",
	nest 4 (vcat [
	  text "R1.cl = ((StgInd *)R1.p)->indirectee;",
	  text "goto again;"
	 ]),
	text "",

--    if fast == 0:

       text "default:",
       nest 4 (
         text "barf(\"" <> fun_ret_label <> text "\");"
       ),
       text "}"
	
	])
      ]),
      text "FE_",
      text "}"
    ]

-- -----------------------------------------------------------------------------
-- Making a stack apply

-- These little functions are like slow entry points.  They provide
-- the layer between the PAP entry code and the function's fast entry
-- point: namely they load arguments off the stack into registers (if
-- available) and jump to the function's entry code.
--
-- On entry: R1 points to the function closure
--	     arguments are on the stack starting at Sp
--
-- Invariant: the list of arguments never contains void.  Since we're only
-- interested in loading arguments off the stack here, we can ignore
-- void arguments.

mkStackApplyEntryLabel:: [ArgRep] -> Doc
mkStackApplyEntryLabel args = text "stg_ap_stk_" <> text (map showArg args)

genStackApply :: [ArgRep] -> Doc
genStackApply args = 
  let fn_entry_label = mkStackApplyEntryLabel args in
  vcat [
    text "IF_" <> parens fn_entry_label,
    text "{",
    nest 4 (text "FB_" $$ body $$ text "FE_"),
    text "}"
   ]
 where
   (assign_regs, sp') = loadRegArgs 0 args
   body = vcat [assign_regs,
		text "Sp += " <> int sp' <> semi,
		text "JMP_(GET_ENTRY(R1.cl));"
		]

-- -----------------------------------------------------------------------------
-- Stack save entry points.
--
-- These code fragments are used to save registers on the stack at a heap
-- check failure in the entry code for a function.  We also have to save R1
-- and the return address (stg_gc_fun_info) on the stack.  See stg_gc_fun_gen
-- in HeapStackCheck.hc for more details.

mkStackSaveEntryLabel :: [ArgRep] -> Doc
mkStackSaveEntryLabel args = text "stg_stk_save_" <> text (map showArg args)

genStackSave :: [ArgRep] -> Doc
genStackSave args =
  let fn_entry_label= mkStackSaveEntryLabel args in
  vcat [
    text "IF_" <> parens fn_entry_label,
    text "{",
    nest 4 (text "FB_" $$ body $$ text "FE_"),
    text "}"
   ]
 where
   body = vcat [text "Sp -= " <> int sp_offset <> semi,
		vcat (map (uncurry assign_reg_to_stk) reg_locs),
		text "Sp[2] = R1.w;",
		text "Sp[1] =" <+> int (sp_offset - std_frame_size) <> semi,
		text "Sp[0] = (W_)&stg_gc_fun_info;",
		text "JMP_(stg_gc_noregs);"
		]

   std_frame_size = 3 -- the std bits of the frame. See StgRetFun in Closures.h,
		      -- and the comment on stg_fun_gc_gen in HeapStackCheck.hc.
   (reg_locs, sp_offset) = assignRegs std_frame_size args

-- -----------------------------------------------------------------------------
-- The prologue...

main = putStr (render the_code)
  where the_code = vcat [
		text "// DO NOT EDIT!",
		text "// Automatically generated by GenApply.hs",
		text "",
		text "#include \"Stg.h\"",
		text "#include \"Rts.h\"",
		text "#include \"RtsFlags.h\"",
		text "#include \"Storage.h\"",
		text "#include \"RtsUtils.h\"",
		text "#include \"Printer.h\"",
		text "#include \"Sanity.h\"",
		text "#include \"Apply.h\"",
		text "",
		text "#include <stdio.h>",

		vcat (intersperse (text "") $ map genApply applyTypes),
		vcat (intersperse (text "") $ map genStackFns stackApplyTypes),

		genStackApplyArray stackApplyTypes,
		genStackSaveArray stackApplyTypes,
		genBitmapArray stackApplyTypes,

		text ""  -- add a newline at the end of the file
	    ]

-- These have been shown to cover about 99% of cases in practice...
applyTypes = [
	[V],
	[F],
	[D],
	[L],
	[N],
	[P],
	[P,V],
	[P,P],
	[P,P,V],
	[P,P,P],
	[P,P,P,P],
	[P,P,P,P,P],
	[P,P,P,P,P,P],
	[P,P,P,P,P,P,P]
   ]

-- No need for V args in the stack apply cases.
-- ToDo: the stack apply and stack save code doesn't make a distinction
-- between N and P (they both live in the same register), only the bitmap
-- changes, so we could share the apply/save code between lots of cases.
stackApplyTypes = [
	[N],
	[P],
	[F],
	[D],
	[L],
	[N,N],
	[N,P],
	[P,N],
	[P,P],
	[N,N,N],
	[N,N,P],
	[N,P,N],
	[N,P,P],
	[P,N,N],
	[P,N,P],
	[P,P,N],
	[P,P,P],
	[P,P,P,P],
	[P,P,P,P,P],
	[P,P,P,P,P,P],
	[P,P,P,P,P,P,P],
	[P,P,P,P,P,P,P,P]
   ]

genStackFns args = genStackApply args $$ genStackSave args


genStackApplyArray types =
  text "StgFun *stg_ap_stack_entries[] = {" $$  
  vcat (map arr_ent types) $$
  text "};"
 where
  arr_ent ty = brackets (arg_const ty) <+> mkStackApplyEntryLabel ty <> comma

genStackSaveArray types =
  text "StgFun *stg_stack_save_entries[] = {" $$  
  vcat (map arr_ent types) $$
  text "};"
 where
  arr_ent ty = brackets (arg_const ty) <+> mkStackSaveEntryLabel ty <> comma

genBitmapArray :: [[ArgRep]] -> Doc
genBitmapArray types =
  vcat [
    text "StgWord stg_arg_bitmaps[] = {",
    vcat (map gen_bitmap types),
    text "};"
  ]
  where
   gen_bitmap ty = brackets (arg_const ty) <+> 
		   text "MK_SMALL_BITMAP" <> parens (
			int (sum (map argSize ty)) <> comma <>
			text (show (mkBitmap ty))) <>
		   comma

arg_const ty = text "ARG_" <> text (map toUpper (map showArg ty))

