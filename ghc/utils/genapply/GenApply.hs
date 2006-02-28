{-# OPTIONS -cpp #-}
module Main(main) where

#include "../../includes/ghcconfig.h"
#include "../../includes/MachRegs.h"
#include "../../includes/Constants.h"


#if __GLASGOW_HASKELL__ >= 504
import Text.PrettyPrint
import Data.Word
import Data.Bits
import Data.List	( intersperse )
import System.Exit
import System.Environment
import System.IO
#else
import System
import IO
import Bits
import Word
import Pretty
import List		( intersperse )
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

data RegStatus = Registerised | Unregisterised

type Reg = String

availableRegs :: RegStatus -> ([Reg],[Reg],[Reg],[Reg])
availableRegs Unregisterised = ([],[],[],[])
availableRegs Registerised =
  ( vanillaRegs MAX_REAL_VANILLA_REG,
    floatRegs   MAX_REAL_FLOAT_REG,
    doubleRegs  MAX_REAL_DOUBLE_REG,
    longRegs    MAX_REAL_LONG_REG
  )

vanillaRegs, floatRegs, doubleRegs, longRegs :: Int -> [Reg]
vanillaRegs n = [ "R" ++ show m | m <- [2..n] ]  -- never use R1
floatRegs   n = [ "F" ++ show m | m <- [1..n] ]
doubleRegs  n = [ "D" ++ show m | m <- [1..n] ]
longRegs    n = [ "L" ++ show m | m <- [1..n] ]

-- -----------------------------------------------------------------------------
-- Loading/saving register arguments to the stack

loadRegArgs :: RegStatus -> Int -> [ArgRep] -> (Doc,Int)
loadRegArgs regstatus sp args 
 = (loadRegOffs reg_locs, sp')
 where (reg_locs, _, sp') = assignRegs regstatus sp args

loadRegOffs :: [(Reg,Int)] -> Doc
loadRegOffs = vcat . map (uncurry assign_stk_to_reg)

saveRegOffs :: [(Reg,Int)] -> Doc
saveRegOffs = vcat . map (uncurry assign_reg_to_stk)

-- a bit like assignRegs in CgRetConv.lhs
assignRegs
	:: RegStatus		-- are we registerised?
	-> Int			-- Sp of first arg
	-> [ArgRep]		-- args
	-> ([(Reg,Int)],	-- regs and offsets to load
	    [ArgRep],		-- left-over args
	    Int)		-- Sp of left-over args
assignRegs regstatus sp args = assign sp args (availableRegs regstatus) []

assign sp [] regs doc = (doc, [], sp)
assign sp (V : args) regs doc = assign sp args regs doc
assign sp (arg : args) regs doc
 = case findAvailableReg arg regs of
    Just (reg, regs') -> assign (sp + argSize arg)  args regs' 
	    		    ((reg, sp) : doc)
    Nothing -> (doc, (arg:args), sp)

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

assign_reg_to_stk reg sp
   = loadSpWordOff (regRep reg) sp <> text " = " <> text reg <> semi

assign_stk_to_reg reg sp
   = text reg <> text " = " <> loadSpWordOff (regRep reg) sp <> semi

regRep ('F':_) = "F_"
regRep ('D':_) = "D_"
regRep ('L':_) = "L_"
regRep _       = "W_"

loadSpWordOff :: String -> Int -> Doc
loadSpWordOff rep off = text rep <> text "[Sp+WDS(" <> int off <> text ")]"

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

mkApplyName args
  = text "stg_ap_" <> text (map showArg args)

mkApplyRetName args
  = mkApplyName args <> text "_ret"

mkApplyFastName args
  = mkApplyName args <> text "_fast"

mkApplyInfoName args
  = mkApplyName args <> text "_info"

genMkPAP regstatus macro jump ticker disamb
	no_load_regs  	-- don't load argumnet regs before jumping
	args_in_regs  	-- arguments are already in regs
	is_pap args all_args_size fun_info_label
  =  smaller_arity_cases
  $$ exact_arity_case
  $$ larger_arity_case
	
  where
    n_args = length args

	-- offset of arguments on the stack at slow apply calls.
    stk_args_slow_offset = 1

    stk_args_offset
	| args_in_regs = 0
	| otherwise    = stk_args_slow_offset

-- The SMALLER ARITY cases:
--	if (arity == 1) {
--	    Sp[0] = Sp[1];
--	    Sp[1] = (W_)&stg_ap_1_info;
--	    JMP_(GET_ENTRY(R1.cl));
    smaller_arity_cases = vcat [ smaller_arity i | i <- [1..n_args-1] ]

    smaller_arity arity
        =  text "if (arity == " <> int arity <> text ") {" $$
           nest 4 (vcat [
	     text "TICK_SLOW_CALL_" <> text ticker <> text "_TOO_MANY();",

		-- load up regs for the call, if necessary
	     load_regs,

		-- If we have more args in registers than are required
		-- for the call, then we must save some on the stack,
		-- and set up the stack for the follow-up call.
		-- If the extra arguments are on the stack, then we must
		-- instead shuffle them down to make room for the info
		-- table for the follow-on call.
	     if overflow_regs
		then save_extra_regs
		else shuffle_extra_args,

		-- for a PAP, we have to arrange that the stack contains a
		-- return address in the even that stg_PAP_entry fails its
		-- heap check.  See stg_PAP_entry in Apply.hc for details.
	     if is_pap 
		then text "R2 = " <> mkApplyInfoName this_call_args <> semi

		else empty,
	     text "jump " <> text jump <> semi
	    ]) $$
 	   text "}"

	where
		-- offsets in case we need to save regs:
	     (reg_locs, _, _)
		= assignRegs regstatus stk_args_offset args

		-- register assignment for *this function call*
	     (reg_locs', reg_call_leftovers, reg_call_sp_stk_args) 
		= assignRegs regstatus stk_args_offset (take arity args)

	     load_regs
		| no_load_regs || args_in_regs = empty
		| otherwise		       = loadRegOffs reg_locs'

	     (this_call_args, rest_args) = splitAt arity args

		-- the offset of the stack args from initial Sp
	     sp_stk_args
		| args_in_regs = stk_args_offset
		| no_load_regs = stk_args_offset
		| otherwise    = reg_call_sp_stk_args

		-- the stack args themselves
	     this_call_stack_args
		| args_in_regs = reg_call_leftovers -- sp offsets are wrong
		| no_load_regs = this_call_args
		| otherwise    = reg_call_leftovers

	     stack_args_size = sum (map argSize this_call_stack_args)
		
	     overflow_regs = args_in_regs && length reg_locs > length reg_locs'

	     save_extra_regs
		= -- we have extra arguments in registers to save
		  let
	           extra_reg_locs = drop (length reg_locs') (reverse reg_locs)
		   adj_reg_locs = [ (reg, off - adj + 1) | 
				    (reg,off) <- extra_reg_locs ]
		   adj = case extra_reg_locs of
			   (reg, fst_off):_ -> fst_off
		   size = snd (last adj_reg_locs)
		   in
	           text "Sp_adj(" <> int (-size - 1) <> text ");" $$
		   saveRegOffs adj_reg_locs $$
	     	   loadSpWordOff "W_" 0 <> text " = " <>
		                mkApplyInfoName rest_args <> semi

	     shuffle_extra_args
		= vcat (map shuffle_down
		         [sp_stk_args .. sp_stk_args+stack_args_size-1]) $$
	     	  loadSpWordOff "W_" (sp_stk_args+stack_args_size-1)
			<> text " = "
	                <> mkApplyInfoName rest_args <> semi $$
	          text "Sp_adj(" <> int (sp_stk_args -  1) <> text ");"

	     shuffle_down i = 
		  loadSpWordOff "W_" (i-1) <> text " = " <>
		  loadSpWordOff "W_" i <> semi

-- The EXACT ARITY case
--
--	if (arity == 1) {
--	    Sp++;
--	    JMP_(GET_ENTRY(R1.cl));

    exact_arity_case 
	= text "if (arity == " <> int n_args <> text ") {" $$
	  let
	     (reg_doc, sp')
		| no_load_regs || args_in_regs = (empty, stk_args_offset)
		| otherwise    = loadRegArgs regstatus stk_args_offset args
	  in
	  nest 4 (vcat [
	    text "TICK_SLOW_CALL_" <> text ticker <> text "_CORRECT();",
	    reg_doc,
	    text "Sp_adj(" <> int sp' <> text ");",
	    if is_pap 
		then text "R2 = " <> fun_info_label <> semi
		else empty,
	    text "jump " <> text jump <> semi
	  ])

-- The LARGER ARITY cases:
--
--	} else /* arity > 1 */ {
--	    BUILD_PAP(1,0,(W_)&stg_ap_v_info);
--	}

    larger_arity_case = 
	   text "} else {" $$
	   let
	     save_regs
	 	| args_in_regs = 
		        text "Sp_adj(" <> int (-sp_offset) <> text ");" $$
			saveRegOffs  reg_locs
		| otherwise =
			empty
	   in
	   nest 4 (vcat [
	  	text "TICK_SLOW_CALL_" <> text ticker <> text "_TOO_FEW();",
		save_regs,
		text macro <> char '(' <> int n_args <> comma <> 
					int all_args_size <>  
					text "," <> fun_info_label <>
					text "," <> text disamb <>
					text ");"
	   ]) $$
	   char '}'
	where
	  -- offsets in case we need to save regs:
	  (reg_locs, leftovers, sp_offset) 
		= assignRegs regstatus stk_args_slow_offset args
		-- BUILD_PAP assumes args start at offset 1

-- -----------------------------------------------------------------------------
-- generate an apply function

-- args is a list of 'p', 'n', 'f', 'd' or 'l'

genApply regstatus args =
   let
    fun_ret_label  = mkApplyRetName args
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map argSize args)
   in
    vcat [
      text "INFO_TABLE_RET(" <> mkApplyName args <> text ", " <>
        int all_args_size <> text "/*framsize*/," <>
	int (fromIntegral (mkBitmap args)) <> text "/*bitmap*/, " <>
        text "RET_SMALL)\n{",
      nest 4 (vcat [
       text "W_ info;",
       text "W_ arity;",

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
    
       text "",
       text "IF_DEBUG(apply,foreign \"C\" debugBelch(\"" <> fun_ret_label <> 
	  text "... \"); foreign \"C\" printClosure(R1 \"ptr\"));",

       text "IF_DEBUG(sanity,foreign \"C\" checkStackFrame(Sp+WDS(" <> int (1 + all_args_size)
	<> text ")\"ptr\"));",

--       text "IF_DEBUG(sanity,checkStackChunk(Sp+" <> int (1 + all_args_size) <>
--	  text ", CurrentTSO->stack + CurrentTSO->stack_size));",
    
       text "TICK_SLOW_CALL(" <> int (length args) <> text ");",

       let do_assert [] _ = []
	   do_assert (arg:args) offset
		| isPtr arg = this : rest
		| otherwise = rest
	        where this = text "ASSERT(LOOKS_LIKE_CLOSURE_PTR(Sp(" 
				 <> int offset <> text ")));"
		      rest = do_assert args (offset + argSize arg)
       in
       vcat (do_assert args 1),

       text  "again:",
       text  "info = %GET_STD_INFO(R1);",

--    if fast == 1:
--        print "    goto *lbls[info->type];";
--    else:
        text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (%INFO_TYPE(info)) {",
	nest 4 (vcat [

--    if fast == 1:
--        print "    bco_lbl:"
--    else:
	text "case BCO: {",
	nest 4 (vcat [
	  text "arity = TO_W_(StgBCO_arity(R1));",
	  text "ASSERT(arity > 0);",
	  genMkPAP regstatus "BUILD_PAP" "ENTRY_LBL(stg_BCO)" "FUN" "BCO"
		True{-stack apply-} False{-args on stack-} False{-not a PAP-}
		args all_args_size fun_info_label
	 ]),
	text "}",

--    if fast == 1:
--        print "    fun_lbl:"
--    else:
        text "case FUN,",
        text "     FUN_1_0,",
        text "     FUN_0_1,",
        text "     FUN_2_0,",
        text "     FUN_1_1,",
        text "     FUN_0_2,",
        text "     FUN_STATIC: {",
	nest 4 (vcat [
	  text "arity = TO_W_(StgFunInfoExtra_arity(%GET_FUN_INFO(R1)));",
	  text "ASSERT(arity > 0);",
          genMkPAP regstatus "BUILD_PAP" "%GET_ENTRY(R1)" "FUN" "FUN"
		False{-reg apply-} False{-args on stack-} False{-not a PAP-}
		args all_args_size fun_info_label
	 ]),
	text "}",

--    if fast == 1:
--        print "    pap_lbl:"
--    else:

	text "case PAP: {",
	nest 4 (vcat [
 	  text "arity = TO_W_(StgPAP_arity(R1));",
	  text "ASSERT(arity > 0);",
	  genMkPAP regstatus "NEW_PAP" "ENTRY_LBL(stg_PAP)" "PAP" "PAP"
		True{-stack apply-} False{-args on stack-} True{-is a PAP-}
		args all_args_size fun_info_label
	 ]),
	text "}",

	text "",

--    if fast == 1:
--        print "    thunk_lbl:"
--    else:
	text "case AP,",
	text "     AP_STACK,",
	text "     CAF_BLACKHOLE,",
	text "     BLACKHOLE,",
	text "     SE_BLACKHOLE,",
	text "     SE_CAF_BLACKHOLE,",
        text "     THUNK,",
        text "     THUNK_1_0,",
        text "     THUNK_0_1,",
        text "     THUNK_2_0,",
        text "     THUNK_1_1,",
        text "     THUNK_0_2,",
        text "     THUNK_STATIC,",
        text "     THUNK_SELECTOR: {",
	nest 4 (vcat [
          text "TICK_SLOW_CALL_UNEVALD(" <> int (length args) <> text ");",
	  text "Sp(0) = " <> fun_info_label <> text ";",
	  text "jump %GET_ENTRY(R1);",
	  text ""
	 ]),
	text "}",

--    if fast == 1:
--        print "    ind_lbl:"
--    else:
        text "case IND,",
        text "     IND_OLDGEN,",
        text "     IND_STATIC,",
        text "     IND_PERM,",
        text "     IND_OLDGEN_PERM: {",
	nest 4 (vcat [
	  text "R1 = StgInd_indirectee(R1);",
	  text "goto again;"
	 ]),
	text "}",
	text "",

--    if fast == 0:

       text "default: {",
       nest 4 (
         text "foreign \"C\" barf(\"" <> fun_ret_label <> text "\");"
       ),
       text "}"
	
	]),
       text "}"
      ]),
      text "}"
    ]

-- -----------------------------------------------------------------------------
-- Making a fast unknown application, args are in regs

genApplyFast regstatus args =
   let
    fun_fast_label = mkApplyFastName args
    fun_ret_label  = text "RET_LBL" <> parens (mkApplyName args)
    fun_info_label = mkApplyInfoName args
    all_args_size  = sum (map argSize args)
   in
    vcat [
     fun_fast_label,
     char '{',
     nest 4 (vcat [     
        text "W_ info;",
        text "W_ arity;",
        text  "info = %GET_STD_INFO(R1);",
        text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (%INFO_TYPE(info)) {",
	nest 4 (vcat [
          text "case FUN,",
          text "     FUN_1_0,",
          text "     FUN_0_1,",
          text "     FUN_2_0,",
          text "     FUN_1_1,",
          text "     FUN_0_2,",
          text "     FUN_STATIC: {",
	  nest 4 (vcat [
	    text "arity = TO_W_(StgFunInfoExtra_arity(%GET_FUN_INFO(R1)));",
	    text "ASSERT(arity > 0);",
            genMkPAP regstatus "BUILD_PAP" "%GET_ENTRY(R1)" "FUN" "FUN"
	  	False{-reg apply-} True{-args in regs-} False{-not a PAP-}
	  	args all_args_size fun_info_label
	   ]),
	  char '}',
	  
	  text "default: {",
	  let
	     (reg_locs, leftovers, sp_offset) = assignRegs regstatus 1 args
		-- leave a one-word space on the top of the stack when
		-- calling the slow version
	  in
	  nest 4 (vcat [
	     text "Sp_adj" <> parens (int (-sp_offset)) <> semi,
	     saveRegOffs reg_locs,
	     text "jump" <+> fun_ret_label <> semi
	  ]),
	  char '}'
	]),
	char '}'
      ]),
     char '}'
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

genStackApply :: RegStatus -> [ArgRep] -> Doc
genStackApply regstatus args = 
  let fn_entry_label = mkStackApplyEntryLabel args in
  vcat [
    fn_entry_label,
    text "{", nest 4 body, text "}"
   ]
 where
   (assign_regs, sp') = loadRegArgs regstatus 0 args
   body = vcat [assign_regs,
		text "Sp_adj" <> parens (int sp') <> semi,
		text "jump %GET_ENTRY(R1);"
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

genStackSave :: RegStatus -> [ArgRep] -> Doc
genStackSave regstatus args =
  let fn_entry_label= mkStackSaveEntryLabel args in
  vcat [
    fn_entry_label,
    text "{", nest 4 body, text "}"
   ]
 where
   body = vcat [text "Sp_adj" <> parens (int (-sp_offset)) <> semi,
		saveRegOffs reg_locs,
		text "Sp(2) = R1;",
		text "Sp(1) =" <+> int stk_args <> semi,
		text "Sp(0) = stg_gc_fun_info;",
		text "jump stg_gc_noregs;"
		]

   std_frame_size = 3 -- the std bits of the frame. See StgRetFun in Closures.h,
		      -- and the comment on stg_fun_gc_gen in HeapStackCheck.hc.
   (reg_locs, leftovers, sp_offset) = assignRegs regstatus std_frame_size args

   -- number of words of arguments on the stack.
   stk_args = sum (map argSize leftovers) + sp_offset - std_frame_size

-- -----------------------------------------------------------------------------
-- The prologue...

main = do
  args <- getArgs
  regstatus <- case args of
		 [] -> return Registerised
		 ["-u"] -> return Unregisterised
		 _other -> do hPutStrLn stderr "syntax: genapply [-u]"
			      exitWith (ExitFailure 1)
  let the_code = vcat [
		text "// DO NOT EDIT!",
		text "// Automatically generated by GenApply.hs",
		text "",
		text "#include \"Cmm.h\"",
		text "#include \"AutoApply.h\"",
		text "",

		vcat (intersperse (text "") $ 
		   map (genApply regstatus) applyTypes),
		vcat (intersperse (text "") $ 
		   map (genStackFns regstatus) stackApplyTypes),

		vcat (intersperse (text "") $ 
		   map (genApplyFast regstatus) applyTypes),

		genStackApplyArray stackApplyTypes,
		genStackSaveArray stackApplyTypes,
		genBitmapArray stackApplyTypes,

		text ""  -- add a newline at the end of the file
	    ]
  -- in
  putStr (render the_code)

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
	[P,P,P,V],
	[P,P,P,P],
	[P,P,P,P,P],
	[P,P,P,P,P,P]
   ]

-- No need for V args in the stack apply cases.
-- ToDo: the stack apply and stack save code doesn't make a distinction
-- between N and P (they both live in the same register), only the bitmap
-- changes, so we could share the apply/save code between lots of cases.
stackApplyTypes = [
	[],
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

genStackFns regstatus args 
  =  genStackApply regstatus args
  $$ genStackSave regstatus args


genStackApplyArray types =
  vcat [
    text "section \"rodata\" {",
    text "stg_ap_stack_entries:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map arr_ent types),
    text "}"
  ]
 where
  arr_ent ty = text "W_" <+> mkStackApplyEntryLabel ty <> semi

genStackSaveArray types =
  vcat [
    text "section \"rodata\" {",
    text "stg_stack_save_entries:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map arr_ent types),
    text "}"
  ]
 where
  arr_ent ty = text "W_" <+> mkStackSaveEntryLabel ty <> semi

genBitmapArray :: [[ArgRep]] -> Doc
genBitmapArray types =
  vcat [
    text "section \"rodata\" {",
    text "stg_arg_bitmaps:",
    text "W_ 0; W_ 0; W_ 0;", -- ARG_GEN, ARG_GEN_BIG, ARG_BCO
    vcat (map gen_bitmap types),
    text "}"
  ]
  where
   gen_bitmap ty = text "W_" <+> int bitmap_val <> semi
	where bitmap_val = 
		(fromIntegral (mkBitmap ty) `shiftL` BITMAP_BITS_SHIFT)
		 .|. sum (map argSize ty)

