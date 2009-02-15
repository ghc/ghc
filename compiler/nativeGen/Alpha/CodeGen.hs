module Alpha.CodeGen ()

where

{-

getRegister :: CmmExpr -> NatM Register

#if !x86_64_TARGET_ARCH
    -- on x86_64, we have %rip for PicBaseReg, but it's not a full-featured
    -- register, it can only be used for rip-relative addressing.
getRegister (CmmReg (CmmGlobal PicBaseReg))
  = do
      reg <- getPicBaseNat wordSize
      return (Fixed wordSize reg nilOL)
#endif

getRegister (CmmReg reg) 
  = return (Fixed (cmmTypeSize (cmmRegType reg)) 
		  (getRegisterReg reg) nilOL)

getRegister tree@(CmmRegOff _ _) 
  = getRegister (mangleIndexTree tree)


#if WORD_SIZE_IN_BITS==32
    -- for 32-bit architectuers, support some 64 -> 32 bit conversions:
    -- TO_W_(x), TO_W_(x >> 32)

getRegister (CmmMachOp (MO_UU_Conv W64 W32)
             [CmmMachOp (MO_U_Shr W64) [x,CmmLit (CmmInt 32 _)]]) = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 (getHiVRegFromLo rlo) code

getRegister (CmmMachOp (MO_SS_Conv W64 W32)
             [CmmMachOp (MO_U_Shr W64) [x,CmmLit (CmmInt 32 _)]]) = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 (getHiVRegFromLo rlo) code

getRegister (CmmMachOp (MO_UU_Conv W64 W32) [x]) = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 rlo code

getRegister (CmmMachOp (MO_SS_Conv W64 W32) [x]) = do
  ChildCode64 code rlo <- iselExpr64 x
  return $ Fixed II32 rlo code       

#endif

-- end of machine-"independent" bit; here we go on the rest...


getRegister (StDouble d)
  = getBlockIdNat 	    	    `thenNat` \ lbl ->
    getNewRegNat PtrRep    	    `thenNat` \ tmp ->
    let code dst = mkSeqInstrs [
	    LDATA RoDataSegment lbl [
		    DATA TF [ImmLab (rational d)]
		],
	    LDA tmp (AddrImm (ImmCLbl lbl)),
	    LD TF dst (AddrReg tmp)]
    in
    	return (Any FF64 code)

getRegister (StPrim primop [x]) -- unary PrimOps
  = case primop of
      IntNegOp -> trivialUCode (NEG Q False) x

      NotOp    -> trivialUCode NOT x

      FloatNegOp  -> trivialUFCode FloatRep (FNEG TF) x
      DoubleNegOp -> trivialUFCode FF64 (FNEG TF) x

      OrdOp -> coerceIntCode IntRep x
      ChrOp -> chrCode x

      Float2IntOp  -> coerceFP2Int    x
      Int2FloatOp  -> coerceInt2FP pr x
      Double2IntOp -> coerceFP2Int    x
      Int2DoubleOp -> coerceInt2FP pr x

      Double2FloatOp -> coerceFltCode x
      Float2DoubleOp -> coerceFltCode x

      other_op -> getRegister (StCall fn CCallConv FF64 [x])
	where
	  fn = case other_op of
		 FloatExpOp    -> fsLit "exp"
		 FloatLogOp    -> fsLit "log"
		 FloatSqrtOp   -> fsLit "sqrt"
		 FloatSinOp    -> fsLit "sin"
		 FloatCosOp    -> fsLit "cos"
		 FloatTanOp    -> fsLit "tan"
		 FloatAsinOp   -> fsLit "asin"
		 FloatAcosOp   -> fsLit "acos"
		 FloatAtanOp   -> fsLit "atan"
		 FloatSinhOp   -> fsLit "sinh"
		 FloatCoshOp   -> fsLit "cosh"
		 FloatTanhOp   -> fsLit "tanh"
		 DoubleExpOp   -> fsLit "exp"
		 DoubleLogOp   -> fsLit "log"
		 DoubleSqrtOp  -> fsLit "sqrt"
		 DoubleSinOp   -> fsLit "sin"
		 DoubleCosOp   -> fsLit "cos"
		 DoubleTanOp   -> fsLit "tan"
		 DoubleAsinOp  -> fsLit "asin"
		 DoubleAcosOp  -> fsLit "acos"
		 DoubleAtanOp  -> fsLit "atan"
		 DoubleSinhOp  -> fsLit "sinh"
		 DoubleCoshOp  -> fsLit "cosh"
		 DoubleTanhOp  -> fsLit "tanh"
  where
    pr = panic "MachCode.getRegister: no primrep needed for Alpha"

getRegister (StPrim primop [x, y]) -- dyadic PrimOps
  = case primop of
      CharGtOp -> trivialCode (CMP LTT) y x
      CharGeOp -> trivialCode (CMP LE) y x
      CharEqOp -> trivialCode (CMP EQQ) x y
      CharNeOp -> int_NE_code x y
      CharLtOp -> trivialCode (CMP LTT) x y
      CharLeOp -> trivialCode (CMP LE) x y

      IntGtOp  -> trivialCode (CMP LTT) y x
      IntGeOp  -> trivialCode (CMP LE) y x
      IntEqOp  -> trivialCode (CMP EQQ) x y
      IntNeOp  -> int_NE_code x y
      IntLtOp  -> trivialCode (CMP LTT) x y
      IntLeOp  -> trivialCode (CMP LE) x y

      WordGtOp -> trivialCode (CMP ULT) y x
      WordGeOp -> trivialCode (CMP ULE) x y
      WordEqOp -> trivialCode (CMP EQQ)  x y
      WordNeOp -> int_NE_code x y
      WordLtOp -> trivialCode (CMP ULT) x y
      WordLeOp -> trivialCode (CMP ULE) x y

      AddrGtOp -> trivialCode (CMP ULT) y x
      AddrGeOp -> trivialCode (CMP ULE) y x
      AddrEqOp -> trivialCode (CMP EQQ)  x y
      AddrNeOp -> int_NE_code x y
      AddrLtOp -> trivialCode (CMP ULT) x y
      AddrLeOp -> trivialCode (CMP ULE) x y
	
      FloatGtOp -> cmpF_code (FCMP TF LE) EQQ x y
      FloatGeOp -> cmpF_code (FCMP TF LTT) EQQ x y
      FloatEqOp -> cmpF_code (FCMP TF EQQ) NE x y
      FloatNeOp -> cmpF_code (FCMP TF EQQ) EQQ x y
      FloatLtOp -> cmpF_code (FCMP TF LTT) NE x y
      FloatLeOp -> cmpF_code (FCMP TF LE) NE x y

      DoubleGtOp -> cmpF_code (FCMP TF LE) EQQ x y
      DoubleGeOp -> cmpF_code (FCMP TF LTT) EQQ x y
      DoubleEqOp -> cmpF_code (FCMP TF EQQ) NE x y
      DoubleNeOp -> cmpF_code (FCMP TF EQQ) EQQ x y
      DoubleLtOp -> cmpF_code (FCMP TF LTT) NE x y
      DoubleLeOp -> cmpF_code (FCMP TF LE) NE x y

      IntAddOp  -> trivialCode (ADD Q False) x y
      IntSubOp  -> trivialCode (SUB Q False) x y
      IntMulOp  -> trivialCode (MUL Q False) x y
      IntQuotOp -> trivialCode (DIV Q False) x y
      IntRemOp  -> trivialCode (REM Q False) x y

      WordAddOp  -> trivialCode (ADD Q False) x y
      WordSubOp  -> trivialCode (SUB Q False) x y
      WordMulOp  -> trivialCode (MUL Q False) x y
      WordQuotOp -> trivialCode (DIV Q True) x y
      WordRemOp  -> trivialCode (REM Q True) x y

      FloatAddOp -> trivialFCode  W32 (FADD TF) x y
      FloatSubOp -> trivialFCode  W32 (FSUB TF) x y
      FloatMulOp -> trivialFCode  W32 (FMUL TF) x y
      FloatDivOp -> trivialFCode  W32 (FDIV TF) x y

      DoubleAddOp -> trivialFCode  W64 (FADD TF) x y
      DoubleSubOp -> trivialFCode  W64 (FSUB TF) x y
      DoubleMulOp -> trivialFCode  W64 (FMUL TF) x y
      DoubleDivOp -> trivialFCode  W64 (FDIV TF) x y

      AddrAddOp  -> trivialCode (ADD Q False) x y
      AddrSubOp  -> trivialCode (SUB Q False) x y
      AddrRemOp  -> trivialCode (REM Q True) x y

      AndOp  -> trivialCode AND x y
      OrOp   -> trivialCode OR  x y
      XorOp  -> trivialCode XOR x y
      SllOp  -> trivialCode SLL x y
      SrlOp  -> trivialCode SRL x y

      ISllOp -> trivialCode SLL x y -- was: panic "AlphaGen:isll"
      ISraOp -> trivialCode SRA x y -- was: panic "AlphaGen:isra"
      ISrlOp -> trivialCode SRL x y -- was: panic "AlphaGen:isrl"

      FloatPowerOp  -> getRegister (StCall (fsLit "pow") CCallConv FF64 [x,y])
      DoublePowerOp -> getRegister (StCall (fsLit "pow") CCallConv FF64 [x,y])
  where
    {- ------------------------------------------------------------
	Some bizarre special code for getting condition codes into
	registers.  Integer non-equality is a test for equality
	followed by an XOR with 1.  (Integer comparisons always set
	the result register to 0 or 1.)  Floating point comparisons of
	any kind leave the result in a floating point register, so we
	need to wrangle an integer register out of things.
    -}
    int_NE_code :: StixTree -> StixTree -> NatM Register

    int_NE_code x y
      = trivialCode (CMP EQQ) x y	`thenNat` \ register ->
	getNewRegNat IntRep		`thenNat` \ tmp ->
	let
	    code = registerCode register tmp
	    src  = registerName register tmp
	    code__2 dst = code . mkSeqInstr (XOR src (RIImm (ImmInt 1)) dst)
	in
	return (Any IntRep code__2)

    {- ------------------------------------------------------------
	Comments for int_NE_code also apply to cmpF_code
    -}
    cmpF_code
	:: (Reg -> Reg -> Reg -> Instr)
	-> Cond
	-> StixTree -> StixTree
	-> NatM Register

    cmpF_code instr cond x y
      = trivialFCode pr instr x y	`thenNat` \ register ->
	getNewRegNat FF64		`thenNat` \ tmp ->
	getBlockIdNat			`thenNat` \ lbl ->
	let
	    code = registerCode register tmp
	    result  = registerName register tmp

	    code__2 dst = code . mkSeqInstrs [
		OR zeroh (RIImm (ImmInt 1)) dst,
		BF cond  result (ImmCLbl lbl),
		OR zeroh (RIReg zeroh) dst,
		NEWBLOCK lbl]
	in
	return (Any IntRep code__2)
      where
	pr = panic "trivialU?FCode: does not use PrimRep on Alpha"
      ------------------------------------------------------------

getRegister (CmmLoad pk mem)
  = getAmode mem    	    	    `thenNat` \ amode ->
    let
    	code = amodeCode amode
    	src   = amodeAddr amode
    	size = primRepToSize pk
    	code__2 dst = code . mkSeqInstr (LD size dst src)
    in
    return (Any pk code__2)

getRegister (StInt i)
  | fits8Bits i
  = let
    	code dst = mkSeqInstr (OR zeroh (RIImm src) dst)
    in
    return (Any IntRep code)
  | otherwise
  = let
    	code dst = mkSeqInstr (LDI Q dst src)
    in
    return (Any IntRep code)
  where
    src = ImmInt (fromInteger i)

getRegister leaf
  | isJust imm
  = let
    	code dst = mkSeqInstr (LDA dst (AddrImm imm__2))
    in
    return (Any PtrRep code)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x


getAmode :: CmmExpr -> NatM Amode
getAmode tree@(CmmRegOff _ _) = getAmode (mangleIndexTree tree)

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#if alpha_TARGET_ARCH

getAmode (StPrim IntSubOp [x, StInt i])
  = getNewRegNat PtrRep		`thenNat` \ tmp ->
    getRegister x		`thenNat` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (-(fromInteger i))
    in
    return (Amode (AddrRegImm reg off) code)

getAmode (StPrim IntAddOp [x, StInt i])
  = getNewRegNat PtrRep		`thenNat` \ tmp ->
    getRegister x		`thenNat` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    	off  = ImmInt (fromInteger i)
    in
    return (Amode (AddrRegImm reg off) code)

getAmode leaf
  | isJust imm
  = return (Amode (AddrImm imm__2) id)
  where
    imm = maybeImm leaf
    imm__2 = case imm of Just x -> x

getAmode other
  = getNewRegNat PtrRep		`thenNat` \ tmp ->
    getRegister other		`thenNat` \ register ->
    let
    	code = registerCode register tmp
    	reg  = registerName register tmp
    in
    return (Amode (AddrReg reg) code)

#endif /* alpha_TARGET_ARCH */


-- -----------------------------------------------------------------------------
-- Generating assignments

-- Assignments are really at the heart of the whole code generation
-- business.  Almost all top-level nodes of any real importance are
-- assignments, which correspond to loads, stores, or register
-- transfers.  If we're really lucky, some of the register transfers
-- will go away, because we can use the destination register to
-- complete the code generation for the right hand side.  This only
-- fails when the right hand side is forced into a fixed register
-- (e.g. the result of a call).

assignMem_IntCode :: Size -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignReg_IntCode :: Size -> CmmReg  -> CmmExpr -> NatM InstrBlock

assignMem_FltCode :: Size -> CmmExpr -> CmmExpr -> NatM InstrBlock
assignReg_FltCode :: Size -> CmmReg  -> CmmExpr -> NatM InstrBlock


assignIntCode pk (CmmLoad dst _) src
  = getNewRegNat IntRep    	    `thenNat` \ tmp ->
    getAmode dst    	    	    `thenNat` \ amode ->
    getRegister src	     	    `thenNat` \ register ->
    let
    	code1   = amodeCode amode []
    	dst__2  = amodeAddr amode
    	code2   = registerCode register tmp []
    	src__2  = registerName register tmp
    	sz      = primRepToSize pk
    	code__2 = asmSeqThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    return code__2

assignIntCode pk dst src
  = getRegister dst	    	    	    `thenNat` \ register1 ->
    getRegister src	    	    	    `thenNat` \ register2 ->
    let
    	dst__2  = registerName register1 zeroh
    	code    = registerCode register2 dst__2
    	src__2  = registerName register2 dst__2
    	code__2 = if isFixed register2
		  then code . mkSeqInstr (OR src__2 (RIReg src__2) dst__2)
    	    	  else code
    in
    return code__2

assignFltCode pk (CmmLoad dst _) src
  = getNewRegNat pk        	    `thenNat` \ tmp ->
    getAmode dst    	    	    `thenNat` \ amode ->
    getRegister src	    	    	    `thenNat` \ register ->
    let
    	code1   = amodeCode amode []
    	dst__2  = amodeAddr amode
    	code2   = registerCode register tmp []
    	src__2  = registerName register tmp
    	sz      = primRepToSize pk
    	code__2 = asmSeqThen [code1, code2] . mkSeqInstr (ST sz src__2 dst__2)
    in
    return code__2

assignFltCode pk dst src
  = getRegister dst	    	    	    `thenNat` \ register1 ->
    getRegister src	    	    	    `thenNat` \ register2 ->
    let
    	dst__2  = registerName register1 zeroh
    	code    = registerCode register2 dst__2
    	src__2  = registerName register2 dst__2
    	code__2 = if isFixed register2
		  then code . mkSeqInstr (FMOV src__2 dst__2)
		  else code
    in
    return code__2


-- -----------------------------------------------------------------------------
-- Generating an non-local jump

-- (If applicable) Do not fill the delay slots here; you will confuse the
-- register allocator.

genJump :: CmmExpr{-the branch target-} -> NatM InstrBlock

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


genJump (CmmLabel lbl)
  | isAsmTemp lbl = returnInstr (BR target)
  | otherwise     = returnInstrs [LDA pv (AddrImm target), JMP zeroh (AddrReg pv) 0]
  where
    target = ImmCLbl lbl

genJump tree
  = getRegister tree	     	    `thenNat` \ register ->
    getNewRegNat PtrRep    	    `thenNat` \ tmp ->
    let
    	dst    = registerName register pv
    	code   = registerCode register pv
    	target = registerName register pv
    in
    if isFixed register then
	returnSeq code [OR dst (RIReg dst) pv, JMP zeroh (AddrReg pv) 0]
    else
    return (code . mkSeqInstr (JMP zeroh (AddrReg pv) 0))


-- -----------------------------------------------------------------------------
--  Unconditional branches

genBranch :: BlockId -> NatM InstrBlock

genBranch = return . toOL . mkBranchInstr


-- -----------------------------------------------------------------------------
--  Conditional jumps

{-
Conditional jumps are always to local labels, so we can use branch
instructions.  We peek at the arguments to decide what kind of
comparison to do.

ALPHA: For comparisons with 0, we're laughing, because we can just do
the desired conditional branch.

-}


genCondJump
    :: BlockId	    -- the branch target
    -> CmmExpr      -- the condition on which to branch
    -> NatM InstrBlock

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

genCondJump id (StPrim op [x, StInt 0])
  = getRegister x	  	    	    `thenNat` \ register ->
    getNewRegNat (registerRep register)
    	    	        	    `thenNat` \ tmp ->
    let
    	code   = registerCode register tmp
    	value  = registerName register tmp
    	pk     = registerRep register
	target = ImmCLbl lbl
    in
    returnSeq code [BI (cmpOp op) value target]
  where
    cmpOp CharGtOp = GTT
    cmpOp CharGeOp = GE
    cmpOp CharEqOp = EQQ
    cmpOp CharNeOp = NE
    cmpOp CharLtOp = LTT
    cmpOp CharLeOp = LE
    cmpOp IntGtOp = GTT
    cmpOp IntGeOp = GE
    cmpOp IntEqOp = EQQ
    cmpOp IntNeOp = NE
    cmpOp IntLtOp = LTT
    cmpOp IntLeOp = LE
    cmpOp WordGtOp = NE
    cmpOp WordGeOp = ALWAYS
    cmpOp WordEqOp = EQQ
    cmpOp WordNeOp = NE
    cmpOp WordLtOp = NEVER
    cmpOp WordLeOp = EQQ
    cmpOp AddrGtOp = NE
    cmpOp AddrGeOp = ALWAYS
    cmpOp AddrEqOp = EQQ
    cmpOp AddrNeOp = NE
    cmpOp AddrLtOp = NEVER
    cmpOp AddrLeOp = EQQ

genCondJump lbl (StPrim op [x, StDouble 0.0])
  = getRegister x	  	    	    `thenNat` \ register ->
    getNewRegNat (registerRep register)
    	    	        	    `thenNat` \ tmp ->
    let
    	code   = registerCode register tmp
    	value  = registerName register tmp
    	pk     = registerRep register
	target = ImmCLbl lbl
    in
    return (code . mkSeqInstr (BF (cmpOp op) value target))
  where
    cmpOp FloatGtOp = GTT
    cmpOp FloatGeOp = GE
    cmpOp FloatEqOp = EQQ
    cmpOp FloatNeOp = NE
    cmpOp FloatLtOp = LTT
    cmpOp FloatLeOp = LE
    cmpOp DoubleGtOp = GTT
    cmpOp DoubleGeOp = GE
    cmpOp DoubleEqOp = EQQ
    cmpOp DoubleNeOp = NE
    cmpOp DoubleLtOp = LTT
    cmpOp DoubleLeOp = LE

genCondJump lbl (StPrim op [x, y])
  | fltCmpOp op
  = trivialFCode pr instr x y 	    `thenNat` \ register ->
    getNewRegNat FF64    	    `thenNat` \ tmp ->
    let
    	code   = registerCode register tmp
    	result = registerName register tmp
	target = ImmCLbl lbl
    in
    return (code . mkSeqInstr (BF cond result target))
  where
    pr = panic "trivialU?FCode: does not use PrimRep on Alpha"

    fltCmpOp op = case op of
	FloatGtOp -> True
	FloatGeOp -> True
	FloatEqOp -> True
	FloatNeOp -> True
	FloatLtOp -> True
	FloatLeOp -> True
	DoubleGtOp -> True
	DoubleGeOp -> True
	DoubleEqOp -> True
	DoubleNeOp -> True
	DoubleLtOp -> True
	DoubleLeOp -> True
	_ -> False
    (instr, cond) = case op of
	FloatGtOp -> (FCMP TF LE, EQQ)
	FloatGeOp -> (FCMP TF LTT, EQQ)
	FloatEqOp -> (FCMP TF EQQ, NE)
	FloatNeOp -> (FCMP TF EQQ, EQQ)
	FloatLtOp -> (FCMP TF LTT, NE)
	FloatLeOp -> (FCMP TF LE, NE)
	DoubleGtOp -> (FCMP TF LE, EQQ)
	DoubleGeOp -> (FCMP TF LTT, EQQ)
	DoubleEqOp -> (FCMP TF EQQ, NE)
	DoubleNeOp -> (FCMP TF EQQ, EQQ)
	DoubleLtOp -> (FCMP TF LTT, NE)
	DoubleLeOp -> (FCMP TF LE, NE)

genCondJump lbl (StPrim op [x, y])
  = trivialCode instr x y    	    `thenNat` \ register ->
    getNewRegNat IntRep    	    `thenNat` \ tmp ->
    let
    	code   = registerCode register tmp
    	result = registerName register tmp
	target = ImmCLbl lbl
    in
    return (code . mkSeqInstr (BI cond result target))
  where
    (instr, cond) = case op of
	CharGtOp -> (CMP LE, EQQ)
	CharGeOp -> (CMP LTT, EQQ)
	CharEqOp -> (CMP EQQ, NE)
	CharNeOp -> (CMP EQQ, EQQ)
	CharLtOp -> (CMP LTT, NE)
	CharLeOp -> (CMP LE, NE)
	IntGtOp -> (CMP LE, EQQ)
	IntGeOp -> (CMP LTT, EQQ)
	IntEqOp -> (CMP EQQ, NE)
	IntNeOp -> (CMP EQQ, EQQ)
	IntLtOp -> (CMP LTT, NE)
	IntLeOp -> (CMP LE, NE)
	WordGtOp -> (CMP ULE, EQQ)
	WordGeOp -> (CMP ULT, EQQ)
	WordEqOp -> (CMP EQQ, NE)
	WordNeOp -> (CMP EQQ, EQQ)
	WordLtOp -> (CMP ULT, NE)
	WordLeOp -> (CMP ULE, NE)
	AddrGtOp -> (CMP ULE, EQQ)
	AddrGeOp -> (CMP ULT, EQQ)
	AddrEqOp -> (CMP EQQ, NE)
	AddrNeOp -> (CMP EQQ, EQQ)
	AddrLtOp -> (CMP ULT, NE)
	AddrLeOp -> (CMP ULE, NE)

-- -----------------------------------------------------------------------------
--  Generating C calls

-- Now the biggest nightmare---calls.  Most of the nastiness is buried in
-- @get_arg@, which moves the arguments to the correct registers/stack
-- locations.  Apart from that, the code is easy.
-- 
-- (If applicable) Do not fill the delay slots here; you will confuse the
-- register allocator.

genCCall
    :: CmmCallTarget		-- function to call
    -> HintedCmmFormals		-- where to put the result
    -> HintedCmmActuals		-- arguments (of mixed type)
    -> NatM InstrBlock

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ccallResultRegs = 

genCCall fn cconv result_regs args
  = mapAccumLNat get_arg (allArgRegs, eXTRA_STK_ARGS_HERE) args
    	    	  	  `thenNat` \ ((unused,_), argCode) ->
    let
    	nRegs = length allArgRegs - length unused
    	code = asmSeqThen (map ($ []) argCode)
    in
    	returnSeq code [
    	    LDA pv (AddrImm (ImmLab (ptext fn))),
    	    JSR ra (AddrReg pv) nRegs,
    	    LDGP gp (AddrReg ra)]
  where
    ------------------------
    {-	Try to get a value into a specific register (or registers) for
	a call.  The first 6 arguments go into the appropriate
	argument register (separate registers for integer and floating
	point arguments, but used in lock-step), and the remaining
	arguments are dumped to the stack, beginning at 0(sp).  Our
	first argument is a pair of the list of remaining argument
	registers to be assigned for this call and the next stack
	offset to use for overflowing arguments.  This way,
	@get_Arg@ can be applied to all of a call's arguments using
	@mapAccumLNat@.
    -}
    get_arg
	:: ([(Reg,Reg)], Int)	-- Argument registers and stack offset (accumulator)
	-> StixTree		-- Current argument
	-> NatM (([(Reg,Reg)],Int), InstrBlock) -- Updated accumulator and code

    -- We have to use up all of our argument registers first...

    get_arg ((iDst,fDst):dsts, offset) arg
      = getRegister arg	    	    	    `thenNat` \ register ->
	let
	    reg  = if isFloatType pk then fDst else iDst
	    code = registerCode register reg
	    src  = registerName register reg
	    pk   = registerRep register
	in
	return (
	    if isFloatType pk then
		((dsts, offset), if isFixed register then
		    code . mkSeqInstr (FMOV src fDst)
		    else code)
	    else
		((dsts, offset), if isFixed register then
		    code . mkSeqInstr (OR src (RIReg src) iDst)
		    else code))

    -- Once we have run out of argument registers, we move to the
    -- stack...

    get_arg ([], offset) arg
      = getRegister arg			`thenNat` \ register ->
	getNewRegNat (registerRep register)
					`thenNat` \ tmp ->
	let
	    code = registerCode register tmp
	    src  = registerName register tmp
	    pk   = registerRep register
	    sz   = primRepToSize pk
	in
	return (([], offset + 1), code . mkSeqInstr (ST sz src (spRel offset)))

trivialCode instr x (StInt y)
  | fits8Bits y
  = getRegister x		`thenNat` \ register ->
    getNewRegNat IntRep		`thenNat` \ tmp ->
    let
    	code = registerCode register tmp
    	src1 = registerName register tmp
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code . mkSeqInstr (instr src1 (RIImm src2) dst)
    in
    return (Any IntRep code__2)

trivialCode instr x y
  = getRegister x		`thenNat` \ register1 ->
    getRegister y		`thenNat` \ register2 ->
    getNewRegNat IntRep		`thenNat` \ tmp1 ->
    getNewRegNat IntRep		`thenNat` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1 []
    	src1  = registerName register1 tmp1
    	code2 = registerCode register2 tmp2 []
    	src2  = registerName register2 tmp2
    	code__2 dst = asmSeqThen [code1, code2] .
    	    	     mkSeqInstr (instr src1 (RIReg src2) dst)
    in
    return (Any IntRep code__2)

------------
trivialUCode instr x
  = getRegister x		`thenNat` \ register ->
    getNewRegNat IntRep		`thenNat` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr (RIReg src) dst)
    in
    return (Any IntRep code__2)

------------
trivialFCode _ instr x y
  = getRegister x		`thenNat` \ register1 ->
    getRegister y		`thenNat` \ register2 ->
    getNewRegNat FF64	`thenNat` \ tmp1 ->
    getNewRegNat FF64	`thenNat` \ tmp2 ->
    let
    	code1 = registerCode register1 tmp1
    	src1  = registerName register1 tmp1

    	code2 = registerCode register2 tmp2
    	src2  = registerName register2 tmp2

    	code__2 dst = asmSeqThen [code1 [], code2 []] .
    	    	      mkSeqInstr (instr src1 src2 dst)
    in
    return (Any FF64 code__2)

trivialUFCode _ instr x
  = getRegister x		`thenNat` \ register ->
    getNewRegNat FF64	`thenNat` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp
    	code__2 dst = code . mkSeqInstr (instr src dst)
    in
    return (Any FF64 code__2)

#if alpha_TARGET_ARCH

coerceInt2FP _ x
  = getRegister x		`thenNat` \ register ->
    getNewRegNat IntRep		`thenNat` \ reg ->
    let
    	code = registerCode register reg
    	src  = registerName register reg

    	code__2 dst = code . mkSeqInstrs [
    	    ST Q src (spRel 0),
    	    LD TF dst (spRel 0),
    	    CVTxy Q TF dst dst]
    in
    return (Any FF64 code__2)

-------------
coerceFP2Int x
  = getRegister x		`thenNat` \ register ->
    getNewRegNat FF64	`thenNat` \ tmp ->
    let
    	code = registerCode register tmp
    	src  = registerName register tmp

    	code__2 dst = code . mkSeqInstrs [
    	    CVTxy TF Q src tmp,
    	    ST TF tmp (spRel 0),
    	    LD Q dst (spRel 0)]
    in
    return (Any IntRep code__2)

#endif /* alpha_TARGET_ARCH */


-}





