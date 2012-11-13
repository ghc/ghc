
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | Evaluation of 32 bit values.
module SPARC.CodeGen.Gen32 (
	getSomeReg,
	getRegister
)

where

import SPARC.CodeGen.CondCode
import SPARC.CodeGen.Amode
import SPARC.CodeGen.Gen64
import SPARC.CodeGen.Base
import SPARC.Stack
import SPARC.Instr
import SPARC.Cond
import SPARC.AddrMode
import SPARC.Imm
import SPARC.Regs
import SPARC.Base
import NCGMonad
import Size
import Reg

import Cmm

import Control.Monad (liftM)
import DynFlags
import OrdList
import Outputable

-- | The dual to getAnyReg: compute an expression into a register, but
-- 	we don't mind which one it is.
getSomeReg :: CmmExpr -> NatM (Reg, InstrBlock)
getSomeReg expr = do
  r <- getRegister expr
  case r of
    Any rep code -> do
	tmp <- getNewRegNat rep
	return (tmp, code tmp)
    Fixed _ reg code -> 
	return (reg, code)



-- | Make code to evaluate a 32 bit expression.
--
getRegister :: CmmExpr -> NatM Register

getRegister (CmmReg reg) 
  = do dflags <- getDynFlags
       let platform = targetPlatform dflags
       return (Fixed (cmmTypeSize (cmmRegType dflags reg))
                     (getRegisterReg platform reg) nilOL)

getRegister tree@(CmmRegOff _ _) 
  = do dflags <- getDynFlags
       getRegister (mangleIndexTree dflags tree)

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


-- Load a literal float into a float register.
--	The actual literal is stored in a new data area, and we load it 
--	at runtime.
getRegister (CmmLit (CmmFloat f W32)) = do

    -- a label for the new data area
    lbl <- getNewLabelNat
    tmp <- getNewRegNat II32

    let code dst = toOL [
            -- the data area         
	    LDATA ReadOnlyData $ Statics lbl
			 [CmmStaticLit (CmmFloat f W32)],

            -- load the literal
	    SETHI (HI (ImmCLbl lbl)) tmp,
	    LD II32 (AddrRegImm tmp (LO (ImmCLbl lbl))) dst] 

    return (Any FF32 code)

getRegister (CmmLit (CmmFloat d W64)) = do
    lbl <- getNewLabelNat
    tmp <- getNewRegNat II32
    let code dst = toOL [
	    LDATA ReadOnlyData $ Statics lbl
			 [CmmStaticLit (CmmFloat d W64)],
	    SETHI (HI (ImmCLbl lbl)) tmp,
	    LD II64 (AddrRegImm tmp (LO (ImmCLbl lbl))) dst] 
    return (Any FF64 code)


-- Unary machine ops
getRegister (CmmMachOp mop [x])
  = case mop of
	-- Floating point negation -------------------------
	MO_F_Neg W32		-> trivialUFCode FF32 (FNEG FF32) x
	MO_F_Neg W64		-> trivialUFCode FF64 (FNEG FF64) x


	-- Integer negation --------------------------------
	MO_S_Neg rep		-> trivialUCode (intSize rep) (SUB False False g0) x
	MO_Not rep		-> trivialUCode (intSize rep) (XNOR False g0) x


	-- Float word size conversion ----------------------
	MO_FF_Conv W64 W32	-> coerceDbl2Flt x
	MO_FF_Conv W32 W64	-> coerceFlt2Dbl x


	-- Float <-> Signed Int conversion -----------------
	MO_FS_Conv from to 	-> coerceFP2Int from to x
	MO_SF_Conv from to 	-> coerceInt2FP from to x


	-- Unsigned integer word size conversions ----------

	-- If it's the same size, then nothing needs to be done.
	MO_UU_Conv from to
	 | from == to    	-> conversionNop (intSize to)  x

	-- To narrow an unsigned word, mask out the high bits to simulate what would 
	--	happen if we copied the value into a smaller register.
	MO_UU_Conv W16 W8	-> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
	MO_UU_Conv W32 W8	-> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))

	-- for narrowing 32 bit to 16 bit, don't use a literal mask value like the W16->W8
	--	case because the only way we can load it is via SETHI, which needs 2 ops.
	--	Do some shifts to chop out the high bits instead.
	MO_UU_Conv W32 W16	
	 -> do	tmpReg		<- getNewRegNat II32
		(xReg, xCode)	<- getSomeReg x
	 	let code dst
			=  	xCode
			`appOL` toOL
				[ SLL xReg   (RIImm $ ImmInt 16) tmpReg
				, SRL tmpReg (RIImm $ ImmInt 16) dst]
				
		return	$ Any II32 code
			
		--	 trivialCode W16 (AND False) x (CmmLit (CmmInt 65535 W16))

	-- To widen an unsigned word we don't have to do anything.
	--	Just leave it in the same register and mark the result as the new size.
	MO_UU_Conv W8  W16	-> conversionNop (intSize W16)  x
	MO_UU_Conv W8  W32	-> conversionNop (intSize W32)  x
	MO_UU_Conv W16 W32	-> conversionNop (intSize W32)  x


	-- Signed integer word size conversions ------------

	-- Mask out high bits when narrowing them
	MO_SS_Conv W16 W8	-> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
	MO_SS_Conv W32 W8	-> trivialCode W8  (AND False) x (CmmLit (CmmInt 255 W8))
	MO_SS_Conv W32 W16	-> trivialCode W16 (AND False) x (CmmLit (CmmInt 65535 W16))

	-- Sign extend signed words when widening them.
	MO_SS_Conv W8  W16	-> integerExtend W8  W16 x
	MO_SS_Conv W8  W32	-> integerExtend W8  W32 x
	MO_SS_Conv W16 W32	-> integerExtend W16 W32 x

	_ 		  	-> panic ("Unknown unary mach op: " ++ show mop)


-- Binary machine ops
getRegister (CmmMachOp mop [x, y]) 
  = case mop of
      MO_Eq _ 		-> condIntReg EQQ x y
      MO_Ne _ 		-> condIntReg NE x y

      MO_S_Gt _		-> condIntReg GTT x y
      MO_S_Ge _		-> condIntReg GE x y
      MO_S_Lt _		-> condIntReg LTT x y
      MO_S_Le _		-> condIntReg LE x y
	      
      MO_U_Gt W32  	-> condIntReg GU  x y
      MO_U_Ge W32  	-> condIntReg GEU x y
      MO_U_Lt W32  	-> condIntReg LU  x y
      MO_U_Le W32  	-> condIntReg LEU x y

      MO_U_Gt W16 	-> condIntReg GU  x y
      MO_U_Ge W16	-> condIntReg GEU x y
      MO_U_Lt W16 	-> condIntReg LU  x y
      MO_U_Le W16 	-> condIntReg LEU x y

      MO_Add W32 	-> trivialCode W32 (ADD False False) x y
      MO_Sub W32 	-> trivialCode W32 (SUB False False) x y

      MO_S_MulMayOflo rep -> imulMayOflo rep x y

      MO_S_Quot W32 	-> idiv True  False x y
      MO_U_Quot W32 	-> idiv False False x y
       
      MO_S_Rem  W32	-> irem True  x y
      MO_U_Rem	W32	-> irem False x y
       
      MO_F_Eq _ 	-> condFltReg EQQ x y
      MO_F_Ne _ 	-> condFltReg NE x y

      MO_F_Gt _ 	-> condFltReg GTT x y
      MO_F_Ge _ 	-> condFltReg GE x y 
      MO_F_Lt _ 	-> condFltReg LTT x y
      MO_F_Le _ 	-> condFltReg LE x y

      MO_F_Add  w	-> trivialFCode w FADD x y
      MO_F_Sub  w  	-> trivialFCode w FSUB x y
      MO_F_Mul  w  	-> trivialFCode w FMUL x y
      MO_F_Quot w  	-> trivialFCode w FDIV x y

      MO_And rep   	-> trivialCode rep (AND False) x y
      MO_Or  rep    	-> trivialCode rep (OR  False) x y
      MO_Xor rep   	-> trivialCode rep (XOR False) x y

      MO_Mul rep 	-> trivialCode rep (SMUL False) x y

      MO_Shl rep   	-> trivialCode rep SLL  x y
      MO_U_Shr rep   	-> trivialCode rep SRL x y
      MO_S_Shr rep   	-> trivialCode rep SRA x y

      _			-> pprPanic "getRegister(sparc) - binary CmmMachOp (1)" (pprMachOp mop)
  where


getRegister (CmmLoad mem pk) = do
    Amode src code <- getAmode mem
    let
	code__2 dst 	= code `snocOL` LD (cmmTypeSize pk) src dst
    return (Any (cmmTypeSize pk) code__2)

getRegister (CmmLit (CmmInt i _))
  | fits13Bits i
  = let
    	src = ImmInt (fromInteger i)
    	code dst = unitOL (OR False g0 (RIImm src) dst)
    in
    	return (Any II32 code)

getRegister (CmmLit lit)
  = let imm = litToImm lit
    	code dst = toOL [
    	    SETHI (HI imm) dst,
    	    OR False dst (RIImm (LO imm)) dst]
    in return (Any II32 code)


getRegister _
	= panic "SPARC.CodeGen.Gen32.getRegister: no match"


-- | sign extend and widen
integerExtend 
	:: Width 		-- ^ width of source expression
	-> Width 		-- ^ width of result
	-> CmmExpr 		-- ^ source expression
	-> NatM Register	

integerExtend from to expr
 = do	-- load the expr into some register
 	(reg, e_code) 	<- getSomeReg expr
	tmp		<- getNewRegNat II32
	let bitCount
		= case (from, to) of
			(W8,  W32)	-> 24
			(W16, W32)	-> 16
			(W8,  W16)	-> 24
			_		-> panic "SPARC.CodeGen.Gen32: no match"
 	let code dst
		= e_code 	

		-- local shift word left to load the sign bit
		`snocOL`  SLL reg (RIImm (ImmInt bitCount)) tmp
			
		-- arithmetic shift right to sign extend
		`snocOL`  SRA tmp (RIImm (ImmInt bitCount)) dst
			
	return (Any (intSize to) code)
				

-- | For nop word format conversions we set the resulting value to have the
--	required size, but don't need to generate any actual code.
--
conversionNop
	:: Size -> CmmExpr -> NatM Register

conversionNop new_rep expr
 = do	e_code <- getRegister expr
	return (setSizeOfRegister e_code new_rep)



-- | Generate an integer division instruction.
idiv :: Bool -> Bool -> CmmExpr -> CmmExpr -> NatM Register
	
-- For unsigned division with a 32 bit numerator, 
--		we can just clear the Y register.
idiv False cc x y 
 = do
	(a_reg, a_code)		<- getSomeReg x
       	(b_reg, b_code)		<- getSomeReg y
	
	let code dst
		= 	a_code 
		`appOL`	b_code  
		`appOL`	toOL
			[ WRY  g0 g0
			, UDIV cc a_reg (RIReg b_reg) dst]
			
	return (Any II32 code)
    	

-- For _signed_ division with a 32 bit numerator,
--		we have to sign extend the numerator into the Y register.
idiv True cc x y 
 = do
	(a_reg, a_code)		<- getSomeReg x
       	(b_reg, b_code)		<- getSomeReg y
	
	tmp			<- getNewRegNat II32
	
	let code dst
		= 	a_code 
		`appOL`	b_code  
		`appOL`	toOL
			[ SRA  a_reg (RIImm (ImmInt 16)) tmp		-- sign extend
			, SRA  tmp   (RIImm (ImmInt 16)) tmp

			, WRY  tmp g0				
			, SDIV cc a_reg (RIReg b_reg) dst]
			
	return (Any II32 code)


-- | Do an integer remainder.
--
--	 NOTE:	The SPARC v8 architecture manual says that integer division
--		instructions _may_ generate a remainder, depending on the implementation.
--		If so it is _recommended_ that the remainder is placed in the Y register.
--
--          The UltraSparc 2007 manual says Y is _undefined_ after division.
--
--		The SPARC T2 doesn't store the remainder, not sure about the others. 
--		It's probably best not to worry about it, and just generate our own
--		remainders. 
--
irem :: Bool -> CmmExpr -> CmmExpr -> NatM Register

-- For unsigned operands: 
--		Division is between a 64 bit numerator and a 32 bit denominator, 
--		so we still have to clear the Y register.
irem False x y 
 = do
    	(a_reg, a_code)	<- getSomeReg x
	(b_reg, b_code)	<- getSomeReg y

	tmp_reg		<- getNewRegNat II32

	let code dst
		= 	a_code
		`appOL`	b_code
		`appOL`	toOL
			[ WRY	g0 g0
			, UDIV  False         a_reg (RIReg b_reg) tmp_reg
			, UMUL  False       tmp_reg (RIReg b_reg) tmp_reg
			, SUB   False False   a_reg (RIReg tmp_reg) dst]
    
    	return	(Any II32 code)

    

-- For signed operands:
--		Make sure to sign extend into the Y register, or the remainder
--		will have the wrong sign when the numerator is negative.
--
--	TODO:	When sign extending, GCC only shifts the a_reg right by 17 bits,
--		not the full 32. Not sure why this is, something to do with overflow?
--		If anyone cares enough about the speed of signed remainder they
--		can work it out themselves (then tell me). -- BL 2009/01/20
irem True x y 
 = do
    	(a_reg, a_code)	<- getSomeReg x
	(b_reg, b_code)	<- getSomeReg y
	
	tmp1_reg	<- getNewRegNat II32
	tmp2_reg	<- getNewRegNat II32
		
	let code dst
		=	a_code
		`appOL`	b_code
		`appOL`	toOL
			[ SRA	a_reg      (RIImm (ImmInt 16)) tmp1_reg	-- sign extend
			, SRA	tmp1_reg   (RIImm (ImmInt 16)) tmp1_reg	-- sign extend
			, WRY	tmp1_reg g0

			, SDIV  False          a_reg (RIReg b_reg)    tmp2_reg	
			, SMUL  False       tmp2_reg (RIReg b_reg)    tmp2_reg
			, SUB   False False    a_reg (RIReg tmp2_reg) dst]
			
	return (Any II32 code)
   

imulMayOflo :: Width -> CmmExpr -> CmmExpr -> NatM Register
imulMayOflo rep a b 
 = do
	(a_reg, a_code) <- getSomeReg a
	(b_reg, b_code) <- getSomeReg b
	res_lo <- getNewRegNat II32
	res_hi <- getNewRegNat II32

	let shift_amt  = case rep of
			  W32 -> 31
			  W64 -> 63
			  _ -> panic "shift_amt"
	
	let code dst = a_code `appOL` b_code `appOL`
                       toOL [
                           SMUL False a_reg (RIReg b_reg) res_lo,
                           RDY res_hi,
                           SRA res_lo (RIImm (ImmInt shift_amt)) res_lo,
                           SUB False False res_lo (RIReg res_hi) dst
                        ]
	return (Any II32 code)


-- -----------------------------------------------------------------------------
-- 'trivial*Code': deal with trivial instructions

-- Trivial (dyadic: 'trivialCode', floating-point: 'trivialFCode',
-- unary: 'trivialUCode', unary fl-pt:'trivialUFCode') instructions.
-- Only look for constants on the right hand side, because that's
-- where the generic optimizer will have put them.

-- Similarly, for unary instructions, we don't have to worry about
-- matching an StInt as the argument, because genericOpt will already
-- have handled the constant-folding.

trivialCode
	:: Width
	-> (Reg -> RI -> Reg -> Instr)
	-> CmmExpr
	-> CmmExpr
	-> NatM Register
	
trivialCode _ instr x (CmmLit (CmmInt y _))
  | fits13Bits y
  = do
      (src1, code) <- getSomeReg x
      let
    	src2 = ImmInt (fromInteger y)
    	code__2 dst = code `snocOL` instr src1 (RIImm src2) dst
      return (Any II32 code__2)


trivialCode _ instr x y = do
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    let
    	code__2 dst = code1 `appOL` code2 `snocOL`
    	    	      instr src1 (RIReg src2) dst
    return (Any II32 code__2)


trivialFCode 
	:: Width
	-> (Size -> Reg -> Reg -> Reg -> Instr)
	-> CmmExpr
	-> CmmExpr
	-> NatM Register

trivialFCode pk instr x y = do
    dflags <- getDynFlags
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    tmp <- getNewRegNat FF64
    let
    	promote x = FxTOy FF32 FF64 x tmp

    	pk1   = cmmExprType dflags x
    	pk2   = cmmExprType dflags y

    	code__2 dst =
    	    	if pk1 `cmmEqType` pk2 then
    	            code1 `appOL` code2 `snocOL`
    	    	    instr (floatSize pk) src1 src2 dst
    	    	else if typeWidth pk1 == W32 then
    	    	    code1 `snocOL` promote src1 `appOL` code2 `snocOL`
    	    	    instr FF64 tmp src2 dst
    	    	else
    	    	    code1 `appOL` code2 `snocOL` promote src2 `snocOL`
    	    	    instr FF64 src1 tmp dst
    return (Any (cmmTypeSize $ if pk1 `cmmEqType` pk2 then pk1 else cmmFloat W64) 
    		code__2)



trivialUCode
	:: Size
	-> (RI -> Reg -> Instr)
	-> CmmExpr
	-> NatM Register
	
trivialUCode size instr x = do
    (src, code) <- getSomeReg x
    let
    	code__2 dst = code `snocOL` instr (RIReg src) dst
    return (Any size code__2)


trivialUFCode 
	:: Size
	-> (Reg -> Reg -> Instr)
	-> CmmExpr
	-> NatM Register 
	
trivialUFCode pk instr x = do
    (src, code) <- getSomeReg x
    let
    	code__2 dst = code `snocOL` instr src dst
    return (Any pk code__2)




-- Coercions -------------------------------------------------------------------

-- | Coerce a integer value to floating point
coerceInt2FP :: Width -> Width -> CmmExpr -> NatM Register
coerceInt2FP width1 width2 x = do
    (src, code) <- getSomeReg x
    let
    	code__2 dst = code `appOL` toOL [
    	    ST (intSize width1) src (spRel (-2)),
    	    LD (intSize width1) (spRel (-2)) dst,
    	    FxTOy (intSize width1) (floatSize width2) dst dst]
    return (Any (floatSize $ width2) code__2)



-- | Coerce a floating point value to integer
--
--   NOTE: On sparc v9 there are no instructions to move a value from an
--	   FP register directly to an int register, so we have to use a load/store.
--
coerceFP2Int :: Width -> Width -> CmmExpr -> NatM Register
coerceFP2Int width1 width2 x 
 = do	let fsize1	= floatSize width1
	    fsize2	= floatSize width2
	
            isize2	= intSize   width2

	(fsrc, code)	<- getSomeReg x
	fdst		<- getNewRegNat fsize2
    
	let code2 dst	
		= 	code
		`appOL` toOL
			-- convert float to int format, leaving it in a float reg.
			[ FxTOy fsize1 isize2 fsrc fdst

			-- store the int into mem, then load it back to move
			--	it into an actual int reg.
			, ST    fsize2 fdst (spRel (-2))
			, LD	isize2 (spRel (-2)) dst]

	return (Any isize2 code2)


-- | Coerce a double precision floating point value to single precision.
coerceDbl2Flt :: CmmExpr -> NatM Register
coerceDbl2Flt x = do
    (src, code) <- getSomeReg x
    return (Any FF32 (\dst -> code `snocOL` FxTOy FF64 FF32 src dst)) 


-- | Coerce a single precision floating point value to double precision
coerceFlt2Dbl :: CmmExpr -> NatM Register
coerceFlt2Dbl x = do
    (src, code) <- getSomeReg x
    return (Any FF64 (\dst -> code `snocOL` FxTOy FF32 FF64 src dst))




-- Condition Codes -------------------------------------------------------------
--
-- Evaluate a comparision, and get the result into a register.
-- 
-- Do not fill the delay slots here. you will confuse the register allocator.
--
condIntReg :: Cond -> CmmExpr -> CmmExpr -> NatM Register
condIntReg EQQ x (CmmLit (CmmInt 0 _)) = do
    (src, code) <- getSomeReg x
    let
	code__2 dst = code `appOL` toOL [
    	    SUB False True g0 (RIReg src) g0,
    	    SUB True False g0 (RIImm (ImmInt (-1))) dst]
    return (Any II32 code__2)

condIntReg EQQ x y = do
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    let
    	code__2 dst = code1 `appOL` code2 `appOL` toOL [
    	    XOR False src1 (RIReg src2) dst,
    	    SUB False True g0 (RIReg dst) g0,
    	    SUB True False g0 (RIImm (ImmInt (-1))) dst]
    return (Any II32 code__2)

condIntReg NE x (CmmLit (CmmInt 0 _)) = do
    (src, code) <- getSomeReg x
    let
    	code__2 dst = code `appOL` toOL [
    	    SUB False True g0 (RIReg src) g0,
    	    ADD True False g0 (RIImm (ImmInt 0)) dst]
    return (Any II32 code__2)

condIntReg NE x y = do
    (src1, code1) <- getSomeReg x
    (src2, code2) <- getSomeReg y
    let
	code__2 dst = code1 `appOL` code2 `appOL` toOL [
    	    XOR False src1 (RIReg src2) dst,
    	    SUB False True g0 (RIReg dst) g0,
    	    ADD True False g0 (RIImm (ImmInt 0)) dst]
    return (Any II32 code__2)

condIntReg cond x y = do
    bid1 <- liftM (\a -> seq a a) getBlockIdNat
    bid2 <- liftM (\a -> seq a a) getBlockIdNat
    CondCode _ cond cond_code <- condIntCode cond x y
    let
	code__2 dst 
	 =	cond_code 
	  `appOL` toOL 
		[ BI cond False bid1
		, NOP

		, OR False g0 (RIImm (ImmInt 0)) dst
		, BI ALWAYS False bid2
		, NOP

		, NEWBLOCK bid1
		, OR False g0 (RIImm (ImmInt 1)) dst
		, BI ALWAYS False bid2
		, NOP

		, NEWBLOCK bid2]

    return (Any II32 code__2)


condFltReg :: Cond -> CmmExpr -> CmmExpr -> NatM Register
condFltReg cond x y = do
    bid1 <- liftM (\a -> seq a a) getBlockIdNat
    bid2 <- liftM (\a -> seq a a) getBlockIdNat

    CondCode _ cond cond_code <- condFltCode cond x y
    let
    	code__2 dst 
	 = 	cond_code 
	  `appOL` toOL 
	  	[ NOP
		, BF cond False bid1
		, NOP

		, OR False g0 (RIImm (ImmInt 0)) dst
		, BI ALWAYS False bid2
		, NOP

		, NEWBLOCK bid1
		, OR False g0 (RIImm (ImmInt 1)) dst
		, BI ALWAYS False bid2
		, NOP

		, NEWBLOCK bid2 ]

    return (Any II32 code__2)




